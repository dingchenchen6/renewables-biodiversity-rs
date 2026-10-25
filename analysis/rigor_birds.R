# ============================================================
# Objective / 分析目标 (audit repairs for the site-level bird analyses / 鸟类部分的审查修补):
#   1. Few clusters (24-38 sites): wild cluster bootstrap with Webb weights;
#      restricted (WCR) p-value and unrestricted percentile-t (WCU) 95% CI
#      聚类少:Webb 权重野生聚类自助法,WCR 求 p 值、WCU 求置信区间
#   2. Leave-one-site-out range for the checklist-level DiD
#   3. Coverage-standardised Hill numbers at C = 0.5 / 0.6 / 0.7
# Input data / 输入数据: cache/birds/site_checklists_b.csv.gz, cache/birds/site_abundance_b.rds,
#   results/site_birds_eligibility_b.csv (record-level inputs are not redistributed)
# Expected output / 预期输出: results/rigor_birds_wcb.csv, results/rigor_birds_loso.csv,
#   results/rigor_birds_coverage.csv
# Main packages / 主要包: data.table, fixest, iNEXT
# ============================================================
suppressPackageStartupMessages({ library(data.table); library(fixest); library(iNEXT) })
set.seed(42)
args <- commandArgs(trailingOnly = FALSE)
here <- dirname(normalizePath(sub("^--file=", "", args[grep("^--file=", args)])))
root <- dirname(here); res <- file.path(root, "results"); cache <- file.path(root, "cache", "birds")
B <- 999
webb <- c(-sqrt(1.5), -1, -sqrt(0.5), sqrt(0.5), 1, sqrt(1.5))
el <- fread(file.path(res, "site_birds_eligibility_b.csv"))
ck <- fread(file.path(cache, "site_checklists_b.csv.gz"))
ab0 <- readRDS(file.path(cache, "site_abundance_b.rds"))
hill <- function(n, q) { p <- n[n > 0] / sum(n); if (q == 1) exp(-sum(p * log(p))) else sum(p^q)^(1 / (1 - q)) }

# 野生聚类自助法 / wild cluster bootstrap (WCR p + WCU percentile-t CI)
wcb <- function(d, y, ctrl, fe) {
  f_u <- as.formula(paste(y, "~ near:post", ctrl, "|", fe)); f_r <- as.formula(paste(y, "~ 1", ctrl, "|", fe))
  if (ctrl == "") f_r <- as.formula(paste(y, "~ 1 |", fe))
  mu <- feols(f_u, data = d, cluster = ~site); mr <- feols(f_r, data = d, cluster = ~site)
  d <- d[obs(mu)]; dr <- copy(d)
  b0 <- coef(mu)["near:post"]; s0 <- se(mu)["near:post"]; t0 <- b0 / s0
  fr <- fitted(mr)[match(obs(mu), obs(mr))]; er <- resid(mr)[match(obs(mu), obs(mr))]
  fu <- fitted(mu); eu <- resid(mu)
  sites <- unique(d$site); gi <- match(d$site, sites)
  tR <- tU <- numeric(B)
  for (b in seq_len(B)) {
    wv <- sample(webb, length(sites), replace = TRUE)[gi]
    d[, ystar := fr + er * wv]; m1 <- feols(as.formula(paste("ystar ~ near:post", ctrl, "|", fe)), data = d, cluster = ~site)
    tR[b] <- coef(m1)["near:post"] / se(m1)["near:post"]
    d[, ystar := fu + eu * wv]; m2 <- feols(as.formula(paste("ystar ~ near:post", ctrl, "|", fe)), data = d, cluster = ~site)
    tU[b] <- (coef(m2)["near:post"] - b0) / se(m2)["near:post"]
  }
  q <- quantile(abs(tU), 0.95, na.rm = TRUE)
  data.table(estimate = b0, se_cluster = s0, p_cluster = 2 * pnorm(-abs(t0)), p_wcr = mean(abs(tR) >= abs(t0), na.rm = TRUE),
             lo_wcu = b0 - q * s0, hi_wcu = b0 + q * s0, n_sites = length(sites), n_obs = nrow(d))
}

prep_ck <- function(sites) {
  d <- ck[site %in% sites & period != "transition"]
  d[, `:=`(near = as.integer(zone == "near"), post = as.integer(period == "post"), month = month(as.IDate(date)),
           dur_na = as.integer(is.na(duration_min) | duration_min <= 0))]
  d[, `:=`(log_dur = fifelse(dur_na == 1, 0, log(pmin(pmax(duration_min, 1), 600))), y = log(n_species))]
  d
}
prep_ab <- function(sites) {
  a <- ab0[event_id %in% ck[site %in% sites]$event_id]
  a <- merge(a, ck[, .(event_id, site, zone, period, duration_min)], by = "event_id")
  a[, survey := paste(site, date, lon4, lat4, sep = "|")]
  sv <- a[, .(count = max(individual_count)), by = .(survey, species, site, zone, period, date)]
  eff <- a[, .(n_obs = uniqueN(observer_id), dur = suppressWarnings(max(duration_min, na.rm = TRUE)), source = source[1]), by = survey]
  eff[!is.finite(dur), dur := NA]
  cap <- sv[, .(cap = quantile(count, 0.99)), by = species]; sv <- merge(sv, cap, by = "species")[, count := pmin(count, cap)]
  keep <- sv[, .(all1 = all(count == 1) & .N > 1), by = survey][all1 == FALSE, survey]
  s <- sv[survey %in% keep]
  out <- merge(s[, .(q1 = hill(count, 1)), by = .(survey, site, zone, period, date)], eff, by = "survey")
  out[, `:=`(near = as.integer(zone == "near"), post = as.integer(period == "post"), month = month(as.IDate(date)),
             dur_na = as.integer(is.na(dur) | dur <= 0))]
  out[, `:=`(log_dur = fifelse(dur_na == 1, 0, log(pmin(pmax(dur, 1), 600))), log_obs = log(n_obs), y = log(q1))]
  list(cl = out[period != "transition"], sv = sv[survey %in% keep], eff = eff)
}
FE <- "site^zone + site^year + month + source"
rows <- list(); loso <- list(); covr <- list()
for (mc in c(3, 5)) {
  sites <- el[min_cell >= mc, site]
  d <- prep_ck(sites)
  rows[[length(rows) + 1]] <- data.table(sample = sprintf("%d sites (>= %d per cell)", length(sites), mc),
                                         outcome = "log species per checklist", wcb(d, "y", "+ log_dur + dur_na", FE))
  for (s in sites) {
    m <- feols(y ~ near:post + log_dur + dur_na | site^zone + site^year + month + source, data = d[site != s], cluster = ~site)
    loso[[length(loso) + 1]] <- data.table(min_cell = mc, dropped = s, estimate = coef(m)["near:post"])
  }
  A <- prep_ab(sites)
  A$cl[, year := as.integer(substr(as.character(date), 1, 4))]
  rows[[length(rows) + 1]] <- data.table(sample = sprintf("%d sites (>= %d per cell)", length(sites), mc),
                                         outcome = "Hill q=1 (cleaned, effort-adjusted)", wcb(A$cl, "y", "+ log_dur + dur_na + log_obs", FE))
  # 覆盖度敏感性 / coverage sensitivity
  inc <- A$cl[, .(survey, site, zone, period)]
  spm <- merge(A$sv[survey %in% inc$survey, .(survey, species)], inc, by = "survey")
  units <- unique(inc[, .(site, zone, period)])
  freq <- lapply(seq_len(nrow(units)), function(i) { u <- units[i]; ss <- inc[site == u$site & zone == u$zone & period == u$period, survey]
    c(length(ss), sort(spm[survey %in% ss, .N, by = species]$N, decreasing = TRUE)) })
  names(freq) <- paste(units$site, units$zone, units$period, sep = "|")
  ok <- sapply(freq, function(v) v[1] >= 3 & length(v) >= 3); freq <- freq[ok]
  for (C in c(0.5, 0.6, 0.7)) {
    est <- as.data.table(estimateD(freq, q = c(0, 1, 2), datatype = "incidence_freq", base = "coverage", level = C, nboot = 0))
    setnames(est, intersect(c("Assemblage", "Site"), names(est))[1], "unit")
    val <- intersect(c("qD", "qTD"), names(est))[1]; ord <- intersect(c("Order.q", "order"), names(est))[1]
    est <- est[, .(unit, q = get(ord), D = get(val))]
    est[, c("site", "zone", "period") := tstrsplit(unit, "|", fixed = TRUE)]
    est[, `:=`(near = as.integer(zone == "near"), post = as.integer(period == "post"), year = 0L, month = 0L, source = "x", site = as.integer(site))]
    for (qq in c(0, 1, 2)) {
      dq <- est[q == qq & is.finite(D) & D > 0][, ly := log(D)]
      r <- wcb(dq, "ly", "+ near + post", "site")
      covr[[length(covr) + 1]] <- data.table(min_cell = mc, coverage = C, q = qq, r)
    }
  }
}
w <- rbindlist(rows); w[, `:=`(pct = 100 * (exp(estimate) - 1), lo_pct = 100 * (exp(lo_wcu) - 1), hi_pct = 100 * (exp(hi_wcu) - 1))]
lo <- rbindlist(loso); cv <- rbindlist(covr); cv[, `:=`(pct = 100 * (exp(estimate) - 1), lo_pct = 100 * (exp(lo_wcu) - 1), hi_pct = 100 * (exp(hi_wcu) - 1))]
fwrite(w, file.path(res, "rigor_birds_wcb.csv")); fwrite(lo, file.path(res, "rigor_birds_loso.csv")); fwrite(cv, file.path(res, "rigor_birds_coverage.csv"))
print(w[, lapply(.SD, function(v) if (is.numeric(v)) signif(v, 3) else v)])
print(lo[, .(min = 100 * (exp(min(estimate)) - 1), max = 100 * (exp(max(estimate)) - 1)), by = min_cell])
print(cv[, .(min_cell, coverage, q, pct = round(pct, 1), lo = round(lo_pct, 1), hi = round(hi_pct, 1), p_wcr = round(p_wcr, 2))])
cat("done\n")
