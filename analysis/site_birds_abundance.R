# ============================================================
# Scientific question / 科学问题:
# Using abundance-based bird diversity (as in Zhang et al. 2026, Science)
# but with the data cleaning and effort corrections our comment called for,
# did diversity near PV sites (0-5 km) change after construction relative
# to 8-20 km at the same site and year?
# 采用 Science 文章的丰度型多样性指标,但按我们评论文章的要求清洗数据、校正努力,
# 建设后近区(0-5 km)相对同站同年远区(8-20 km)的多样性是否变化?
#
# Objective / 分析目标: three tiers on the same ring-BACI design
#   1. Science-style naive: raw per-checklist Shannon, no cleaning, no effort
#   2. Cleaned + effort-adjusted checklist Hill numbers (q = 1, 2), abundance
#   3. Coverage-standardised Hill numbers (q = 0, 1, 2) per site x zone x
#      period from incidence frequencies (iNEXT), robust to effort
#   同一近远区设计下三层:朴素 Shannon;清洗 + 努力校正的清单 Hill 数;覆盖度标准化 Hill 数
#
# Input data / 输入数据:
#   cache/birds/site_checklists_b.csv.gz, cache/birds/site_abundance_b.rds
#   (CBRC + eBird/GBIF, merged and deduplicated; record-level, not redistributed)
#   results/site_birds_eligibility_b.csv
# Cleaning / 数据清洗(与评论文章一致 / as in our comment):
#   a. merge same-day same-point checklists from different observers into one
#      survey; per-species count = max across observers (avoid double counting)
#      同日同点多人清单合并为一次调查,物种计数取最大值
#   b. drop surveys in which every species is recorded as 1 (presence-only
#      recording; counts carry no abundance information) - sensitivity keeps them
#      剔除所有计数为 1 的调查(仅记有无);敏感性版本保留
#   c. winsorise counts at each species' 99th percentile (flock outliers)
#   d. keep single-species surveys (H = 0 is a valid value; dropping on the
#      outcome would select the sample) 保留单物种调查(按因变量剔除即样本选择)
# Effort / 努力: log duration (missing indicator), log number of observers,
#   source, month; observer fixed effects as robustness; tier 3 standardises
#   sample coverage directly. 第 3 层直接标准化样本覆盖度。
# Expected output / 预期输出: results/site_birds_abundance_did.csv,
#   results/site_birds_abundance_coverage.csv, results/site_birds_abundance_cleaning.csv,
#   results/figures/site_birds_abundance.png|pdf
# Main packages / 主要包: data.table, fixest, iNEXT, ggplot2
# ============================================================
suppressPackageStartupMessages({ library(data.table); library(fixest); library(iNEXT); library(ggplot2) })
set.seed(42)
args <- commandArgs(trailingOnly = FALSE)
here <- dirname(normalizePath(sub("^--file=", "", args[grep("^--file=", args)])))
root <- dirname(here); res <- file.path(root, "results"); cache <- file.path(root, "cache", "birds")
MIN_CELL <- as.integer(Sys.getenv("SB_MIN_CELL", "3"))

el <- fread(file.path(res, "site_birds_eligibility_b.csv"))
ck <- fread(file.path(cache, "site_checklists_b.csv.gz"))[site %in% el[min_cell >= MIN_CELL, site]]
ab <- readRDS(file.path(cache, "site_abundance_b.rds"))[event_id %in% ck$event_id]
ab <- merge(ab, ck[, .(event_id, site, zone, period, e, year, duration_min)], by = "event_id")

hill <- function(n, q) {  # 样本 Hill 数 / sample Hill number of order q
  p <- n[n > 0] / sum(n); if (q == 1) exp(-sum(p * log(p))) else sum(p^q)^(1 / (1 - q))
}

# ---- 0. Science 式朴素版:原始清单 / naive per-checklist ----------------------
naive <- ab[, .(H = { p <- individual_count / sum(individual_count); -sum(p * log(p)) }), by = .(event_id, site, zone, period, e, year)]

# ---- 1. 清洗 / cleaning -------------------------------------------------------
log_clean <- list(raw_checklists = uniqueN(ab$event_id))
ab[, survey := paste(site, date, lon4, lat4, sep = "|")]
sv <- ab[, .(count = max(individual_count)), by = .(survey, species, site, zone, period, e, year, date)]
eff <- ab[, .(n_obs = uniqueN(observer_id), dur = max(duration_min, na.rm = TRUE), source = source[1],
              observer = observer_id[1]), by = survey]
eff[!is.finite(dur), dur := NA]
log_clean$surveys_after_merge <- uniqueN(sv$survey)
cap <- sv[, .(cap = quantile(count, 0.99)), by = species]
sv <- merge(sv, cap, by = "species"); sv[, count := pmin(count, cap)][, cap := NULL]
pres_only <- sv[, .(all1 = all(count == 1) & .N > 1), by = survey]
log_clean$presence_only_surveys <- sum(pres_only$all1)
build <- function(drop_presence_only) {
  keep <- if (drop_presence_only) pres_only[all1 == FALSE, survey] else pres_only$survey
  s <- sv[survey %in% keep]
  out <- s[, .(q1 = hill(count, 1), q2 = hill(count, 2), richness = .N, log_ind = log(sum(count))),
           by = .(survey, site, zone, period, e, year, date)]
  out <- merge(out, eff, by = "survey")
  out[, `:=`(near = as.integer(zone == "near"), post = as.integer(period == "post"),
             month = month(as.IDate(date)), dur_na = as.integer(is.na(dur) | dur <= 0))]
  out[, `:=`(log_dur = fifelse(dur_na == 1, 0, log(pmin(pmax(dur, 1), 600))), log_obs = log(n_obs))]
  out
}
cl <- build(TRUE); cl_keep <- build(FALSE)
log_clean$surveys_analysed <- uniqueN(cl$survey)
fwrite(as.data.table(log_clean), file.path(res, sprintf("site_birds_abundance_cleaning_min%d.csv", MIN_CELL)))
print(as.data.table(log_clean))

# ---- 2. 清单层面 DiD / survey-level DiD ---------------------------------------
fx <- "| site^zone + site^year + month + source"
fit <- function(y, d, ctrl = TRUE, obs_fe = FALSE) {
  rhs <- if (ctrl) "near:post + log_dur + dur_na + log_obs" else "near:post"
  fe <- if (obs_fe) paste(fx, "+ observer") else fx
  m <- feols(as.formula(paste(y, "~", rhs, fe)), data = d[period != "transition"], cluster = ~site)
  ct <- coeftable(m)["near:post", ]; data.table(estimate = ct[1], se = ct[2], p = ct[4], n = nobs(m))
}
naive[, `:=`(near = as.integer(zone == "near"), post = as.integer(period == "post"), month = month(as.IDate(year * 0 + 1)))]
naive <- merge(naive, unique(ab[, .(event_id, source, date)]), by = "event_id")
naive[, month := month(as.IDate(date))]
rows <- list(
  data.table(tier = "1 Science-style naive", outcome = "Shannon H (raw checklist)", fit("H", naive, ctrl = FALSE)),
  data.table(tier = "2 cleaned + effort", outcome = "Hill q=1 (exp Shannon)", fit("log(q1)", cl)),
  data.table(tier = "2 cleaned + effort", outcome = "Hill q=2 (inverse Simpson)", fit("log(q2)", cl)),
  data.table(tier = "2 cleaned + effort", outcome = "log total individuals", fit("log_ind", cl)),
  data.table(tier = "2 cleaned + effort + observer FE", outcome = "Hill q=1 (exp Shannon)", fit("log(q1)", cl, obs_fe = TRUE)),
  data.table(tier = "2 sensitivity: keep presence-only", outcome = "Hill q=1 (exp Shannon)", fit("log(q1)", cl_keep)))
did <- rbindlist(rows)
did[, `:=`(pct = fifelse(grepl("log", outcome) | tier != "1 Science-style naive", 100 * (exp(estimate) - 1), NA_real_),
           lo = estimate - 1.96 * se, hi = estimate + 1.96 * se, min_cell = MIN_CELL, n_sites = uniqueN(cl$site))]
did[tier == "1 Science-style naive", pct := NA]
print(did[, .(tier, outcome, est = signif(estimate, 3), lo = signif(lo, 3), hi = signif(hi, 3), p = signif(p, 2), pct = round(pct, 1), n)])
fwrite(did, file.path(res, sprintf("site_birds_abundance_did_min%d.csv", MIN_CELL)))

# ---- 3. 覆盖度标准化 Hill 数 / coverage-standardised Hill numbers --------------
inc <- cl[period != "transition", .(survey, site, zone, period)]
spm <- merge(sv[survey %in% inc$survey, .(survey, species)], inc, by = "survey")
units <- unique(inc[, .(site, zone, period)])
freq <- lapply(seq_len(nrow(units)), function(i) {
  u <- units[i]; ss <- inc[site == u$site & zone == u$zone & period == u$period, survey]
  f <- spm[survey %in% ss, .N, by = species]$N
  c(length(ss), sort(f, decreasing = TRUE))
})
names(freq) <- paste(units$site, units$zone, units$period, sep = "|")
ok <- sapply(freq, function(v) v[1] >= 3 & length(v) >= 3)
freq <- freq[ok]; units <- units[ok]
# 共同覆盖度:各单元外推到 2 倍样本量时覆盖度的最小值(Chao 推荐)/ common coverage
cov_ref <- min(sapply(freq, function(v) iNEXT:::Chat.Ind(v[-1], 2 * v[1]) |> tryCatch(error = function(e) NA)), na.rm = TRUE)
cov_ref <- max(min(cov_ref, 0.95), 0.6)
est <- estimateD(freq, q = c(0, 1, 2), datatype = "incidence_freq", base = "coverage", level = cov_ref, nboot = 0)
est <- as.data.table(est)
setnames(est, intersect(c("Assemblage", "Site"), names(est))[1], "unit")
val <- intersect(c("qD", "qTD"), names(est))[1]; ord <- intersect(c("Order.q", "order"), names(est))[1]
est <- est[, .(unit, q = get(ord), D = get(val))]
est[, c("site", "zone", "period") := tstrsplit(unit, "|", fixed = TRUE)]
est[, `:=`(near = as.integer(zone == "near"), post = as.integer(period == "post"))]
cov_rows <- rbindlist(lapply(c(0, 1, 2), function(qq) {
  d <- est[q == qq & is.finite(D) & D > 0]
  m <- feols(log(D) ~ near * post | site, data = d, cluster = ~site)  # 每站仅 4 个单元,用站点固定效应避免饱和 / site FE only (4 units per site)
  ct <- coeftable(m)["near:post", ]
  data.table(q = qq, estimate = ct[1], se = ct[2], p = ct[4], n_units = nrow(d), n_sites = uniqueN(d$site), coverage = cov_ref)
}))
cov_rows[, `:=`(pct = 100 * (exp(estimate) - 1), lo_pct = 100 * (exp(estimate - 1.96 * se) - 1),
                hi_pct = 100 * (exp(estimate + 1.96 * se) - 1), min_cell = MIN_CELL)]
print(cov_rows); fwrite(cov_rows, file.path(res, sprintf("site_birds_abundance_coverage_min%d.csv", MIN_CELL)))

# ---- 4. 图 / figure -----------------------------------------------------------
fd <- rbind(did[tier != "1 Science-style naive" , .(label = paste(tier, outcome, sep = ": "), pct = 100 * (exp(estimate) - 1),
                                                     lo = 100 * (exp(lo) - 1), hi = 100 * (exp(hi) - 1))],
            cov_rows[, .(label = paste0("3 coverage-standardised: Hill q=", q, " (C = ", round(coverage, 2), ")"),
                         pct, lo = lo_pct, hi = hi_pct)])
fd[, label := factor(label, levels = rev(label))]
fig <- ggplot(fd, aes(pct, label)) + geom_vline(xintercept = 0, linewidth = 0.3) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0, colour = "#1F6F78") + geom_point(colour = "#1F6F78", size = 2) +
  labs(x = "Near (0-5 km) x post-construction, % change (95% CI)", y = NULL,
       title = sprintf("%d data-rich PV sites; cleaned surveys", uniqueN(cl$site))) +
  theme_classic(base_size = 10, base_family = "Arial Unicode MS")
if (MIN_CELL == 3) {
  ggsave(file.path(res, "figures", "site_birds_abundance.png"), fig, width = 7.5, height = 3.6, dpi = 300)
  ggsave(file.path(res, "figures", "site_birds_abundance.pdf"), fig, width = 7.5, height = 3.6, device = cairo_pdf)
}
cat("done\n")
