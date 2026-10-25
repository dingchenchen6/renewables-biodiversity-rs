# ============================================================
# Objective / 分析目标:
# Leave-one-site-out for the multispecies occupancy near x post effect
# in the 24 data-rich sites, to locate what drives the negative estimate.
# 对 24 个数据充足站点的多物种占域"近区 × 建设后"效应做逐站剔除,定位负效应的来源。
# Input / 输入: same as site_birds_did.R (SB_MIN_CELL = 5)
# Output / 输出: results/rigor_birds_occ_loso.csv
# Settings / 设置: msPGOcc, 3 chains x 6000 iterations (burn 2000, thin 4);
#   4 sites fitted in parallel. 3 条链,每条 6000 次迭代;4 个进程并行。
# ============================================================
suppressPackageStartupMessages({ library(data.table); library(spOccupancy); library(parallel) })
set.seed(42)
args <- commandArgs(trailingOnly = FALSE)
here <- dirname(normalizePath(sub("^--file=", "", args[grep("^--file=", args)])))
root <- dirname(here); res <- file.path(root, "results"); cache <- file.path(root, "cache", "birds")
el <- fread(file.path(res, "site_birds_eligibility_b.csv")); sites <- el[min_cell >= 5, site]
ck <- fread(file.path(cache, "site_checklists_b.csv.gz"))[site %in% sites & period != "transition"]
sp <- readRDS(file.path(cache, "site_species_b.rds"))
ck[, `:=`(near = as.integer(zone == "near"), post = as.integer(period == "post"), doy = yday(as.IDate(date)),
          dur_na = as.integer(is.na(duration_min) | duration_min <= 0))]
ck[, log_dur := fifelse(dur_na == 1, 0, log(pmin(pmax(duration_min, 1), 600)))]
fit_one <- function(drop) {
  set.seed(42)
  d <- if (is.na(drop)) copy(ck) else ck[site != drop]
  d[, unit := paste(site, zone, period, sep = "|")]
  dd <- d[, .SD[sample(.N, min(.N, 25L))], by = unit][, rep := seq_len(.N), by = unit]
  units <- unique(dd[, .(unit, site, near, post)])[order(unit)]
  s2 <- sp[event_id %in% dd$event_id]; s2 <- merge(s2, dd[, .(event_id, unit, rep)], by = "event_id")
  spp <- s2[, .(n = uniqueN(unit)), by = species][n >= 8][order(-n)][seq_len(min(.N, 120))]$species
  J <- nrow(units); K <- max(dd$rep); N <- length(spp)
  y <- array(NA_real_, c(N, J, K)); uix <- match(dd$unit, units$unit)
  for (r in seq_len(nrow(dd))) y[, uix[r], dd$rep[r]] <- 0
  h <- s2[species %in% spp]; y[cbind(match(h$species, spp), match(h$unit, units$unit), h$rep)] <- 1
  mk <- function(v) { m <- matrix(NA_real_, J, K); m[cbind(uix, dd$rep)] <- v; m }
  dc <- list(log_dur = mk(as.numeric(scale(dd$log_dur))), doy = mk(as.numeric(scale(dd$doy))), ebird = mk(as.numeric(dd$source == "ebird")))
  oc <- data.frame(near = units$near, post = units$post, site = as.integer(factor(units$site)))
  f <- msPGOcc(occ.formula = ~ near * post + (1 | site), det.formula = ~ log_dur + doy + I(doy^2) + ebird,
               data = list(y = y, occ.covs = oc, det.covs = dc), n.samples = 6000, n.burn = 2000, n.thin = 4,
               n.chains = 3, verbose = FALSE)
  b <- f$beta.comm.samples[, "near:post"]
  data.table(dropped = drop, mean = mean(b), lo = quantile(b, .025), hi = quantile(b, .975), p_neg = mean(b < 0),
             rhat = f$rhat$beta.comm[which(colnames(f$beta.comm.samples) == "near:post")], n_species = N)
}
out <- rbindlist(mclapply(c(NA, sites), fit_one, mc.cores = 4))
fwrite(out, file.path(res, "rigor_birds_occ_loso.csv")); print(out[, lapply(.SD, function(v) if (is.numeric(v)) signif(v, 3) else v)])
cat("done\n")
