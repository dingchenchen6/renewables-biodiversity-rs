# ============================================================
# Scientific question / 科学问题:
# Is the community near x post occupancy effect a property of the data, or of
# which <= 25 checklists happen to be subsampled per site x zone x period unit?
# 群落层面"近区 × 建设后"占域效应,是数据本身的信号,还是取决于每个单元随机抽到哪 25 份清单?
#
# Why / 缘由: the leave-one-site-out refit of all 24 sites gave +0.09 (-0.19, 0.36)
#   while site_birds_did.R reported -0.41 (-0.75, -0.04) with the same model; the only
#   difference is the random subsample. 逐站剔除脚本里 24 站全样本重拟合为 +0.09,
#   主脚本为 -0.41,模型相同,差别只在随机子样本。
# Input data / 输入数据: results/site_birds_eligibility_b.csv, cache/birds/site_checklists_b.csv.gz,
#   cache/birds/site_species_b.rds (record-level inputs are not redistributed)
# Main workflow / 主要流程: for 8 seeds x {38 sites (cells >= 3), 24 sites (cells >= 5)}:
#   subsample <= 25 checklists per unit, fit msPGOcc (3 chains x 10,000, burn 4,000, thin 6)
#   对 8 个种子 × 两个样本:每单元重抽 <= 25 份清单,拟合 msPGOcc(链加长以改善收敛)
# Expected output / 预期输出: results/rigor_birds_occ_seeds.csv
# Key assumptions / 关键假设: same occupancy and detection formulas as site_birds_did.R
# Main packages / 主要包: data.table, spOccupancy, parallel
# Output directory / 输出路径: results/
# ============================================================
suppressPackageStartupMessages({ library(data.table); library(spOccupancy); library(parallel) })
args <- commandArgs(trailingOnly = FALSE)
here <- dirname(normalizePath(sub("^--file=", "", args[grep("^--file=", args)])))
root <- dirname(here); res <- file.path(root, "results"); cache <- file.path(root, "cache", "birds")
el <- fread(file.path(res, "site_birds_eligibility_b.csv"))
ck0 <- fread(file.path(cache, "site_checklists_b.csv.gz"))[site %in% el[min_cell >= 3, site] & period != "transition"]
sp <- readRDS(file.path(cache, "site_species_b.rds"))
ck0[, `:=`(near = as.integer(zone == "near"), post = as.integer(period == "post"), doy = yday(as.IDate(date)),
           dur_na = as.integer(is.na(duration_min) | duration_min <= 0))]
ck0[, log_dur := fifelse(dur_na == 1, 0, log(pmin(pmax(duration_min, 1), 600)))]
setorder(ck0, event_id)   # 固定行序,结果只随种子变化 / fixed row order so results vary only with the seed

fit_one <- function(min_cell, seed) {
  set.seed(seed)
  # 用 el$min_cell 避免与函数参数同名的列被 data.table 误读 / avoid column-argument name clash
  d <- ck0[site %in% el$site[el$min_cell >= min_cell]]
  d[, unit := paste(site, zone, period, sep = "|")]
  dd <- d[, .SD[sample(.N, min(.N, 25L))], by = unit][, rep := seq_len(.N), by = unit]
  units <- unique(dd[, .(unit, site, near, post)])[order(unit)]
  s2 <- merge(sp[event_id %in% dd$event_id], dd[, .(event_id, unit, rep)], by = "event_id")
  spp <- s2[, .(n = uniqueN(unit)), by = species][n >= 8][order(-n)][seq_len(min(.N, 120))]$species
  J <- nrow(units); K <- max(dd$rep); N <- length(spp)
  y <- array(NA_real_, c(N, J, K)); uix <- match(dd$unit, units$unit)
  for (r in seq_len(nrow(dd))) y[, uix[r], dd$rep[r]] <- 0
  h <- s2[species %in% spp]; y[cbind(match(h$species, spp), match(h$unit, units$unit), h$rep)] <- 1
  mk <- function(v) { m <- matrix(NA_real_, J, K); m[cbind(uix, dd$rep)] <- v; m }
  dc <- list(log_dur = mk(as.numeric(scale(dd$log_dur))), doy = mk(as.numeric(scale(dd$doy))), ebird = mk(as.numeric(dd$source == "ebird")))
  oc <- data.frame(near = units$near, post = units$post, site = as.integer(factor(units$site)))
  f <- msPGOcc(occ.formula = ~ near * post + (1 | site), det.formula = ~ log_dur + doy + I(doy^2) + ebird,
               data = list(y = y, occ.covs = oc, det.covs = dc), n.samples = 10000, n.burn = 4000, n.thin = 6,
               n.chains = 3, verbose = FALSE)
  b <- f$beta.comm.samples[, "near:post"]
  data.table(min_cell = min_cell, n_sites = uniqueN(units$site), seed = seed, mean = mean(b), lo = quantile(b, .025), hi = quantile(b, .975),
             p_neg = mean(b < 0), rhat = f$rhat$beta.comm[which(colnames(f$beta.comm.samples) == "near:post")], n_species = N)
}
grid <- CJ(min_cell = c(3L, 5L), seed = 1:8)
out <- rbindlist(mcmapply(fit_one, grid$min_cell, grid$seed, SIMPLIFY = FALSE, mc.cores = 4))
fwrite(out, file.path(res, "rigor_birds_occ_seeds.csv"))
print(out[, lapply(.SD, function(v) if (is.numeric(v)) signif(v, 3) else v)])
cat("done\n")
