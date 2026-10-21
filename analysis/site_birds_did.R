# ============================================================
# Scientific question / 科学问题:
# Around data-rich PV sites, did bird checklists within 5 km of the site
# change after construction relative to checklists 8-20 km away at the
# same site and in the same year, once survey effort is accounted for?
# 在观鸟数据充足的光伏站周边,校正调查努力后,建设后 5 km 内的清单相对同站同年
# 8-20 km 的清单是否发生变化?
#
# Objective / 分析目标:
#   A. Checklist-level staggered DiD (ring-BACI): near x post with site x zone
#      and site x year fixed effects, effort controls, event study.
#   B. Multispecies occupancy (spOccupancy::msPGOcc): units = site x zone x
#      period, replicate checklists, detection ~ duration + day + source,
#      occupancy ~ near x post + (1 | site).
#   A. 清单层面交错 DiD(近远区对照):站点×区、站点×年固定效应,控制努力,事件研究。
#   B. 多物种占域模型:单元为站点×区×时段,检测随时长、日期、来源变化。
#
# Input data / 输入数据:
#   cache/birds/site_checklists_b.csv.gz (built by site_birds_build.py with
#   near <= 5 km, far 8-20 km, other-PV exclusion 3 km, cohorts 2018-2023);
#   results/site_birds_eligibility_b.csv; species records from the bird
#   community dynamics project (combined_events_dedup_v3.rds; not redistributed)
# Key assumptions / 关键假设:
#   Within a site and year, near and far checklists share regional shocks;
#   site x year fixed effects make staggered timing safe (no already-treated
#   units serve as controls). Data-sparse sites are excluded by design, so
#   results describe sites where people go birding, not typical PV sites.
#   同站同年近远区共享区域冲击;站点×年固定效应使交错时点下不会以已处理单元作对照。
#   稀疏站点按设计被排除,结论只代表有人观鸟的站点。
# Expected output / 预期输出: results/site_birds_did.csv, results/site_birds_es.csv,
#   results/site_birds_occ.csv, results/figures/site_birds_did.png|pdf
# Main packages / 主要包: data.table, fixest, spOccupancy, ggplot2
# ============================================================
suppressPackageStartupMessages({ library(data.table); library(fixest); library(spOccupancy); library(ggplot2) })
set.seed(42)
args <- commandArgs(trailingOnly = FALSE)
here <- dirname(normalizePath(sub("^--file=", "", args[grep("^--file=", args)])))
root <- dirname(here); res <- file.path(root, "results"); cache <- file.path(root, "cache", "birds")
MIN_CELL <- as.integer(Sys.getenv("SB_MIN_CELL", "3"))

el <- fread(file.path(res, "site_birds_eligibility_b.csv"))
ck <- fread(file.path(cache, "site_checklists_b.csv.gz"))
keep_sites <- el[min_cell >= MIN_CELL, site]
d <- ck[site %in% keep_sites & period != "transition"]
d[, `:=`(near = as.integer(zone == "near"), post = as.integer(period == "post"),
         month = month(as.IDate(date)), doy = yday(as.IDate(date)),
         dur_na = as.integer(is.na(duration_min) | duration_min <= 0))]
d[, log_dur := fifelse(dur_na == 1, 0, log(pmin(duration_min, 600)))]
d[, y := log(n_species)]
cat("sites:", uniqueN(d$site), " checklists:", nrow(d), "\n"); print(d[, .N, by = .(zone, period)])

# ---- A. 清单层面 DiD / checklist-level DiD ------------------------------------
m1 <- feols(y ~ near:post + log_dur + dur_na | site^zone + site^year + month + source, data = d, cluster = ~site)
m2 <- feols(y ~ near:post + log_dur + dur_na | site^zone + site^year + month + source + observer_id, data = d, cluster = ~site)
m3 <- fepois(n_species ~ near:post + log_dur + dur_na | site^zone + site^year + month + source, data = d, cluster = ~site)
tab <- rbindlist(lapply(list(`log richness` = m1, `log richness + observer FE` = m2, `Poisson richness` = m3), function(m) {
  ct <- coeftable(m)["near:post", ]
  data.table(estimate = ct[1], se = ct[2], p = ct[4], n = nobs(m))
}), idcol = "model")
tab[, `:=`(pct = 100 * (exp(estimate) - 1), lo_pct = 100 * (exp(estimate - 1.96 * se) - 1),
           hi_pct = 100 * (exp(estimate + 1.96 * se) - 1), min_cell = MIN_CELL, n_sites = uniqueN(d$site))]
print(tab)
fwrite(tab, file.path(res, sprintf("site_birds_did_min%d.csv", MIN_CELL)))

de <- ck[site %in% keep_sites]
de[, `:=`(near = as.integer(zone == "near"), month = month(as.IDate(date)),
          dur_na = as.integer(is.na(duration_min) | duration_min <= 0))]
de[, `:=`(log_dur = fifelse(dur_na == 1, 0, log(pmin(duration_min, 600))), y = log(n_species))]
es <- feols(y ~ i(e, near, ref = -1) + log_dur + dur_na | site^zone + site^year + month + source, data = de, cluster = ~site)
est <- as.data.table(coeftable(es), keep.rownames = "term")[grepl("^e::", term)]
est[, e := as.integer(sub("e::(-?\\d+):near", "\\1", term))]
est <- rbind(est[, .(e, est = Estimate, se = `Std. Error`)], data.table(e = -1L, est = 0, se = 0))
fwrite(est, file.path(res, sprintf("site_birds_es_min%d.csv", MIN_CELL)))

# ---- B. 多物种占域模型 / multispecies occupancy ---------------------------------
sp_file <- file.path(cache, "site_species_b.rds")
if (!file.exists(sp_file)) {
  x <- readRDS("/Users/dingchenchen/Documents/New project/bird_dynamic_occupancy_analysis/data/derived_v3/combined_events_dedup_v3.rds")
  saveRDS(unique(x[event_id %in% d$event_id, .(event_id, species)]), sp_file); rm(x); gc()
}
sp <- readRDS(sp_file)
d[, unit := paste(site, zone, period, sep = "|")]
# 每单元最多随机 25 份清单 / up to 25 random checklists per unit
dd <- d[, .SD[sample(.N, min(.N, 25L))], by = unit]
dd[, rep := seq_len(.N), by = unit]
units <- unique(dd[, .(unit, site, near, post)])[order(unit)]
spp <- sp[event_id %in% dd$event_id, .(n_units = uniqueN(dd[match(event_id, dd$event_id), unit])), by = species]
spp <- spp[n_units >= 8][order(-n_units)][seq_len(min(.N, 120))]$species
J <- nrow(units); K <- max(dd$rep); N <- length(spp)
cat("occupancy: units", J, " max reps", K, " species", N, "\n")
y_arr <- array(NA_real_, dim = c(N, J, K))
uix <- match(dd$unit, units$unit)
det <- sp[species %in% spp & event_id %in% dd$event_id]
for (r in seq_len(nrow(dd))) y_arr[, uix[r], dd$rep[r]] <- 0
hit <- merge(det, dd[, .(event_id, unit, rep)], by = "event_id")
y_arr[cbind(match(hit$species, spp), match(hit$unit, units$unit), hit$rep)] <- 1
mk <- function(v) { m <- matrix(NA_real_, J, K); m[cbind(uix, dd$rep)] <- v; m }
det.covs <- list(log_dur = mk(as.numeric(scale(dd$log_dur))), doy = mk(as.numeric(scale(dd$doy))),
                 ebird = mk(as.numeric(dd$source == "ebird")))
occ.covs <- data.frame(near = units$near, post = units$post, site = as.integer(factor(units$site)))
fit <- msPGOcc(occ.formula = ~ near * post + (1 | site), det.formula = ~ log_dur + doy + I(doy^2) + ebird,
               data = list(y = y_arr, occ.covs = occ.covs, det.covs = det.covs),
               n.samples = as.integer(Sys.getenv("SB_NS", "8000")), n.burn = as.integer(Sys.getenv("SB_NB", "3000")), n.thin = as.integer(Sys.getenv("SB_NT", "5")), n.chains = 3, verbose = FALSE)
b <- fit$beta.comm.samples
rh <- fit$rhat$beta.comm
occ_tab <- data.table(term = colnames(b), mean = colMeans(b), lo = apply(b, 2, quantile, .025),
                      hi = apply(b, 2, quantile, .975), p_neg = colMeans(b < 0), rhat = rh)
sp_int <- fit$beta.samples[, grepl("near:post", colnames(fit$beta.samples))]
occ_tab <- rbind(occ_tab, data.table(term = "share of species with negative near:post", mean = mean(colMeans(sp_int) < 0),
                                     lo = NA, hi = NA, p_neg = NA, rhat = NA))
# 派生丰富度:各单元潜在占域之和 / derived richness = sum of latent occupancy per unit
z <- fit$z.samples  # draws x species x units
rich <- apply(z, c(1, 3), sum)
did_r <- apply(rich, 1, function(r) {
  m <- tapply(r, list(units$near, units$post), mean); (m["1", "1"] - m["1", "0"]) - (m["0", "1"] - m["0", "0"])
})
occ_tab <- rbind(occ_tab, data.table(term = "derived richness DiD (species)", mean = mean(did_r),
                                     lo = quantile(did_r, .025), hi = quantile(did_r, .975), p_neg = mean(did_r < 0), rhat = NA))
occ_tab[, `:=`(n_units = J, n_species = N, n_sites = uniqueN(units$site), min_cell = MIN_CELL)]
print(occ_tab); fwrite(occ_tab, file.path(res, sprintf("site_birds_occ_min%d.csv", MIN_CELL)))

# ---- 图 / figure -----------------------------------------------------------
if (MIN_CELL == 3) {
  est[, `:=`(lo = est - 1.96 * se, hi = est + 1.96 * se)]
  fig <- ggplot(est[order(e)], aes(e, est)) +
    annotate("rect", xmin = -0.5, xmax = 0.5, ymin = -Inf, ymax = Inf, fill = "grey90") +
    geom_hline(yintercept = 0, linewidth = 0.3) +
    geom_errorbar(aes(ymin = lo, ymax = hi), width = 0, colour = "#1F6F78") +
    geom_point(colour = "#1F6F78", size = 2) + scale_x_continuous(breaks = -4:3) +
    labs(x = "Years relative to construction (grey = construction year)",
         y = "Near (0-5 km) minus far (8-20 km)\nlog species per checklist",
         title = sprintf("%d data-rich PV sites, %s checklists", uniqueN(d$site), format(nrow(de), big.mark = ","))) +
    theme_classic(base_size = 10, base_family = "Arial Unicode MS")
  ggsave(file.path(res, "figures", "site_birds_did.png"), fig, width = 6, height = 3.8, dpi = 300)
  ggsave(file.path(res, "figures", "site_birds_did.pdf"), fig, width = 6, height = 3.8, device = cairo_pdf)
}
cat("done\n")
