# ============================================================
# Scientific question / 科学问题:
# Is the on-site NDVI decline after PV construction in NW drylands robust to
# spatial dependence, matching choices, province composition and a placebo
# null built from never-built locations?
# 西北旱区光伏建设后站内 NDVI 的下降,对空间相关、匹配设定、省份构成以及
# 由从未建设点构造的安慰剂零分布是否稳健?
#
# Objective / 分析目标 (audit repairs / 审查修补):
#   1. Standard errors clustered on 50 km spatial blocks (multiplier bootstrap)
#      and HonestDiD with block-level covariance
#   2. Matching without replacement (1:1, 1:3) as sensitivity
#   3. Leave-one-province-out
#   4. Placebo-in-time: pseudo construction years assigned to matched
#      never-built points (200 draws) -> empirical null for the ATT
#   5. Holm adjustment across the four outcomes (primary: on-site NDVI mean)
# Input data / 输入数据: results/matched_units_covariates.csv,
#   results/matched_units_ndvi_panel.csv, results/bird_coverage_near_pv.csv (site
#   centroids), cache/nw_candidates.csv (candidate coordinates)
# Expected output / 预期输出: results/rigor_pilot_*.csv
# Key assumptions / 关键假设: anticipation of 1 year was chosen after inspecting
#   the event study (post hoc); both 0 and 1 are reported.
#   1 年预期期是看过事件研究后定的(事后选择),0 与 1 两种都报告。
# Main packages / 主要包: data.table, MatchIt, did, HonestDiD
# ============================================================
suppressPackageStartupMessages({ library(data.table); library(MatchIt); library(did); library(HonestDiD); library(sf) })
set.seed(42)
args <- commandArgs(trailingOnly = FALSE)
here <- dirname(normalizePath(sub("^--file=", "", args[grep("^--file=", args)])))
root <- dirname(here); res <- file.path(root, "results")
ALB <- "+proj=aea +lat_1=25 +lat_2=47 +lat_0=0 +lon_0=105 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
BLOCK_M <- 50000

# ---- 数据与空间块 / data and spatial blocks -----------------------------------
cv <- fread(file.path(res, "matched_units_covariates.csv"))
pn <- fread(file.path(res, "matched_units_ndvi_panel.csv")); pn[, year := as.integer(year)]
xy_s <- fread(file.path(res, "bird_coverage_near_pv.csv"))[, .(unit_id = paste0("s", fid), lon, lat)]
xy_c <- fread(file.path(root, "cache", "nw_candidates.csv"))[, .(unit_id, lon, lat)]
xy <- rbind(xy_s, xy_c)
p_alb <- st_coordinates(st_transform(st_as_sf(xy, coords = c("lon", "lat"), crs = 4326), ALB))
xy[, block := paste(floor(p_alb[, 1] / BLOCK_M), floor(p_alb[, 2] / BLOCK_M), sep = "_")]
lc_group <- function(x) fcase(x == 1, "cropland", x %in% 2:3, "forest_shrub", x == 4, "grassland", x == 7, "barren", default = "other")
cv[, lcg := lc_group(round(lc2017))]
pre <- pn[year <= 2016 & !is.na(site_mean), .(ndvi_lvl = mean(site_mean), ndvi_slope = coef(lm(site_mean ~ year))[2],
          sd_lvl = mean(site_stdDev, na.rm = TRUE), n_pre = .N), by = unit_id]
u <- merge(merge(cv, pre, by = "unit_id"), xy[, .(unit_id, block)], by = "unit_id")
u <- u[n_pre >= 5 & !is.na(pvout) & !is.na(slope) & province != "NA" & lcg != "other"]
u[, treat := as.integer(type == "pv")]
cat("units:", nrow(u), " pv:", sum(u$treat), " missing block:", sum(is.na(u$block)), "\n")

do_match <- function(dat, ratio = 3, replace = TRUE) {
  m <- matchit(treat ~ ndvi_lvl + ndvi_slope + sd_lvl + pvout + slope + elev, data = dat, method = "nearest",
               distance = "mahalanobis", exact = ~ province + lcg, ratio = ratio, replace = replace)
  as.data.table(match.data(m))
}
panel_of <- function(md) {
  d <- merge(pn, md[, .(unit_id, treat, weights, construction_year, block, province)], by = "unit_id")
  d[, `:=`(g = fifelse(treat == 1, as.integer(construction_year), 0L), id = as.integer(factor(unit_id)))]
  d
}
fit_cs <- function(d, y, antic, cl = c("id", "block"), boot = TRUE) {
  att_gt(yname = y, tname = "year", idname = "id", gname = "g", data = as.data.frame(d[!is.na(get(y))]),
         weightsname = "weights", control_group = "nevertreated", base_period = "universal",
         allow_unbalanced_panel = TRUE, anticipation = antic, est_method = "reg",
         bstrap = boot, biters = 999, clustervars = if (boot) cl else NULL, print_details = FALSE)
}
# 按空间块聚合影响函数得到协方差 / block-level covariance from influence functions
block_vcov <- function(inf, ids, d) {
  blk <- unique(d[, .(id, block)])[order(id)]
  b <- blk$block[match(ids, blk$id)]
  S <- rowsum(inf, b); crossprod(S) / nrow(inf)^2
}
outcomes <- c("site_mean", "site_stdDev", "ring_mean", "ring_stdDev")

md <- do_match(u)
d <- panel_of(md)
cat("matched pv:", sum(md$treat), " controls:", sum(md$treat == 0), " blocks:", uniqueN(d$block), "\n")

# ---- 1. 主估计:空间块聚类 + HonestDiD / main estimates ------------------------
main <- list(); es_all <- list(); hd_all <- list()
for (antic in c(0, 1)) for (y in outcomes) {
  a <- fit_cs(d, y, antic)
  ov <- aggte(a, type = "dynamic", min_e = 1, max_e = 3, na.rm = TRUE, cband = FALSE)
  dyn <- aggte(a, type = "dynamic", min_e = -9, max_e = 3, na.rm = TRUE, cband = FALSE)
  main[[paste(antic, y)]] <- data.table(anticipation = antic, outcome = y, att = ov$overall.att, se = ov$overall.se)
  es_all[[paste(antic, y)]] <- data.table(anticipation = antic, outcome = y, e = dyn$egt, est = dyn$att.egt, se = dyn$se.egt)
  # HonestDiD,协方差按空间块 / block-clustered covariance
  keep <- dyn$egt >= -5 & dyn$egt <= 3 & dyn$egt != -1 - antic
  e_k <- dyn$egt[keep]; bh <- dyn$att.egt[keep]
  ids <- sort(unique(d[!is.na(get(y))]$id))
  S <- block_vcov(dyn$inf.function$dynamic.inf.func.e[, keep, drop = FALSE], ids, d)
  npre <- sum(e_k < -1 - antic); npost <- sum(e_k > -1 - antic)
  lv <- as.numeric(e_k[e_k > -1 - antic] %in% 1:3); lv <- lv / sum(lv)
  orig <- constructOriginalCS(betahat = bh, sigma = S, numPrePeriods = npre, numPostPeriods = npost, l_vec = lv)
  hd <- tryCatch(createSensitivityResults_relativeMagnitudes(betahat = bh, sigma = S, numPrePeriods = npre,
                   numPostPeriods = npost, Mbarvec = seq(0.25, 2, by = 0.25), l_vec = lv), error = function(e) NULL)
  hd_all[[paste(antic, y)]] <- rbind(data.table(anticipation = antic, outcome = y, Mbar = 0, lb = as.numeric(orig$lb), ub = as.numeric(orig$ub)),
                                     if (!is.null(hd)) data.table(anticipation = antic, outcome = y, Mbar = hd$Mbar, lb = as.numeric(hd$lb), ub = as.numeric(hd$ub)))
}
main <- rbindlist(main)
main[, p := 2 * pnorm(-abs(att / se))]
main[, p_holm := p.adjust(p, "holm"), by = anticipation]
main[, `:=`(lo = att - 1.96 * se, hi = att + 1.96 * se)]
fwrite(main, file.path(res, "rigor_pilot_main.csv")); fwrite(rbindlist(es_all), file.path(res, "rigor_pilot_eventstudy.csv"))
fwrite(rbindlist(hd_all), file.path(res, "rigor_pilot_honestdid.csv"))
print(main[, lapply(.SD, function(v) if (is.numeric(v)) signif(v, 3) else v)])

# ---- 2. 匹配设定敏感性 / matching sensitivity ----------------------------------
sens <- list()
for (cfg in list(list(r = 1, rep = FALSE), list(r = 3, rep = FALSE), list(r = 5, rep = TRUE))) {
  mdx <- do_match(u, cfg$r, cfg$rep); dx <- panel_of(mdx)
  a <- fit_cs(dx, "site_mean", 1); ov <- aggte(a, type = "dynamic", min_e = 1, max_e = 3, na.rm = TRUE, cband = FALSE)
  sens[[length(sens) + 1]] <- data.table(spec = sprintf("1:%d %s replacement", cfg$r, if (cfg$rep) "with" else "without"),
                                         att = ov$overall.att, se = ov$overall.se, n_pv = sum(mdx$treat), n_ctrl = sum(mdx$treat == 0))
}
# ---- 3. 逐省剔除 / leave-one-province-out -------------------------------------
for (pr in unique(d[treat == 1]$province)) {
  dx <- d[province != pr]
  a <- fit_cs(dx, "site_mean", 1); ov <- aggte(a, type = "dynamic", min_e = 1, max_e = 3, na.rm = TRUE, cband = FALSE)
  sens[[length(sens) + 1]] <- data.table(spec = paste("drop", pr), att = ov$overall.att, se = ov$overall.se,
                                         n_pv = uniqueN(dx[treat == 1]$id), n_ctrl = uniqueN(dx[treat == 0]$id))
}
sens <- rbindlist(sens); sens[, `:=`(lo = att - 1.96 * se, hi = att + 1.96 * se)]
fwrite(sens, file.path(res, "rigor_pilot_sensitivity.csv")); print(sens[, lapply(.SD, function(v) if (is.numeric(v)) signif(v, 3) else v)])

# ---- 4. 伪建设年份安慰剂 / placebo-in-time null ---------------------------------
# 只在匹配的从未建设点里,随机挑出与处理组同样多的点,按处理组的建设年份分布赋伪年份
ctrl <- unique(d[treat == 0, .(id, weights)]); gdist <- unique(d[treat == 1, .(id, g)])$g
n_fake <- min(length(gdist), floor(nrow(ctrl) / 2))
plc <- rbindlist(lapply(seq_len(200), function(r) {
  fk <- sample(ctrl$id, n_fake)
  dx <- d[treat == 0][, g := 0L][id %in% fk, g := sample(gdist, 1), by = id]
  a <- tryCatch(fit_cs(dx, "site_mean", 1, boot = FALSE), error = function(e) NULL)
  if (is.null(a)) return(NULL)
  ov <- tryCatch(aggte(a, type = "dynamic", min_e = 1, max_e = 3, na.rm = TRUE, bstrap = FALSE), error = function(e) NULL)
  if (is.null(ov)) NULL else data.table(rep = r, att = ov$overall.att)
}))
obs <- main[anticipation == 1 & outcome == "site_mean"]$att
fwrite(plc, file.path(res, "rigor_pilot_placebo.csv"))
cat(sprintf("placebo reps: %d; placebo mean %.4f, 2.5-97.5%% [%.4f, %.4f]; observed %.4f; empirical p = %.3f\n",
            nrow(plc), mean(plc$att), quantile(plc$att, .025), quantile(plc$att, .975), obs,
            (1 + sum(abs(plc$att) >= abs(obs))) / (1 + nrow(plc))))
cat("done\n")
