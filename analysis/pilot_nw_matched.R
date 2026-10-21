# ============================================================
# Scientific question / 科学问题:
# Once PV sites are compared with never-built locations matched on their
# 2010-2016 NDVI trajectory and siting variables, is there a credible
# effect of construction on greenness and heterogeneity?
# 与按 2010-2016 年 NDVI 轨迹及选址变量匹配的"从未建设"地点比较后,
# 光伏建设对绿度与异质性是否有可信的效应?
#
# Objective / 分析目标:
# Repair gate G2 of the NW pilot: matched never-treated controls, pre-period
# back to 2010, a hold-out pre-trend test on years not used for matching,
# and HonestDiD bounds. 修复西北试点的 G2:匹配的从未建设对照、前期延至 2010、
# 用未参与匹配的年份做留出预趋势检验、HonestDiD 敏感性界。
#
# Input data / 输入数据:
#   results/matched_units_covariates.csv, results/matched_units_ndvi_panel.csv (gee/11)
# Workflow / 主要流程:
#   1. Matching features: NDVI level, slope and SD level over 2010-2016;
#      PVOUT, slope, elevation; exact on province and CLCD 2017 cover group
#      匹配变量:2010-2016 年 NDVI 水平、斜率、标准差;PVOUT、坡度、海拔;省份与 CLCD 2017 覆盖精确匹配
#   2. Mahalanobis nearest neighbour, 3:1 with replacement (MatchIt)
#   3. Callaway-Sant'Anna with matched never-treated controls (weights)
#   4. Hold-out pre-test on ATT(g,t) with 2017 <= t <= g-2
#      留出预趋势检验:只用 2017 年至建设前两年的 ATT(g,t)
#   5. HonestDiD relative-magnitude bounds for the e = 1..3 average
# Expected output / 预期输出:
#   results/matched_balance.csv, results/matched_att.csv, results/matched_eventstudy.csv,
#   results/matched_honestdid.csv, results/figures/pilot_nw_matched.png|pdf
# Key assumptions / 关键假设:
#   Conditional on the 2010-2016 trajectory and siting covariates, untreated
#   NDVI paths of PV sites and matched locations would have stayed parallel.
#   以 2010-2016 年轨迹和选址变量为条件,未建设情形下两者 NDVI 路径平行。
# Main packages / 主要包: data.table, MatchIt, did, HonestDiD, ggplot2
# Output directory / 输出路径: results/
# ============================================================
suppressPackageStartupMessages({
  library(data.table); library(MatchIt); library(did); library(HonestDiD); library(ggplot2)
})
set.seed(42)
args <- commandArgs(trailingOnly = FALSE)
here <- dirname(normalizePath(sub("^--file=", "", args[grep("^--file=", args)])))
res <- file.path(dirname(here), "results")

# ---- 1. 数据与匹配变量 / data and matching features --------------------------
cv <- fread(file.path(res, "matched_units_covariates.csv"))
pn <- fread(file.path(res, "matched_units_ndvi_panel.csv"))
pn[, year := as.integer(year)]
# CLCD 2017 类别:1 耕地、2 森林、3 灌丛、4 草地、7 裸地 / CLCD 2017 classes
lc_group <- function(x) fcase(x == 1, "cropland", x %in% 2:3, "forest_shrub", x == 4, "grassland",
                              x == 7, "barren", default = "other")
cv[, lcg := lc_group(round(lc2017))]
pre <- pn[year <= 2016 & !is.na(site_mean),
          .(ndvi_lvl = mean(site_mean), ndvi_slope = coef(lm(site_mean ~ year))[2],
            sd_lvl = mean(site_stdDev, na.rm = TRUE), n_pre = .N), by = unit_id]
u <- merge(cv, pre, by = "unit_id")
u <- u[n_pre >= 5 & !is.na(pvout) & !is.na(slope) & province != "NA" & lcg != "other"]
u[, treat := as.integer(type == "pv")]
cat("units for matching: pv", sum(u$treat), " candidates", sum(1 - u$treat), "\n")

m <- matchit(treat ~ ndvi_lvl + ndvi_slope + sd_lvl + pvout + slope + elev, data = u,
             method = "nearest", distance = "mahalanobis", exact = ~ province + lcg,
             ratio = 3, replace = TRUE)
md <- as.data.table(match.data(m))
cat("matched: pv", sum(md$treat), " controls", sum(1 - md$treat), "\n")

# 平衡性:标准化均值差 / balance as standardised mean differences
smd <- function(d, w = NULL) {
  vars <- c("ndvi_lvl", "ndvi_slope", "sd_lvl", "pvout", "slope", "elev")
  if (is.null(w)) w <- rep(1, nrow(d))
  rbindlist(lapply(vars, function(v) {
    x <- d[[v]]; t1 <- d$treat == 1
    m1 <- weighted.mean(x[t1], w[t1]); m0 <- weighted.mean(x[!t1], w[!t1])
    data.table(var = v, smd = (m1 - m0) / sd(u[[v]][u$treat == 1]))
  }))
}
bal <- merge(smd(u)[, .(var, smd_before = round(smd, 3))], smd(md, md$weights)[, .(var, smd_after = round(smd, 3))], by = "var")
fwrite(bal, file.path(res, "matched_balance.csv")); print(bal)

# ---- 2. 估计面板 / estimation panel ------------------------------------------
d <- merge(pn, md[, .(unit_id, treat, weights, construction_year)], by = "unit_id")
d[, `:=`(g = fifelse(treat == 1, as.integer(construction_year), 0L), id = as.integer(factor(unit_id)))]

outcomes <- c(site_mean = "站内 NDVI 均值 / on-site NDVI mean", site_stdDev = "站内 NDVI 标准差 / on-site NDVI SD",
              ring_mean = "外围环 NDVI 均值 / ring NDVI mean", ring_stdDev = "外围环 NDVI 标准差 / ring NDVI SD")

holdout_test <- function(a) {
  # 只用未参与匹配的年份:2017 <= t <= g - 2 / hold-out years only
  sel <- which(a$t >= 2017 & a$t <= a$group - 2)
  b <- a$att[sel]; inf <- a$inf.function$inf.func[, sel, drop = FALSE]
  V <- crossprod(inf) / nrow(inf)^2
  W <- as.numeric(t(b) %*% MASS::ginv(V) %*% b)
  c(p = pchisq(W, df = length(b), lower.tail = FALSE), k = length(b), max_abs = max(abs(b)))
}

es_rows <- list(); att_rows <- list(); hd_rows <- list()
for (y in names(outcomes)) {
  dd <- as.data.frame(d[!is.na(get(y))])
  a <- att_gt(yname = y, tname = "year", idname = "id", gname = "g", data = dd, weightsname = "weights",
              control_group = "nevertreated", base_period = "universal", allow_unbalanced_panel = TRUE,
              est_method = "reg", bstrap = TRUE, biters = 999, clustervars = "id", print_details = FALSE)
  dyn <- aggte(a, type = "dynamic", min_e = -9, max_e = 3, na.rm = TRUE)
  ov <- aggte(a, type = "dynamic", min_e = 1, max_e = 3, na.rm = TRUE)
  ho <- holdout_test(a)
  es_rows[[y]] <- data.table(outcome = y, e = dyn$egt, est = dyn$att.egt, se = dyn$se.egt)
  att_rows[[y]] <- data.table(outcome = y, att = ov$overall.att, se = ov$overall.se,
                              holdout_p = ho[["p"]], holdout_k = ho[["k"]], holdout_max = ho[["max_abs"]])

  # HonestDiD:相对幅度界,目标为 e = 1..3 平均 / relative-magnitude bounds for mean of e = 1..3
  keep <- dyn$egt >= -5 & dyn$egt <= 3 & dyn$egt != -1
  e_k <- dyn$egt[keep]; b <- dyn$att.egt[keep]
  inf <- dyn$inf.function$dynamic.inf.func.e[, keep, drop = FALSE]
  S <- crossprod(inf) / nrow(inf)^2
  npre <- sum(e_k < 0); npost <- sum(e_k >= 0)
  lvec <- as.numeric(e_k[e_k >= 0] %in% 1:3); lvec <- lvec / sum(lvec)
  hd <- tryCatch(createSensitivityResults_relativeMagnitudes(betahat = b, sigma = S, numPrePeriods = npre,
                   numPostPeriods = npost, Mbarvec = c(0.5, 1, 1.5, 2), l_vec = lvec),
                 error = function(e) NULL)
  orig <- constructOriginalCS(betahat = b, sigma = S, numPrePeriods = npre, numPostPeriods = npost, l_vec = lvec)
  if (!is.null(hd)) hd_rows[[y]] <- rbind(data.table(outcome = y, Mbar = 0, lb = orig$lb, ub = orig$ub),
                                         data.table(outcome = y, Mbar = hd$Mbar, lb = hd$lb, ub = hd$ub))
}
es <- rbindlist(es_rows); att <- rbindlist(att_rows); hdt <- rbindlist(hd_rows)
att[, `:=`(lo = att - 1.96 * se, hi = att + 1.96 * se)]
fwrite(es, file.path(res, "matched_eventstudy.csv")); fwrite(att, file.path(res, "matched_att.csv"))
fwrite(hdt, file.path(res, "matched_honestdid.csv"))
print(att[, lapply(.SD, function(v) if (is.numeric(v)) signif(v, 3) else v)])
print(hdt[, lapply(.SD, function(v) if (is.numeric(v)) signif(v, 3) else v)])

# ---- 3. 图:匹配对照 vs 上一版晚建对照 / figure: matched vs not-yet-treated -----
old <- fread(file.path(res, "pilot_nw_eventstudy.csv"))[method == "CS", .(outcome, e, est, se)]
old[, design := "晚建站点对照 / not-yet-built"]
es[, design := "匹配的从未建设点 / matched never-built"]
fe <- rbind(es, old)[e >= -9 & e <= 3]
fe[, `:=`(lo = est - 1.96 * se, hi = est + 1.96 * se, panel = factor(outcomes[outcome], levels = outcomes))]
pd <- position_dodge(width = 0.5)
fig <- ggplot(fe, aes(e, est, colour = design, shape = design)) +
  annotate("rect", xmin = -0.5, xmax = 0.5, ymin = -Inf, ymax = Inf, fill = "grey90") +
  annotate("rect", xmin = -9.5, xmax = -4.5, ymin = -Inf, ymax = Inf, fill = "#EEF4F4", alpha = 0.7) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0, position = pd) +
  geom_point(size = 1.6, position = pd) +
  facet_wrap(~panel, scales = "free_y", ncol = 2) +
  scale_colour_manual(values = c("#1F6F78", "#E9A23B")) + scale_shape_manual(values = c(16, 17)) +
  scale_x_continuous(breaks = -9:3) +
  labs(x = "Years relative to construction (grey = construction year; pale = years mostly used for matching)",
       y = "Effect on NDVI statistic", colour = NULL, shape = NULL) +
  theme_classic(base_size = 10, base_family = "Arial Unicode MS") +
  theme(legend.position = "top", strip.background = element_blank(), strip.text = element_text(face = "bold"))
ggsave(file.path(res, "figures", "pilot_nw_matched.png"), fig, width = 8, height = 5.8, dpi = 300)
ggsave(file.path(res, "figures", "pilot_nw_matched.pdf"), fig, width = 8, height = 5.8, device = cairo_pdf)
cat("done\n")
