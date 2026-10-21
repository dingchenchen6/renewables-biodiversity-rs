# ============================================================
# Scientific question / 科学问题:
# Did 100-km grid cells that gained substantial PV (or wind) in
# 2020-2024 show a different change in detection-corrected bird
# diversity than matched cells with almost no new wind or solar?
# 2020-2024 年新增大量光伏(或风电)的 100 km 网格,其检测校正后的鸟类多样性
# 变化是否不同于几乎无新增风光的匹配网格?
#
# Objective / 分析目标:
# Landscape-scale test using the occupancy posterior of the China bird
# community dynamics project (200 species, spatial multispecies dynamic
# occupancy, CBRC + eBird/GBIF merged and deduplicated). Site-scale
# occupancy is infeasible: < 1.2% of PV sites have >= 5 checklists within
# 5 km both before and after construction (analysis/bird_coverage_near_pv.py).
# 用鸟类群落动态项目的占域后验做景观尺度检验。站点尺度占域不可行:
# 建设前后 5 km 内均有 ≥ 5 份清单的光伏站不足 1.2%。
#
# Input data / 输入数据:
#   china-bird-community-dynamics results_v3: diversity per grid x period
#   (posterior mean and SD), land-use, HFI and climate change tables;
#   results/grid_pv_wind_exposure.csv (GRW, assigned by site centroid)
# Workflow / 主要流程:
#   1. Outcome: change P4 (2015-19) -> P5 (2020-24); placebo P3 -> P4
#      结局:P4→P5 变化;安慰剂:P3→P4 变化
#   2. Treated: PV built 2020-24 >= 5 km2 (wind: >= 50 turbines);
#      controls: PV 2018-24 < 0.5 km2 and < 5 turbines
#   3. Mahalanobis 1:3 matching with replacement on baseline, pre-trend,
#      contemporaneous climate, HFI and land-cover change, location
#   4. ATT = weighted mean difference; SE from posterior SDs and matching
#      weights; continuous dose model (WLS) as a cross-check
# Expected output / 预期输出: results/grid_birds_pv_att.csv,
#   results/grid_birds_pv_balance.csv, results/figures/grid_birds_pv.png|pdf
# Key assumptions / 关键假设:
#   PV covers <= 1.7% of any grid cell, so this tests landscape-scale
#   association, not site effects; spatial smoothing in the occupancy model
#   (NNGP) shrinks local signals toward neighbours, biasing toward zero.
#   光伏最多占网格面积 1.7%,只能检验景观尺度关联;占域模型的空间平滑会把局地信号拉向邻域,偏向零。
# Main packages / 主要包: data.table, MatchIt, ggplot2
# Output directory / 输出路径: results/
# ============================================================
suppressPackageStartupMessages({ library(data.table); library(MatchIt); library(ggplot2) })
set.seed(42)
args <- commandArgs(trailingOnly = FALSE)
here <- dirname(normalizePath(sub("^--file=", "", args[grep("^--file=", args)])))
res <- file.path(dirname(here), "results")
bd <- "/Users/dingchenchen/Documents/New project/china-bird-community-dynamics/results/results_v3"

metrics <- c(corrected_richness = "检测校正丰富度 / corrected richness", shannon = "Shannon",
             rao_q = "Rao's Q(功能)/ functional Rao's Q", trait_volume = "性状体积 / trait volume",
             fdis_prob = "功能离散度 / functional dispersion", pd_prob_mctavish = "系统发育多样性 / phylogenetic diversity")

# ---- 1. 结局与协变量 / outcomes and covariates ---------------------------------
dv <- fread(file.path(bd, "table_diversity_summary_v3_full_200sp_ar1_spatial_extended.csv"))[metric %in% names(metrics)]
w <- dcast(dv, grid_cell + metric ~ period, value.var = c("mean", "sd"))
w[, `:=`(d45 = mean_P5 - mean_P4, se45 = sqrt(sd_P5^2 + sd_P4^2),
         d34 = mean_P4 - mean_P3, se34 = sqrt(sd_P4^2 + sd_P3^2))]

lu <- fread(file.path(bd, "table_landuse_change_v3.csv"))
lu <- lu[, .(grid_cell, lon = centroid_lon, lat = centroid_lat,
             forest4 = forest_P4, grass4 = grassland_P4, crop4 = cropland_P4, imp4 = impervious_P4,
             d_forest = forest_P5 - forest_P4, d_grass = grassland_P5 - grassland_P4,
             d_crop = cropland_P5 - cropland_P4, d_imp = impervious_P5 - impervious_P4)]
hf <- fread(file.path(bd, "table_hfi_change_v3.csv"))[, .(grid_cell, hfi4 = hfi_P4, d_hfi = hfi_P5 - hfi_P4)]
cl <- fread(file.path(bd, "table_climate_change_v3.csv"))[, .(grid_cell, tmp4 = tmp_P4, d_tmp = tmp_P5 - tmp_P4)]
ex <- fread(file.path(res, "grid_pv_wind_exposure.csv"))
cov <- Reduce(function(a, b) merge(a, b, by = "grid_cell"), list(lu, hf, cl, ex))

# ---- 2. 匹配与估计 / matching and estimation -----------------------------------
att_one <- function(dm, treat_rule, label) {
  out <- list(); bal <- list()
  for (mt in names(metrics)) {
    d <- merge(dm[metric == mt], cov, by = "grid_cell")
    d <- d[complete.cases(d[, .(d45, d34, mean_P4, forest4, grass4, crop4, d_hfi, d_tmp, d_crop, d_imp)])]
    d[, treat := as.integer(eval(treat_rule))]
    d <- d[treat == 1 | (pv18_19 + pv20_24 < 0.5 & wind18_19 + wind20_24 < 5)]
    if (sum(d$treat) < 10) next
    m <- matchit(treat ~ mean_P4 + d34 + forest4 + grass4 + crop4 + hfi4 + tmp4 + d_hfi + d_tmp +
                   d_crop + d_imp + d_forest + lon + lat,
                 data = d, method = "nearest", distance = "mahalanobis", ratio = 3, replace = TRUE)
    md <- as.data.table(match.data(m))
    est <- function(y, se) {
      t1 <- md[treat == 1]; t0 <- md[treat == 0]
      mu1 <- mean(t1[[y]]); mu0 <- weighted.mean(t0[[y]], t0$weights)
      # 方差:处理组样本方差 + 后验测量误差;对照组加权 / variance incl. posterior SD
      v1 <- (var(t1[[y]]) + mean(t1[[se]]^2)) / nrow(t1)
      w0 <- t0$weights / sum(t0$weights)
      v0 <- sum(w0^2 * ((t0[[y]] - mu0)^2 + t0[[se]]^2))
      c(att = mu1 - mu0, se = sqrt(v1 + v0), base = mean(t1$mean_P4))
    }
    a <- est("d45", "se45"); pl <- est("d34", "se34")
    out[[mt]] <- data.table(exposure = label, metric = mt, n_treated = sum(md$treat), n_controls = sum(md$treat == 0),
                            att = a[["att"]], se = a[["se"]], baseline_P4 = a[["base"]],
                            placebo_att = pl[["att"]], placebo_se = pl[["se"]])
    if (mt == "corrected_richness") {
      vars <- c("mean_P4", "d34", "forest4", "grass4", "crop4", "hfi4", "tmp4", "d_hfi", "d_tmp", "d_crop", "d_imp", "lon", "lat")
      sdv <- sapply(vars, function(v) sd(d[treat == 1][[v]]))
      before <- sapply(vars, function(v) mean(d[treat == 1][[v]]) - mean(d[treat == 0][[v]])) / sdv
      after <- sapply(vars, function(v) mean(md[treat == 1][[v]]) - weighted.mean(md[treat == 0][[v]], md[treat == 0]$weights)) / sdv
      bal[[label]] <- data.table(exposure = label, var = vars, smd_before = round(before, 3), smd_after = round(after, 3))
    }
  }
  list(att = rbindlist(out), bal = rbindlist(bal))
}

r_pv <- att_one(w, quote(pv20_24 >= 5), "PV >= 5 km2 (2020-24)")
r_wd <- att_one(w, quote(wind20_24 >= 50), "wind >= 50 turbines (2020-24)")
att <- rbind(r_pv$att, r_wd$att)
att[, `:=`(lo = att - 1.96 * se, hi = att + 1.96 * se, pct_of_base = 100 * att / baseline_P4,
           placebo_lo = placebo_att - 1.96 * placebo_se, placebo_hi = placebo_att + 1.96 * placebo_se)]
fwrite(att, file.path(res, "grid_birds_pv_att.csv"))
fwrite(rbind(r_pv$bal, r_wd$bal), file.path(res, "grid_birds_pv_balance.csv"))
print(att[, .(exposure, metric, n_treated, att = signif(att, 3), lo = signif(lo, 3), hi = signif(hi, 3),
              pct = round(pct_of_base, 2), placebo = signif(placebo_att, 3), p_lo = signif(placebo_lo, 3), p_hi = signif(placebo_hi, 3))])
print(rbind(r_pv$bal, r_wd$bal))

# ---- 3. 连续剂量交叉检验 / continuous dose cross-check (WLS) --------------------
dose <- rbindlist(lapply(names(metrics), function(mt) {
  d <- merge(w[metric == mt], cov, by = "grid_cell")
  d <- d[complete.cases(d[, .(d45, d34, forest4, grass4, crop4, d_hfi, d_tmp, d_crop, d_imp)])]
  f <- lm(d45 ~ log1p(pv20_24) + log1p(wind20_24) + d34 + mean_P4 + forest4 + grass4 + crop4 + hfi4 + tmp4 +
            d_hfi + d_tmp + d_crop + d_imp + d_forest + poly(lon, 2) + poly(lat, 2), data = d, weights = 1 / se45^2)
  co <- summary(f)$coefficients
  data.table(metric = mt, term = c("log1p(PV km2)", "log1p(wind turbines)"),
             est = co[2:3, 1], se = co[2:3, 2], p = co[2:3, 4], n = nrow(d))
}))
fwrite(dose, file.path(res, "grid_birds_pv_dose.csv")); print(dose[, lapply(.SD, function(v) if (is.numeric(v)) signif(v, 3) else v)])

# ---- 4. 图 / figure ------------------------------------------------------------
fd <- rbind(att[, .(exposure, metric, kind = "P4→P5 效应 / effect", est = pct_of_base, lo = 100 * lo / baseline_P4, hi = 100 * hi / baseline_P4)],
            att[, .(exposure, metric, kind = "P3→P4 安慰剂 / placebo", est = 100 * placebo_att / baseline_P4,
                    lo = 100 * placebo_lo / baseline_P4, hi = 100 * placebo_hi / baseline_P4)])
fd[, metric := factor(metrics[metric], levels = rev(metrics))]
fig <- ggplot(fd, aes(est, metric, colour = kind, shape = kind)) +
  geom_vline(xintercept = 0, linewidth = 0.3) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0, position = position_dodge(width = 0.5)) +
  geom_point(size = 2, position = position_dodge(width = 0.5)) +
  facet_wrap(~exposure) +
  scale_colour_manual(values = c("#1F6F78", "#E9A23B")) + scale_shape_manual(values = c(16, 17)) +
  labs(x = "Matched difference in change, % of treated-grid baseline (95% CI)", y = NULL, colour = NULL, shape = NULL) +
  theme_classic(base_size = 10, base_family = "Arial Unicode MS") +
  theme(legend.position = "top", strip.background = element_blank(), strip.text = element_text(face = "bold"))
ggsave(file.path(res, "figures", "grid_birds_pv.png"), fig, width = 8.5, height = 4.2, dpi = 300)
ggsave(file.path(res, "figures", "grid_birds_pv.pdf"), fig, width = 8.5, height = 4.2, device = cairo_pdf)
cat("done\n")
