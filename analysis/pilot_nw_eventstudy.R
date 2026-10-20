# ============================================================
# Scientific question / 科学问题:
# In northwest China's drylands, does ground-mounted PV construction
# change greenness (NDVI mean) and its spatial heterogeneity (NDVI SD),
# on site and in the 60-500 m ring, relative to not-yet-built sites?
# 西北旱区地面光伏建设是否改变站内及 60-500 m 外围的绿度(NDVI 均值)
# 与空间异质性(NDVI 标准差)?对照为尚未建设的站点。
#
# Objective / 分析目标:
# Staggered DiD pilot for M3/M4: H1 displacement (NDVI down, SD down),
# H2 neutral or greening (NDVI 0 or up, SD 0), H3 homogenisation
# (NDVI up, SD down). 交错 DiD 试点,判别 H1/H2/H3。
#
# Input data / 输入数据:
#   results/pilot_nw_panel.csv (gee/09): site-year NDVI mean/SD, 2016-2024
#   results/clcd_preconstruction_solar_by_site.csv (gee/04): prior-year CLCD
# Workflow / 主要流程:
#   1. Clean panel; cohort g = GRW construction year (TZ-SAM consistent)
#      清洗面板;处理年 g 取 GRW 建设年(已与 TZ-SAM 核对一致)
#   2. Callaway-Sant'Anna ATT(g,t), not-yet-treated controls, dynamic
#      aggregation e = -5..3 / CS 估计,尚未建设站点作对照,动态聚合
#   3. Sun-Abraham (fixest::sunab) on years <= 2022 as a cross-check
#      Sun-Abraham 互证(限 2022 年前,使 2023 队列成为窗口内未处理组)
#   4. Overall ATT (e >= 1) by prior-year cover: barren / grassland / cropland
#      按建设前覆盖分层的总体 ATT(建后第 1 年起)
# Expected output / 预期输出:
#   results/pilot_nw_eventstudy.csv, results/pilot_nw_att.csv,
#   results/figures/pilot_nw_eventstudy.png|pdf
# Key assumptions / 关键假设:
#   Parallel trends conditional on cohort; year g is a transition year;
#   on-site 30 m NDVI after construction mixes panels and inter-row ground,
#   so on-site change is partly mechanical; the ring is the cleaner outcome.
#   以队列为条件的平行趋势;建设当年为过渡期;建后站内 30 m 像元混合面板与
#   板间地表,站内变化部分是机械性的,外围环更干净。
# Main packages / 主要包: data.table, did, fixest, ggplot2
# Output directory / 输出路径: results/
# ============================================================

suppressPackageStartupMessages({
  library(data.table)  # 数据处理 / data wrangling
  library(did)         # Callaway-Sant'Anna
  library(fixest)      # Sun-Abraham
  library(ggplot2)     # 作图 / plotting
})
set.seed(42)

args <- commandArgs(trailingOnly = FALSE)
here <- dirname(normalizePath(sub("^--file=", "", args[grep("^--file=", args)])))
res <- file.path(dirname(here), "results")

# ---- 1. 读取与清洗 / load and clean ----------------------------------------
p <- fread(file.path(res, "pilot_nw_panel.csv"))
p <- p[!is.na(site_mean) & site_count >= 20]          # 站内至少 20 个有效像元 / >= 20 valid pixels
p[, g := construction_year]
p[, fid := as.integer(fid)]

cl <- fread(file.path(res, "clcd_preconstruction_solar_by_site.csv"))
sh <- grep("^share_", names(cl), value = TRUE)
cl[, cover := sub("share_", "", sh[max.col(replace(as.matrix(.SD), is.na(as.matrix(.SD)), -1))]), .SDcols = sh]
p <- merge(p, cl[, .(fid, cover)], by = "fid", all.x = TRUE)
cat("sites:", uniqueN(p$fid), " rows:", nrow(p), "\n")
print(p[, .(n = uniqueN(fid)), by = .(g)][order(g)])
print(p[, .(n = uniqueN(fid)), by = .(cover)][order(-n)])

outcomes <- c(site_mean = "站内 NDVI 均值 / on-site NDVI mean",
              site_stdDev = "站内 NDVI 标准差 / on-site NDVI SD",
              ring_mean = "外围环 NDVI 均值 / ring NDVI mean",
              ring_stdDev = "外围环 NDVI 标准差 / ring NDVI SD")

# ---- 2. Callaway-Sant'Anna ----------------------------------------------
cs_fit <- function(y, d) {
  d <- d[!is.na(get(y))]
  att_gt(yname = y, tname = "year", idname = "fid", gname = "g", data = as.data.frame(d),
         control_group = "notyettreated", base_period = "universal",
         allow_unbalanced_panel = TRUE, est_method = "reg", bstrap = TRUE, biters = 999,
         clustervars = "fid", print_details = FALSE)
}

# 建设前系数的联合 Wald 检验 / joint Wald test of pre-period event-study coefficients
pre_wald <- function(dyn, antic = 0) {
  pre <- dyn$egt < -1 - antic
  inf <- dyn$inf.function$dynamic.inf.func.e[, pre, drop = FALSE]
  b <- dyn$att.egt[pre]
  W <- as.numeric(t(b) %*% solve(crossprod(inf) / nrow(inf)^2, b))
  pchisq(W, df = length(b), lower.tail = FALSE)
}

es_rows <- list(); att_rows <- list()
for (y in names(outcomes)) {
  a <- cs_fit(y, p)
  dyn <- aggte(a, type = "dynamic", min_e = -5, max_e = 3, na.rm = TRUE)
  es_rows[[y]] <- data.table(outcome = y, method = "CS", e = dyn$egt, est = dyn$att.egt,
                             se = dyn$se.egt, crit = dyn$crit.val.egt)
  # 总体 ATT 只取建后第 1 年起(建设当年为过渡期)/ overall ATT for e >= 1
  ov <- aggte(a, type = "dynamic", min_e = 1, max_e = 3, na.rm = TRUE)
  att_rows[[paste(y, "all")]] <- data.table(outcome = y, stratum = "all", att = ov$overall.att,
                                            se = ov$overall.se, n_sites = uniqueN(p[!is.na(get(y))]$fid),
                                            pretest_p = pre_wald(dyn))
  for (s in c("barren", "grassland", "cropland")) {
    ds <- p[cover == s]
    if (uniqueN(ds[g <= 2021]$fid) < 15 || uniqueN(ds[g >= 2022]$fid) < 15) next
    as <- tryCatch(cs_fit(y, ds), error = function(e) NULL)
    if (is.null(as)) next
    os <- aggte(as, type = "dynamic", min_e = 1, max_e = 3, na.rm = TRUE)
    ds_dyn <- aggte(as, type = "dynamic", min_e = -5, max_e = 3, na.rm = TRUE)
    att_rows[[paste(y, s)]] <- data.table(outcome = y, stratum = s, att = os$overall.att, se = os$overall.se,
                                          n_sites = uniqueN(ds$fid), pretest_p = pre_wald(ds_dyn))
  }
}

# ---- 3. Sun-Abraham 互证 / cross-check ------------------------------------
q <- p[year <= 2022]
q[, g_sa := fifelse(g >= 2023, 10000L, as.integer(g))]   # 2023 队列在窗口内未处理 / never treated in window
for (y in names(outcomes)) {
  f <- feols(as.formula(paste0(y, " ~ sunab(g_sa, year, ref.p = -1) | fid + year")),
             data = q, cluster = ~fid, notes = FALSE)
  ct <- as.data.table(coeftable(f), keep.rownames = "term")
  ct <- ct[grepl("^year::", term)]
  ct[, e := as.integer(sub("year::", "", term))]
  es_rows[[paste(y, "SA")]] <- data.table(outcome = y, method = "SA", e = ct$e, est = ct$Estimate,
                                          se = ct$`Std. Error`, crit = 1.96)
}

es <- rbindlist(es_rows); att <- rbindlist(att_rows)
es[, `:=`(lo = est - crit * se, hi = est + crit * se)]
att[, `:=`(lo = att - 1.96 * se, hi = att + 1.96 * se)]
fwrite(es, file.path(res, "pilot_nw_eventstudy.csv"))
fwrite(att, file.path(res, "pilot_nw_att.csv"))
print(att[, lapply(.SD, function(v) if (is.numeric(v)) signif(v, 3) else v)])

# ---- 4. 事件研究图 / event-study figure ------------------------------------
es[, panel := factor(outcomes[outcome], levels = outcomes)]
es <- es[e >= -5 & e <= 3]
pd <- position_dodge(width = 0.45)
fig <- ggplot(es, aes(e, est, colour = method, shape = method)) +
  annotate("rect", xmin = -0.5, xmax = 0.5, ymin = -Inf, ymax = Inf, fill = "grey90") +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_vline(xintercept = -0.5, linetype = "dashed", linewidth = 0.3) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0, position = pd) +
  geom_point(size = 1.8, position = pd) +
  facet_wrap(~panel, scales = "free_y", ncol = 2) +
  scale_colour_manual(values = c(CS = "#1F6F78", SA = "#E9A23B"),
                      labels = c(CS = "Callaway-Sant'Anna", SA = "Sun-Abraham")) +
  scale_shape_manual(values = c(CS = 16, SA = 17),
                     labels = c(CS = "Callaway-Sant'Anna", SA = "Sun-Abraham")) +
  scale_x_continuous(breaks = -5:3) +
  labs(x = "Years relative to construction (grey = construction year)",
       y = "Effect on NDVI statistic", colour = NULL, shape = NULL) +
  theme_classic(base_size = 10, base_family = "Arial Unicode MS") +
  theme(legend.position = "top", strip.background = element_blank(),
        strip.text = element_text(face = "bold"))
ggsave(file.path(res, "figures", "pilot_nw_eventstudy.png"), fig, width = 7.5, height = 5.6, dpi = 300)
ggsave(file.path(res, "figures", "pilot_nw_eventstudy.pdf"), fig, width = 7.5, height = 5.6, device = cairo_pdf)
cat("done\n")
