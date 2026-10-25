# ============================================================
# Figure 2 / 图 2: Can the facility data support a staggered design?
# 设施数据能否支撑交错设计:建设日期与建设前覆盖的校验
# Message / 核心信息: GRW dates are mostly right but ~15% lag by about a year, so the
#   design uses GRW-TZ-SAM consistent sites; land-cover products agree moderately
#   (kappa 0.62) and disagree most on forest; forest turbines are closer to CLCD.
#   GRW 日期大多可靠但约 15% 偏晚约一年;两套覆盖产品中度一致,森林分歧最大;森林风机更接近 CLCD。
# Panels / 分图: a Sentinel-2 break offset (n = 100); b GRW vs TZ-SAM by build year
#   (n = 7,987); c dominant-class agreement CLCD vs GLC (area %); d forest share by
#   stratum (Beta posterior, n = 25 interpretable images) and total.
# Uncertainty / 不确定性: Wilson 95% CI (a, b); bootstrap kappa CI; Beta(1,1) posterior (d)
# Output / 输出: figures_main/fig2_quality.pdf|png
# ============================================================
suppressPackageStartupMessages(library(data.table))
source("analysis/fig_theme.R")
wilson <- function(x, n, z = 1.96) { p <- x / n; c0 <- (p + z^2 / (2 * n)) / (1 + z^2 / n); h <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / (1 + z^2 / n); c(p, c0 - h, c0 + h) }
rs <- fread("results/rigor_small.csv")

b <- fread("results/g1_breaks_seasonal.csv")
bo <- b[, .N, by = break_offset][order(break_offset)]
w77 <- rs[item == "G1: S2 break within +/-2 quarters"]
pa <- ggplot(bo, aes(break_offset, N)) +
  annotate("rect", xmin = -2.5, xmax = 2.5, ymin = -Inf, ymax = Inf, fill = PAL$seq[1], alpha = 0.6) +
  geom_col(width = 0.75, fill = PAL$cat[1]) + geom_vline(xintercept = 0, colour = PAL$ink2, linewidth = 0.3, linetype = "dashed") +
  annotate("text", x = -5.3, y = max(bo$N) * 1.02, hjust = 0, vjust = 1, size = 2.1, colour = PAL$ink1,
           label = sprintf("Within ±2 quarters: %.0f%%\n(95%% CI %.0f–%.0f%%; n = %d)", 100 * w77$est, 100 * w77$lo, 100 * w77$hi, w77$n)) +
  scale_x_continuous(breaks = -5:4) + scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Sentinel-2 break minus GRW quarter", y = "Sites", tag = "a")

tz <- fread("results/g1_tzsam_crosscheck.csv")[overlap >= 0.5 & as.IDate(tz_U) >= as.IDate("2018-04-01")]
ty <- tz[, .N, by = .(construction_year, verdict)][, n := sum(N), by = construction_year][, p := N / n]
ty[, verdict := factor(verdict, levels = c("grw_early", "grw_late", "consistent"), labels = c("GRW early", "GRW late", "Consistent"))]
ny <- unique(ty[, .(construction_year, n)])
pb <- ggplot(ty, aes(factor(construction_year), 100 * p, fill = verdict)) + geom_col(width = 0.72, colour = "white", linewidth = 0.2) +
  geom_text(data = ny, aes(factor(construction_year), 103, label = format(n, big.mark = ",")), inherit.aes = FALSE, size = 1.9, colour = PAL$ink2) +
  scale_fill_manual(values = c("GRW early" = PAL$cat[4], "GRW late" = PAL$cat[2], "Consistent" = PAL$seq[6]), name = NULL, guide = guide_legend(reverse = TRUE)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 100, 25)) + coord_cartesian(ylim = c(0, 106), clip = "off") +
  labs(x = "GRW year of construction", y = "Sites vs TZ-SAM dates (%)", tag = "b")

ct <- fread("results/clcd_vs_glc_pv_dominant_crosstab.csv")
cl <- melt(ct, id.vars = "dom_glc", variable.name = "dom_clcd", value.name = "pct")
lv <- c("barren", "grassland", "cropland", "forest", "shrub", "water", "wetland", "impervious")
cl <- cl[dom_glc %in% lv & dom_clcd %in% lv]
cl[, `:=`(dom_glc = factor(dom_glc, levels = rev(lv)), dom_clcd = factor(dom_clcd, levels = lv))]
kp <- rs[item == "CLCD vs GLC dominant class: Cohen's kappa"]
pc <- ggplot(cl, aes(dom_clcd, dom_glc, fill = pct)) + geom_tile(colour = "white", linewidth = 0.3) +
  geom_text(aes(label = ifelse(pct >= 1, sprintf("%.0f", pct), "")), size = 1.8, colour = ifelse(cl$pct > 55, "white", PAL$ink1)) +
  scale_fill_gradientn(colours = PAL$seq[c(1, 4, 7, 10, 13)], limits = c(0, 100), name = "Area (%)") +
  scale_x_discrete(labels = tools::toTitleCase) + scale_y_discrete(labels = tools::toTitleCase) +
  labs(x = "CLCD v1.0.5 dominant class", y = "GLC_FCS30D dominant class", tag = "c",
       subtitle = sprintf("Cohen's κ = %.2f (95%% CI %.2f–%.2f), n = %s sites", kp$est, kp$lo, kp$hi, format(kp$n, big.mark = ","))) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1), panel.grid = element_blank(), axis.line = element_blank(),
        legend.key.width = unit(14, "pt"), legend.key.height = unit(4, "pt"), plot.subtitle = element_text(size = 6, colour = PAL$ink2))

s <- merge(fread("results/wind_forest_sample.csv"), fread("results/wind_forest_visual_calls.csv"), by = "code")[call %in% c("forest", "nonforest", "edge")]
s[, forest := as.integer(call %in% c("forest", "edge"))]
Nh <- c(both_forest = 3721, clcd_only = 5346, glc_only = 892)
set.seed(42)
st <- rbindlist(lapply(names(Nh), function(h) { x <- sum(s[stratum == h]$forest); n <- nrow(s[stratum == h]); d <- rbeta(20000, 1 + x, 1 + n - x)
  data.table(stratum = h, est = median(d), lo = quantile(d, .025), hi = quantile(d, .975), lab = sprintf("%d/%d images; N = %s", x, n, format(Nh[[h]], big.mark = ","))) }))
st[, stratum := factor(stratum, levels = rev(names(Nh)), labels = rev(c("Both products: forest", "CLCD only: forest", "GLC only: forest")))]
ft <- rs[item == "Forest turbines, sampled strata (Beta posterior; edge = forest)"]
pd <- ggplot(st, aes(est, stratum)) + geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0, colour = PAL$cat[3], linewidth = 0.5) +
  geom_point(size = 1.6, colour = PAL$cat[3]) + geom_text(aes(x = 0.02, label = lab), hjust = 0, vjust = -1.1, size = 1.8, colour = PAL$ink2) +
  scale_x_continuous(limits = c(0, 1), labels = label_percent()) +
  labs(x = "Share interpreted as forest (Beta posterior, 95% CrI)", y = NULL, tag = "d",
       subtitle = sprintf("Forest turbines in sampled strata: %s (95%% CrI %s–%s)", format(round(ft$est, -1), big.mark = ","),
                          format(round(ft$lo, -1), big.mark = ","), format(round(ft$hi, -1), big.mark = ","))) +
  theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = PAL$grid, linewidth = 0.2), plot.subtitle = element_text(size = 6, colour = PAL$ink2))
fig <- (pa | pb) / (pc | pd) + plot_layout(heights = c(1, 1.15))
save_fig(fig, "fig2_quality", h_mm = 120)
cat("done\n")
