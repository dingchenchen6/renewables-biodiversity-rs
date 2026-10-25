# ============================================================
# Figure 3 / 图 3: Did PV construction change greenness and heterogeneity on site
#   and around it? NW dryland pilot, matched never-built design
# 西北旱区试点:光伏建设是否改变站内与周边的绿度和异质性(匹配从未建设点设计)
# Message / 核心信息: the most robust signal is higher NDVI heterogeneity 60-500 m
#   around sites; the on-site NDVI decline is borderline once spatial dependence
#   is accounted for. 最稳健的信号是外围异质性上升;站内绿度下降在考虑空间相关后为边缘显著。
# Panels / 分图: a-d event studies (Callaway-Sant'Anna, 1-year anticipation, reference
#   e = -2; 95% CI from multiplier bootstrap clustered on 50 km blocks); e HonestDiD
#   relative-magnitude bounds (covariance from 199 block-bootstrap replicates);
#   f robustness of the on-site effect.
# Sample / 样本: 362 PV sites vs 530 matched never-built points, 480 blocks
# Output / 输出: figures_main/fig3_pilot.pdf|png
# ============================================================
suppressPackageStartupMessages(library(data.table))
source("analysis/fig_theme.R")
es <- fread("results/rigor_pilot_eventstudy.csv")[anticipation == 1 & e >= -9 & e <= 3]
lab <- c(site_mean = "On-site\nNDVI mean", site_stdDev = "On-site\nNDVI heterogeneity (SD)",
         ring_mean = "Surroundings 60–500 m\nNDVI mean", ring_stdDev = "Surroundings 60–500 m\nNDVI heterogeneity (SD)")
es[, `:=`(lo = est - 1.96 * se, hi = est + 1.96 * se, panel = factor(lab[outcome], levels = lab))]
es[e == -2, `:=`(lo = 0, hi = 0)]
mk_es <- function(y, tg, ylab = NULL) {
  d <- es[outcome == y]
  ggplot(d, aes(e, est)) +
    annotate("rect", xmin = -9.5, xmax = -4.5, ymin = -Inf, ymax = Inf, fill = PAL$grid, alpha = 0.55) +
    annotate("rect", xmin = -1.5, xmax = 0.5, ymin = -Inf, ymax = Inf, fill = PAL$seq[1], alpha = 0.7) +
    geom_hline(yintercept = 0, colour = PAL$ink2, linewidth = 0.3) +
    geom_errorbar(aes(ymin = lo, ymax = hi, colour = e >= 1), width = 0, linewidth = 0.5) +
    geom_point(aes(colour = e >= 1), size = 1.4) +
    scale_colour_manual(values = c(`FALSE` = PAL$muted, `TRUE` = PAL$cat[2]), guide = "none") +
    scale_x_continuous(breaks = seq(-9, 3, 3)) +
    labs(x = "Years relative to construction", y = ylab, title = lab[[y]], tag = tg) +
    theme(plot.title = element_text(size = 6.5, face = "plain"))
}
pa <- mk_es("site_mean", "a", "Effect (NDVI units)"); pb <- mk_es("site_stdDev", "b"); pc <- mk_es("ring_mean", "c"); pd <- mk_es("ring_stdDev", "d")

hd <- fread("results/rigor_pilot_blockboot_honestdid.csv")[anticipation == 1]
hd[, panel := factor(lab[outcome], levels = lab)]
pe <- ggplot(hd, aes(Mbar, ymin = lb, ymax = ub, colour = outcome)) + geom_hline(yintercept = 0, colour = PAL$ink2, linewidth = 0.3) +
  geom_errorbar(width = 0.07, linewidth = 0.5, position = position_dodge(width = 0.1)) +
  scale_colour_manual(values = c(site_mean = PAL$cat[2], ring_stdDev = PAL$cat[1]), labels = c(site_mean = "On-site NDVI mean", ring_stdDev = "Surroundings NDVI SD"), name = NULL, guide = guide_legend(nrow = 2)) +
  scale_x_continuous(breaks = seq(0, 2, 0.5)) +
  labs(x = expression(paste(bar(M), " (post / max pre-period violation)")), y = "Robust 95% CI, mean of e = 1–3", tag = "e") +
  theme(legend.position = "inside", legend.position.inside = c(0.02, 0.02), legend.justification = c(0, 0), plot.margin = margin(3, 8, 3, 3))

rb <- fread("results/rigor_pilot_sensitivity.csv")
mn <- fread("results/rigor_pilot_main.csv")[anticipation == 1 & outcome == "site_mean"]
bb <- fread("results/rigor_pilot_blockboot_att.csv")[anticipation == 1 & outcome == "site_mean"]
mn0 <- fread("results/rigor_pilot_main.csv")[anticipation == 0 & outcome == "site_mean"]
rb <- rbind(data.table(spec = "Main, multiplier (50-km blocks)", att = mn$att, lo = mn$lo, hi = mn$hi, grp = "Main"),
            data.table(spec = "Main, block bootstrap (199)", att = bb$att, lo = bb$lo_pct, hi = bb$hi_pct, grp = "Main"),
            data.table(spec = "No anticipation", att = mn0$att, lo = mn0$lo, hi = mn0$hi, grp = "Spec"),
            rb[, .(spec = sub("Uygur Zizhiqu|Huizu Zizhiqu|Zizhiqu| Sheng", "", spec), att, lo, hi, grp = ifelse(grepl("^drop", spec), "Drop province", "Matching"))])
rb[, spec := sub("Nei Mongol", "Inner Mongolia", spec)]
rb[, spec := sub("without replacement", "no replacement", sub("^drop", "Drop", sub("1:5 with replacement", "1:5 with replacement", spec)))]
rb[, spec := factor(spec, levels = rev(spec))]
pf <- ggplot(rb, aes(att, spec, colour = grp)) + geom_vline(xintercept = 0, colour = PAL$ink2, linewidth = 0.3) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0, linewidth = 0.5) + geom_point(size = 1.4) +
  scale_colour_manual(values = c(Main = PAL$cat[2], Spec = PAL$cat[4], Matching = PAL$cat[1], `Drop province` = PAL$muted), guide = "none") +
  labs(x = "On-site NDVI effect, mean of e = 1–3 (95% CI)", y = NULL, tag = "f") +
  theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = PAL$grid, linewidth = 0.2), axis.text.y = element_text(size = 5.6), plot.margin = margin(3, 8, 3, 3))
# 上排四个事件研究,下排 HonestDiD 与稳健性;两排分别封装,免得 f 的长标签挤压上排面板
# Top row: four event studies; bottom: HonestDiD + robustness (rows wrapped separately so long labels in f do not squeeze a-d)
top <- pa | pb | pc | pd
bot <- pe | pf
fig <- wrap_elements(full = top) / wrap_elements(full = bot) + plot_layout(heights = c(1, 1.05))
save_fig(fig, "fig3_pilot", h_mm = 118)
cat("done\n")
