# ============================================================
# Figure 5 / 图 5: Can citizen-science bird data detect a local effect of PV
#   construction? Near (0-5 km) vs far (8-20 km) BACI at the site level
# 公民科学鸟类数据能否检出光伏建设的局地效应?站点水平近区(0-5 km)对远区(8-20 km)BACI
# Message / 核心信息: only 38 of 7,109 dated PV sites have >= 3 checklists in every
#   zone x period cell; effects on richness, Hill numbers and coverage-standardised
#   diversity are all compatible with zero once few-cluster inference is used, and the
#   occupancy result changes sign between the 38- and 24-site samples.
#   7,109 个日期可靠的光伏站点中只有 38 个满足四格各 >= 3 份清单;采用少聚类推断后,
#   丰富度、Hill 数与覆盖度标准化多样性的效应都与 0 相容;占域模型结果在 38 站与 24 站样本间符号相反。
# Panels / 分图: a eligible sites (CBNR-style China map); b data funnel;
#   c effect estimates across outcomes (WCB = wild cluster bootstrap, Webb weights, 999 draws);
#   d event study of species per checklist; e leave-one-site-out; f occupancy near x post.
# Input / 输入: results/rigor_birds_*.csv, results/site_birds_*.csv, cache/pv_centroids.csv
#   (only site centroids are plotted; no record-level bird data)
# Output / 输出: figures_main/fig5_birds.pdf|png
# ============================================================
suppressPackageStartupMessages({ library(data.table); library(sf) })
source("analysis/fig_theme.R"); source("analysis/china_std_map.R")
pct <- function(x) 100 * (exp(x) - 1)

# --- a: 站点地图 / site map ---------------------------------------------------
el <- fread("results/site_birds_eligibility_b.csv")
tz <- fread("results/g1_tzsam_crosscheck.csv")[verdict == "consistent" & overlap >= 0.5]
pv <- fread("cache/pv_centroids.csv")
n_dated <- pv[fid %in% tz$fid & year %between% c(2018, 2023), .N]
el[, all4 := pmin(far_post, far_pre, near_post, near_pre)]
el <- merge(el, pv[, .(site = fid, lon, lat)], by = "site")
el[, grp := fcase(all4 >= 5, "All four cells ≥ 5", all4 >= 3, "All four cells ≥ 3", default = "≥ 1 checklist within 20 km")]
el[, grp := factor(grp, levels = c("≥ 1 checklist within 20 km", "All four cells ≥ 3", "All four cells ≥ 5"))]
xy <- st_coordinates(st_transform(st_as_sf(el, coords = c("lon", "lat"), crs = 4326), CN_CRS)); el[, `:=`(x = xy[, 1], y = xy[, 2])]
pa <- cn_map(list(geom_point(data = el[order(grp)], aes(x, y, colour = grp, size = grp), shape = 16, alpha = 0.9)),
             list(scale_colour_manual(values = c(PAL$muted, PAL$cat[1], PAL$cat[2]), name = "PV sites built 2018–2023"),
                  scale_size_manual(values = c(0.35, 1.3, 1.3), name = "PV sites built 2018–2023")), tag = "a")
pa <- pa & theme(legend.spacing.y = unit(4, "pt"))

# --- b: 数据漏斗 / data funnel ----------------------------------------------------
fn <- data.table(step = c("PV sites with verified dates", "≥ 1 checklist within 20 km", "All four cells ≥ 1",
                          "All four cells ≥ 3", "All four cells ≥ 5"),
                 n = c(n_dated, nrow(el), el[all4 >= 1, .N], el[all4 >= 3, .N], el[all4 >= 5, .N]))
fn[, step := factor(step, levels = rev(step))]
# 对数轴上 geom_col 从 0 起画会被丢弃,改用从 10 起的粗线段 / log axis: draw bars as thick segments from 10
pb <- ggplot(fn, aes(n, step)) + geom_segment(aes(x = 10, xend = n, yend = step, colour = step), linewidth = 5.5, lineend = "butt") +
  scale_colour_manual(values = setNames(PAL$seq[c(4, 6, 8, 10, 12)], levels(fn$step)), guide = "none") +
  geom_text(aes(label = format(n, big.mark = ",")), hjust = -0.15, size = 2.1, colour = PAL$ink1) +
  scale_x_log10(limits = c(10, 3e4), breaks = c(10, 100, 1000, 10000), labels = c("10", "100", "1,000", "10,000"), expand = c(0, 0)) +
  labs(x = "Number of sites (log scale)", y = NULL, tag = "b") +
  theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = PAL$grid, linewidth = 0.2))

# --- c: 效应汇总 / effect estimates across outcomes -------------------------------
w <- fread("results/rigor_birds_wcb.csv"); cv <- fread("results/rigor_birds_coverage.csv")[coverage == 0.6]
d3 <- fread("results/site_birds_did_min3.csv"); d5 <- fread("results/site_birds_did_min5.csv")
a3 <- fread("results/site_birds_abundance_did_min3.csv"); a5 <- fread("results/site_birds_abundance_did_min5.csv")
smp <- function(n) ifelse(n >= 38, "38 sites (cells ≥ 3)", "24 sites (cells ≥ 5)")
rows <- rbind(
  w[, .(lab = ifelse(grepl("species", outcome), "Species per checklist", "Hill q = 1, cleaned"), sample = smp(n_sites), est = pct, lo = lo_pct, hi = hi_pct, ci = "Wild cluster bootstrap")],
  rbind(d3, d5)[model != "log richness", .(lab = ifelse(grepl("observer", model), "Species per checklist + observer FE", "Species per checklist, Poisson"), sample = smp(n_sites), est = pct, lo = lo_pct, hi = hi_pct, ci = "Cluster-robust")],
  rbind(a3, a5)[grepl("^2", tier) & !(outcome == "Hill q=1 (exp Shannon)" & tier == "2 cleaned + effort"),
                .(lab = fcase(outcome == "Hill q=2 (inverse Simpson)", "Hill q = 2, cleaned",
                              outcome == "log total individuals", "Total individuals, cleaned",
                              grepl("observer", tier), "Hill q = 1, cleaned + observer FE",
                              default = "Hill q = 1, presence-only lists kept"),
                  sample = smp(n_sites), est = pct, lo = pct(lo), hi = pct(hi), ci = "Cluster-robust")],
  cv[, .(lab = sprintf("Coverage-standardised q = %d (C = 0.6)", q), sample = smp(n_sites), est = pct, lo = lo_pct, hi = hi_pct, ci = "Wild cluster bootstrap")])
lv <- c("Species per checklist", "Species per checklist + observer FE", "Species per checklist, Poisson",
        "Hill q = 1, cleaned", "Hill q = 1, cleaned + observer FE", "Hill q = 1, presence-only lists kept", "Hill q = 2, cleaned",
        "Total individuals, cleaned", sprintf("Coverage-standardised q = %d (C = 0.6)", 0:2))
rows[, `:=`(lab = factor(lab, levels = rev(lv)), sample = factor(sample, levels = c("38 sites (cells ≥ 3)", "24 sites (cells ≥ 5)")))]
dodge <- position_dodge(width = 0.6)
pc <- ggplot(rows, aes(est, lab, colour = sample, shape = ci)) + geom_vline(xintercept = 0, colour = PAL$ink2, linewidth = 0.3) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0, linewidth = 0.45, position = dodge) +
  geom_point(size = 1.4, position = dodge, fill = "white", stroke = 0.5) +
  scale_colour_manual(values = c(PAL$cat[1], PAL$cat[2]), name = NULL) +
  scale_shape_manual(values = c(`Wild cluster bootstrap` = 16, `Cluster-robust` = 21), name = NULL) +
  coord_cartesian(xlim = c(-45, 50)) +
  labs(x = "Near-zone change after construction relative to far zone (%, 95% CI)", y = NULL, tag = "c") +
  guides(colour = guide_legend(order = 1, nrow = 1), shape = guide_legend(order = 2, nrow = 1)) +
  theme(legend.position = "top", legend.box = "vertical", legend.spacing.y = unit(0, "pt"), legend.margin = margin(0, 0, 0, 0),
        panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = PAL$grid, linewidth = 0.2), axis.text.y = element_text(size = 5.6))

# --- d: 事件研究 / event study, species per checklist ----------------------------
es <- rbind(fread("results/site_birds_es_min3.csv")[, sample := "38 sites (cells ≥ 3)"], fread("results/site_birds_es_min5.csv")[, sample := "24 sites (cells ≥ 5)"])
es[, `:=`(est_p = pct(est), lo = pct(est - 1.96 * se), hi = pct(est + 1.96 * se), sample = factor(sample, levels = levels(rows$sample)))]
pd <- ggplot(es, aes(e, est_p, colour = sample)) +
  annotate("rect", xmin = -0.5, xmax = 0.5, ymin = -Inf, ymax = Inf, fill = PAL$seq[1], alpha = 0.7) +
  geom_hline(yintercept = 0, colour = PAL$ink2, linewidth = 0.3) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0, linewidth = 0.45, position = position_dodge(width = 0.45)) +
  geom_point(size = 1.3, position = position_dodge(width = 0.45)) +
  scale_colour_manual(values = c(PAL$cat[1], PAL$cat[2]), guide = "none") + scale_x_continuous(breaks = -4:3) +
  labs(x = "Years relative to construction", y = "Species per checklist, near vs far (%)", tag = "d")

# --- e: 逐站剔除 / leave-one-site-out ---------------------------------------------
lo <- fread("results/rigor_birds_loso.csv")[, `:=`(est_p = pct(estimate), sample = factor(smp(ifelse(min_cell == 3, 38, 24)), levels = levels(rows$sample)))]
full <- w[grepl("species", outcome), .(sample = factor(smp(n_sites), levels = levels(rows$sample)), est_p = pct)]
pe <- ggplot(lo, aes(est_p, sample, colour = sample)) + geom_vline(xintercept = 0, colour = PAL$ink2, linewidth = 0.3) +
  geom_point(position = position_jitter(height = 0.12, seed = 1), size = 0.9, alpha = 0.7) +
  geom_point(data = full, shape = 124, size = 4, colour = PAL$ink1) +
  scale_colour_manual(values = c(PAL$cat[1], PAL$cat[2]), guide = "none") +
  labs(x = "Species per checklist, near vs far (%)\nwith one site removed (bar: all sites)", y = NULL, tag = "e") +
  theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = PAL$grid, linewidth = 0.2))

# --- f: 多物种占域 / multispecies occupancy, community near x post --------------------
oc <- rbind(fread("results/site_birds_occ_min3.csv")[term == "near:post", .(spec = "All sites", sample = "38 sites (cells ≥ 3)", mean = as.numeric(mean), lo = as.numeric(lo), hi = as.numeric(hi))],
            fread("results/site_birds_occ_min5.csv")[term == "near:post", .(spec = "All sites", sample = "24 sites (cells ≥ 5)", mean = as.numeric(mean), lo = as.numeric(lo), hi = as.numeric(hi))])
if (file.exists("results/rigor_birds_occ_loso.csv")) {
  ol <- fread("results/rigor_birds_occ_loso.csv")[!is.na(dropped)]
  oc <- rbind(oc, ol[, .(spec = "One site removed", sample = "24 sites (cells ≥ 5)", mean, lo, hi)])
}
oc[, sample := factor(sample, levels = levels(rows$sample))]
pf <- ggplot(oc[spec == "All sites"], aes(mean, sample, colour = sample)) + geom_vline(xintercept = 0, colour = PAL$ink2, linewidth = 0.3) +
  { if (any(oc$spec != "All sites")) geom_point(data = oc[spec != "All sites"], position = position_nudge(y = -0.25), size = 0.8, alpha = 0.6, shape = 16) } +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0, linewidth = 0.5) + geom_point(size = 1.6) +
  scale_colour_manual(values = c(PAL$cat[1], PAL$cat[2]), guide = "none") +
  labs(x = "Community near × post on occupancy\n(logit, posterior mean and 95% CrI)", y = NULL, tag = "f") +
  theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = PAL$grid, linewidth = 0.2))

# --- 拼版 / assemble: rows wrapped separately so long labels do not squeeze the map ---
r1 <- wrap_elements(full = pa) | pb
r2 <- pc | pd
r3 <- pe | pf
fig <- (r1 + plot_layout(widths = c(1.5, 1))) / (r2 + plot_layout(widths = c(1.55, 1))) / (r3 + plot_layout(widths = c(1, 1))) +
  plot_layout(heights = c(1.25, 1.1, 0.55))
save_fig(fig, "fig5_birds", h_mm = 205)
# 幻灯片用的单独 c 图 / panel c alone for the slide deck
dir.create("figures_main/panels", showWarnings = FALSE)
save_fig(pc + labs(tag = NULL), "fig5c_bird_effects", h_mm = 92, w_mm = 150, dir = "figures_main/panels")
cat("dated sites:", n_dated, " funnel:", paste(fn$n, collapse = " > "), "\n")
cat("done\n")
