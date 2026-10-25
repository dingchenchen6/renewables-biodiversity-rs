# ============================================================
# Figure 4 / 图 4: Biodiversity exposure of realised siting (BirdLife BOTW 2025 + IUCN ranges)
# 实际选址的生物多样性暴露
# Message / 核心信息: after 2022 new PV moved to species-poorer cells, beyond what
#   provincial composition explains, while the share of new PV inside the ranges of
#   dryland threatened species rose. 2022 年后新建光伏落在更贫乏的位置(超出省份构成
#   的解释),但落在旱地受威胁物种分布区内的比例上升。
# Panels / 分图: a species richness (3,165 species, 5 km) with new PV sites;
#   b facility-weighted national percentile vs province-preserving null;
#   c change 2018-21 -> 2022-24 by taxon (PV); d share of new PV inside ranges of
#   the ten most-exposed threatened species, by period.
# Uncertainty / 不确定性: 25 km spatial-block bootstrap (999); null = 999 random
#   relocations within the same province.
# Output / 输出: figures_main/fig4_exposure.pdf|png
# ============================================================
suppressPackageStartupMessages({ library(sf); library(terra); library(data.table) })
source("analysis/fig_theme.R"); source("analysis/china_std_map.R"); sf_use_s2(FALSE)
L <- cn_load()
# 物种丰富度栅格 / richness raster on the 5 km grid
ALB <- CN_CRS
prov0 <- st_transform(st_make_valid(st_read("/Users/dingchenchen/Documents/New project/bird_dynamic_occupancy_analysis/data/china_boundary/省面.shp", quiet = TRUE)), ALB)
tmpl <- rast(ext(vect(st_union(prov0))), resolution = 5000, crs = ALB)
rg <- readRDS("cache/exposure_range_cells.rds")
cnt <- tabulate(unlist(lapply(rg, `[[`, "cells")), nbins = ncell(tmpl))
rich <- data.table(cell = which(cnt > 0), S = cnt[cnt > 0])
rich <- rich[cell %in% cells(rasterize(vect(st_union(prov0)), tmpl))]   # 只保留陆地网格 / land cells only
xy <- xyFromCell(tmpl, rich$cell); rich[, `:=`(x = xy[, 1], y = xy[, 2])]
inside <- lengths(st_intersects(st_as_sf(rich, coords = c("x", "y"), crs = CN_CRS), L$china)) > 0
rich <- rich[inside]   # 限于标准地图国土 / within the standard-map territory
pv <- fread("cache/pv_centroids.csv")[, period := fifelse(year <= 2021, "2018–2021", "2022–2024")]
p <- st_coordinates(st_transform(st_as_sf(pv, coords = c("lon", "lat"), crs = 4326), ALB)); pv[, `:=`(x = p[, 1], y = p[, 2])]
pv_big <- pv[area >= 1e5]   # 只画 ≥ 10 ha 站点,避免遮挡 / sites >= 10 ha only, for legibility
pa <- cn_map(list(geom_raster(data = rich, aes(x, y, fill = S)),
                  geom_point(data = pv_big[order(-area)], aes(x, y, colour = period, size = area / 1e6), shape = 16, alpha = 0.8)),
             list(scale_fill_gradientn(colours = c("#f7f5ef", PAL$seq[c(2, 5, 8, 11, 13)]), name = "Species (5 km)", breaks = c(200, 400, 600, 800)),
                  scale_colour_manual(values = c(COL$p1, COL$p2), name = "New PV ≥10 ha"),
                  scale_size_area(max_size = 2.6, guide = "none"),
                  guides(fill = guide_colourbar(title.position = "top", direction = "horizontal", order = 2, barwidth = unit(40, "pt"), barheight = unit(4, "pt")),
                         colour = guide_legend(order = 1, override.aes = list(size = 1.8))),
                  theme(legend.box = "vertical")), tag = "a")
# cn_theme() 是完整主题会覆盖前面的设置,图例间距须在之后追加 / cn_theme() is complete, so add legend spacing afterwards
pa <- pa & theme(legend.spacing.y = unit(6, "pt"))

ob <- fread("results/rigor_exposure_observed.csv")[taxon == "all" & metric %in% c("pct_S", "pct_thr")]
nl <- fread("results/rigor_exposure_null.csv")[metric %in% c("pct_S", "pct_thr")]
lab_m <- c(pct_S = "All species", pct_thr = "Threatened species")
ob[, `:=`(metric = factor(lab_m[metric], levels = lab_m), grp = paste(type, period))]
nl[, `:=`(metric = factor(lab_m[metric], levels = lab_m), grp = paste(type, period))]
lv <- c("PV 2018-2021", "PV 2022-2024", "wind 2018-2021", "wind 2022-2024")
ob[, grp := factor(grp, levels = rev(lv))]; nl[, grp := factor(grp, levels = rev(lv))]
nl[, p_lab := ifelse(p_lower < 0.001, "P < 0.001", sprintf("P = %.3f", p_lower))]
pb <- ggplot() + geom_vline(xintercept = 50, colour = PAL$axis, linewidth = 0.3, linetype = "dashed") +
  geom_errorbarh(data = nl, aes(y = grp, xmin = null_lo, xmax = null_hi), height = 0.35, colour = PAL$muted, linewidth = 1.6, alpha = 0.45) +
  geom_errorbarh(data = ob, aes(y = grp, xmin = lo, xmax = hi, colour = grepl("2022", grp)), height = 0, linewidth = 0.55) +
  geom_point(data = ob, aes(est, grp, colour = grepl("2022", grp)), size = 1.6) +
  geom_text(data = nl, aes(x = 62, y = grp, label = p_lab), hjust = 0, size = 1.8, colour = PAL$ink2) +
  facet_wrap(~metric, ncol = 1) + scale_colour_manual(values = c(`FALSE` = COL$p1, `TRUE` = COL$p2), guide = "none") +
  scale_y_discrete(labels = function(x) sub("wind", "Wind", sub("-", "–", x))) + coord_cartesian(xlim = c(28, 72), clip = "off") +
  labs(x = "Facility-weighted national percentile", y = NULL, tag = "b", subtitle = "Points: observed (25-km block bootstrap 95% CI)\nGrey: random siting within the same province (95%); P, one-sided") +
  theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = PAL$grid, linewidth = 0.2), plot.margin = margin(3, 22, 3, 3),
        plot.subtitle = element_text(size = 5.6, colour = PAL$ink2, lineheight = 1.05))

df <- fread("results/rigor_exposure_period_diff.csv")[type == "PV" & metric %in% c("pct_S", "pct_thr")]
df[, `:=`(metric = factor(lab_m[metric], levels = lab_m), taxon = factor(c(all = "All taxa", birds = "Birds", mammals = "Mammals", amphibians = "Amphibians", reptiles = "Reptiles")[taxon], levels = rev(c("All taxa", "Birds", "Mammals", "Amphibians", "Reptiles"))))]
pc <- ggplot(df, aes(diff, taxon, colour = metric)) + geom_vline(xintercept = 0, colour = PAL$axis, linewidth = 0.3) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0, linewidth = 0.5, position = position_dodge(width = 0.55)) +
  geom_point(size = 1.5, position = position_dodge(width = 0.55)) +
  scale_colour_manual(values = c(PAL$cat[1], PAL$cat[2]), name = NULL, guide = guide_legend(reverse = TRUE)) +
  labs(x = "Change in percentile, 2018–21 to 2022–24 (PV)", y = NULL, tag = "c") +
  theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = PAL$grid, linewidth = 0.2))

sh <- fread("results/rigor_exposure_species_shares.csv")
cn <- c("Falco cherrug" = "Saker falcon", "Aquila nipalensis" = "Steppe eagle", "Gazella subgutturosa" = "Goitered gazelle",
        "Vormela peregusna" = "Marbled polecat", "Haliaeetus leucoryphus" = "Pallas's fish eagle", "Elaphe taeniura" = "Beauty rat snake",
        "Halcyon pileata" = "Black-capped kingfisher", "Aquila heliaca" = "Eastern imperial eagle", "Gekko swinhonis" = "Swinhoe's gecko",
        "Corvus pectoralis" = "Collared crow")
dif <- sh[period == "difference"][order(share)]
sh2 <- sh[period != "difference"]
sh2[, lab := factor(sprintf("%s (%s)", cn[species], category), levels = sprintf("%s (%s)", cn[dif$species], dif$category))]
dif[, lab := factor(sprintf("%s (%s)", cn[species], category), levels = levels(sh2$lab))]
dif[, sig := ifelse(lo > 0 | hi < 0, "*", "")]
seg <- dcast(sh2, lab ~ period, value.var = "share")
pd <- ggplot() + geom_segment(data = seg, aes(x = 100 * `2018-2021`, xend = 100 * `2022-2024`, y = lab, yend = lab), colour = PAL$axis, linewidth = 0.6) +
  geom_errorbarh(data = sh2, aes(y = lab, xmin = 100 * lo, xmax = 100 * hi, colour = period), height = 0, linewidth = 0.35, alpha = 0.7,
                 position = position_dodge(width = 0.45)) +
  geom_point(data = sh2, aes(100 * share, lab, colour = period), size = 1.6, position = position_dodge(width = 0.45)) +
  geom_text(data = dif, aes(x = 101, y = lab, label = sprintf("%+.0f%s", 100 * share, sig)), hjust = 0, size = 1.9, colour = PAL$ink1) +
  scale_colour_manual(values = c(`2018-2021` = COL$p1, `2022-2024` = COL$p2), labels = c("2018–2021", "2022–2024"), name = NULL) +
  coord_cartesian(xlim = c(0, 100), clip = "off") +
  labs(x = "Share of new PV area inside the species' range (%)", y = NULL, tag = "d") +
  theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = PAL$grid, linewidth = 0.2), plot.margin = margin(3, 24, 3, 3))
# 地图单独封装,不与 d 的长 y 轴标签对齐,地图才能占满左上格
# Wrap the map and the bottom row so the map is not squeezed by axis alignment with d
right <- pb / pc + plot_layout(heights = c(1.5, 1))
fig <- (wrap_elements(full = pa) | right) / wrap_elements(full = pd) + plot_layout(heights = c(1.3, 1))
fig[[1]] <- fig[[1]] + plot_layout(widths = c(1.45, 1))
save_fig(fig, "fig4_exposure", h_mm = 175)
cat("done\n")
