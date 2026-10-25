# ============================================================
# Shared figure style / 图件公共样式
# 与作者本人红外相机兽类群落项目(14_litdb_analysis/scripts/00_setup.R)的配色与主题一致,
# 中国地图一律用 analysis/china_std_map.R(GS(2024)0650 标准地图 + CBNR 版式)。
# Palette and theme follow the author's camera-trap project; national maps use the
# shared CBNR-layout module. 183 mm double column, Helvetica 7 pt, PDF + 600 dpi PNG.
# ============================================================
suppressPackageStartupMessages({ library(ggplot2); library(patchwork); library(scales); library(ragg) })
PAL <- list(
  cat  = c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300", "#4a3aa7", "#e34948"),
  seq  = c("#cde2fb", "#b7d3f6", "#9ec5f4", "#86b6ef", "#6da7ec", "#5598e7", "#3987e5",
           "#2a78d6", "#256abf", "#1c5cab", "#184f95", "#104281", "#0d366b"),
  ink1 = "#0b0b0b", ink2 = "#52514e", muted = "#898781", grid = "#e1e0d9", axis = "#c3c2b7")
BASE <- 7
COL <- list(p1 = PAL$cat[1], p2 = PAL$cat[2], pv = PAL$cat[2], wind = PAL$cat[1], pos = PAL$cat[3], neg = PAL$cat[2],
            ink = PAL$ink1, ref = PAL$ink2, muted = PAL$muted)
# 土地覆盖:低饱和、可区分 / land-cover colours (muted, distinguishable)
LC_COL <- c(cropland = "#eda100", grassland = "#7fb069", sparse = "#c8dca6", barren = "#d9c7a0", forest = "#1f6b4a",
            shrub = "#a08a3c", water = "#2a78d6", wetland = "#9ec5f4", impervious = "#898781")
theme_pub <- function(base_size = BASE, base_family = "Helvetica") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(text = element_text(colour = PAL$ink1, family = base_family, size = base_size),
          axis.text = element_text(colour = PAL$ink2, size = base_size - 0.5),
          axis.title = element_text(colour = PAL$ink1, size = base_size),
          axis.ticks = element_line(colour = PAL$axis, linewidth = 0.25), axis.ticks.length = unit(1.5, "pt"),
          axis.line = element_line(colour = PAL$axis, linewidth = 0.3),
          panel.grid = element_blank(), panel.grid.major.y = element_line(colour = PAL$grid, linewidth = 0.2),
          legend.position = "top", legend.justification = "left", legend.direction = "horizontal",
          legend.title = element_text(size = base_size), legend.text = element_text(size = base_size - 0.5),
          legend.key.size = unit(7, "pt"), legend.margin = margin(0, 0, 0, 0),
          plot.title = element_text(size = base_size + 1, face = "bold", hjust = 0, margin = margin(b = 3)),
          plot.title.position = "plot", plot.tag = element_text(size = base_size + 2, face = "bold"),
          plot.background = element_rect(fill = "white", colour = NA),
          plot.margin = margin(3, 4, 3, 3), strip.text = element_text(size = base_size, face = "bold", hjust = 0))
}
theme_set(theme_pub())
save_fig <- function(p, name, h_mm, w_mm = 183, dir = "figures_main") {
  ggsave(file.path(dir, paste0(name, ".pdf")), p, width = w_mm, height = h_mm, units = "mm", device = cairo_pdf)
  ggsave(file.path(dir, paste0(name, ".png")), p, width = w_mm, height = h_mm, units = "mm", dpi = 600, device = agg_png, bg = "white")
}
