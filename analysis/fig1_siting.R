# ============================================================
# Figure 1 / 图 1: Where China's 2018-2024 ground-mounted PV and onshore wind went
# 2018-2024 年中国新建地面光伏与陆上风电的位置与建设前生境
# Message / 核心信息: new PV shifted to barren drylands after 2022; large sites take most
#   of the area on barren land, small sites sit on cropland.
#   2022 年后新建光伏转向荒漠;大站以面积为主、多在裸地,小站多在耕地。
# Panels / 分图: a PV sites (size = area, colour = period); b wind turbines per 50 km cell;
#   c prior-year cover by build year (CLCD v1.0.5); d prior-year cover by site size
# Maps / 地图: standard map GS(2024)0650 with the CBNR layout (analysis/china_std_map.R)
# Input / 输入: cache/pv_centroids.csv, cache/grw_wind_2018plus.csv, results/clcd_summary_pv_by_*.csv
# Output / 输出: figures_main/fig1_siting.pdf|png
# ============================================================
suppressPackageStartupMessages({ library(sf); library(data.table) })
source("analysis/fig_theme.R"); source("analysis/china_std_map.R"); sf_use_s2(FALSE)
L <- cn_load()
tf <- function(d) { p <- st_coordinates(st_transform(st_as_sf(d, coords = c("lon", "lat"), crs = 4326), CN_CRS)); d[, `:=`(x = p[, 1], y = p[, 2])] }
pv <- tf(fread("cache/pv_centroids.csv"))[, period := fifelse(year <= 2021, "2018–2021", "2022–2024")]
wd <- tf(fread("cache/grw_wind_2018plus.csv"))
G <- 50000
wg <- wd[, .N, by = .(gx = (floor(x / G) + 0.5) * G, gy = (floor(y / G) + 0.5) * G)]

pa <- cn_map(list(geom_point(data = pv[order(-area)], aes(x, y, size = area / 1e6, fill = period), shape = 21, colour = "white", stroke = 0.05, alpha = 0.85)),
             list(scale_size(range = c(0.25, 3.4), breaks = c(1, 10, 50), name = "Site area (km²)"),
                  scale_fill_manual(values = c(COL$p1, COL$p2), name = "Built"),
                  guides(fill = guide_legend(order = 1, override.aes = list(size = 2.2)), size = guide_legend(order = 2, override.aes = list(fill = PAL$muted))),
                  theme(legend.box = "horizontal")), tag = "a")
# cn_theme() 为完整主题,图例间距须事后追加 / cn_theme() is complete, so set legend spacing afterwards
pa <- pa & theme(legend.spacing.y = unit(6, "pt"))
pb <- cn_map(list(geom_tile(data = wg, aes(gx, gy, fill = N), width = G, height = G)),
             list(scale_fill_gradientn(colours = PAL$seq[c(1, 4, 7, 10, 13)], trans = "log10", name = "Turbines per 50-km cell",
                                       labels = label_comma(), breaks = c(1, 10, 100, 1000)),
                  guides(fill = guide_colourbar(title.position = "top", direction = "horizontal", barwidth = unit(40, "pt"), barheight = unit(4, "pt")))), tag = "b")

cls <- c("barren", "grassland", "cropland", "forest", "shrub", "water", "wetland", "impervious")
lab <- c(barren = "Barren", grassland = "Grassland", cropland = "Cropland", forest = "Forest", shrub = "Shrub", water = "Water", wetland = "Wetland", impervious = "Impervious")
yr <- melt(fread("results/clcd_summary_pv_by_year.csv"), id.vars = "construction_year", measure.vars = cls)
yr[, variable := factor(variable, levels = rev(cls))]
km <- pv[, .(km2 = sum(area) / 1e6), by = .(construction_year = year)]
pc <- ggplot(yr, aes(factor(construction_year), value, fill = variable)) +
  geom_col(width = 0.72, colour = "white", linewidth = 0.2) +
  geom_text(data = km, aes(factor(construction_year), 104, label = format(round(km2), big.mark = ",")), inherit.aes = FALSE, size = 1.9, colour = PAL$ink2) +
  annotate("text", x = 0.35, y = 104, label = "km²", hjust = 0, size = 1.9, colour = PAL$ink2) +
  scale_fill_manual(values = LC_COL[rev(cls)], labels = lab[rev(cls)], name = NULL, guide = guide_legend(reverse = TRUE, nrow = 1)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 100, 25)) + coord_cartesian(ylim = c(0, 107), clip = "off") +
  labs(x = "Year of construction", y = "New PV area by prior-year cover (%)", tag = "c")
sz <- melt(fread("results/clcd_summary_pv_by_size.csv"), id.vars = "size", measure.vars = cls)
sz[, size := factor(size, levels = c("1-10ha", "10-100ha", ">100ha"), labels = c("1–10 ha", "10–100 ha", ">100 ha"))]
sz[, variable := factor(variable, levels = rev(cls))]
nsz <- pv[, .(n = .N, a = sum(area)), by = .(size = cut(area, c(0, 1e5, 1e6, 1e10), labels = c("1–10 ha", "10–100 ha", ">100 ha")))][, share := 100 * a / sum(a)]
pd <- ggplot(sz, aes(size, value, fill = variable)) + geom_col(width = 0.66, colour = "white", linewidth = 0.2) +
  geom_text(data = nsz, aes(size, 105.5, label = sprintf("n = %s\n%.0f%% of area", trimws(format(n, big.mark = ",")), share)), inherit.aes = FALSE, size = 1.9, colour = PAL$ink2) +
  scale_fill_manual(values = LC_COL[rev(cls)], guide = "none") +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 100, 25)) + coord_cartesian(ylim = c(0, 107), clip = "off") +
  labs(x = "Site size", y = NULL, tag = "d")
fig <- (pa | pb) / ((pc | pd) + plot_layout(widths = c(1.75, 1), guides = "collect") & theme(legend.position = "bottom")) + plot_layout(heights = c(1.25, 1))
save_fig(fig, "fig1_siting", h_mm = 150)
cat("inset overlaps land:", L$inset_overlaps_land, "\n")
