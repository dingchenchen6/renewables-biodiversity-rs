# 来源 / Source: 复制自 ~/Documents/china-camera-trap-synthesis/R/china_std_map.R(作者本人红外相机兽类群落项目),
# 与 CBNR 数据集 Figure 2a 版式一致;仅复制,未改动。Copied verbatim from the author's camera-trap project.
# ============================================================
# china_std_map.R — 中国标准地图底图与 CBNR 版式 / China standard basemap with the CBNR layout
# Objective / 目标:
#   为 14、15、16 目录的全部全国地图提供统一底图与版式,完全参照 CBNR 数据集 Figure 2a 的 R 复刻流程:
#   · 底图:自然资源部标准地图 GS(2024)0650(CGCS2000 地理坐标;省面、省级特殊界线、十段线;南海诸岛为真实地理位置)
#   · 投影:Albers 等积(25°N/47°N,中央经线 105°E)
#   · 主图:裁至海南岛以南;南海诸岛鹰眼图(105–125°E、2.5–26.5°N,含完整台湾岛)置于右下角台湾以东空白海域,黑框
#   · 图框:整幅地图外加黑色图框;鹰眼图与图框右下角重合;指北针在右上角(图框东侧预留海域,不压盖黑龙江);图例在左下角藏南以南空白区
#   Shared basemap/layout for all national maps following the CBNR Figure 2a workflow (standard map GS(2024)0650).
# Input / 输入: ~/Documents/SDMs/GS(2024)0650审图号_CBNR中国shp/{省, 省_境界线, 十段线}.shp(复制自 CBNR 项目 tasks/中国shp)
# Key assumptions / 关键假设: 仅重投影标准地图数据,不修改要素;数据层(网格、点)按真实地理位置绘制,主图与鹰眼图同步
# Packages / 主要包: sf, ggplot2, patchwork, ggspatial
# ============================================================
suppressPackageStartupMessages({ library(sf); library(ggplot2); library(patchwork) })

CN_DIR <- path.expand("~/Documents/SDMs/GS(2024)0650审图号_CBNR中国shp")
CN_CRS <- "+proj=aea +lat_1=25 +lat_2=47 +lat_0=0 +lon_0=105 +datum=WGS84 +units=m +no_defs"   # CBNR 投影 / CBNR projection
CN_STYLE <- list(land = "#f3f2ee", pline = "#000000", outline = "#000000", dash = "#3d3c3a", special = "#000000", frame = "#000000")   # 省界黑色细线(CBNR)/ black province lines
CN_INSET_LL <- c(xmin = 105, xmax = 125, ymin = 2.5, ymax = 26.5)                            # 鹰眼图经纬范围(含台湾全岛) / inset extent incl. all of Taiwan

cn_short <- function(x) sub("((壮族|回族|维吾尔)?自治区|特别行政区|省|市)$", "", x)          # 省名简称 / short province names

# 读取、重投影并计算版式(会话内缓存) / load, reproject and compute layout (cached per session)
cn_load <- local({
  cache <- NULL
  function(dir = CN_DIR) {
    if (!is.null(cache)) return(cache)
    rd <- function(f) st_make_valid(st_transform(st_read(file.path(dir, paste0(f, ".shp")), quiet = TRUE, options = "ENCODING=UTF-8"), CN_CRS))
    prov <- rd("省"); prov$NAME <- cn_short(prov$省名)
    L <- list(crs = st_crs(CN_CRS), prov = prov, special = rd("省_境界线"), dash = rd("十段线"))
    L$china <- st_make_valid(st_union(prov))
    L$outline <- st_cast(st_boundary(L$china), "MULTILINESTRING")                              # 国界与海岸外缘 / national outline
    # 主图范围:全国外包框;南界取 12.5°N(CBNR 版式,为右下鹰眼图与左下图例留出空间)/ main extent; bottom at 12.5°N as in CBNR
    tw <- st_cast(prov[prov$NAME == "台湾", ], "POLYGON") |> suppressWarnings(); tw <- tw[which.max(st_area(tw)), ]
    bb <- st_bbox(st_union(prov[prov$NAME != "海南", ]))
    ll_box <- function(b, n = 60) { lon <- seq(b[["xmin"]], b[["xmax"]], length.out = n); lat <- seq(b[["ymin"]], b[["ymax"]], length.out = n)   # 沿经纬框边缘加密取点 / densified box edge
      st_as_sf(data.frame(x = c(lon, lon, rep(b[["xmin"]], n), rep(b[["xmax"]], n)), y = c(rep(b[["ymin"]], n), rep(b[["ymax"]], n), lat, lat)), coords = c("x", "y"), crs = 4326) }
    y_bot <- st_coordinates(st_transform(st_as_sf(data.frame(x = 110, y = 12.5), coords = c("x", "y"), crs = 4326), CN_CRS))[, 2]
    # CBNR 图框:西侧留 3%、东侧留 8%(右上指北针与右下鹰眼图所在海域)、北侧留 4% / CBNR neat-frame margins
    L$main_x <- c(bb[["xmin"]] - 0.03 * (bb[["xmax"]] - bb[["xmin"]]), bb[["xmax"]] + 0.08 * (bb[["xmax"]] - bb[["xmin"]]))
    L$main_y <- c(y_bot, bb[["ymax"]] + 0.04 * (bb[["ymax"]] - y_bot))
    ib <- st_bbox(st_transform(ll_box(CN_INSET_LL), CN_CRS))
    L$inset_x <- c(ib[["xmin"]], ib[["xmax"]]); L$inset_y <- c(ib[["ymin"]], ib[["ymax"]])
    L[c("main_x", "main_y", "inset_x", "inset_y")] <- lapply(L[c("main_x", "main_y", "inset_x", "inset_y")], function(v) as.numeric(unname(v)))   # 去除名称 / drop names
    # 鹰眼图位置(npc,CBNR):与图框右下角重合(共用右、下边框);顶边低于台湾岛南端,左边不越过台湾岛东岸;宽度按两图比例换算
    # Inset flush with the bottom-right corner of the neat frame (CBNR); top below Taiwan's southern tip, left edge east of Taiwan
    rx <- function(x) (x - L$main_x[1]) / diff(L$main_x); ry <- function(y) (y - L$main_y[1]) / diff(L$main_y)
    asp <- (diff(L$inset_y) / diff(L$inset_x)) * (diff(L$main_x) / diff(L$main_y))     # 框高/框宽(npc)/ box height-to-width in npc
    top <- unname(ry(st_bbox(tw)[["ymin"]]) - 0.008); w <- top / asp
    left_min <- unname(rx(st_bbox(tw)[["xmax"]]) + 0.008)
    if (1 - w < left_min) { w <- 1 - left_min; top <- w * asp }                       # 不越过台湾东岸 / keep east of Taiwan
    L$inset_box <- c(left = 1 - w, bottom = 0, right = 1, top = top)
    # 核查:鹰眼图不得压盖主图中的大陆、海南岛与台湾岛(面积 > 500 km² 的陆块)/ check the inset never covers major land masses
    big <- suppressWarnings(st_cast(L$china, "POLYGON")); big <- big[as.numeric(st_area(big)) > 5e8]
    ibox <- st_as_sfc(st_bbox(c(xmin = L$main_x[1] + L$inset_box[["left"]] * diff(L$main_x), xmax = L$main_x[2], ymin = L$main_y[1], ymax = L$main_y[1] + top * diff(L$main_y)), crs = st_crs(CN_CRS)))
    L$inset_overlaps_land <- any(lengths(st_intersects(ibox, big)) > 0)
    if (L$inset_overlaps_land) warning("南海诸岛鹰眼图压盖主图陆地 / inset overlaps main-map land")
    cache <<- L; L
  }
})

cn_land <- function(fill = CN_STYLE$land) geom_sf(data = cn_load()$prov, fill = fill, colour = NA)

# 省界(白)、特殊界线、国界外缘、十段线(画在数据层之上) / boundaries drawn above data layers
cn_lines <- function(pline_col = CN_STYLE$pline, scale = 1) {
  L <- cn_load()
  list(geom_sf(data = L$prov, fill = NA, colour = pline_col, linewidth = 0.12 * scale),          # 省界 / province boundaries
       geom_sf(data = L$special, colour = CN_STYLE$special, linewidth = 0.12 * scale),
       geom_sf(data = L$outline, colour = CN_STYLE$outline, linewidth = 0.25 * scale),         # 国界与海岸外缘 / national outline
       geom_sf(data = L$dash, colour = CN_STYLE$dash, linewidth = 0.4 * scale))
}

cn_theme <- function() theme_void() + theme(plot.tag = element_text(size = 9, face = "bold"), plot.margin = margin(2, 2, 2, 2),
                                             panel.border = element_rect(fill = NA, colour = CN_STYLE$frame, linewidth = 0.4),   # CBNR 图框 / neat frame
                                             legend.title = element_text(size = 5.6), legend.text = element_text(size = 5.1), legend.key.size = unit(5, "pt"),
                                             legend.spacing.y = unit(0.5, "pt"), legend.key.spacing.y = unit(0.4, "pt"))
cn_coord_main <- function() { L <- cn_load(); coord_sf(crs = L$crs, xlim = L$main_x, ylim = L$main_y, expand = FALSE) }
cn_coord <- cn_coord_main
# 左下图例(不得压盖国土,渲染后须目检) / legend bottom-left inside the free box (verify after rendering)
cn_legend_bl <- function() theme(legend.position = "inside", legend.position.inside = c(0.012, 0.012), legend.justification = c(0, 0),
                                 legend.background = element_blank(), legend.margin = margin(0, 0, 0, 5), legend.box.spacing = unit(0, "pt"))   # 左留边,色带端标签不出框 / keep labels inside the frame
cn_north <- function(h_mm = 5.5) ggspatial::annotation_north_arrow(location = "tr", which_north = "grid", height = unit(h_mm, "mm"), width = unit(h_mm * 0.5, "mm"),
                                                                   pad_x = unit(0.8, "mm"), pad_y = unit(0.8, "mm"),
                                                                   style = ggspatial::north_arrow_orienteering(text_size = 4.5, line_width = 0.3, fill = c("white", "black")))

# 南海诸岛鹰眼图 / South China Sea inset
cn_inset <- function(data_layers = list(), scales = list(), pline_col = CN_STYLE$pline) {
  L <- cn_load()
  ggplot() + cn_land() + data_layers + cn_lines(pline_col, scale = 0.8) + scales +
    coord_sf(crs = L$crs, xlim = L$inset_x, ylim = L$inset_y, expand = FALSE) + theme_void() +
    theme(legend.position = "none", panel.border = element_rect(fill = NA, colour = CN_STYLE$frame, linewidth = 0.4),
          panel.background = element_rect(fill = "white", colour = NA), plot.margin = margin(0, 0, 0, 0))
}

# 一步式 CBNR 版式地图 / one-call CBNR-style national map
#   legend: "bl" 左下(默认) | "bottom" 图下 | "none";inset_layers 默认与主图数据层相同
cn_map <- function(data_layers = list(), scales = list(), tag = NULL, pline_col = CN_STYLE$pline, north = TRUE, legend = "bl", inset_layers = data_layers) {
  L <- cn_load()
  main <- ggplot() + cn_land() + data_layers + cn_lines(pline_col) + scales + cn_coord_main() + cn_theme() + labs(tag = tag) +
    switch(legend, bl = cn_legend_bl(), bottom = theme(legend.position = "bottom"), none = theme(legend.position = "none"))
  if (north) main <- main + cn_north()
  b <- L$inset_box
  main + inset_element(cn_inset(inset_layers, scales, pline_col), left = b[["left"]], bottom = b[["bottom"]], right = b[["right"]], top = b[["top"]], align_to = "panel", clip = TRUE)
}

# 数据裁剪/转换 / clip or transform data to the basemap CRS
cn_clip <- function(x) suppressWarnings(st_intersection(st_transform(x, CN_CRS), cn_load()$china))
cn_tf <- function(x) st_transform(x, CN_CRS)
