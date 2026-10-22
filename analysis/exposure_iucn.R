# ============================================================
# Scientific question / 科学问题:
# Which species ranges did China's 2018-2024 ground-mounted PV and onshore
# wind actually land in, and did the post-2022 shift toward deserts lower
# exposure for species-rich, threatened or small-ranged assemblages?
# 2018-2024 年中国实际新建的地面光伏与陆上风电落在哪些物种的分布区内?
# 2022 年后转向荒漠,是否降低了对高丰富度、受威胁和小分布区物种的暴露?
#
# Objective / 分析目标: realised-siting exposure analysis with expert range
#   maps (BirdLife BOTW 2025 for birds; IUCN for mammals, amphibians,
#   reptiles), comparable in spirit to the planning-scenario metrics of
#   Zhou et al. (2026, Nat. Ecol. Evol.).
#   用专家分布图做"实际选址"的暴露分析,口径接近 Zhou 等 2026 的情景指标。
# Input data / 输入数据:
#   BOTW_2025.gpkg; iucn_{mammals,amphibians,reptiles}.gpkg; IUCN Red List
#   DwC-A (category for birds via sisid); China provinces (省面.shp);
#   cache/grw_solar_2018plus.geojson; cache/grw_wind_2018plus.csv
# Workflow / 主要流程:
#   1. Ranges: presence 1-2 (extant/probably extant), origin 1-2 (native/
#      reintroduced), seasonal 1-3 (resident/breeding/non-breeding)
#   2. Rasterise each species to a 5 km Albers grid over China
#   3. Cell metrics: richness, threatened (CR/EN/VU), small-ranged (bottom
#      quartile of China range within taxon)
#   4. Facility exposure: area-weighted (PV) or turbine-weighted (wind) mean
#      metric and its percentile among all China land cells, by period
#   5. Species exposure: PV km2 and turbines inside each range
# Expected output / 预期输出: results/exposure_facility_summary.csv,
#   results/exposure_species.csv, results/figures/exposure_iucn.png|pdf
# Key assumptions / 关键假设: expert ranges over-predict local occurrence
#   (commission), so exposure is potential, not realised, co-occurrence;
#   PV sites are assigned to the 5 km cell of their centroid.
#   专家分布图会高估局地出现,暴露为潜在共存而非实际出现;光伏按质心归入 5 km 网格。
# Main packages / 主要包: sf, terra, data.table, ggplot2
# Output directory / 输出路径: results/
# ============================================================
suppressPackageStartupMessages({ library(sf); library(terra); library(data.table); library(ggplot2) })
sf_use_s2(FALSE); set.seed(42)
args <- commandArgs(trailingOnly = FALSE)
here <- dirname(normalizePath(sub("^--file=", "", args[grep("^--file=", args)])))
root <- dirname(here); res <- file.path(root, "results"); cache <- file.path(root, "cache")
ALB <- "+proj=aea +lat_1=25 +lat_2=47 +lat_0=0 +lon_0=105 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
NP <- "/Users/dingchenchen/Documents/New project"
FILES <- c(birds = file.path(NP, "kimi_k3_migration_20260722/data/botw/BOTW_2025.gpkg"),
           mammals = file.path(NP, "edge-detection-bias-global/data/raw/iucn_gpkg/iucn_mammals.gpkg"),
           amphibians = file.path(NP, "edge-detection-bias-global/data/raw/iucn_gpkg/iucn_amphibians.gpkg"),
           reptiles = file.path(NP, "edge-detection-bias-global/data/raw/iucn_gpkg/iucn_reptiles.gpkg"))
DWCA <- "/Users/dingchenchen/Documents/New records/herp-new-distribution-records/source_data/conservation/iucn_latest_dwca"

# ---- 1. 模板与中国掩膜 / template and China mask ---------------------------
prov <- st_read(file.path(NP, "bird_dynamic_occupancy_analysis/data/china_boundary/省面.shp"), quiet = TRUE)
china <- st_union(st_make_valid(st_transform(prov, ALB)))
tmpl <- rast(ext(vect(china)), resolution = 5000, crs = ALB)
land <- rasterize(vect(china), tmpl, field = 1)
land_cells <- which(!is.na(values(land)))
bbox_wkt <- st_as_text(st_as_sfc(st_bbox(c(xmin = 73, ymin = 3, xmax = 136, ymax = 54), crs = 4326)))

# 鸟类等级:sisid 对应红色名录 taxonID / bird categories via sisid
dist <- fread(file.path(DWCA, "distribution.txt"), header = FALSE, sep = "\t", quote = "", select = c(1, 6),
              col.names = c("taxonID", "status"))
cat_map <- c("Critically Endangered" = "CR", "Endangered" = "EN", "Vulnerable" = "VU", "Near Threatened" = "NT",
             "Least Concern" = "LC", "Data Deficient" = "DD", "Extinct in the Wild" = "EW", "Extinct" = "EX")
dist[, category := cat_map[status]]
dist <- unique(dist[!is.na(category), .(taxonID, category)], by = "taxonID")

ranges_file <- file.path(cache, "exposure_range_cells.rds")
if (!file.exists(ranges_file)) {
  out <- list()
  for (tx in names(FILES)) {
    lay <- st_layers(FILES[[tx]])$name[1]
    r <- st_read(FILES[[tx]], layer = lay, wkt_filter = bbox_wkt, quiet = TRUE)
    names(r)[names(r) == attr(r, "sf_column")] <- "geom"; st_geometry(r) <- "geom"
    r <- r[r$presence %in% 1:2 & r$origin %in% 1:2 & r$seasonal %in% 1:3, ]
    if (tx == "birds") r$category <- dist$category[match(r$sisid, dist$taxonID)]
    r <- st_transform(st_make_valid(r), ALB)
    sp <- unique(r$sci_name)
    cat(tx, ": polygons", nrow(r), " species in bbox", length(sp), "\n")
    for (s in sp) {
      g <- r[r$sci_name == s, ]
      cells <- tryCatch({
        v <- vect(st_simplify(st_union(g), dTolerance = 1000))
        rr <- rasterize(v, tmpl, field = 1, touches = TRUE)
        intersect(which(!is.na(values(rr))), land_cells)
      }, error = function(e) integer(0))
      if (length(cells)) out[[length(out) + 1]] <- list(taxon = tx, species = s,
                                                         category = as.character(g$category[1]), cells = cells)
    }
    rm(r); invisible(gc())
  }
  saveRDS(out, ranges_file)
}
rg <- readRDS(ranges_file)
spt <- rbindlist(lapply(rg, function(x) data.table(taxon = x$taxon, species = x$species, category = x$category,
                                                   n_cells = length(x$cells))))
spt[, threatened := category %in% c("CR", "EN", "VU")]
spt[, small_range := n_cells <= quantile(n_cells, 0.25), by = taxon]
cat("species with range in China:\n"); print(spt[, .(n = .N, threatened = sum(threatened)), by = taxon])

# ---- 2. 网格指标 / cell metrics ---------------------------------------------
nc <- ncell(tmpl)
cell_dt <- rbindlist(lapply(seq_along(rg), function(i) data.table(i = i, cell = rg[[i]]$cells)))
cell_dt <- merge(cell_dt, spt[, .(i = .I, taxon, threatened, small_range)], by = "i")
met <- cell_dt[, .(S = .N, S_thr = sum(threatened), S_small = sum(small_range)), by = .(cell, taxon)]
met_all <- cell_dt[, .(S = .N, S_thr = sum(threatened), S_small = sum(small_range)), by = cell][, taxon := "all"]
met <- rbind(met, met_all)
base <- CJ(cell = land_cells, taxon = c(names(FILES), "all"))
met <- merge(base, met, by = c("cell", "taxon"), all.x = TRUE)
for (v in c("S", "S_thr", "S_small")) set(met, which(is.na(met[[v]])), v, 0L)
# 全国百分位 / national percentile among land cells
for (v in c("S", "S_thr", "S_small")) met[, paste0("pct_", v) := 100 * frank(get(v), ties.method = "average") / .N, by = taxon]

# ---- 3. 设施暴露 / facility exposure ----------------------------------------
pv <- st_read(file.path(cache, "grw_solar_2018plus.geojson"), quiet = TRUE)
pvc <- st_transform(st_centroid(st_make_valid(pv)), ALB)
pv_dt <- data.table(cell = cellFromXY(tmpl, st_coordinates(pvc)), area_km2 = pv$area / 1e6, year = pv$construction_year)
wd <- fread(file.path(cache, "grw_wind_2018plus.csv"))
wxy <- st_coordinates(st_transform(st_as_sf(wd, coords = c("lon", "lat"), crs = 4326), ALB))
wd_dt <- data.table(cell = cellFromXY(tmpl, wxy), w = 1, year = wd$construction_year)
fac <- rbind(pv_dt[, .(cell, w = area_km2, year, type = "PV (area-weighted)")], wd_dt[, .(cell, w, year, type = "wind (turbine-weighted)")])
fac <- fac[cell %in% land_cells]
fac[, period := fifelse(year <= 2021, "2018-2021", "2022-2024")]
fx <- merge(fac, met, by = "cell", allow.cartesian = TRUE)
summ <- fx[, .(mean_S = weighted.mean(S, w), mean_thr = weighted.mean(S_thr, w), mean_small = weighted.mean(S_small, w),
               pct_S = weighted.mean(pct_S, w), pct_thr = weighted.mean(pct_S_thr, w), pct_small = weighted.mean(pct_S_small, w),
               share_any_thr = weighted.mean(S_thr > 0, w), share_any_small = weighted.mean(S_small > 0, w)),
           by = .(type, period, taxon)]
nat <- met[, .(type = "China land (all cells)", period = "baseline", mean_S = mean(S), mean_thr = mean(S_thr), mean_small = mean(S_small),
               pct_S = 50, pct_thr = 50, pct_small = 50, share_any_thr = mean(S_thr > 0), share_any_small = mean(S_small > 0)), by = taxon]
summ <- rbind(summ, nat)[order(taxon, type, period)]
fwrite(summ, file.path(res, "exposure_facility_summary.csv"))
print(summ[taxon == "all"]); print(summ[taxon != "all" & type != "China land (all cells)", .(type, period, taxon, pct_S = round(pct_S), pct_thr = round(pct_thr), pct_small = round(pct_small))])

# ---- 4. 物种暴露 / species exposure -----------------------------------------
pv_cell <- pv_dt[cell %in% land_cells, .(pv_km2 = sum(area_km2)), by = cell]
wd_cell <- wd_dt[cell %in% land_cells, .(turbines = .N), by = cell]
sx <- merge(cell_dt[, .(i, cell)], pv_cell, by = "cell", all.x = TRUE)
sx <- merge(sx, wd_cell, by = "cell", all.x = TRUE)
sx <- sx[, .(pv_km2 = sum(pv_km2, na.rm = TRUE), turbines = sum(turbines, na.rm = TRUE),
             cells_with_fac = sum(!is.na(pv_km2) | !is.na(turbines))), by = i]
spx <- cbind(spt, sx[order(i)][, .(pv_km2, turbines, cells_with_fac)])
spx[, share_range_with_fac := cells_with_fac / n_cells]
fwrite(spx, file.path(res, "exposure_species.csv"))
cat("species exposed to any new PV or wind:", sum(spx$cells_with_fac > 0), "of", nrow(spx),
    "; threatened exposed:", sum(spx$cells_with_fac > 0 & spx$threatened), "of", sum(spx$threatened), "\n")
print(spx[threatened == TRUE][order(-share_range_with_fac)][1:15, .(taxon, species, category, n_cells, pv_km2 = round(pv_km2, 1), turbines, share = round(share_range_with_fac, 3))])

# ---- 5. 图 / figure ---------------------------------------------------------
fd <- melt(summ[type != "China land (all cells)" & taxon != "all"], id.vars = c("type", "period", "taxon"),
           measure.vars = c("pct_S", "pct_thr", "pct_small"), variable.name = "metric", value.name = "pct")
fd[, metric := factor(metric, levels = c("pct_S", "pct_thr", "pct_small"),
                      labels = c("物种数 / richness", "受威胁物种数 / threatened", "小分布区物种数 / small-ranged"))]
fig <- ggplot(fd, aes(pct, taxon, colour = period, shape = period)) +
  geom_vline(xintercept = 50, linewidth = 0.3, linetype = "dashed") + geom_point(size = 2.4) +
  facet_grid(type ~ metric) + scale_colour_manual(values = c("#1F6F78", "#E9A23B")) +
  scale_x_continuous(limits = c(0, 100)) +
  labs(x = "Facility-weighted national percentile (50 = China average)", y = NULL, colour = NULL, shape = NULL) +
  theme_classic(base_size = 10, base_family = "Arial Unicode MS") +
  theme(legend.position = "top", strip.background = element_blank(), strip.text = element_text(face = "bold"))
ggsave(file.path(res, "figures", "exposure_iucn.png"), fig, width = 9, height = 4.6, dpi = 300)
ggsave(file.path(res, "figures", "exposure_iucn.pdf"), fig, width = 9, height = 4.6, device = cairo_pdf)
cat("done\n")

# ---- 6. 旱地受威胁物种的分时段暴露 / period split for top-exposed threatened species ----
# 2022 年后光伏转向荒漠:看暴露最高的受威胁物种,其分布区内新增光伏是否随之上升
top <- spx[threatened == TRUE][order(-pv_km2)][1:10]
pv_per <- pv_dt[cell %in% land_cells][, period := fifelse(year <= 2021, "2018-2021", "2022-2024")]
tp <- rbindlist(lapply(seq_len(nrow(top)), function(k) {
  i <- which(spt$species == top$species[k] & spt$taxon == top$taxon[k])[1]
  pv_per[cell %in% rg[[i]]$cells, .(pv_km2 = sum(area_km2)), by = period][, `:=`(species = top$species[k], category = top$category[k], taxon = top$taxon[k])]
}))
tot <- pv_per[, .(total_km2 = sum(area_km2)), by = period]
tp <- merge(tp, tot, by = "period")[, share_of_new_pv := pv_km2 / total_km2]
fwrite(tp, file.path(res, "exposure_top_threatened_by_period.csv"))
print(dcast(tp, species + category ~ period, value.var = "share_of_new_pv"))
cn <- c("Falco cherrug" = "猎隼", "Aquila nipalensis" = "草原雕", "Gazella subgutturosa" = "鹅喉羚",
        "Vormela peregusna" = "虎鼬", "Haliaeetus leucoryphus" = "玉带海雕", "Elaphe taeniura" = "黑眉锦蛇",
        "Halcyon pileata" = "蓝翡翠", "Aquila heliaca" = "白肩雕", "Gekko swinhonis" = "无蹼壁虎", "Corvus pectoralis" = "白颈鸦")
tp[, label := paste0(fifelse(species %in% names(cn), cn[species], species), " ", species, " (", category, ")")]
ord <- dcast(tp, label ~ period, value.var = "share_of_new_pv")[order(`2022-2024` - `2018-2021`)]$label
tp[, label := factor(label, levels = ord)]
w2 <- dcast(tp, label ~ period, value.var = "share_of_new_pv")
fig2 <- ggplot() +
  geom_segment(data = w2, aes(x = 100 * `2018-2021`, xend = 100 * `2022-2024`, y = label, yend = label), colour = "grey60",
               arrow = arrow(length = unit(0.12, "cm"))) +
  geom_point(data = tp, aes(100 * share_of_new_pv, label, colour = period), size = 2.4) +
  scale_colour_manual(values = c("#1F6F78", "#E9A23B")) +
  labs(x = "Share of newly built PV area inside the species' range (%)", y = NULL, colour = NULL) +
  theme_classic(base_size = 10, base_family = "Arial Unicode MS") + theme(legend.position = "top")
ggsave(file.path(res, "figures", "exposure_threatened_shift.png"), fig2, width = 7.2, height = 4, dpi = 300)
ggsave(file.path(res, "figures", "exposure_threatened_shift.pdf"), fig2, width = 7.2, height = 4, device = cairo_pdf)
