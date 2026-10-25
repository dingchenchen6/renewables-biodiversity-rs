# ============================================================
# Scientific question / 科学问题:
# Is the drop in biodiversity exposure of new PV after 2022 larger than
# chance, and does siting avoid species-rich places within provinces?
# 2022 年后新建光伏的生物多样性暴露下降是否超出随机波动?省内选址是否
# 主动避开了物种丰富的位置?
#
# Objective / 分析目标 (audit repairs / 审查修补):
#   1. 25 km spatial-block bootstrap (999) for facility-weighted percentiles,
#      period differences and species-level shares of new PV
#   2. Province-preserving random-siting null (999 draws): each facility is
#      relocated to a random land cell in its own province
# Input data / 输入数据: cache/exposure_range_cells.rds (from exposure_iucn.R),
#   China provinces, cache/grw_solar_2018plus.geojson, cache/grw_wind_2018plus.csv
# Expected output / 预期输出: results/rigor_exposure_*.csv
# Key assumptions / 关键假设: blocks of 25 km capture most spatial clustering of
#   facilities (large bases span several 5 km cells); the null keeps each
#   province's share of facilities fixed. 25 km 块足以吸收设施的空间聚集;零模型
#   保持各省设施份额不变。
# Main packages / 主要包: sf, terra, data.table
# ============================================================
suppressPackageStartupMessages({ library(sf); library(terra); library(data.table) })
sf_use_s2(FALSE); set.seed(42)
args <- commandArgs(trailingOnly = FALSE)
here <- dirname(normalizePath(sub("^--file=", "", args[grep("^--file=", args)])))
root <- dirname(here); res <- file.path(root, "results"); cache <- file.path(root, "cache")
ALB <- "+proj=aea +lat_1=25 +lat_2=47 +lat_0=0 +lon_0=105 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
NP <- "/Users/dingchenchen/Documents/New project"
B <- 999; BLOCK <- 25000

prov <- st_transform(st_make_valid(st_read(file.path(NP, "bird_dynamic_occupancy_analysis/data/china_boundary/省面.shp"), quiet = TRUE)), ALB)
china <- st_union(prov)
tmpl <- rast(ext(vect(china)), resolution = 5000, crs = ALB)
land <- rasterize(vect(china), tmpl, field = 1); land_cells <- which(!is.na(values(land)))
prov$pid <- seq_len(nrow(prov))
pr <- rasterize(vect(prov), tmpl, field = "pid"); pid <- values(pr)[, 1]

rg <- readRDS(file.path(cache, "exposure_range_cells.rds"))
spt <- rbindlist(lapply(rg, function(x) data.table(taxon = x$taxon, species = x$species, category = x$category, n_cells = length(x$cells))))
spt[, threatened := category %in% c("CR", "EN", "VU")]
spt[, small_range := n_cells <= quantile(n_cells, 0.25), by = taxon]
cell_dt <- rbindlist(lapply(seq_along(rg), function(i) data.table(i = i, cell = rg[[i]]$cells)))
cell_dt <- merge(cell_dt, spt[, .(i = .I, taxon, threatened, small_range)], by = "i")
met <- rbind(cell_dt[, .(S = .N, S_thr = sum(threatened), S_small = sum(small_range)), by = .(cell, taxon)],
             cell_dt[, .(S = .N, S_thr = sum(threatened), S_small = sum(small_range)), by = cell][, taxon := "all"])
base <- CJ(cell = land_cells, taxon = c("birds", "mammals", "amphibians", "reptiles", "all"))
met <- merge(base, met, by = c("cell", "taxon"), all.x = TRUE)
for (v in c("S", "S_thr", "S_small")) set(met, which(is.na(met[[v]])), v, 0L)
met[, `:=`(pct_S = 100 * frank(S, ties.method = "average") / .N, pct_thr = 100 * frank(S_thr, ties.method = "average") / .N,
           any_small = as.numeric(S_small > 0)), by = taxon]
nat_any_small <- met[taxon == "all", mean(any_small)]

pv <- st_read(file.path(cache, "grw_solar_2018plus.geojson"), quiet = TRUE)
pxy <- st_coordinates(st_transform(st_centroid(st_make_valid(pv)), ALB))
wd <- fread(file.path(cache, "grw_wind_2018plus.csv"))
wxy <- st_coordinates(st_transform(st_as_sf(wd, coords = c("lon", "lat"), crs = 4326), ALB))
fac <- rbind(data.table(type = "PV", x = pxy[, 1], y = pxy[, 2], w = pv$area / 1e6, year = pv$construction_year),
             data.table(type = "wind", x = wxy[, 1], y = wxy[, 2], w = 1, year = wd$construction_year))
fac[, cell := cellFromXY(tmpl, cbind(x, y))]
fac <- fac[cell %in% land_cells]
fac[, `:=`(period = fifelse(year <= 2021, "2018-2021", "2022-2024"), pid = pid[cell],
           block = paste(floor(x / BLOCK), floor(y / BLOCK), sep = "_"))]

metrics <- c("pct_S", "pct_thr", "any_small")
lookup <- lapply(c("birds", "mammals", "amphibians", "reptiles", "all"), function(tx) {
  m <- met[taxon == tx]; v <- matrix(NA_real_, ncell(tmpl), 3); v[m$cell, ] <- as.matrix(m[, ..metrics]); v })
names(lookup) <- c("birds", "mammals", "amphibians", "reptiles", "all")
wmean <- function(cells, w, tx) colSums(lookup[[tx]][cells, , drop = FALSE] * w) / sum(w)

# ---- 1. 观测值与空间块自助法 / observed + block bootstrap ----------------------
rows <- list(); diffs <- list()
for (tp in c("PV", "wind")) for (tx in names(lookup)) {
  per <- list()
  for (pd in c("2018-2021", "2022-2024")) {
    f <- fac[type == tp & period == pd]; obs <- wmean(f$cell, f$w, tx)
    bl <- split(seq_len(nrow(f)), f$block); nb <- length(bl)
    bt <- t(replicate(B, { idx <- unlist(bl[sample(nb, nb, replace = TRUE)], use.names = FALSE); wmean(f$cell[idx], f$w[idx], tx) }))
    per[[pd]] <- bt
    rows[[length(rows) + 1]] <- data.table(type = tp, taxon = tx, period = pd, metric = metrics, est = obs,
                                           lo = apply(bt, 2, quantile, .025), hi = apply(bt, 2, quantile, .975), n_blocks = nb)
  }
  dd <- per[["2022-2024"]] - per[["2018-2021"]]
  ob <- rows[[length(rows)]]$est - rows[[length(rows) - 1]]$est
  diffs[[length(diffs) + 1]] <- data.table(type = tp, taxon = tx, metric = metrics, diff = ob,
                                           lo = apply(dd, 2, quantile, .025), hi = apply(dd, 2, quantile, .975),
                                           p_boot = apply(dd, 2, function(z) min(1, 2 * min(mean(z <= 0), mean(z >= 0)))))
}
obs_tab <- rbindlist(rows); dif_tab <- rbindlist(diffs)

# ---- 2. 同省随机选址零模型 / province-preserving random-siting null -------------
cells_by_pid <- split(land_cells, pid[land_cells])
nul <- list()
for (tp in c("PV", "wind")) for (pd in c("2018-2021", "2022-2024")) {
  f <- fac[type == tp & period == pd & !is.na(pid)]
  draws <- t(replicate(B, {
    rc <- unlist(lapply(split(seq_len(nrow(f)), f$pid), function(ix) { pool <- cells_by_pid[[as.character(f$pid[ix[1]])]]; pool[sample.int(length(pool), length(ix), replace = TRUE)] }))
    o <- unlist(split(seq_len(nrow(f)), f$pid)); wmean(rc, f$w[o], "all") }))
  obs <- wmean(f$cell, f$w, "all")
  nul[[length(nul) + 1]] <- data.table(type = tp, period = pd, metric = metrics, observed = obs, null_mean = colMeans(draws),
                                       null_lo = apply(draws, 2, quantile, .025), null_hi = apply(draws, 2, quantile, .975),
                                       p_lower = (1 + colSums(draws <= matrix(obs, B, 3, byrow = TRUE))) / (B + 1))
}
nul <- rbindlist(nul)

# ---- 3. 受威胁物种份额的自助法区间 / bootstrap CIs for threatened-species shares --
top <- c("Falco cherrug", "Aquila nipalensis", "Gazella subgutturosa", "Vormela peregusna", "Haliaeetus leucoryphus",
         "Elaphe taeniura", "Halcyon pileata", "Aquila heliaca", "Gekko swinhonis", "Corvus pectoralis")
sh <- list()
for (s in top) {
  i <- which(spt$species == s)[1]; inr <- rep(FALSE, ncell(tmpl)); inr[rg[[i]]$cells] <- TRUE
  per <- list()
  for (pd in c("2018-2021", "2022-2024")) {
    f <- fac[type == "PV" & period == pd]; bl <- split(seq_len(nrow(f)), f$block); nb <- length(bl)
    obs <- sum(f$w[inr[f$cell]]) / sum(f$w)
    bt <- replicate(B, { idx <- unlist(bl[sample(nb, nb, replace = TRUE)], use.names = FALSE); sum(f$w[idx][inr[f$cell[idx]]]) / sum(f$w[idx]) })
    per[[pd]] <- bt
    sh[[length(sh) + 1]] <- data.table(species = s, category = spt$category[i], taxon = spt$taxon[i], period = pd, share = obs,
                                       lo = quantile(bt, .025), hi = quantile(bt, .975))
  }
  dd <- per[[2]] - per[[1]]
  sh[[length(sh) + 1]] <- data.table(species = s, category = spt$category[i], taxon = spt$taxon[i], period = "difference",
                                     share = sh[[length(sh)]]$share - sh[[length(sh) - 1]]$share, lo = quantile(dd, .025), hi = quantile(dd, .975))
}
sh <- rbindlist(sh)
fwrite(obs_tab, file.path(res, "rigor_exposure_observed.csv")); fwrite(dif_tab, file.path(res, "rigor_exposure_period_diff.csv"))
fwrite(nul, file.path(res, "rigor_exposure_null.csv")); fwrite(sh, file.path(res, "rigor_exposure_species_shares.csv"))
cat("national share of cells with any small-ranged species:", round(nat_any_small, 3), "\n")
print(obs_tab[taxon == "all", lapply(.SD, function(v) if (is.numeric(v)) signif(v, 3) else v)])
print(dif_tab[taxon == "all", lapply(.SD, function(v) if (is.numeric(v)) signif(v, 3) else v)])
print(nul[, lapply(.SD, function(v) if (is.numeric(v)) signif(v, 3) else v)])
print(sh[, lapply(.SD, function(v) if (is.numeric(v)) signif(v, 3) else v)])
cat("done\n")
