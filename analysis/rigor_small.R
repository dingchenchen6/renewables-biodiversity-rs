# ============================================================
# Objective / 分析目标:
# Small statistical repairs flagged by the audit.
#   (a) G1: Wilson 95% intervals for agreement proportions
#   (b) CLCD vs GLC_FCS30D: Cohen's kappa for dominant pre-construction class
#   (c) Forest turbines: stratified estimate with Beta posteriors per stratum
#       (replaces the invalid sum of per-stratum Wilson bounds)
# 审查指出的小修补:G1 比例区间;两产品一致性 κ;森林风机分层 Beta 后验估计。
# Input / 输入: results/g1_breaks_seasonal.csv, results/g1_tzsam_crosscheck.csv,
#   results/clcd_preconstruction_solar_by_site.csv, results/preconstruction_landcover_by_site.csv,
#   results/wind_forest_sample.csv, results/wind_forest_visual_calls.csv, results/wind_forest_crosstab.csv
# Output / 输出: results/rigor_small.csv
# Main packages / 主要包: data.table
# ============================================================
suppressPackageStartupMessages(library(data.table))
set.seed(42)
args <- commandArgs(trailingOnly = FALSE)
here <- dirname(normalizePath(sub("^--file=", "", args[grep("^--file=", args)])))
res <- file.path(dirname(here), "results")
wilson <- function(x, n, z = 1.96) {
  p <- x / n; c0 <- (p + z^2 / (2 * n)) / (1 + z^2 / n); h <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)
  c(p = p, lo = c0 - h, hi = c0 + h)
}
out <- list()

# (a) G1 / 日期校验
b <- fread(file.path(res, "g1_breaks_seasonal.csv"))
k <- sum(between(b$break_offset, -2, 2)); w <- wilson(k, nrow(b))
out[["g1_s2_within2"]] <- data.table(item = "G1: S2 break within +/-2 quarters", x = k, n = nrow(b), est = w[1], lo = w[2], hi = w[3])
tz <- fread(file.path(res, "g1_tzsam_crosscheck.csv"))
good <- tz[overlap >= 0.5 & as.IDate(tz_U) >= as.IDate("2018-04-01")]
for (v in c("consistent", "grw_late", "grw_early")) {
  w <- wilson(sum(good$verdict == v), nrow(good))
  out[[paste0("tz_", v)]] <- data.table(item = paste("G1: GRW vs TZ-SAM", v), x = sum(good$verdict == v), n = nrow(good), est = w[1], lo = w[2], hi = w[3])
}

# (b) Cohen's kappa / 两产品主导类别一致性
cl <- fread(file.path(res, "clcd_preconstruction_solar_by_site.csv"))[n_pix > 0]
CL <- c("cropland", "forest", "shrub", "grassland", "water", "snow_ice", "barren", "impervious", "wetland")
cl[, dom := CL[max.col(as.matrix(.SD), ties.method = "first")], .SDcols = paste0("share_", CL)]
g <- fread(file.path(res, "preconstruction_landcover_by_site.csv"))
g[, share_grassland := share_grassland + share_sparse_vegetation]
setnames(g, c("share_bare", "share_shrubland", "share_ice"), c("share_barren", "share_shrub", "share_snow_ice"))
g[, dom_g := CL[max.col(as.matrix(.SD), ties.method = "first")], .SDcols = paste0("share_", CL)]
m <- merge(cl[, .(fid, dom)], g[, .(fid, dom_g)], by = "fid")
tab <- table(factor(m$dom, CL), factor(m$dom_g, CL)); po <- sum(diag(tab)) / sum(tab)
pe <- sum(rowSums(tab) * colSums(tab)) / sum(tab)^2; kap <- (po - pe) / (1 - pe)
# κ 的自助法区间 / bootstrap CI for kappa
kb <- replicate(999, { i <- sample(nrow(m), replace = TRUE); tb <- table(factor(m$dom[i], CL), factor(m$dom_g[i], CL))
  a <- sum(diag(tb)) / sum(tb); e <- sum(rowSums(tb) * colSums(tb)) / sum(tb)^2; (a - e) / (1 - e) })
out[["kappa"]] <- data.table(item = "CLCD vs GLC dominant class: Cohen's kappa", x = NA, n = nrow(m), est = kap, lo = quantile(kb, .025), hi = quantile(kb, .975))
out[["agree"]] <- data.table(item = "CLCD vs GLC dominant class: raw agreement", x = sum(diag(tab)), n = nrow(m), est = po, lo = wilson(sum(diag(tab)), nrow(m))[2], hi = wilson(sum(diag(tab)), nrow(m))[3])

# (c) 森林风机:分层 Beta 后验 / stratified Beta posterior for forest turbines
s <- merge(fread(file.path(res, "wind_forest_sample.csv")), fread(file.path(res, "wind_forest_visual_calls.csv")), by = "code")
s <- s[call %in% c("forest", "nonforest", "edge")]
s[, forest := as.integer(call %in% c("forest", "edge"))]   # 林缘按森林计,敏感性见下 / edge counted as forest
Nh <- c(both_forest = 3721, clcd_only = 5346, glc_only = 892)
draws <- sapply(names(Nh), function(h) { x <- sum(s[stratum == h]$forest); n <- nrow(s[stratum == h]); rbeta(20000, 1 + x, 1 + n - x) })
tot <- draws %*% Nh
out[["forest_total"]] <- data.table(item = "Forest turbines, sampled strata (Beta posterior; edge = forest)", x = NA, n = nrow(s),
                                    est = median(tot), lo = quantile(tot, .025), hi = quantile(tot, .975))
s[, forest2 := as.integer(call == "forest")]
draws2 <- sapply(names(Nh), function(h) { x <- sum(s[stratum == h]$forest2); n <- nrow(s[stratum == h]); rbeta(20000, 1 + x, 1 + n - x) })
tot2 <- draws2 %*% Nh
out[["forest_total_edge_nonforest"]] <- data.table(item = "Forest turbines, sampled strata (edge = non-forest)", x = NA, n = nrow(s),
                                                   est = median(tot2), lo = quantile(tot2, .025), hi = quantile(tot2, .975))
r <- rbindlist(out)
fwrite(r, file.path(res, "rigor_small.csv"))
print(r[, .(item, x, n, est = signif(est, 4), lo = signif(lo, 4), hi = signif(hi, 4))])
