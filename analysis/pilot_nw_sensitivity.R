# ============================================================
# Scientific question / 科学问题:
# Is the pre-trend in on-site NDVI explained by anticipation
# (clearing/fencing before panels become visible) or by
# province-specific greening trends?
# 站内 NDVI 的预趋势,能否由预期效应(面板可见前的清表、围栏)
# 或省份间绿度趋势差异解释?
#
# Objective / 分析目标: G2 sensitivity for the NW pilot.
#   Variants: anticipation = 0 / 2 years; unconditional vs
#   province-conditional parallel trends (doubly robust).
#   变体:预期期 0 或 2 年;无条件或以省份为条件的平行趋势(双重稳健)。
# Input data / 输入数据: results/pilot_nw_panel.csv
# Expected output / 预期输出: results/pilot_nw_sensitivity.csv
# Main packages / 主要包: data.table, did
# ============================================================
suppressPackageStartupMessages({ library(data.table); library(did) })
set.seed(42)
args <- commandArgs(trailingOnly = FALSE)
here <- dirname(normalizePath(sub("^--file=", "", args[grep("^--file=", args)])))
res <- file.path(dirname(here), "results")

p <- fread(file.path(res, "pilot_nw_panel.csv"))
p <- p[!is.na(site_mean) & site_count >= 20 & province != "NA"]
p[, g := construction_year]
# 省份样本过少时并入"其他" / pool provinces with few sites
small <- p[, uniqueN(fid), by = province][V1 < 25, province]
p[, prov := fifelse(province %in% small, "other", province)]

run <- function(y, antic, cond) {
  d <- as.data.frame(p[!is.na(get(y))])
  a <- att_gt(yname = y, tname = "year", idname = "fid", gname = "g", data = d,
              xformla = if (cond) ~ prov else ~ 1, est_method = if (cond) "dr" else "reg",
              control_group = "notyettreated", anticipation = antic, base_period = "universal",
              allow_unbalanced_panel = TRUE, bstrap = TRUE, biters = 999, clustervars = "fid",
              print_details = FALSE)
  dyn <- aggte(a, type = "dynamic", min_e = -5, max_e = 3, na.rm = TRUE)
  ov <- aggte(a, type = "dynamic", min_e = 1, max_e = 3, na.rm = TRUE)
  pre <- dyn$egt < -1 - antic
  # 建设前系数的联合 Wald 检验(影响函数协方差)/ joint Wald test of pre-period coefficients
  inf <- dyn$inf.function$dynamic.inf.func.e[, pre, drop = FALSE]
  V <- crossprod(inf) / nrow(inf)^2
  b <- dyn$att.egt[pre]
  W <- as.numeric(t(b) %*% solve(V, b))
  data.table(outcome = y, anticipation = antic, conditional_on_province = cond,
             pretest_p = pchisq(W, df = length(b), lower.tail = FALSE), max_abs_pre = max(abs(b)),
             att_e1_3 = ov$overall.att, se = ov$overall.se,
             es = paste(sprintf("%+d:%+.3f", dyn$egt, dyn$att.egt), collapse = " "))
}
out <- rbindlist(lapply(c("site_mean", "ring_mean", "site_stdDev", "ring_stdDev"), function(y)
  rbindlist(list(run(y, 0, FALSE), run(y, 2, FALSE), run(y, 0, TRUE), run(y, 2, TRUE)))))
out[, `:=`(lo = att_e1_3 - 1.96 * se, hi = att_e1_3 + 1.96 * se)]
fwrite(out, file.path(res, "pilot_nw_sensitivity.csv"))
print(out[, .(outcome, anticipation, conditional_on_province, pretest_p = signif(pretest_p, 2),
              max_abs_pre = round(max_abs_pre, 4), att = round(att_e1_3, 4), lo = round(lo, 4), hi = round(hi, 4))])
cat("\nprovince groups:\n"); print(p[, uniqueN(fid), by = prov])
