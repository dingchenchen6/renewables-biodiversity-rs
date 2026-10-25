# ============================================================
# Objective / 分析目标:
# Spatial-block bootstrap of the full event-study vector for the NW pilot
# (matched never-built design), used as the covariance input to HonestDiD and
# as a check on the multiplier-bootstrap SE. 50 km blocks resampled with
# replacement (199 replicates); duplicated units receive new ids.
# 西北试点事件研究系数向量的空间块自助法:50 km 块有放回重抽 199 次,
# 协方差输入 HonestDiD,并核对乘子自助法标准误。
# Input / 输入: as rigor_pilot_matched.R
# Output / 输出: results/rigor_pilot_blockboot_draws.csv, results/rigor_pilot_blockboot_honestdid.csv,
#   results/rigor_pilot_blockboot_att.csv
# Main packages / 主要包: data.table, MatchIt, did, HonestDiD, parallel
# ============================================================
suppressPackageStartupMessages({ library(data.table); library(MatchIt); library(did); library(HonestDiD); library(sf); library(parallel) })
set.seed(42)
args <- commandArgs(trailingOnly = FALSE)
here <- dirname(normalizePath(sub("^--file=", "", args[grep("^--file=", args)])))
root <- dirname(here); res <- file.path(root, "results")
src <- readLines(file.path(here, "rigor_pilot_matched.R"))
# 复用主脚本的数据准备、匹配与面板构建(到主估计之前)/ reuse data prep, matching and panel code
eval(parse(text = src[grep("^ALB <-", src):(grep("^# ---- 1. 主估计", src) - 1)]))
R <- 199; E <- -5:3
cfgs <- list(c(y = "site_mean", a = "1"), c(y = "ring_stdDev", a = "1"), c(y = "site_mean", a = "0"))
one_rep <- function(r, y, antic) {
  set.seed(1000 + r)
  bl <- unique(d$block); pick <- sample(bl, length(bl), replace = TRUE)
  dx <- rbindlist(lapply(seq_along(pick), function(k) d[block == pick[k]][, id := id * 1000 + k]))
  a <- tryCatch(att_gt(yname = y, tname = "year", idname = "id", gname = "g", data = as.data.frame(dx[!is.na(get(y))]),
                       weightsname = "weights", control_group = "nevertreated", base_period = "universal",
                       allow_unbalanced_panel = TRUE, anticipation = antic, est_method = "reg", bstrap = FALSE, print_details = FALSE),
                error = function(e) NULL)
  if (is.null(a)) return(NULL)
  dyn <- tryCatch(aggte(a, type = "dynamic", min_e = min(E), max_e = max(E), na.rm = TRUE, bstrap = FALSE), error = function(e) NULL)
  ov <- tryCatch(aggte(a, type = "dynamic", min_e = 1, max_e = 3, na.rm = TRUE, bstrap = FALSE), error = function(e) NULL)
  if (is.null(dyn) || is.null(ov)) return(NULL)
  data.table(rep = r, e = c(dyn$egt, 99), est = c(dyn$att.egt, ov$overall.att))
}
draws <- list(); hdl <- list(); attl <- list()
for (cf in cfgs) {
  y <- cf[["y"]]; antic <- as.integer(cf[["a"]])
  a0 <- att_gt(yname = y, tname = "year", idname = "id", gname = "g", data = as.data.frame(d[!is.na(get(y))]), weightsname = "weights",
               control_group = "nevertreated", base_period = "universal", allow_unbalanced_panel = TRUE, anticipation = antic,
               est_method = "reg", bstrap = FALSE, print_details = FALSE)
  dyn0 <- aggte(a0, type = "dynamic", min_e = min(E), max_e = max(E), na.rm = TRUE, bstrap = FALSE)
  ov0 <- aggte(a0, type = "dynamic", min_e = 1, max_e = 3, na.rm = TRUE, bstrap = FALSE)
  bt <- rbindlist(mclapply(seq_len(R), one_rep, y = y, antic = antic, mc.cores = 6))
  bt[, `:=`(outcome = y, anticipation = antic)]; draws[[length(draws) + 1]] <- bt
  W <- dcast(bt[e != 99], rep ~ e, value.var = "est")
  ref <- -1 - antic; keep <- setdiff(intersect(E, as.integer(names(W)[-1])), ref)
  S <- cov(as.matrix(W[, as.character(keep), with = FALSE]), use = "pairwise.complete.obs")
  bh <- dyn0$att.egt[match(keep, dyn0$egt)]
  npre <- sum(keep < ref); npost <- sum(keep > ref)
  lv <- as.numeric(keep[keep > ref] %in% 1:3); lv <- lv / sum(lv)
  orig <- constructOriginalCS(betahat = bh, sigma = S, numPrePeriods = npre, numPostPeriods = npost, l_vec = lv)
  hd <- createSensitivityResults_relativeMagnitudes(betahat = bh, sigma = S, numPrePeriods = npre, numPostPeriods = npost,
                                                   Mbarvec = seq(0.25, 2, by = 0.25), l_vec = lv)
  hdl[[length(hdl) + 1]] <- rbind(data.table(outcome = y, anticipation = antic, Mbar = 0, lb = as.numeric(orig$lb), ub = as.numeric(orig$ub)),
                                  data.table(outcome = y, anticipation = antic, Mbar = hd$Mbar, lb = as.numeric(hd$lb), ub = as.numeric(hd$ub)))
  ob <- bt[e == 99]$est
  # 预趋势联合检验(块自助协方差)/ joint pre-trend Wald test with block-bootstrap covariance
  pre_k <- keep[keep < ref]; bp <- dyn0$att.egt[match(pre_k, dyn0$egt)]
  Sp <- S[as.character(pre_k), as.character(pre_k)]
  Wst <- as.numeric(t(bp) %*% solve(Sp, bp))
  attl[[length(attl) + 1]] <- data.table(outcome = y, anticipation = antic, att = ov0$overall.att, se_block = sd(ob),
                                         lo_pct = quantile(ob, .025), hi_pct = quantile(ob, .975), n_reps = length(ob),
                                         pretest_p = pchisq(Wst, df = length(bp), lower.tail = FALSE), pre_k = length(bp))
  cat(y, antic, "done\n")
}
fwrite(rbindlist(draws), file.path(res, "rigor_pilot_blockboot_draws.csv"))
fwrite(rbindlist(hdl), file.path(res, "rigor_pilot_blockboot_honestdid.csv"))
att <- rbindlist(attl); fwrite(att, file.path(res, "rigor_pilot_blockboot_att.csv"))
print(att[, lapply(.SD, function(v) if (is.numeric(v)) signif(v, 3) else v)])
print(rbindlist(hdl)[Mbar %in% c(0, 0.25, 0.5, 0.75, 1), lapply(.SD, function(v) if (is.numeric(v)) signif(v, 3) else v)])
cat("done\n")
