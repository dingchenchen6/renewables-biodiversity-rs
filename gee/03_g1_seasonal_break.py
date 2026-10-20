# ============================================================
# Scientific question / 科学问题:
# Does the GRW construction quarter match the quarter in which the
# land surface changes, once seasonality is removed? (Gate G1)
# 去除季节性后,GRW 建设季度与地表突变季度是否一致?(关口 G1)
#
# Objective / 分析目标:
# Re-date the step change in the Sentinel-2 series from
# 02_g1_date_check.py with a season + step model. A plain
# before/after mean split picks seasonal transitions instead.
# 对 02 脚本取得的季度序列,用"季节哑变量 + 阶跃"模型重新定位断点;
# 单纯前后均值差会把季节转换误判为断点。
#
# Input data / 输入数据: results/g1_sample_quarterly.csv
# Workflow / 主要流程:
#   1. For each candidate k, OLS per band: season dummies + 1[t >= k]
#      对每个候选断点 k,逐波段拟合季节哑变量 + 建后指示
#   2. Pick k with the smallest pooled SSE; record step size
#      选合并残差平方和最小的 k,记录阶跃幅度
#   3. Agreement with GRW quarter; edge pile-up check
#      计算与 GRW 季度的一致率,并检查窗口边界堆积
# Expected output / 预期输出: results/g1_breaks_seasonal.csv
# Key assumptions / 关键假设: one dominant step; seasonality stable
#   across years within the window. 窗口内只有一次主导突变,季节型稳定。
# Main packages / 主要包: pandas, numpy
# Output directory / 输出路径: results/
# ============================================================
import logging
from pathlib import Path

import numpy as np
import pandas as pd

logging.basicConfig(level=logging.INFO, format="%(asctime)s %(message)s")
logger = logging.getLogger(__name__)

RES = Path(__file__).resolve().parents[1] / "results"
BANDS = ["B2", "B3", "B4", "B8", "B11", "B12"]
MIN_SIDE = 3  # 断点两侧至少 3 个有效季度 / at least 3 valid quarters each side


def fit_sse(y: np.ndarray, season: np.ndarray, post: np.ndarray | None) -> float:
    """Pooled SSE of OLS with season dummies (+ optional step) over bands.

    多波段合并的残差平方和。
    """
    cols = [(season == s).astype(float) for s in range(4)]
    if post is not None:
        cols.append(post.astype(float))
    x = np.column_stack(cols)
    beta, *_ = np.linalg.lstsq(x, y, rcond=None)
    return float(((y - x @ beta) ** 2).sum())


def detect(g: pd.DataFrame) -> dict:
    """Best step offset for one site / 单站最佳断点."""
    g = g.dropna(subset=BANDS).sort_values("offset")
    off = g["offset"].to_numpy()
    y = g[BANDS].to_numpy(float) / 10000.0
    q0 = int(g["construction_quarter"].iloc[0]) - 1
    season = (q0 + off) % 4
    sse0 = fit_sse(y, season, None)
    best = (None, np.inf)
    for k in off:
        post = off >= k
        if post.sum() < MIN_SIDE or (~post).sum() < MIN_SIDE:
            continue
        sse = fit_sse(y, season, post)
        if sse < best[1]:
            best = (int(k), sse)
    k, sse = best
    return {"break_offset": k, "r2_gain": (1 - sse / sse0) if k is not None else np.nan, "n_valid": len(off),
            "k_min": int(off[MIN_SIDE - 1]) + 1 if len(off) >= 2 * MIN_SIDE else None,
            "k_max": int(off[-MIN_SIDE]) if len(off) >= 2 * MIN_SIDE else None}


def main() -> None:
    """Detect breaks and summarise agreement / 检测断点并汇总一致率."""
    df = pd.read_csv(RES / "g1_sample_quarterly.csv")
    rows = []
    for fid, g in df.groupby("fid"):
        r = detect(g)
        r.update(fid=fid, construction_year=int(g["construction_year"].iloc[0]), area=g["area"].iloc[0])
        rows.append(r)
    b = pd.DataFrame(rows)
    b["at_edge"] = (b["break_offset"] == b["k_min"]) | (b["break_offset"] == b["k_max"])
    b.to_csv(RES / "g1_breaks_seasonal.csv", index=False)
    v = b.dropna(subset=["break_offset"])
    logger.info("sites with a break: %s / %s; at window edge: %s", len(v), len(b), int(v["at_edge"].sum()))
    logger.info("offset distribution:\n%s", v["break_offset"].value_counts().sort_index().to_string())
    for lo, hi in [(0, 0), (-1, 0), (-2, 0), (-1, 1), (-2, 2)]:
        logger.info("offset in [%s, %s]: %.1f%%", lo, hi, 100 * v["break_offset"].between(lo, hi).mean())
    strong = v[v["r2_gain"] >= v["r2_gain"].median()]
    logger.info("stronger half (r2_gain >= %.2f): in [-2, 2] %.1f%%, in [-2, 0] %.1f%%", v["r2_gain"].median(),
                100 * strong["break_offset"].between(-2, 2).mean(), 100 * strong["break_offset"].between(-2, 0).mean())


if __name__ == "__main__":
    main()
