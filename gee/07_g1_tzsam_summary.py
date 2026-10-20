# ============================================================
# Objective / 分析目标:
# Summarise GRW vs TZ-SAM date agreement and merge with the visual
# (05) and automatic (03) G1 checks.
# 汇总 GRW 与 TZ-SAM 的日期一致性,并与目视(05)、自动(03)结果合并。
# Input / 输入: results/g1_tzsam_crosscheck.csv, results/g1_breaks_seasonal.csv,
#   results/g1_visual_calls.csv
# Output / 输出: results/g1_tzsam_summary.csv
# Rule / 判定规则(季度 q):
#   GRW late  = GRW quarter start is > 2 q after TZ upper bound
#   GRW early = GRW quarter end is > 2 q before TZ lower bound
#   GRW 晚于 TZ 上界 2 个季度以上为"偏晚";早于 TZ 下界 2 个季度以上为"偏早"
# ============================================================
import logging
from pathlib import Path

import pandas as pd

logging.basicConfig(level=logging.INFO, format="%(asctime)s %(message)s")
logger = logging.getLogger(__name__)
RES = Path(__file__).resolve().parents[1] / "results"
MIN_OVERLAP = 0.5
TZ_CENSOR = pd.Timestamp("2018-04-01")  # TZ 上界早于此视为左删失 / TZ left-censored


def classify(d: pd.DataFrame) -> pd.DataFrame:
    """Add date bounds, lag in quarters and a verdict / 计算偏差与判定."""
    u = pd.to_datetime(d["tz_upper"].astype(str).str[:10], format="%Y/%m/%d", errors="coerce")
    lo = pd.to_datetime(d["tz_lower"].astype(str).str[:10], format="%Y/%m/%d", errors="coerce")
    q0 = pd.to_datetime(dict(year=d["construction_year"], month=(d["construction_quarter"] - 1) * 3 + 1, day=1))
    q1 = q0 + pd.offsets.QuarterEnd(0)
    late = (q0 - u).dt.days / 91.3
    early = (lo - q1).dt.days / 91.3
    verdict = pd.Series("consistent", index=d.index)
    verdict[late > 2] = "grw_late"
    verdict[early > 2] = "grw_early"
    return d.assign(tz_U=u, tz_L=lo, late_q=late.round(2), verdict=verdict)


def main() -> None:
    """Print agreement tables / 输出一致性表."""
    d = classify(pd.read_csv(RES / "g1_tzsam_crosscheck.csv", usecols=lambda c: c in {
        "fid", "construction_year", "construction_quarter", "area", "tz_upper", "tz_lower", "overlap"}))
    d.to_csv(RES / "g1_tzsam_crosscheck.csv", index=False)
    good = d[(d["overlap"] >= MIN_OVERLAP) & (d["tz_U"] >= TZ_CENSOR)]
    logger.info("matched %s; overlap>=%.1f and TZ not censored: %s", len(d), MIN_OVERLAP, len(good))
    tab = good["verdict"].value_counts(normalize=True).rename("share").to_frame()
    tab["n"] = good["verdict"].value_counts()
    logger.info("\n%s", tab.round(3).to_string())
    by = pd.crosstab(good["construction_year"], good["verdict"], normalize="index").round(3)
    logger.info("\n%s", by.to_string())
    size = pd.cut(good["area"], [0, 1e5, 1e6, 1e9], labels=["1-10ha", "10-100ha", ">100ha"])
    logger.info("\n%s", pd.crosstab(size, good["verdict"], normalize="index").round(3).to_string())
    logger.info("late lag (quarters) among grw_late: median %.1f, IQR %.1f-%.1f",
                *good.loc[good["verdict"] == "grw_late", "late_q"].quantile([.5, .25, .75]))
    summ = pd.concat([tab, by.stack().rename("share_by_year").to_frame()], axis=0)
    summ.to_csv(RES / "g1_tzsam_summary.csv")

    b = pd.read_csv(RES / "g1_breaks_seasonal.csv")
    vis = pd.read_csv(RES / "g1_visual_calls.csv")
    m = b.merge(vis, on="fid", how="left").merge(d[["fid", "overlap", "late_q", "verdict"]], on="fid", how="left")
    m["auto_ok"] = m["break_offset"].between(-2, 2)
    logger.info("\n%s", pd.crosstab(m["auto_ok"], m["verdict"].fillna("no_match")).to_string())
    logger.info("\n%s", m[m["visual"].notna()][["fid", "break_offset", "visual", "overlap", "late_q", "verdict"]]
                .sort_values("visual").to_string(index=False))


if __name__ == "__main__":
    main()
