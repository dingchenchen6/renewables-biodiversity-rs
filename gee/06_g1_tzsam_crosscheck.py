# ============================================================
# Scientific question / 科学问题:
# Do GRW construction dates agree with an independent dataset
# (TZ-SAM construction-date bounds)? (gate G1, external check)
# GRW 建设日期与独立数据集 TZ-SAM 的建设时间上下界是否一致?(G1 外部核对)
#
# Objective / 分析目标:
# Spatially join post-2018 GRW PV polygons in China to TZ-SAM
# 2025Q1 polygons; compare the GRW quarter with TZ-SAM's
# construction-date interval [construc_1, constructe].
# 将 2018 年后 GRW 光伏多边形与 TZ-SAM 多边形空间连接,比较日期。
#
# Input data / 输入数据: GRW SOLAR_V1; TZ-SAM 2025Q1 analysis polygons;
#   results/g1_breaks_seasonal.csv (sample flags)
# Expected output / 预期输出: results/g1_tzsam_crosscheck.csv (with overlap share)
# Key assumptions / 关键假设: constructe = latest date by which the
#   site existed (upper bound); construc_1 = lower bound, if present.
#   constructe 为建成时间上界,construc_1 为下界(可缺)。
# Main packages / 主要包: earthengine-api, pandas
# ============================================================
import logging
from pathlib import Path

import ee
import pandas as pd

logging.basicConfig(level=logging.INFO, format="%(asctime)s %(message)s")
logger = logging.getLogger(__name__)

PROJECT = "renewable-conservation"
RES = Path(__file__).resolve().parents[1] / "results"
TZ = "projects/sat-io/open-datasets/TZERO/TZ-SOLAR-2025Q1_ANALYSIS_POLYGONS"


def main() -> None:
    """Join GRW 2018+ to TZ-SAM and export date pairs / 连接并导出日期对."""
    ee.Initialize(project=PROJECT)
    box = ee.Geometry.Rectangle([73, 18, 135, 54], None, False)
    grw = (ee.FeatureCollection("projects/sat-io/open-datasets/GRW/SOLAR_V1")
           .filter(ee.Filter.eq("COUNTRY", "China")).filter(ee.Filter.gte("construction_year", 2018)))
    tz = ee.FeatureCollection(TZ).filterBounds(box)
    # 取重叠面积最大的 TZ 多边形,并记录重叠比例,避免扩建地块误配到相邻老一期
    # keep the TZ polygon with the largest overlap; record the overlap share
    join = ee.Join.saveAll("tzs")
    flt = ee.Filter.intersects(leftField=".geo", rightField=".geo", maxError=10)

    def best(f):
        g = f.geometry()
        a = g.area(10)
        tzs = ee.List(f.get("tzs")).map(
            lambda t: ee.Feature(t).set("ov", ee.Feature(t).geometry().intersection(g, 10).area(10)))
        top = ee.Feature(ee.FeatureCollection(tzs).sort("ov", False).first())
        low = top.get("construc_1")
        return (f.set("tz_upper", top.get("constructe"),
                      "tz_lower", ee.Algorithms.If(low, low, ""),
                      "overlap", ee.Number(top.get("ov")).divide(a))
                .set("tzs", None))

    cols = ["fid", "construction_year", "construction_quarter", "area", "tz_upper", "tz_lower", "overlap"]
    rows = []
    for y in range(2018, 2025):
        j = join.apply(grw.filter(ee.Filter.eq("construction_year", y)), tz, flt).map(best)
        arr = j.reduceColumns(ee.Reducer.toList().repeat(len(cols)), cols).get("list").getInfo()
        df = pd.DataFrame(dict(zip(cols, arr)))
        rows.append(df)
        logger.info("year %s: %s GRW sites matched to TZ-SAM", y, len(df))
    d = pd.concat(rows, ignore_index=True)
    d.to_csv(RES / "g1_tzsam_crosscheck.csv", index=False)


if __name__ == "__main__":
    main()
