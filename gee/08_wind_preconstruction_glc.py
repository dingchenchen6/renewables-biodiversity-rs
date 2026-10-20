# ============================================================
# Scientific question / 科学问题:
# Where did China's post-2018 onshore turbines go, by prior-year
# 30 m land cover? Forest siting is the main biodiversity concern
# in the south and southwest.
# 2018 年后新建陆上风机在建设前一年的 30 m 覆盖是什么?南方与西南
# 风机进入森林是主要的生物多样性关切。
#
# Input data / 输入数据: GRW WIND_V1; GLC_FCS30D annual (to 2022)
# Workflow / 主要流程: point sample of Y-1 land cover per turbine
#   (2024 capped at 2022), grouped classes as in script 01
#   逐风机读取建设前一年覆盖(2024 年用 2022),类别归并同脚本 01
# Expected output / 预期输出: results/wind_preconstruction_glc_summary.csv
# Main packages / 主要包: earthengine-api, pandas
# ============================================================
import importlib.util
import logging
from pathlib import Path

import ee
import pandas as pd

logging.basicConfig(level=logging.INFO, format="%(asctime)s %(message)s")
logger = logging.getLogger(__name__)

PROJECT = "renewable-conservation"
HERE = Path(__file__).resolve().parent
RES = HERE.parent / "results"

# 复用脚本 01 的类别归并 / reuse grouping from script 01
_spec = importlib.util.spec_from_file_location("lc01", HERE / "01_preconstruction_landcover.py")
lc01 = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(lc01)


def main() -> None:
    """Histogram of prior-year class by build year / 按建设年统计前一年覆盖."""
    ee.Initialize(project=PROJECT)
    wind = (ee.FeatureCollection("projects/sat-io/open-datasets/GRW/WIND_V1")
            .filter(ee.Filter.eq("COUNTRY", "China")))
    rows = []
    for y in range(2018, 2025):
        lc_year = min(y - 1, lc01.LC_LAST_YEAR)
        img = lc01.grouped_landcover(lc_year)
        pts = img.reduceRegions(collection=wind.filter(ee.Filter.eq("construction_year", y)),
                                reducer=ee.Reducer.first().setOutputs(["lc"]), scale=30, tileScale=4)
        hist = pts.aggregate_histogram("lc").getInfo()
        # 落在无数据像元上的点记为 null,剔除并计数 / drop and count no-data points
        n_null = hist.pop("null", 0)
        n = sum(hist.values())
        for gid, (name, _) in lc01.GROUPS.items():
            c = sum(v for k, v in hist.items() if int(float(k)) == gid)
            rows.append({"construction_year": y, "lc_year": lc_year, "class": name, "n": c, "share": c / n,
                         "n_nodata": n_null})
        logger.info("year %s: %s turbines (%s on no-data pixels)", y, n, n_null)
    df = pd.DataFrame(rows)
    df.to_csv(RES / "wind_preconstruction_glc_by_year.csv", index=False)
    tot = df.groupby("class")["n"].sum()
    logger.info("\n%s", (tot / tot.sum() * 100).round(1).sort_values(ascending=False).to_string())
    logger.info("\n%s", df.pivot(index="construction_year", columns="class", values="share").mul(100).round(1)
                [["grassland", "bare", "cropland", "forest", "shrubland", "sparse_vegetation"]].to_string())


if __name__ == "__main__":
    main()
