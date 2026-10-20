# ============================================================
# Scientific question / 科学问题:
# In China's northwest drylands, does PV construction change on-site
# and surrounding greenness and its spatial heterogeneity relative to
# not-yet-built sites? (pilot for M3/M4, H1-H3)
# 西北旱区光伏建设是否改变站内与外围的绿度及其空间异质性?
# 对照为尚未建设的站点(M3/M4 试点,检验 H1-H3)。
#
# Objective / 分析目标:
# Build a site-year panel (2016-2024) of growing-season Landsat NDVI
# mean and spatial SD, on site and in a 60-500 m ring.
# 构建 2016-2024 年站点-年份面板:生长季 Landsat NDVI 均值与空间标准差,
# 分站内与 60-500 m 外围环。
#
# Input data / 输入数据:
#   GRW SOLAR_V1; results/g1_tzsam_crosscheck.csv (date-consistent sites);
#   LANDSAT/LC08|LC09/C02/T1_L2; FAO/GAUL/2015/level1 (province filter)
# Workflow / 主要流程:
#   1. Sites: NW five provinces, built 2019-2024, >= 10 ha, GRW-TZ consistent
#      样本:西北五省区、2019-2024 年建成、≥ 10 公顷、GRW 与 TZ 日期一致
#   2. Jun-Sep median NDVI per year, cloud/shadow/snow masked
#      逐年 6-9 月去云中位数 NDVI
#   3. Mean and SD on site; ring excludes all GRW PV polygons
#      站内与外围环的均值、标准差;外围环剔除所有光伏地块
# Expected output / 预期输出: results/pilot_nw_panel.csv
# Key assumptions / 关键假设:
#   On-site NDVI after construction mixes panels and inter-row ground at
#   30 m, so on-site change is partly mechanical; the ring is cleaner.
#   建后站内 30 m 像元混合面板与板间地表,站内变化部分是机械性的;外围环更干净。
# Main packages / 主要包: earthengine-api, pandas
# Output directory / 输出路径: results/
# ============================================================
import logging
from pathlib import Path

import ee
import pandas as pd

logging.basicConfig(level=logging.INFO, format="%(asctime)s %(message)s")
logger = logging.getLogger(__name__)

PROJECT = "renewable-conservation"
RES = Path(__file__).resolve().parents[1] / "results"
PROVINCES = ["Xinjiang Uygur Zizhiqu", "Gansu Sheng", "Qinghai Sheng", "Ningxia Huizu Zizhiqu", "Nei Mongol Zizhiqu"]
COHORTS = range(2019, 2025)
YEARS = range(2016, 2025)
MIN_AREA = 100_000
RING_IN, RING_OUT = 60, 500
BATCH = 60


def consistent_fids() -> list[int]:
    """Sites whose GRW date agrees with TZ-SAM / 日期一致的站点."""
    d = pd.read_csv(RES / "g1_tzsam_crosscheck.csv")
    ok = d[(d["verdict"] == "consistent") & (d["overlap"] >= 0.5) & (d["area"] >= MIN_AREA)]
    return ok["fid"].astype(int).tolist()


def ndvi_year(year: int) -> ee.Image:
    """Jun-Sep median NDVI from Landsat 8/9 L2 / 生长季中位数 NDVI."""
    def prep(img):
        qa = img.select("QA_PIXEL")
        # 位 1 膨胀云、3 云、4 云影、5 雪 / dilated cloud, cloud, shadow, snow
        clear = qa.bitwiseAnd(0b111010).eq(0)
        sr = img.select(["SR_B4", "SR_B5"]).multiply(0.0000275).add(-0.2)
        return sr.normalizedDifference(["SR_B5", "SR_B4"]).rename("ndvi").updateMask(clear)
    col = (ee.ImageCollection("LANDSAT/LC08/C02/T1_L2").merge(ee.ImageCollection("LANDSAT/LC09/C02/T1_L2"))
           .filterDate(f"{year}-06-01", f"{year}-09-30").map(prep))
    return col.median()


def site_table(grw: ee.FeatureCollection, prov: ee.FeatureCollection) -> pd.DataFrame:
    """One-off site list with province / 一次性取站点清单与省份."""
    sites = (grw.filter(ee.Filter.inList("construction_year", list(COHORTS)))
             .filter(ee.Filter.inList("fid", consistent_fids())).filterBounds(prov.geometry()))

    def add_prov(f):
        hit = prov.filterBounds(f.geometry())
        return f.set("province", ee.Algorithms.If(hit.size().gt(0), hit.first().get("ADM1_NAME"), "NA"))
    cols = ["fid", "construction_year", "construction_quarter", "area", "province"]
    arr = sites.map(add_prov).reduceColumns(ee.Reducer.toList().repeat(len(cols)), cols).get("list").getInfo()
    return pd.DataFrame(dict(zip(cols, arr)))


def main() -> None:
    """Extract the site-year panel in batches / 分批提取站点-年份面板."""
    ee.Initialize(project=PROJECT)
    prov = (ee.FeatureCollection("FAO/GAUL/2015/level1").filter(ee.Filter.eq("ADM0_NAME", "China"))
            .filter(ee.Filter.inList("ADM1_NAME", PROVINCES)))
    grw = ee.FeatureCollection("projects/sat-io/open-datasets/GRW/SOLAR_V1").filter(ee.Filter.eq("COUNTRY", "China"))
    meta = site_table(grw, prov)
    logger.info("pilot sites: %s; by cohort %s", len(meta), meta["construction_year"].value_counts().sort_index().to_dict())
    # 只画西北范围内的光伏地块作掩膜 / paint only NW PV polygons for the ring mask
    nw_box = ee.Geometry.Rectangle([73, 31, 127, 50], None, False)
    pv_mask = ee.Image(0).paint(grw.filterBounds(nw_box), 1).Not()
    red = ee.Reducer.mean().combine(ee.Reducer.stdDev(), sharedInputs=True).combine(
        ee.Reducer.count(), sharedInputs=True)
    cols = ["fid", "mean", "stdDev", "count"]
    fids = meta["fid"].astype(int).tolist()
    rows = []
    for y in YEARS:
        img = ndvi_year(y)
        for i in range(0, len(fids), BATCH):
            sites = grw.filter(ee.Filter.inList("fid", fids[i:i + BATCH])).select(["fid"])
            ring = sites.map(lambda f: f.setGeometry(
                f.geometry().buffer(RING_OUT, 10).difference(f.geometry().buffer(RING_IN, 10), 10)))
            for part, fc, im in (("site", sites, img), ("ring", ring, img.updateMask(pv_mask))):
                out = im.reduceRegions(collection=fc, reducer=red, scale=30, tileScale=8)
                out = out.map(lambda f: f.set("mean", ee.Algorithms.If(f.get("mean"), f.get("mean"), -9),
                                              "stdDev", ee.Algorithms.If(f.get("stdDev"), f.get("stdDev"), -9)))
                arr = out.reduceColumns(ee.Reducer.toList().repeat(len(cols)), cols).get("list").getInfo()
                rows.append(pd.DataFrame(dict(zip(cols, arr))).assign(year=y, part=part))
        logger.info("year %s done", y)
    panel = pd.concat(rows, ignore_index=True).replace(-9, pd.NA)
    wide = panel.pivot_table(index=["fid", "year"], columns="part", values=["mean", "stdDev", "count"],
                             aggfunc="first")
    wide.columns = [f"{p}_{v}" for v, p in wide.columns]
    wide = wide.reset_index().merge(meta, on="fid", how="left")
    wide.to_csv(RES / "pilot_nw_panel.csv", index=False)
    logger.info("panel rows: %s", len(wide))


if __name__ == "__main__":
    main()
