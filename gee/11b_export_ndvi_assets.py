# ============================================================
# Objective / 分析目标:
# Server-side batch export of the 2010-2024 NDVI panel for the matched
# design (365 PV sites + 3,000 never-built candidate disks), one GEE
# table asset per year, then read the assets back into one CSV.
# 以 GEE 批处理任务导出匹配设计所需的 2010-2024 年 NDVI 面板(每年一个
# 表格资产),再读回合并。交互式请求会超时,批处理任务不受此限。
#
# Input data / 输入数据: results/matched_units_covariates.csv,
#   cache/nw_candidates.csv, GRW SOLAR_V1, Landsat 5/7/8/9 C2 L2
# Usage / 用法:
#   python gee/11b_export_ndvi_assets.py submit    # 提交 15 个任务 / submit tasks
#   python gee/11b_export_ndvi_assets.py collect   # 任务完成后读回 / read back
# Expected output / 预期输出: results/matched_units_ndvi_panel.csv
# Main packages / 主要包: earthengine-api, pandas
# ============================================================
import importlib.util
import logging
import sys
from pathlib import Path

import ee
import pandas as pd

logging.basicConfig(level=logging.INFO, format="%(asctime)s %(message)s")
logger = logging.getLogger(__name__)

HERE = Path(__file__).resolve().parent
RES, CACHE = HERE.parent / "results", HERE.parent / "cache"
PROJECT = "renewable-conservation"
FOLDER = f"projects/{PROJECT}/assets/rsbio_ndvi"
_spec = importlib.util.spec_from_file_location("m11", HERE / "11_matched_controls_panel.py")
m11 = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(m11)


def units() -> ee.FeatureCollection:
    """Site and ring geometries for PV sites and candidates / 单元与外围环几何."""
    cv = pd.read_csv(RES / "matched_units_covariates.csv")
    fids = cv.loc[cv["type"] == "pv", "fid"].astype(int).tolist()
    grw = ee.FeatureCollection("projects/sat-io/open-datasets/GRW/SOLAR_V1")
    sites = grw.filter(ee.Filter.inList("fid", fids)).map(
        lambda f: ee.Feature(f.geometry(), {"unit_id": ee.String("s").cat(ee.Number(f.get("fid")).format("%d"))}))
    c = pd.read_csv(CACHE / "nw_candidates.csv")
    cand = ee.FeatureCollection([ee.Feature(ee.Geometry.Point([lo, la]).buffer(m11.DISK_R, 10), {"unit_id": u})
                                 for lo, la, u in zip(c["lon"], c["lat"], c["unit_id"])])
    allu = sites.merge(cand)
    site_part = allu.map(lambda f: f.set("part", "site"))
    ring_part = allu.map(lambda f: f.setGeometry(f.geometry().buffer(m11.RING_OUT, 10).difference(
        f.geometry().buffer(m11.RING_IN, 10), 10)).set("part", "ring"))
    return site_part.merge(ring_part)


def year_image(year: int, pv_free: ee.Image, region: ee.Geometry | None = None) -> ee.Image:
    """Band 'ndvi' and PV-masked 'ndvi_free' for one growing season / 单一生长季的两个波段.

    先按日期与范围筛选再逐景计算(否则 GEE 会对全球影像逐景运算而超时);
    2010-2012 用 TM/ETM+,2013 年起只用 OLI,使每年只含一类传感器,
    2013 年的传感器切换由年份固定效应吸收。
    Filter by date and bounds before mapping (mapping first forces per-scene
    work on the global archive); one sensor family per year.
    """
    def coll(ids: list[str]) -> ee.ImageCollection:
        c = ee.ImageCollection(ids[0])
        for i in ids[1:]:
            c = c.merge(ee.ImageCollection(i))
        c = c.filterDate(f"{year}-06-01", f"{year}-09-30")
        return c.filterBounds(region) if region is not None else c
    if year < 2013:
        col = coll(["LANDSAT/LT05/C02/T1_L2", "LANDSAT/LE07/C02/T1_L2"]).map(lambda i: m11._prep(i, "SR_B3", "SR_B4"))
    else:
        col = coll(["LANDSAT/LC08/C02/T1_L2", "LANDSAT/LC09/C02/T1_L2"]).map(lambda i: m11._prep(i, "SR_B4", "SR_B5"))
    nd = col.median()
    return nd.rename("ndvi").addBands(nd.updateMask(pv_free).rename("ndvi_free"))


def submit() -> None:
    """Submit one export task per year / 每年提交一个导出任务."""
    try:
        ee.data.createAsset({"type": "FOLDER"}, FOLDER)
    except ee.EEException as e:
        logger.info("folder: %s", e)
    grw_nw = (ee.FeatureCollection("projects/sat-io/open-datasets/GRW/SOLAR_V1")
              .filter(ee.Filter.eq("COUNTRY", "China"))
              .filterBounds(ee.Geometry.Rectangle([73, 31, 127, 50], None, False)))
    pv_free = ee.Image(0).paint(grw_nw, 1).Not()
    fc = units()
    red = ee.Reducer.mean().combine(ee.Reducer.stdDev(), sharedInputs=True)
    for y in m11.YEARS:
        out = year_image(y, pv_free).reduceRegions(collection=fc, reducer=red, scale=30, tileScale=8)
        out = out.map(lambda f: ee.Feature(None, f.toDictionary()).set("year", y))
        task = ee.batch.Export.table.toAsset(collection=out, description=f"rsbio_ndvi_{y}",
                                             assetId=f"{FOLDER}/ndvi_{y}")
        task.start()
        logger.info("submitted %s: %s", y, task.id)


def collect() -> None:
    """Read finished assets back and reshape / 读回并整形."""
    rows = []
    cols = ["unit_id", "part", "year", "ndvi_mean", "ndvi_stdDev", "ndvi_free_mean", "ndvi_free_stdDev"]
    for y in m11.YEARS:
        fc = ee.FeatureCollection(f"{FOLDER}/ndvi_{y}")
        fc = fc.map(lambda f: f.set({c: ee.Algorithms.If(f.get(c), f.get(c), -9) for c in cols[3:]}))
        arr = fc.reduceColumns(ee.Reducer.toList().repeat(len(cols)), cols).get("list").getInfo()
        rows.append(pd.DataFrame(dict(zip(cols, arr))))
        logger.info("read %s", y)
    d = pd.concat(rows).replace(-9, pd.NA)
    # 站内用全部像元,外围环用剔除光伏后的像元 / unit: all pixels; ring: PV-free pixels
    site = d[d["part"] == "site"][["unit_id", "year", "ndvi_mean", "ndvi_stdDev"]].rename(
        columns={"ndvi_mean": "site_mean", "ndvi_stdDev": "site_stdDev"})
    ring = d[d["part"] == "ring"][["unit_id", "year", "ndvi_free_mean", "ndvi_free_stdDev"]].rename(
        columns={"ndvi_free_mean": "ring_mean", "ndvi_free_stdDev": "ring_stdDev"})
    panel = site.merge(ring, on=["unit_id", "year"], how="outer")
    panel.to_csv(RES / "matched_units_ndvi_panel.csv", index=False)
    logger.info("panel rows: %s, units: %s", len(panel), panel["unit_id"].nunique())


if __name__ == "__main__":
    ee.Initialize(project=PROJECT)
    {"submit": submit, "collect": collect}[sys.argv[1]]()
