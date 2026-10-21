# ============================================================
# Scientific question / 科学问题:
# Does the on-site pre-trend in the NW pilot disappear when PV sites
# are compared with never-built locations matched on their 2010-2016
# NDVI trajectory and siting variables?
# 若与按 2010-2016 年 NDVI 轨迹及选址变量匹配的"从未建设"地点比较,
# 西北试点站内的预趋势是否消失?
#
# Objective / 分析目标:
# Build a 2010-2024 NDVI panel for the 365 pilot sites and for random
# never-built candidate disks, plus matching covariates.
# 为 365 个试点站与随机"从未建设"候选圆盘构建 2010-2024 年 NDVI 面板及匹配变量。
#
# Input data / 输入数据:
#   results/pilot_nw_panel.csv (site list); GRW SOLAR_V1;
#   Landsat 5/7/8/9 C2 L2; GLC_FCS30D (2017 cover); SRTM; Global Solar Atlas PVOUT
# Workflow / 主要流程:
#   1. Candidates (built locally to avoid GEE time-outs): random points in the
#      NW five provinces, > 2 km beyond any GRW PV footprint, CLCD 2017 not
#      water/snow/impervious/wetland; disk r = 300 m
#      候选点(本地生成,避免 GEE 超时):西北五省区随机点,距任何光伏边缘 > 2 km,
#      CLCD 2017 年非水体/冰雪/不透水/湿地;圆盘半径 300 m
#   2. One 15-band image of Jun-Sep median NDVI (2010-2024), reduced once
#      15 个年份的生长季 NDVI 合成一个多波段影像,一次性统计
#   3. Mean and SD on unit and in a 60-500 m ring beyond its edge
#      单元内与边缘外 60-500 m 环的均值和标准差
# Expected output / 预期输出:
#   results/matched_units_covariates.csv, results/matched_units_ndvi_panel.csv
# Key assumptions / 关键假设:
#   Sensor changes (TM/ETM+/OLI) are common to all units in a year and are
#   absorbed by year fixed effects. 传感器差异对同年所有单元相同,由年份固定效应吸收。
# Main packages / 主要包: earthengine-api, pandas
# Output directory / 输出路径: results/
# ============================================================
import json
import logging
import os
from pathlib import Path

import ee
import numpy as np
import pandas as pd
import rasterio
from pyproj import Transformer
from scipy.spatial import cKDTree
from shapely.geometry import Point, shape
from shapely.ops import unary_union

logging.basicConfig(level=logging.INFO, format="%(asctime)s %(message)s")
logger = logging.getLogger(__name__)

PROJECT = "renewable-conservation"
ROOT = Path(__file__).resolve().parents[1]
RES, CACHE = ROOT / "results", ROOT / "cache"
CLCD_2017 = Path(os.environ.get("CLCD_DIR", ROOT.parents[0] / "data_raw" / "CLCD_v1.0.5")) / "CLCD_v01_2017_albert.tif"
ALBERS = "+proj=aea +lat_1=25 +lat_2=47 +lat_0=0 +lon_0=105 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
CLCD_EXCLUDE = {0, 5, 6, 8, 9}  # 无数据、水体、冰雪、不透水面、湿地 / nodata, water, snow, impervious, wetland
PROVINCES = ["Xinjiang Uygur Zizhiqu", "Gansu Sheng", "Qinghai Sheng", "Ningxia Huizu Zizhiqu", "Nei Mongol Zizhiqu"]
YEARS = list(range(2010, 2025))
N_CAND, SEED, DISK_R, EXCL_M = 3000, 42, 300, 2000
RING_IN, RING_OUT = 60, 500
BATCH = 150


def _prep(img: ee.Image, red: str, nir: str) -> ee.Image:
    qa = img.select("QA_PIXEL")
    clear = qa.bitwiseAnd(0b111010).eq(0)  # 膨胀云、云、云影、雪 / dilated cloud, cloud, shadow, snow
    sr = img.select([red, nir]).multiply(0.0000275).add(-0.2)
    return sr.normalizedDifference([nir, red]).rename("ndvi").updateMask(clear)


def ndvi_stack() -> ee.Image:
    """15-band growing-season NDVI, 2010-2024 / 生长季 NDVI 多波段影像."""
    tm = (ee.ImageCollection("LANDSAT/LT05/C02/T1_L2").merge(ee.ImageCollection("LANDSAT/LE07/C02/T1_L2"))
          .map(lambda i: _prep(i, "SR_B3", "SR_B4")))
    oli = (ee.ImageCollection("LANDSAT/LC08/C02/T1_L2").merge(ee.ImageCollection("LANDSAT/LC09/C02/T1_L2"))
           .map(lambda i: _prep(i, "SR_B4", "SR_B5")))
    allc = tm.merge(oli)
    bands = [allc.filterDate(f"{y}-06-01", f"{y}-09-30").median().rename(f"y{y}") for y in YEARS]
    return ee.Image.cat(bands)


def clcd_at(lon: np.ndarray, lat: np.ndarray) -> np.ndarray:
    """CLCD 2017 class at points (local file) / 本地读取 CLCD 2017 类别."""
    x, y = Transformer.from_crs("EPSG:4326", ALBERS, always_xy=True).transform(lon, lat)
    with rasterio.open(CLCD_2017) as r:
        return np.array([v[0] for v in r.sample(list(zip(x, y)))], dtype=int)


def local_candidates(prov: ee.FeatureCollection, grw: ee.FeatureCollection) -> pd.DataFrame:
    """Random never-built candidates generated locally / 本地生成候选点."""
    fp = CACHE / "nw_provinces.geojson"
    if not fp.exists():
        simp = prov.map(lambda f: f.simplify(500)).select(["ADM1_NAME"])
        fp.write_text(json.dumps(simp.getInfo()))
    feats = json.loads(fp.read_text())["features"]
    polys = {f["properties"]["ADM1_NAME"]: shape(f["geometry"]) for f in feats}
    union = unary_union(list(polys.values()))
    fpv = CACHE / "grw_china_centroids.csv"
    if not fpv.exists():
        g = grw.map(lambda f: f.set("cx", f.geometry().centroid(10).coordinates().get(0),
                                    "cy", f.geometry().centroid(10).coordinates().get(1)))
        arr = g.reduceColumns(ee.Reducer.toList().repeat(3), ["cx", "cy", "area"]).get("list").getInfo()
        pd.DataFrame({"lon": arr[0], "lat": arr[1], "area": arr[2]}).to_csv(fpv, index=False)
    pv = pd.read_csv(fpv)
    tf = Transformer.from_crs("EPSG:4326", ALBERS, always_xy=True)
    px, py = tf.transform(pv["lon"].to_numpy(), pv["lat"].to_numpy())
    tree = cKDTree(np.c_[px, py])
    pv_r = np.sqrt(pv["area"].to_numpy() / np.pi)  # 等面积圆半径 / equal-area radius
    rng = np.random.default_rng(SEED)
    keep = []
    minx, miny, maxx, maxy = union.bounds
    while len(keep) < N_CAND:
        lon = rng.uniform(minx, maxx, 20000); lat = rng.uniform(miny, maxy, 20000)
        inside = np.array([union.contains(Point(a, b)) for a, b in zip(lon, lat)])
        lon, lat = lon[inside], lat[inside]
        x, y = tf.transform(lon, lat)
        # 距最近若干光伏站边缘 > 2 km / > 2 km beyond the edge of nearby PV sites
        dist, idx = tree.query(np.c_[x, y], k=8)
        ok = ((dist - pv_r[idx]) > EXCL_M + DISK_R).all(axis=1)
        lon, lat = lon[ok], lat[ok]
        lc = clcd_at(lon, lat)
        ok = ~np.isin(lc, list(CLCD_EXCLUDE))
        for a, b, c in zip(lon[ok], lat[ok], lc[ok]):
            prov_name = next((k for k, v in polys.items() if v.contains(Point(a, b))), "NA")
            keep.append((a, b, int(c), prov_name))
        logger.info("candidates kept: %s", len(keep))
    df = pd.DataFrame(keep[:N_CAND], columns=["lon", "lat", "lc2017", "province"])
    df["unit_id"] = [f"c{i:05d}" for i in range(len(df))]
    return df


def reduce_units(units: ee.FeatureCollection, stack: ee.Image, pv_free: ee.Image, ring_in: float,
                 ring_out: float) -> pd.DataFrame:
    """Mean/SD per unit and ring for all 15 bands / 单元与外围环的均值、标准差."""
    red = ee.Reducer.mean().combine(ee.Reducer.stdDev(), sharedInputs=True)
    ids = units.aggregate_array("unit_id").getInfo()
    out = []
    for i in range(0, len(ids), BATCH):
        sub = units.filter(ee.Filter.inList("unit_id", ids[i:i + BATCH]))
        ring = sub.map(lambda f: f.setGeometry(f.geometry().buffer(ring_out, 10).difference(
            f.geometry().buffer(ring_in, 10), 10)))
        for part, fc, im in (("site", sub, stack), ("ring", ring, stack.updateMask(pv_free))):
            r = im.reduceRegions(collection=fc.select(["unit_id"]), reducer=red, scale=30, tileScale=8)
            cols = ["unit_id"] + [f"y{y}_{s}" for y in YEARS for s in ("mean", "stdDev")]
            r = r.map(lambda f: f.set({c: ee.Algorithms.If(f.get(c), f.get(c), -9) for c in cols[1:]}))
            arr = r.reduceColumns(ee.Reducer.toList().repeat(len(cols)), cols).get("list").getInfo()
            df = pd.DataFrame(dict(zip(cols, arr))).melt(id_vars="unit_id")
            df[["year", "stat"]] = df["variable"].str.extract(r"y(\d{4})_(\w+)")
            df["part"] = part
            out.append(df.drop(columns="variable"))
        logger.info("units %s-%s done", i, min(i + BATCH, len(ids)))
    long = pd.concat(out).replace(-9, pd.NA)
    wide = long.pivot_table(index=["unit_id", "year"], columns=["part", "stat"], values="value", aggfunc="first")
    wide.columns = [f"{p}_{s}" for p, s in wide.columns]
    return wide.reset_index()


def main() -> None:
    """Build covariates and NDVI panel for sites and candidates / 构建面板."""
    ee.Initialize(project=PROJECT)
    prov = (ee.FeatureCollection("FAO/GAUL/2015/level1").filter(ee.Filter.eq("ADM0_NAME", "China"))
            .filter(ee.Filter.inList("ADM1_NAME", PROVINCES)))
    grw = ee.FeatureCollection("projects/sat-io/open-datasets/GRW/SOLAR_V1").filter(ee.Filter.eq("COUNTRY", "China"))
    grw_nw = grw.filterBounds(ee.Geometry.Rectangle([73, 31, 127, 50], None, False))
    pv_free = ee.Image(0).paint(grw_nw, 1).Not()

    meta = pd.read_csv(RES / "pilot_nw_panel.csv").drop_duplicates("fid")[["fid", "construction_year", "area", "province"]]
    sites = grw.filter(ee.Filter.inList("fid", meta["fid"].astype(int).tolist())).map(
        lambda f: f.set("unit_id", ee.String("s").cat(ee.Number(f.get("fid")).format("%d"))))
    fc_c = CACHE / "nw_candidates.csv"
    if fc_c.exists():
        cdf = pd.read_csv(fc_c)
    else:
        cdf = local_candidates(prov, grw)
        cdf.to_csv(fc_c, index=False)
    cand = ee.FeatureCollection([ee.Feature(ee.Geometry.Point([lo, la]).buffer(DISK_R, 10), {"unit_id": u})
                                 for lo, la, u in zip(cdf["lon"], cdf["lat"], cdf["unit_id"])])
    logger.info("candidates: %s", len(cdf))

    # 匹配变量:PVOUT、坡度、海拔取自 GEE;2017 覆盖取 CLCD 本地文件
    # covariates: PVOUT, slope, elevation from GEE; 2017 cover from local CLCD
    dem = ee.Image("USGS/SRTMGL1_003")
    cov = ee.Image.cat([
        ee.Image("projects/sat-io/open-datasets/global_solar_atlas/pvout_LTAy_AvgDailyTotals").select(0).rename("pvout"),
        ee.Terrain.slope(dem).rename("slope"), dem.rename("elev")])
    cols = ["unit_id", "pvout", "slope", "elev"]
    rows = []
    # 分批提取,避免超时 / batched to avoid time-outs
    for fc in (sites, cand):
        ids = fc.aggregate_array("unit_id").getInfo()
        for i in range(0, len(ids), 300):
            sub = fc.filter(ee.Filter.inList("unit_id", ids[i:i + 300])).select(["unit_id"])
            r = cov.reduceRegions(collection=sub, reducer=ee.Reducer.mean(), scale=90, tileScale=8)
            r = r.map(lambda f: f.set({c: ee.Algorithms.If(f.get(c), f.get(c), -9) for c in cols[1:]}))
            arr = r.reduceColumns(ee.Reducer.toList().repeat(len(cols)), cols).get("list").getInfo()
            rows.append(pd.DataFrame(dict(zip(cols, arr))))
        logger.info("covariates extracted for %s units", len(ids))
    cv = pd.concat(rows).replace(-9, pd.NA)
    # 处理站 2017 覆盖取地块质心 / PV sites: CLCD 2017 at polygon centroid
    feats = json.loads((CACHE / "grw_solar_2018plus.geojson").read_text())["features"]
    cen = {int(f["properties"]["fid"]): shape(f["geometry"]).centroid for f in feats
           if int(f["properties"]["fid"]) in set(meta["fid"].astype(int))}
    meta["lc2017"] = clcd_at(np.array([cen[f].x for f in meta["fid"].astype(int)]),
                             np.array([cen[f].y for f in meta["fid"].astype(int)]))
    meta = meta.assign(unit_id="s" + meta["fid"].astype(int).astype(str))
    attrs = pd.concat([meta[["unit_id", "province", "construction_year", "area", "fid", "lc2017"]],
                       cdf[["unit_id", "province", "lc2017"]].assign(construction_year=pd.NA,
                                                                     area=np.pi * DISK_R ** 2, fid=pd.NA)])
    cv = cv.merge(attrs, on="unit_id", how="left")
    cv["type"] = cv["unit_id"].str[0].map({"s": "pv", "c": "candidate"})
    cv.to_csv(RES / "matched_units_covariates.csv", index=False)
    logger.info("covariates: %s pv, %s candidates", (cv["type"] == "pv").sum(), (cv["type"] == "candidate").sum())

    stack = ndvi_stack()
    panel = pd.concat([reduce_units(sites, stack, pv_free, RING_IN, RING_OUT),
                       reduce_units(cand, stack, pv_free, RING_IN, RING_OUT)])
    panel.to_csv(RES / "matched_units_ndvi_panel.csv", index=False)
    logger.info("panel rows: %s", len(panel))


if __name__ == "__main__":
    main()
