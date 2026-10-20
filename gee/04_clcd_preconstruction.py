# ============================================================
# Scientific question / 科学问题:
# What did post-2018 PV and wind occupy in the year before
# construction, according to CLCD (China's own 30 m annual map)?
# 依据 CLCD(中国 30 m 年度土地覆盖),2018 年后新建光伏与风机
# 在建设前一年占用的是什么土地覆盖?
#
# Objective / 分析目标:
# Re-derive pre-construction land cover with CLCD v1.0.5 (1985-2025),
# so 2023-2024 sites get a true prior year, and add wind turbines.
# 用 CLCD v1.0.5 复算,使 2023-2024 年站点读到真正的前一年覆盖,并纳入风机。
#
# Input data / 输入数据:
#   GRW SOLAR_V1 / WIND_V1 (GEE); CLCD v1.0.5 COGs on Zenodo
#   (doi:10.5281/zenodo.18180184), read by HTTP range requests
#   CLCD 按需远程读取所需窗口,不整幅下载
# Workflow / 主要流程:
#   1. Export 2018+ China sites from GEE to cache/ / 导出站点几何
#   2. Reproject to CLCD Albers / 投影到 CLCD 的 Albers
#   3. Polygon class shares (PV) and point class (wind) for Y-1
#      光伏取多边形内各类占比,风机取点位像元类别
# Expected output / 预期输出:
#   results/clcd_preconstruction_solar_by_site.csv
#   results/clcd_preconstruction_wind_by_turbine.csv
# Key assumptions / 关键假设:
#   CLCD codes 1 cropland, 2 forest, 3 shrub, 4 grassland, 5 water,
#   6 snow/ice, 7 barren, 8 impervious, 9 wetland (Zenodo class table).
# Main packages / 主要包: earthengine-api, rasterio, shapely, pyproj, pandas
# Output directory / 输出路径: results/, cache/
# Run / 运行:
#   HTTPS_PROXY=socks5h://127.0.0.1:7897 GDAL_HTTP_PROXY=127.0.0.1:7897 \
#   python gee/04_clcd_preconstruction.py
# ============================================================
import json
import logging
import os
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path

import ee
import numpy as np
import pandas as pd
import rasterio
from pyproj import Transformer
from rasterio.features import geometry_mask
from rasterio.windows import from_bounds
from shapely.geometry import shape
from shapely.ops import transform as shp_transform

os.environ.setdefault("GDAL_DISABLE_READDIR_ON_OPEN", "EMPTY_DIR")
os.environ.setdefault("CPL_VSIL_CURL_ALLOWED_EXTENSIONS", ".tif,/content")
os.environ.setdefault("GDAL_CACHEMAX", "2048")
os.environ.setdefault("GDAL_HTTP_MAX_RETRY", "5")
os.environ.setdefault("GDAL_HTTP_RETRY_DELAY", "2")

logging.basicConfig(level=logging.INFO, format="%(asctime)s %(message)s")
logger = logging.getLogger(__name__)

PROJECT = "renewable-conservation"
ROOT = Path(__file__).resolve().parents[1]
CACHE, RES = ROOT / "cache", ROOT / "results"
# 远程按需读取会触发 Zenodo 限流(HTTP 429);建议先下载 2017-2023 年全国幅,
# 设 CLCD_DIR 指向本地目录 / remote range reads hit Zenodo rate limits (HTTP 429);
# prefer local copies via CLCD_DIR
_LOCAL = os.environ.get("CLCD_DIR")
URL = (f"{_LOCAL}/CLCD_v01_{{y}}_albert.tif" if _LOCAL else
       "/vsicurl/https://zenodo.org/api/records/18180184/files/CLCD_v01_{y}_albert.tif/content")
CLASSES = {1: "cropland", 2: "forest", 3: "shrub", 4: "grassland", 5: "water",
           6: "snow_ice", 7: "barren", 8: "impervious", 9: "wetland"}
ALBERS = "+proj=aea +lat_1=25 +lat_2=47 +lat_0=0 +lon_0=105 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
WORKERS = 8 if _LOCAL else 2


def export_sites() -> tuple[list[dict], pd.DataFrame]:
    """Export 2018+ China PV polygons and wind points (cached).

    从 GEE 导出 2018 年后新建站点;已缓存则直接读取。
    """
    CACHE.mkdir(exist_ok=True)
    fp_s, fp_w = CACHE / "grw_solar_2018plus.geojson", CACHE / "grw_wind_2018plus.csv"
    if fp_s.exists() and fp_w.exists():
        return json.loads(fp_s.read_text())["features"], pd.read_csv(fp_w)
    ee.Initialize(project=PROJECT)
    keep = ["fid", "area", "construction_year", "construction_quarter"]
    feats = []
    for y in range(2018, 2025):
        fc = (ee.FeatureCollection("projects/sat-io/open-datasets/GRW/SOLAR_V1")
              .filter(ee.Filter.eq("COUNTRY", "China")).filter(ee.Filter.eq("construction_year", y)).select(keep))
        feats += fc.getInfo()["features"]
        logger.info("solar %s exported (%s total)", y, len(feats))
    fp_s.write_text(json.dumps({"type": "FeatureCollection", "features": feats}))
    rows = []
    for y in range(2018, 2025):
        fc = (ee.FeatureCollection("projects/sat-io/open-datasets/GRW/WIND_V1")
              .filter(ee.Filter.eq("COUNTRY", "China")).filter(ee.Filter.eq("construction_year", y))
              .map(lambda f: f.set("lon", f.geometry().coordinates().get(0), "lat", f.geometry().coordinates().get(1))))
        cols = ["fid", "construction_year", "construction_quarter", "lon", "lat"]
        arr = fc.reduceColumns(ee.Reducer.toList().repeat(len(cols)), cols).get("list").getInfo()
        rows.append(pd.DataFrame(dict(zip(cols, arr))))
        logger.info("wind %s exported (%s)", y, len(rows[-1]))
    wind = pd.concat(rows, ignore_index=True)
    wind.to_csv(fp_w, index=False)
    return feats, wind


def _solar_chunk(year: int, items: list[tuple[dict, object]]) -> list[dict]:
    # 每个线程独立打开数据集 / one dataset handle per thread
    out = []
    with rasterio.open(URL.format(y=year)) as r:
        for props, geom in items:
            win = from_bounds(*geom.bounds, transform=r.transform).round_offsets().round_lengths()
            win = rasterio.windows.Window(win.col_off - 1, win.row_off - 1, win.width + 2, win.height + 2)
            arr = r.read(1, window=win, boundless=True, fill_value=0)
            inside = ~geometry_mask([geom], out_shape=arr.shape, transform=r.window_transform(win))
            vals = arr[inside & (arr > 0)]
            if vals.size == 0:  # 小地块兜底取质心像元 / centroid fallback
                vals = np.array([v[0] for v in r.sample([(geom.centroid.x, geom.centroid.y)])])
                vals = vals[vals > 0]
            rec = dict(props, lc_year=year, n_pix=int(vals.size))
            for k, name in CLASSES.items():
                rec[f"share_{name}"] = float((vals == k).mean()) if vals.size else np.nan
            out.append(rec)
    return out


def _wind_chunk(year: int, xy: np.ndarray) -> list[int]:
    with rasterio.open(URL.format(y=year)) as r:
        return [int(v[0]) for v in r.sample([tuple(p) for p in xy])]


def _split(n: int, k: int) -> list[slice]:
    edges = np.linspace(0, n, k + 1).astype(int)
    return [slice(a, b) for a, b in zip(edges[:-1], edges[1:]) if b > a]


def main() -> None:
    """Run CLCD extraction for PV and wind / 光伏与风机的 CLCD 提取."""
    feats, wind = export_sites()
    tf = Transformer.from_crs("EPSG:4326", ALBERS, always_xy=True)
    solar_rows = []
    by_year: dict[int, list] = {}
    for f in feats:
        p = f["properties"]
        g = shp_transform(tf.transform, shape(f["geometry"]))
        by_year.setdefault(int(p["construction_year"]) - 1, []).append((p, g))
    for y, items in sorted(by_year.items()):
        # 按 512 像元块排序,让相邻地块复用缓存块 / sort by block for cache reuse
        items.sort(key=lambda it: (int(-it[1].centroid.y // 15360), int(it[1].centroid.x // 15360)))
        with ThreadPoolExecutor(WORKERS) as ex:
            parts = ex.map(lambda s: _solar_chunk(y, items[s]), _split(len(items), WORKERS))
            for part in parts:
                solar_rows += part
        logger.info("solar LC year %s done: %s sites", y, len(items))
    s = pd.DataFrame(solar_rows)
    s.to_csv(RES / "clcd_preconstruction_solar_by_site.csv", index=False)

    x, yv = tf.transform(wind["lon"].to_numpy(), wind["lat"].to_numpy())
    wind = wind.assign(x=x, y=yv, lc_year=wind["construction_year"] - 1, clcd=0)
    for y, g in wind.groupby("lc_year"):
        g = g.assign(bx=(g["x"] // 15360).astype(int), by=(-g["y"] // 15360).astype(int)).sort_values(["by", "bx"])
        xy = g[["x", "y"]].to_numpy()
        with ThreadPoolExecutor(WORKERS) as ex:
            vals = sum(ex.map(lambda sl: _wind_chunk(y, xy[sl]), _split(len(xy), WORKERS)), [])
        wind.loc[g.index, "clcd"] = vals
        logger.info("wind LC year %s done: %s turbines", y, len(g))
    wind["clcd_class"] = wind["clcd"].map(CLASSES)
    wind.drop(columns=["x", "y"]).to_csv(RES / "clcd_preconstruction_wind_by_turbine.csv", index=False)
    logger.info("finished: %s PV sites, %s turbines", len(s), len(wind))


if __name__ == "__main__":
    main()
