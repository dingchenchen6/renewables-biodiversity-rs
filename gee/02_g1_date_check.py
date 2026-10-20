# ============================================================
# Scientific question / 科学问题:
# Are GRW construction quarters accurate enough for a quarterly
# staggered difference-in-differences design in China? (Gate G1)
# GRW 的建设季度在中国是否足够准确,能支撑季度精度的交错 DiD?(关口 G1)
#
# Objective / 分析目标:
# Independently date the land-surface step change at a stratified
# random sample of sites with Sentinel-2, and compare with GRW.
# 在分层随机样本站点上用哨兵-2 独立识别地表突变季度,与 GRW 比对。
#
# Input data / 输入数据:
#   GRW SOLAR_V1; COPERNICUS/S2_SR_HARMONIZED (SCL cloud mask)
# Workflow / 主要流程:
#   1. Sample 25 sites per build year 2020-2023, area >= 1 ha
#      2020-2023 每年随机抽 25 站,面积 ≥ 1 公顷
#   2. Quarterly median reflectance, GRW quarter -8 ... +6
#      GRW 季度前 8 个、后 6 个季度的中位数反射率
#   3. Single step-change split maximising the before/after distance
#      寻找前后均值距离最大的单一断点
# Expected output / 预期输出:
#   results/g1_sample_quarterly.csv, results/g1_breaks.csv
# Key assumptions / 关键假设:
#   Construction produces one dominant spectral step; GRW quarter =
#   first quarter the site is visible.
#   建设造成一次主导性光谱突变;GRW 季度为站点首次可见的季度。
# Main packages / 主要包: earthengine-api, pandas, numpy
# Output directory / 输出路径: results/
# ============================================================
import logging
from pathlib import Path

import ee
import numpy as np
import pandas as pd

logging.basicConfig(level=logging.INFO, format="%(asctime)s %(message)s")
logger = logging.getLogger(__name__)

PROJECT = "renewable-conservation"
OUT = Path(__file__).resolve().parents[1] / "results"
SEED = 42
PER_YEAR = 25
BUILD_YEARS = (2020, 2021, 2022, 2023)
MIN_AREA_M2 = 10_000
# 窗口需远宽于预期误差,否则断点堆积在窗口边界(v1 用 ±4 即出现此问题)
# window must be much wider than the expected error, else breaks pile up at the edges
OFFSETS = list(range(-8, 7))
BANDS = ["B2", "B3", "B4", "B8", "B11", "B12"]
BATCH = 10


def sample_sites() -> ee.FeatureCollection:
    """Stratified random sample by build year / 按建设年分层随机抽样."""
    s = (ee.FeatureCollection("projects/sat-io/open-datasets/GRW/SOLAR_V1")
         .filter(ee.Filter.eq("COUNTRY", "China"))
         .filter(ee.Filter.gte("area", MIN_AREA_M2)))
    parts = [s.filter(ee.Filter.eq("construction_year", y)).randomColumn("r", SEED).sort("r").limit(PER_YEAR)
             for y in BUILD_YEARS]
    return ee.FeatureCollection(parts).flatten()


def _masked(img: ee.Image) -> ee.Image:
    # 保留植被、裸地、水体、未分类像元,去除云、云影、雪
    # keep SCL 4/5/6/7, drop cloud, shadow, snow
    scl = img.select("SCL")
    keep = scl.eq(4).Or(scl.eq(5)).Or(scl.eq(6)).Or(scl.eq(7))
    return img.updateMask(keep).select(BANDS)


def quarterly_means(site: ee.Feature) -> ee.FeatureCollection:
    """Mean of quarterly median reflectance for each offset / 各季度中位数反射率均值."""
    geom = site.geometry()
    y = ee.Number(site.get("construction_year"))
    q = ee.Number(site.get("construction_quarter"))
    base = ee.Date.fromYMD(y, q.subtract(1).multiply(3).add(1), 1)
    col = ee.ImageCollection("COPERNICUS/S2_SR_HARMONIZED").filterBounds(geom)

    def one(off):
        off = ee.Number(off)
        start = base.advance(off.multiply(3), "month")
        sub = col.filterDate(start, start.advance(3, "month")).map(_masked)
        stats = sub.median().reduceRegion(ee.Reducer.mean(), geom, 20, maxPixels=1e8, tileScale=4)
        return ee.Feature(None, stats).set({"fid": site.get("fid"), "offset": off, "n_img": sub.size(),
                                            "area": site.get("area"), "construction_year": y,
                                            "construction_quarter": q})
    return ee.FeatureCollection(ee.List(OFFSETS).map(one))


def best_split(mat: np.ndarray) -> tuple[int, float]:
    """Offset k maximising ||mean(after) - mean(before)||; after includes k.

    返回使前后均值距离最大的断点 k(k 及之后为建后)。
    """
    best_k, best_d = None, -1.0
    for i in range(1, len(OFFSETS)):
        pre, post = mat[:i], mat[i:]
        pre, post = pre[~np.isnan(pre).any(axis=1)], post[~np.isnan(post).any(axis=1)]
        if len(pre) < 2 or len(post) < 2:
            continue
        d = float(np.linalg.norm(post.mean(axis=0) - pre.mean(axis=0)))
        if d > best_d:
            best_k, best_d = OFFSETS[i], d
    return best_k, best_d


def main() -> None:
    """Fetch quarterly series in batches, detect breaks, summarise G1."""
    ee.Initialize(project=PROJECT)
    sites = sample_sites()
    meta = sites.select(["fid", "area", "construction_year", "construction_quarter", "landcover_in_2018"],
                        retainGeometry=False).getInfo()["features"]
    fids = [f["properties"]["fid"] for f in meta]
    logger.info("sampled %s sites", len(fids))
    rows = []
    for i in range(0, len(fids), BATCH):
        chunk = sites.filter(ee.Filter.inList("fid", fids[i:i + BATCH]))
        fc = chunk.map(quarterly_means).flatten()
        rows += [f["properties"] for f in fc.getInfo()["features"]]
        logger.info("batch %s done (%s rows)", i // BATCH + 1, len(rows))
    df = pd.DataFrame(rows)
    OUT.mkdir(exist_ok=True)
    df.to_csv(OUT / "g1_sample_quarterly.csv", index=False)

    breaks = []
    for fid, g in df.groupby("fid"):
        g = g.set_index("offset").reindex(OFFSETS)
        mat = g[BANDS].to_numpy(dtype=float) / 10000.0
        k, d = best_split(mat)
        breaks.append({"fid": fid, "construction_year": g["construction_year"].dropna().iloc[0],
                       "area": g["area"].dropna().iloc[0], "break_offset": k, "break_dist": d,
                       "n_valid_quarters": int((~np.isnan(mat).any(axis=1)).sum()),
                       "at_window_edge": k in (OFFSETS[2], OFFSETS[-2])})
    b = pd.DataFrame(breaks)
    b.to_csv(OUT / "g1_breaks.csv", index=False)
    v = b.dropna(subset=["break_offset"])
    logger.info("sites with a detectable break: %s / %s", len(v), len(b))
    logger.info("break offset distribution:\n%s", v["break_offset"].value_counts().sort_index().to_string())
    for w in (0, 1, 2):
        logger.info("|offset| <= %s: %.1f%%", w, 100 * (v["break_offset"].abs() <= w).mean())
    logger.info("offset in [-2, 0]: %.1f%%", 100 * v["break_offset"].between(-2, 0).mean())
    logger.info("breaks at window edge: %s", int(v["at_window_edge"].sum()))


if __name__ == "__main__":
    main()
