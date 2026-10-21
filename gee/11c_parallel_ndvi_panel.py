# ============================================================
# Objective / 分析目标:
# Build the 2010-2024 NDVI panel for the matched design with many small
# interactive requests run in parallel, because this project has no
# writable asset root for batch exports. Each (year, batch) result is
# cached, so the run can resume; a timed-out batch is split in half.
# 本项目暂无可写资产根目录,批处理导出不可用;改为小批量交互式请求并行运行。
# 每个(年份, 批次)结果单独缓存,可断点续跑;超时的批次对半拆分重试。
#
# Input data / 输入数据: results/matched_units_covariates.csv, cache/nw_candidates.csv
# Expected output / 预期输出: results/matched_units_ndvi_panel.csv
# Main packages / 主要包: earthengine-api, pandas
# ============================================================
import importlib.util
import logging
from concurrent.futures import ThreadPoolExecutor, as_completed
from pathlib import Path

import ee
import pandas as pd

logging.basicConfig(level=logging.INFO, format="%(asctime)s %(message)s")
logger = logging.getLogger(__name__)

HERE = Path(__file__).resolve().parent
RES, PARTS = HERE.parent / "results", HERE.parent / "cache" / "ndvi_parts"
PROJECT, BATCH, WORKERS = "renewable-conservation", 30, 8
_spec = importlib.util.spec_from_file_location("m11b", HERE / "11b_export_ndvi_assets.py")
m11b = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(m11b)
COLS = ["unit_id", "part", "ndvi_mean", "ndvi_stdDev", "ndvi_free_mean", "ndvi_free_stdDev"]


def batch_fc(ids: list[str], cand: pd.DataFrame) -> ee.FeatureCollection:
    """Site and ring features for this batch only / 只组装本批单元的几何."""
    m = m11b.m11
    fids = [int(u[1:]) for u in ids if u.startswith("s")]
    sites = (ee.FeatureCollection("projects/sat-io/open-datasets/GRW/SOLAR_V1")
             .filter(ee.Filter.inList("fid", fids))
             .map(lambda f: ee.Feature(f.geometry(), {"unit_id": ee.String("s").cat(ee.Number(f.get("fid")).format("%d"))})))
    c = cand[cand["unit_id"].isin(ids)]
    disks = ee.FeatureCollection([ee.Feature(ee.Geometry.Point([lo, la]).buffer(m.DISK_R, 10), {"unit_id": u})
                                  for lo, la, u in zip(c["lon"], c["lat"], c["unit_id"])])
    units = sites.merge(disks)
    ring = units.map(lambda f: f.setGeometry(f.geometry().buffer(m.RING_OUT, 10).difference(
        f.geometry().buffer(m.RING_IN, 10), 10)).set("part", "ring"))
    return units.map(lambda f: f.set("part", "site")).merge(ring)


def run_batch(img: ee.Image, ids: list[str], cand: pd.DataFrame) -> pd.DataFrame:
    """Reduce one batch; split in half on failure / 单批统计,失败对半拆分."""
    red = ee.Reducer.mean().combine(ee.Reducer.stdDev(), sharedInputs=True)
    out = img.reduceRegions(collection=batch_fc(ids, cand), reducer=red, scale=30, tileScale=8)
    out = out.map(lambda f: f.set({c: ee.Algorithms.If(f.get(c), f.get(c), -9) for c in COLS[2:]}))
    try:
        arr = out.reduceColumns(ee.Reducer.toList().repeat(len(COLS)), COLS).get("list").getInfo()
        return pd.DataFrame(dict(zip(COLS, arr)))
    except ee.EEException as e:
        if len(ids) == 1:
            logger.error("unit %s failed: %s", ids[0], e)
            return pd.DataFrame(columns=COLS)
        h = len(ids) // 2
        return pd.concat([run_batch(img, ids[:h], cand), run_batch(img, ids[h:], cand)])


def main() -> None:
    """Parallel extraction with caching / 并行提取并缓存."""
    ee.Initialize(project=PROJECT)
    PARTS.mkdir(parents=True, exist_ok=True)
    cand = pd.read_csv(HERE.parent / "cache" / "nw_candidates.csv")
    ids = sorted(set(pd.read_csv(RES / "matched_units_covariates.csv")["unit_id"]))
    grw_nw = (ee.FeatureCollection("projects/sat-io/open-datasets/GRW/SOLAR_V1")
              .filter(ee.Filter.eq("COUNTRY", "China"))
              .filterBounds(ee.Geometry.Rectangle([73, 31, 127, 50], None, False)))
    pv_free = ee.Image(0).paint(grw_nw, 1).Not()
    jobs = [(y, i) for y in m11b.m11.YEARS for i in range(0, len(ids), BATCH)
            if not (PARTS / f"{y}_{i:05d}.csv").exists()]
    logger.info("jobs to run: %s", len(jobs))

    def job(y: int, i: int) -> str:
        df = run_batch(m11b.year_image(y, pv_free), ids[i:i + BATCH], cand).assign(year=y)
        df.to_csv(PARTS / f"{y}_{i:05d}.csv", index=False)
        return f"{y}_{i}"

    done = 0
    with ThreadPoolExecutor(WORKERS) as ex:
        futs = [ex.submit(job, y, i) for y, i in jobs]
        for f in as_completed(futs):
            try:
                f.result()
            except Exception as e:  # noqa: BLE001  记录后继续,重跑时自动补齐 / log and continue
                logger.error("job failed: %s", e)
            done += 1
            if done % 50 == 0:
                logger.info("%s / %s jobs done", done, len(jobs))
    d = pd.concat([pd.read_csv(p) for p in sorted(PARTS.glob("*.csv"))]).replace(-9, pd.NA)
    site = d[d["part"] == "site"][["unit_id", "year", "ndvi_mean", "ndvi_stdDev"]].rename(
        columns={"ndvi_mean": "site_mean", "ndvi_stdDev": "site_stdDev"})
    ring = d[d["part"] == "ring"][["unit_id", "year", "ndvi_free_mean", "ndvi_free_stdDev"]].rename(
        columns={"ndvi_free_mean": "ring_mean", "ndvi_free_stdDev": "ring_stdDev"})
    panel = site.merge(ring, on=["unit_id", "year"], how="outer")
    panel.to_csv(RES / "matched_units_ndvi_panel.csv", index=False)
    logger.info("panel rows: %s, units: %s (expected %s x %s)", len(panel), panel["unit_id"].nunique(),
                len(ids), len(m11b.m11.YEARS))


if __name__ == "__main__":
    main()
