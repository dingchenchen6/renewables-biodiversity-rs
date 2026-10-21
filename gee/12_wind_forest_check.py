# ============================================================
# Scientific question / 科学问题:
# How many post-2018 turbines were really sited in forest? CLCD says
# 12.5%, GLC_FCS30D says 6.3%; which product is right where they disagree?
# 2018 年后新建风机中多少真正建在森林里?两套产品在分歧处谁更可信?
#
# Objective / 分析目标:
# Per-turbine GLC_FCS30D prior-year class, cross-tab with CLCD, then a
# stratified sample for visual interpretation on Sentinel-2 imagery.
# 逐台取 GLC 前一年类别,与 CLCD 交叉,再分层抽样做哨兵-2 目视判读。
#
# Input data / 输入数据: results/clcd_preconstruction_wind_by_turbine.csv;
#   GLC_FCS30D; COPERNICUS/S2_HARMONIZED (TOA, available from 2016)
# Expected output / 预期输出: results/wind_forest_crosstab.csv,
#   results/wind_forest_sample.csv, results/wind_forest_visual/sheet_*.png
# Key assumptions / 关键假设: 10 m true colour in leaf-on season can
#   separate closed canopy from grass, crop or bare ground around a pad;
#   it cannot reliably separate open woodland from shrubland.
#   10 m 生长季真彩色可区分郁闭林冠与草地、耕地、裸地;疏林与灌丛难以区分。
# Main packages / 主要包: earthengine-api, pandas, requests, Pillow
# ============================================================
import importlib.util
import io
import logging
from pathlib import Path

import ee
import pandas as pd
import requests
from PIL import Image, ImageDraw

logging.basicConfig(level=logging.INFO, format="%(asctime)s %(message)s")
logger = logging.getLogger(__name__)

PROJECT = "renewable-conservation"
HERE = Path(__file__).resolve().parent
RES = HERE.parent / "results"
OUT = RES / "wind_forest_visual"
PER_STRATUM, SEED, PX, HALF_M = 12, 42, 220, 400
PROXIES = {"https": "socks5h://127.0.0.1:7897", "http": "socks5h://127.0.0.1:7897"}

_spec = importlib.util.spec_from_file_location("lc01", HERE / "01_preconstruction_landcover.py")
lc01 = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(lc01)
GID = {gid: name for gid, (name, _) in lc01.GROUPS.items()}


def glc_per_turbine(w: pd.DataFrame) -> pd.DataFrame:
    """GLC prior-year class for every turbine / 逐台 GLC 前一年类别."""
    out = []
    for y, g in w.groupby("construction_year"):
        img = lc01.grouped_landcover(min(int(y) - 1, lc01.LC_LAST_YEAR))
        for i in range(0, len(g), 5000):
            part = g.iloc[i:i + 5000]
            fc = ee.FeatureCollection([ee.Feature(ee.Geometry.Point([lo, la]), {"fid": int(f)})
                                       for f, lo, la in zip(part["fid"], part["lon"], part["lat"])])
            s = img.reduceRegions(collection=fc, reducer=ee.Reducer.first().setOutputs(["glc"]), scale=30)
            s = s.map(lambda f: f.set("glc", ee.Algorithms.If(f.get("glc"), f.get("glc"), -1)))
            arr = s.reduceColumns(ee.Reducer.toList().repeat(2), ["fid", "glc"]).get("list").getInfo()
            out.append(pd.DataFrame({"fid": arr[0], "glc": arr[1]}))
        logger.info("GLC sampled for %s turbines built %s", len(g), y)
    d = pd.concat(out)
    d["glc_class"] = d["glc"].astype(int).map(GID)
    return w.merge(d[["fid", "glc_class"]], on="fid", how="left")


def thumb(lon: float, lat: float, year: int) -> Image.Image:
    """Leaf-on Sentinel-2 true colour around a turbine / 机位周围生长季真彩色."""
    pt = ee.Geometry.Point([lon, lat])
    region = pt.buffer(HALF_M).bounds()
    col = (ee.ImageCollection("COPERNICUS/S2_HARMONIZED").filterBounds(pt)
           .filterDate(f"{year}-06-01", f"{year}-09-30").filter(ee.Filter.lt("CLOUDY_PIXEL_PERCENTAGE", 30)))
    img = col.median().visualize(bands=["B4", "B3", "B2"], min=300, max=2500)
    ring = ee.Image().byte().paint(ee.FeatureCollection([ee.Feature(pt.buffer(60))]), 1, 2).visualize(palette=["FFD400"])
    url = img.blend(ring).getThumbURL({"region": region, "dimensions": PX, "format": "png"})
    r = requests.get(url, proxies=PROXIES, timeout=120)
    r.raise_for_status()
    return Image.open(io.BytesIO(r.content)).convert("RGB").resize((PX, PX))


def main() -> None:
    """Cross-tab, sample and build contact sheets / 交叉表、抽样、图集."""
    ee.Initialize(project=PROJECT)
    w = pd.read_csv(RES / "clcd_preconstruction_wind_by_turbine.csv")
    w = w[w["clcd"] > 0]
    w = glc_per_turbine(w)
    ct = pd.crosstab(w["clcd_class"] == "forest", w["glc_class"] == "forest", margins=True)
    ct.index.name, ct.columns.name = "clcd_forest", "glc_forest"
    ct.to_csv(RES / "wind_forest_crosstab.csv")
    logger.info("\n%s", ct.to_string())
    logger.info("GLC class among CLCD-forest turbines:\n%s",
                w.loc[w["clcd_class"] == "forest", "glc_class"].value_counts(normalize=True).round(3).to_string())

    strata = {"both_forest": (w["clcd_class"] == "forest") & (w["glc_class"] == "forest"),
              "clcd_only": (w["clcd_class"] == "forest") & (w["glc_class"] != "forest"),
              "glc_only": (w["clcd_class"] != "forest") & (w["glc_class"] == "forest")}
    sample = pd.concat([w[m].sample(PER_STRATUM, random_state=SEED).assign(stratum=k) for k, m in strata.items()])
    sample["img_year"] = (sample["construction_year"] - 1).clip(lower=2016)
    sample = sample.reset_index(drop=True)
    sample["code"] = [f"T{i + 1:02d}" for i in range(len(sample))]  # 盲判编号 / blind codes
    sample.to_csv(RES / "wind_forest_sample.csv", index=False)

    OUT.mkdir(parents=True, exist_ok=True)
    # 打乱顺序做盲判,图上只标编号 / shuffled, labelled by code only, for blind calls
    order = sample.sample(frac=1, random_state=SEED + 1)
    tiles = []
    for _, r in order.iterrows():
        try:
            t = thumb(r["lon"], r["lat"], int(r["img_year"]))
        except (requests.RequestException, ee.EEException) as e:
            logger.error("%s failed: %s", r["code"], e)
            t = Image.new("RGB", (PX, PX), "grey")
        d = ImageDraw.Draw(t)
        d.rectangle([0, 0, 34, 14], fill="white")
        d.text((3, 2), r["code"], fill=(0, 0, 0))
        tiles.append(t)
    cols = 6
    for k in range(0, len(tiles), 18):
        grp = tiles[k:k + 18]
        rows = (len(grp) + cols - 1) // cols
        sheet = Image.new("RGB", (cols * PX, rows * PX), "white")
        for j, t in enumerate(grp):
            sheet.paste(t, ((j % cols) * PX, (j // cols) * PX))
        sheet.save(OUT / f"sheet_{k // 18 + 1}.png")
    logger.info("sheets written")


if __name__ == "__main__":
    main()
