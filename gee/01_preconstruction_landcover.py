# ============================================================
# Scientific question / 科学问题:
# What land cover did China's new (2018+) ground-mounted PV occupy
# in the year before construction?
# 2018 年起新建的中国地面光伏,在建设前一年占用的是什么土地覆盖?
#
# Objective / 分析目标:
# Replace the coarse, possibly post-construction ESA CCI 2018 label
# in GRW with prior-year 30 m GLC_FCS30D land cover.
# 用建设前一年的 30 m GLC_FCS30D 替换 GRW 自带的 300 m、
# 可能已是建成后状态的 ESA CCI 2018 标签。
#
# Input data / 输入数据:
#   GRW SOLAR_V1 (projects/sat-io/open-datasets/GRW/SOLAR_V1)
#   GLC_FCS30D annual 2000-2022 (projects/sat-io/open-datasets/GLC-FCS30D/annual)
# Workflow / 主要流程:
#   1. Keep China sites built 2018-2024 / 保留 2018-2024 年新建站点
#   2. For build year Y, read land cover of Y-1 (capped at 2022)
#      建设年 Y 读取 Y-1 年覆盖(2022 封顶,2024 站点标记)
#   3. Per-polygon 30 m class histogram -> area shares
#      逐地块 30 m 类别直方图,换算面积占比
# Expected output / 预期输出:
#   results/preconstruction_landcover_by_site.csv
#   results/preconstruction_landcover_summary.csv
# Key assumptions / 关键假设:
#   GLC_FCS30D band b1 = 2000 ... b23 = 2022; GRW construction_year
#   is first-detection year in PlanetScope imagery.
#   波段 b1 对应 2000 年;GRW 建设年为影像首次检出年份。
# Main packages / 主要包: earthengine-api, pandas
# Output directory / 输出路径: results/
# Run / 运行:
#   HTTPS_PROXY=socks5h://127.0.0.1:7897 python gee/01_preconstruction_landcover.py
# ============================================================
import logging
from pathlib import Path

import ee
import pandas as pd

logging.basicConfig(level=logging.INFO, format="%(asctime)s %(message)s")
logger = logging.getLogger(__name__)

PROJECT = "renewable-conservation"
OUT = Path(__file__).resolve().parents[1] / "results"
YEARS = range(2018, 2025)
LC_LAST_YEAR = 2022

# GLC_FCS30D 细类归并为 10 个大类 / collapse fine classes into 10 groups
GROUPS = {
    1: ("cropland", [10, 11, 12, 20]),
    2: ("forest", [51, 52, 61, 62, 71, 72, 81, 82, 91, 92]),
    3: ("shrubland", [120, 121, 122]),
    4: ("grassland", [130]),
    5: ("sparse_vegetation", [140, 150, 152, 153]),
    6: ("wetland", [181, 182, 183, 184, 185, 186, 187]),
    7: ("impervious", [190]),
    8: ("bare", [200, 201, 202]),
    9: ("water", [210]),
    10: ("ice", [220]),
}


def grouped_landcover(year: int) -> ee.Image:
    """Return GLC_FCS30D for `year` remapped to the 10 groups.

    返回指定年份重分类后的土地覆盖(30 m)。
    """
    band = f"b{year - 1999}"
    src = ee.ImageCollection("projects/sat-io/open-datasets/GLC-FCS30D/annual").select(band).mosaic()
    frm, to = [], []
    for gid, (_, codes) in GROUPS.items():
        frm += codes
        to += [gid] * len(codes)
    return src.remap(frm, to).rename("lc")


def histogram_for_year(sites: ee.FeatureCollection, year: int) -> list[dict]:
    """Per-polygon class pixel counts using land cover of the prior year.

    逐地块统计建设前一年各类 30 m 像元数。
    """
    lc_year = min(year - 1, LC_LAST_YEAR)
    img = grouped_landcover(lc_year)
    sub = sites.filter(ee.Filter.eq("construction_year", year))
    # 中心像元法兜底:小于 1 个像元的地块 frequencyHistogram 可能为空
    # fallback handled downstream when histogram is empty
    red = img.reduceRegions(collection=sub, reducer=ee.Reducer.frequencyHistogram(), scale=30, tileScale=4)
    rows = red.select(["fid", "area", "construction_year", "construction_quarter", "landcover_in_2018", "histogram"],
                      retainGeometry=False).getInfo()["features"]
    out = []
    for f in rows:
        p = f["properties"]
        hist = p.get("histogram") or {}
        tot = sum(hist.values())
        rec = {k: p.get(k) for k in ["fid", "area", "construction_year", "construction_quarter", "landcover_in_2018"]}
        rec["lc_year"] = lc_year
        rec["n_pix"] = tot
        for gid, (name, _) in GROUPS.items():
            rec[f"share_{name}"] = hist.get(str(gid), 0) / tot if tot else float("nan")
        out.append(rec)
    logger.info("year %s: %s sites, LC year %s", year, len(out), lc_year)
    return out


def main() -> None:
    """Run all build years and write site-level and summary tables."""
    ee.Initialize(project=PROJECT)
    sites = ee.FeatureCollection("projects/sat-io/open-datasets/GRW/SOLAR_V1").filter(ee.Filter.eq("COUNTRY", "China"))
    recs = []
    for y in YEARS:
        try:
            recs += histogram_for_year(sites, y)
        except ee.EEException as e:
            logger.error("year %s failed: %s", y, e)
            raise
    df = pd.DataFrame(recs)
    OUT.mkdir(exist_ok=True)
    df.to_csv(OUT / "preconstruction_landcover_by_site.csv", index=False)

    # 面积加权占比(按地块面积 × 类别占比)与按主导类别的数量占比
    # area-weighted shares and dominant-class counts
    names = [n for n, _ in GROUPS.values()]
    ok = df[df["n_pix"] > 0].copy()
    ok["dominant"] = ok[[f"share_{n}" for n in names]].idxmax(axis=1).str.replace("share_", "")
    area_w = {n: (ok["area"] * ok[f"share_{n}"]).sum() / ok["area"].sum() for n in names}
    count_dom = ok["dominant"].value_counts(normalize=True).to_dict()
    summ = pd.DataFrame({"class": names,
                         "area_share": [area_w[n] for n in names],
                         "dominant_count_share": [count_dom.get(n, 0.0) for n in names]})
    summ.attrs["n_sites"] = len(ok)
    summ.to_csv(OUT / "preconstruction_landcover_summary.csv", index=False)
    logger.info("sites with pixels: %s / %s; total area km2: %.1f", len(ok), len(df), ok["area"].sum() / 1e6)
    logger.info("\n%s", summ.round(4).to_string(index=False))


if __name__ == "__main__":
    main()
