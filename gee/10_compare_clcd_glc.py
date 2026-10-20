# ============================================================
# Objective / 分析目标:
# Compare pre-construction land cover from CLCD v1.0.5 and
# GLC_FCS30D for post-2018 PV and wind; report shares by year and
# size, and site-level agreement of the dominant class.
# 比较 CLCD 与 GLC_FCS30D 给出的建设前覆盖:按年份、规模的占比,
# 以及逐站主导类别的一致率。
# Input / 输入: results/clcd_preconstruction_*.csv,
#   results/preconstruction_landcover_by_site.csv, results/wind_preconstruction_glc_by_year.csv
# Output / 输出: results/clcd_summary_*.csv
# Class harmonisation / 类别对齐: GLC sparse_vegetation is merged into
#   grassland and bare is renamed barren to match CLCD's 9 classes.
#   GLC 的稀疏植被并入草地、裸地对应 CLCD 的 barren,以便逐站比较。
# ============================================================
import logging
from pathlib import Path

import pandas as pd

logging.basicConfig(level=logging.INFO, format="%(message)s")
logger = logging.getLogger(__name__)
RES = Path(__file__).resolve().parents[1] / "results"
CL = ["cropland", "forest", "shrub", "grassland", "water", "snow_ice", "barren", "impervious", "wetland"]
pd.set_option("display.width", 200)


def area_shares(df: pd.DataFrame, by: str | None) -> pd.DataFrame:
    """Area-weighted class shares (%) / 面积加权占比."""
    w = df[[f"share_{c}" for c in CL]].mul(df["area"], axis=0)
    w.columns = CL
    if by is None:
        return (w.sum() / df["area"].sum() * 100).round(1).to_frame("area_pct")
    return (w.groupby(df[by], observed=True).sum().div(df.groupby(by, observed=True)["area"].sum(), axis=0) * 100).round(1)


def main() -> None:
    """Print and save comparison tables / 输出对比表."""
    s = pd.read_csv(RES / "clcd_preconstruction_solar_by_site.csv")
    logger.info("PV sites %s, with pixels %s, total area %.0f km2", len(s), (s["n_pix"] > 0).sum(), s["area"].sum() / 1e6)
    s = s[s["n_pix"] > 0].copy()
    overall = area_shares(s, None)
    overall["dominant_count_pct"] = (s[[f"share_{c}" for c in CL]].idxmax(axis=1).str[6:]
                                     .value_counts(normalize=True).reindex(CL).fillna(0) * 100).round(1)
    logger.info("PV overall (CLCD, prior year):\n%s", overall.to_string())
    yr = area_shares(s, "construction_year")
    logger.info("PV by year:\n%s", yr[["barren", "grassland", "cropland", "forest", "water", "wetland", "impervious"]].to_string())
    s["size"] = pd.cut(s["area"], [0, 1e5, 1e6, 1e9], labels=["1-10ha", "10-100ha", ">100ha"])
    sz = area_shares(s, "size")
    logger.info("PV by size:\n%s", sz[["barren", "grassland", "cropland", "forest", "water"]].to_string())

    g = pd.read_csv(RES / "preconstruction_landcover_by_site.csv")
    g["share_grassland"] = g["share_grassland"] + g["share_sparse_vegetation"]
    g = g.rename(columns={"share_bare": "share_barren", "share_shrubland": "share_shrub", "share_ice": "share_snow_ice"})
    gl = ["cropland", "forest", "shrub", "grassland", "water", "snow_ice", "barren", "impervious", "wetland"]
    g["dom_glc"] = g[[f"share_{c}" for c in gl]].idxmax(axis=1).str[6:]
    s["dom_clcd"] = s[[f"share_{c}" for c in CL]].idxmax(axis=1).str[6:]
    m = s[["fid", "area", "construction_year", "dom_clcd"]].merge(g[["fid", "dom_glc"]], on="fid")
    agree = (m["dom_clcd"] == m["dom_glc"]).mean()
    agree_area = (m["area"] * (m["dom_clcd"] == m["dom_glc"])).sum() / m["area"].sum()
    logger.info("dominant class agreement CLCD vs GLC: %.1f%% of sites, %.1f%% of area", agree * 100, agree_area * 100)
    ct = pd.crosstab(m["dom_glc"], m["dom_clcd"], values=m["area"], aggfunc="sum", normalize="index").mul(100).round(1)
    logger.info("rows GLC, cols CLCD (area %%):\n%s", ct.to_string())

    w = pd.read_csv(RES / "clcd_preconstruction_wind_by_turbine.csv")
    w = w[w["clcd"] > 0]
    wo = (w["clcd_class"].value_counts(normalize=True) * 100).round(1)
    logger.info("wind turbines %s; CLCD prior-year class %%:\n%s", len(w), wo.to_string())
    wy = pd.crosstab(w["construction_year"], w["clcd_class"], normalize="index").mul(100).round(1)
    logger.info("wind by year:\n%s", wy.to_string())
    logger.info("wind on forest: %s turbines", int((w["clcd_class"] == "forest").sum()))

    overall.to_csv(RES / "clcd_summary_pv_overall.csv")
    yr.to_csv(RES / "clcd_summary_pv_by_year.csv")
    sz.to_csv(RES / "clcd_summary_pv_by_size.csv")
    ct.to_csv(RES / "clcd_vs_glc_pv_dominant_crosstab.csv")
    wy.to_csv(RES / "clcd_summary_wind_by_year.csv")


if __name__ == "__main__":
    main()
