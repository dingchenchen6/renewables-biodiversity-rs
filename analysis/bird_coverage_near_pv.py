# ============================================================
# Scientific question / 科学问题:
# Are there enough bird checklists around PV sites, before and after
# construction, to estimate site-level biodiversity change?
# 光伏站周边在建设前后是否有足够的观鸟清单,能估计站点尺度的多样性变化?
#
# Objective / 分析目标: feasibility gate before multispecies occupancy
#   modelling. 多物种占域模型之前的可行性关口。
# Input data / 输入数据:
#   cache/birds/events_unique.csv.gz (CBRC + eBird/GBIF, merged and
#   deduplicated by the bird community dynamics project, 464,870 checklists)
#   cache/grw_solar_2018plus.geojson; results/g1_tzsam_crosscheck.csv;
#   results/pilot_nw_panel.csv; cache/nw_candidates.csv
# Workflow / 主要流程:
#   distance from each checklist to the site edge (centroid distance minus
#   equal-area radius); count checklists within 2 km and 5 km in the three
#   years before (g-3..g-1) and after (g+1..g+3) construction
#   清单到站点边缘的距离(质心距离减等面积半径);统计建设前后各 3 年、2/5 km 内的清单数
# Expected output / 预期输出: results/bird_coverage_near_pv.csv (+ summary)
# Main packages / 主要包: pandas, numpy, scipy, pyproj, shapely
# ============================================================
import json
import logging
from pathlib import Path

import numpy as np
import pandas as pd
from pyproj import Transformer
from scipy.spatial import cKDTree
from shapely.geometry import shape

logging.basicConfig(level=logging.INFO, format="%(message)s")
logger = logging.getLogger(__name__)
ROOT = Path(__file__).resolve().parents[1]
RES, CACHE = ROOT / "results", ROOT / "cache"
ALBERS = "+proj=aea +lat_1=25 +lat_2=47 +lat_0=0 +lon_0=105 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
RADII = (2000, 5000)
WIN = 3


def main() -> None:
    """Count checklists near sites pre/post construction / 统计建设前后清单数."""
    tf = Transformer.from_crs("EPSG:4326", ALBERS, always_xy=True)
    ev = pd.read_csv(CACHE / "birds" / "events_unique.csv.gz", usecols=["event_id", "lon", "lat", "year"])
    ev = ev.dropna()
    ex, ey = tf.transform(ev["lon"].to_numpy(), ev["lat"].to_numpy())
    tree = cKDTree(np.c_[ex, ey])
    yrs = ev["year"].to_numpy()

    feats = json.loads((CACHE / "grw_solar_2018plus.geojson").read_text())["features"]
    tz = pd.read_csv(RES / "g1_tzsam_crosscheck.csv")
    ok = set(tz.loc[(tz["verdict"] == "consistent") & (tz["overlap"] >= 0.5), "fid"].astype(int))
    nw = set(pd.read_csv(RES / "pilot_nw_panel.csv")["fid"].astype(int))
    rows = []
    for f in feats:
        p = f["properties"]
        fid = int(p["fid"])
        if fid not in ok:
            continue
        c = shape(f["geometry"]).centroid
        x, y = tf.transform(c.x, c.y)
        r0 = np.sqrt(p["area"] / np.pi)
        g = int(p["construction_year"])
        rec = {"fid": fid, "g": g, "area": p["area"], "nw_pilot": fid in nw, "lon": c.x, "lat": c.y}
        for rad in RADII:
            idx = tree.query_ball_point([x, y], r0 + rad)
            yy = yrs[idx]
            rec[f"pre_{rad // 1000}km"] = int(((yy >= g - WIN) & (yy <= g - 1)).sum())
            rec[f"post_{rad // 1000}km"] = int(((yy >= g + 1) & (yy <= g + WIN)).sum())
        rows.append(rec)
    d = pd.DataFrame(rows)
    d.to_csv(RES / "bird_coverage_near_pv.csv", index=False)

    def summ(sub: pd.DataFrame, label: str) -> dict:
        out = {"set": label, "n_sites": len(sub)}
        for rad in (2, 5):
            pre, post = sub[f"pre_{rad}km"], sub[f"post_{rad}km"]
            out[f"both>=1_{rad}km"] = round(((pre >= 1) & (post >= 1)).mean() * 100, 1)
            out[f"both>=5_{rad}km"] = round(((pre >= 5) & (post >= 5)).mean() * 100, 1)
            out[f"n_both>=5_{rad}km"] = int(((pre >= 5) & (post >= 5)).sum())
            out[f"median_pre_{rad}km"] = float(pre.median())
        return out

    post_ok = d[d["g"] <= 2021]  # 建后需有 3 年观测 / need 3 post years (data to 2024)
    s = pd.DataFrame([summ(post_ok, "national, built 2018-2021"),
                      summ(post_ok[post_ok["nw_pilot"]], "NW pilot, built 2019-2021"),
                      summ(post_ok[post_ok["area"] >= 1e6], "national, >100 ha")])
    s.to_csv(RES / "bird_coverage_near_pv_summary.csv", index=False)
    logger.info("\n%s", s.T.to_string())

    # 对照候选点:以 2017-2019 对 2021-2023 / candidates: 2017-2019 vs 2021-2023
    c = pd.read_csv(CACHE / "nw_candidates.csv")
    cx, cy = tf.transform(c["lon"].to_numpy(), c["lat"].to_numpy())
    for rad in RADII:
        cnt_pre = np.array([((yrs[i] >= 2017) & (yrs[i] <= 2019)).sum() for i in tree.query_ball_point(np.c_[cx, cy], 300 + rad)])
        cnt_post = np.array([((yrs[i] >= 2021) & (yrs[i] <= 2023)).sum() for i in tree.query_ball_point(np.c_[cx, cy], 300 + rad)])
        logger.info("NW candidates %s km: both>=1 %.1f%%, both>=5 %.1f%%", rad // 1000,
                    100 * ((cnt_pre >= 1) & (cnt_post >= 1)).mean(), 100 * ((cnt_pre >= 5) & (cnt_post >= 5)).mean())


if __name__ == "__main__":
    main()
