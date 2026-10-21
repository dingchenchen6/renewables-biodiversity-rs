# ============================================================
# Scientific question / 科学问题:
# Around PV sites with enough bird surveys, did checklists close to the
# site (0-3 km) change after construction relative to checklists farther
# away (5-10 km) at the same site and in the same year?
# 在观鸟数据足够的光伏站周边,建设后近区(0-3 km)清单相对同站同年远区(5-10 km)
# 是否发生变化?
#
# Objective / 分析目标: build the checklist-level dataset for a ring-BACI,
#   staggered difference-in-differences and a multispecies occupancy model.
#   为近远区对照、交错 DiD 与多物种占域模型构建清单级数据。
# Input data / 输入数据:
#   cache/birds/events_unique.csv.gz (CBRC + eBird/GBIF, merged and deduplicated)
#   cache/grw_solar_2018plus.geojson; cache/grw_china_centroids.csv (all GRW PV);
#   results/g1_tzsam_crosscheck.csv (date-consistent sites)
# Workflow / 主要流程:
#   1. Sites: GRW and TZ-SAM consistent, built 2018-2022
#   2. Each checklist is assigned to its nearest study site within 10 km of the
#      edge; zone near (0-3 km) or far (5-10 km); 3-5 km dropped
#   3. Far-zone checklists within 5 km of any other PV site are dropped
#   4. Event time e = year - g, window -4..3; e = 0 is the transition year
#   5. Eligible (data-rich) sites: >= MIN checklists in each zone x period cell
# Expected output / 预期输出:
#   cache/birds/site_checklists.csv.gz (record-level, not redistributed)
#   results/site_birds_eligibility.csv (aggregate)
# Key assumptions / 关键假设: distance to edge = centroid distance minus the
#   equal-area radius; a checklist location is its reported point.
#   到边缘的距离用质心距离减等面积半径近似;清单位置取其报告点位。
# Main packages / 主要包: pandas, numpy, scipy, pyproj, shapely
# ============================================================
import json
import logging
import os
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
# 几何设定可由环境变量覆盖,用于敏感性分析 / geometry overridable for sensitivity runs
NEAR = int(os.environ.get("SB_NEAR", 3000))
FAR_IN = int(os.environ.get("SB_FAR_IN", 5000))
FAR_OUT = int(os.environ.get("SB_FAR_OUT", 10000))
OTHER_PV = int(os.environ.get("SB_OTHER_PV", 5000))
G_MAX = int(os.environ.get("SB_G_MAX", 2022))
TAG = os.environ.get("SB_TAG", "")
E_MIN, E_MAX = -4, 3


def main() -> None:
    """Build checklist-level data and site eligibility / 构建清单数据与站点资格."""
    tf = Transformer.from_crs("EPSG:4326", ALBERS, always_xy=True)
    tz = pd.read_csv(RES / "g1_tzsam_crosscheck.csv")
    ok = set(tz.loc[(tz["verdict"] == "consistent") & (tz["overlap"] >= 0.5), "fid"].astype(int))
    rows = []
    for f in json.loads((CACHE / "grw_solar_2018plus.geojson").read_text())["features"]:
        p = f["properties"]
        if int(p["fid"]) in ok and 2018 <= int(p["construction_year"]) <= G_MAX:
            c = shape(f["geometry"]).centroid
            rows.append((int(p["fid"]), int(p["construction_year"]), p["area"], c.x, c.y))
    s = pd.DataFrame(rows, columns=["fid", "g", "area", "lon", "lat"])
    s["x"], s["y"] = tf.transform(s["lon"].to_numpy(), s["lat"].to_numpy())
    s["r"] = np.sqrt(s["area"] / np.pi)
    logger.info("study sites: %s", len(s))

    ev = pd.read_csv(CACHE / "birds" / "events_unique.csv.gz").dropna(subset=["lon", "lat", "year"])
    ev["x"], ev["y"] = tf.transform(ev["lon"].to_numpy(), ev["lat"].to_numpy())
    # 最近研究站点(按到边缘距离)/ nearest study site by edge distance
    st = cKDTree(s[["x", "y"]].to_numpy())
    dist, idx = st.query(ev[["x", "y"]].to_numpy(), k=5, distance_upper_bound=FAR_OUT + 6000)
    edge = np.where(np.isfinite(dist), dist - s["r"].to_numpy()[np.clip(idx, 0, len(s) - 1)], np.inf)
    best = edge.argmin(axis=1)
    ev["edge_m"] = edge[np.arange(len(ev)), best]
    ev["site"] = np.where(np.isfinite(ev["edge_m"]), s["fid"].to_numpy()[np.clip(idx[np.arange(len(ev)), best], 0, len(s) - 1)], -1)
    ev = ev[ev["edge_m"] <= FAR_OUT].copy()
    ev["zone"] = np.select([ev["edge_m"] <= NEAR, ev["edge_m"] >= FAR_IN], ["near", "far"], "buffer")
    ev = ev[ev["zone"] != "buffer"]
    # 远区剔除距其他任何光伏站 < 5 km 的清单 / drop far checklists near other PV
    allpv = pd.read_csv(CACHE / "grw_china_centroids.csv")
    ax, ay = tf.transform(allpv["lon"].to_numpy(), allpv["lat"].to_numpy())
    at = cKDTree(np.c_[ax, ay]); ar = np.sqrt(allpv["area"].to_numpy() / np.pi)
    d2, i2 = at.query(ev[["x", "y"]].to_numpy(), k=3)
    own_x = ev["site"].map(s.set_index("fid")["x"]).to_numpy()
    own_y = ev["site"].map(s.set_index("fid")["y"]).to_numpy()
    other = np.ones(len(ev), dtype=bool)
    for k in range(3):
        same = (np.abs(ax[i2[:, k]] - own_x) < 1) & (np.abs(ay[i2[:, k]] - own_y) < 1)
        other &= same | ((d2[:, k] - ar[i2[:, k]]) > OTHER_PV)
    ev = ev[(ev["zone"] == "near") | other].copy()
    ev = ev.merge(s[["fid", "g"]].rename(columns={"fid": "site"}), on="site")
    ev["e"] = ev["year"] - ev["g"]
    ev = ev[(ev["e"] >= E_MIN) & (ev["e"] <= E_MAX)].copy()
    ev["period"] = np.select([ev["e"] < 0, ev["e"] > 0], ["pre", "post"], "transition")
    ev.drop(columns=["x", "y"]).to_csv(CACHE / "birds" / f"site_checklists{TAG}.csv.gz", index=False)

    cnt = ev[ev["period"] != "transition"].pivot_table(index="site", columns=["zone", "period"], values="event_id",
                                                        aggfunc="count", fill_value=0)
    cnt.columns = [f"{z}_{p}" for z, p in cnt.columns]
    for c in ["near_pre", "near_post", "far_pre", "far_post"]:
        if c not in cnt:
            cnt[c] = 0
    cnt["min_cell"] = cnt[["near_pre", "near_post", "far_pre", "far_post"]].min(axis=1)
    cnt = cnt.reset_index().merge(s[["fid", "g", "area"]].rename(columns={"fid": "site"}), on="site")
    cnt.to_csv(RES / f"site_birds_eligibility{TAG}.csv", index=False)
    for m in (3, 5, 10, 20):
        e = cnt[cnt["min_cell"] >= m]
        logger.info("sites with >= %s checklists in every zone x period cell: %s (cohorts %s)", m, len(e),
                    e["g"].value_counts().sort_index().to_dict())
    logger.info("checklists kept (all sites): %s", len(ev))


if __name__ == "__main__":
    main()
