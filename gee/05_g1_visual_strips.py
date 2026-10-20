# ============================================================
# Scientific question / 科学问题:
# For sites where the automatic break disagrees with GRW, which
# date is right? (visual check for gate G1)
# 自动断点与 GRW 不一致的站点,哪个日期是对的?(G1 目视复核)
#
# Objective / 分析目标:
# Build one strip of quarterly Sentinel-2 true-colour thumbnails per
# site (GRW quarter -6 ... +4, step 2) with the GRW polygon outlined.
# 为每站生成哨兵-2 季度真彩色缩略图条带(GRW 季度 -6 至 +4,每隔 2 季),
# 叠加 GRW 多边形轮廓,供目视判读面板首次出现的季度。
#
# Input data / 输入数据: results/g1_breaks_seasonal.csv; GRW; S2 SR
# Expected output / 预期输出: results/g1_visual/strip_<fid>.png, contact sheets
# Main packages / 主要包: earthengine-api, requests, Pillow
# ============================================================
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
ROOT = Path(__file__).resolve().parents[1]
OUT = ROOT / "results" / "g1_visual"
OFFSETS = [-6, -4, -2, 0, 2, 4]
PX = 160
PROXIES = {"https": "socks5h://127.0.0.1:7897", "http": "socks5h://127.0.0.1:7897"}


def quarter_rgb(geom: ee.Geometry, start: ee.Date) -> ee.Image:
    """Cloud-masked quarterly median true colour / 去云季度中位数真彩色."""
    col = (ee.ImageCollection("COPERNICUS/S2_SR_HARMONIZED").filterBounds(geom)
           .filterDate(start, start.advance(3, "month")))
    def mask(img):
        scl = img.select("SCL")
        return img.updateMask(scl.eq(4).Or(scl.eq(5)).Or(scl.eq(6)).Or(scl.eq(7)))
    return col.map(mask).median().visualize(bands=["B4", "B3", "B2"], min=0, max=3000)


def strip(site: ee.Feature, props: dict) -> Image.Image:
    """Thumbnail strip for one site / 单站缩略图条带."""
    geom = site.geometry()
    region = geom.bounds().buffer(150).bounds()
    base = ee.Date.fromYMD(int(props["construction_year"]), (int(props["construction_quarter"]) - 1) * 3 + 1, 1)
    outline = ee.Image().byte().paint(ee.FeatureCollection([site]), 1, 2).visualize(palette=["FFD400"])
    tiles = []
    for off in OFFSETS:
        img = quarter_rgb(geom, base.advance(off * 3, "month")).blend(outline)
        url = img.getThumbURL({"region": region, "dimensions": PX, "format": "png"})
        r = requests.get(url, proxies=PROXIES, timeout=120)
        r.raise_for_status()
        tiles.append(Image.open(io.BytesIO(r.content)).convert("RGB").resize((PX, PX)))
    canvas = Image.new("RGB", (PX * len(OFFSETS), PX + 18), "white")
    d = ImageDraw.Draw(canvas)
    for i, (off, t) in enumerate(zip(OFFSETS, tiles)):
        canvas.paste(t, (i * PX, 18))
        d.text((i * PX + 4, 3), f"{off:+d}", fill=(0, 0, 0))
    d.text((PX * len(OFFSETS) - 150, 3), f"fid {props['fid']} auto {int(props['break_offset']):+d}", fill=(180, 0, 0))
    return canvas


def main() -> None:
    """Strips for disagreeing sites, then contact sheets / 生成条带与总览图."""
    ee.Initialize(project=PROJECT)
    b = pd.read_csv(ROOT / "results" / "g1_breaks_seasonal.csv")
    meta = pd.read_csv(ROOT / "results" / "g1_sample_quarterly.csv")[["fid", "construction_year", "construction_quarter"]].drop_duplicates("fid")
    todo = b[~b["break_offset"].between(-2, 2)].drop(columns="construction_year").merge(meta, on="fid")
    OUT.mkdir(parents=True, exist_ok=True)
    grw = ee.FeatureCollection("projects/sat-io/open-datasets/GRW/SOLAR_V1")
    strips = []
    for _, p in todo.iterrows():
        site = ee.Feature(grw.filter(ee.Filter.eq("fid", int(p["fid"]))).first())
        try:
            s = strip(site, p.to_dict())
        except (requests.RequestException, ee.EEException) as e:
            logger.error("fid %s failed: %s", p["fid"], e)
            continue
        s.save(OUT / f"strip_{int(p['fid'])}.png")
        strips.append(s)
        logger.info("fid %s done (%s/%s)", p["fid"], len(strips), len(todo))
    for k in range(0, len(strips), 8):
        group = strips[k:k + 8]
        sheet = Image.new("RGB", (group[0].width, sum(g.height for g in group)), "white")
        y = 0
        for g in group:
            sheet.paste(g, (0, y))
            y += g.height
        sheet.save(OUT / f"sheet_{k // 8 + 1}.png")


if __name__ == "__main__":
    main()
