# 中国风光电扩张的生物多样性影响:遥感事后评估工作流

**在线页面**:https://dingchenchen6.github.io/renewables-biodiversity-rs/

研究工作流 v0.2(2026 年 10 月),丁晨晨,北京大学。

以遥感识别的真实设施足迹与季度建设日期为暴露,以不依赖观测努力的栖息地状况指标与物种暴露为结局,以晚建站点为反事实,评估中国地面光伏与陆上风电扩张的生物多样性代价,并检验 Zhou et al. (2026, *Nat. Ecol. Evol.*) 的选址情景预测。

| 文件 | 内容 |
|---|---|
| `index.html` | 在线页面:定位、证据台账、核心问题、七个模块、数据源与获取、质量关口、两周 pilot、GEE 起步脚本、文献 |
| `RS_Renewables_Biodiversity_Workflow.pptx` / `.pdf` | 24 页幻灯片 |
| `build_deck.js` | 幻灯片生成脚本(pptxgenjs) |
| `gee/01_preconstruction_landcover.py` | 2018 年后新建光伏的建设前一年 30 m 土地覆盖(GLC_FCS30D) |
| `gee/02_g1_date_check.py` | G1:抽 100 站取哨兵-2 季度反射率序列 |
| `gee/03_g1_seasonal_break.py` | G1:季节哑变量 + 阶跃模型定位断点,与 GRW 季度比对 |
| `gee/04_clcd_preconstruction.py` | 用 CLCD v1.0.5(1985–2025)复算光伏与风机的建设前覆盖;设 `CLCD_DIR` 读本地文件,远程读取会被 Zenodo 限流 |
| `gee/05_g1_visual_strips.py` | G1 目视复核:不一致站点的哨兵-2 季度真彩色序列 |
| `gee/06_g1_tzsam_crosscheck.py` · `gee/07_g1_tzsam_summary.py` | G1 外部核对:GRW 与 TZ-SAM 按最大重叠匹配,比较建设日期 |
| `gee/08_wind_preconstruction_glc.py` | 风机的建设前一年 30 m 覆盖 |
| `gee/09_pilot_nw_panel.py` | 西北旱区试点:365 站 × 2016–2024 年 Landsat 生长季 NDVI 均值与标准差(站内、60–500 m 外围环) |
| `gee/10_compare_clcd_glc.py` | CLCD 与 GLC_FCS30D 建设前覆盖对比 |
| `analysis/pilot_nw_eventstudy.R` · `analysis/pilot_nw_sensitivity.R` | 交错 DiD(Callaway–Sant'Anna、Sun–Abraham)与敏感性(预期期、省份条件化) |
| `gee/11_matched_controls_panel.py` · `gee/11c_parallel_ndvi_panel.py` | 匹配设计:本地生成"从未建设"候选点,提取匹配变量与 2010–2024 年 NDVI 面板 |
| `gee/12_wind_forest_check.py` | 森林风机:逐台比较 CLCD 与 GLC,分层抽样哨兵-2 盲判定标 |
| `analysis/pilot_nw_matched.R` | 匹配的从未建设对照 + 留出预趋势检验 + HonestDiD(`ANTIC=1` 为主设定) |
| `analysis/bird_coverage_near_pv.py` · `analysis/site_birds_build.py` · `analysis/site_birds_did.R` | 站点周边观鸟覆盖;近远区对照交错 DiD(校正努力)与多物种占域模型 |
| `analysis/site_birds_abundance.R` | 丰度型多样性(Science 指标):清洗、努力校正与覆盖度标准化 Hill 数 |
| `analysis/grid_birds_pv.R` | 100 km 网格检测校正多样性的匹配对照(背景参照) |
| `results/` | 上述脚本输出(站点级与汇总表、目视图集 `g1_visual/`;GRW、TZ-SAM、GLC_FCS30D 的派生量) |

GEE 数据集 ID 已于 2026-10-20 在项目 renewable-conservation 中在线实测。Python 脚本运行:`HTTPS_PROXY=socks5h://127.0.0.1:7897 python gee/01_preconstruction_landcover.py`(国内网络需代理;需 earthengine-api、pandas、pysocks)。CLCD v1.0.5(Yang & Huang,Zenodo doi:10.5281/zenodo.18180184,CC BY 4.0)需先下载 2017–2023 年全国幅,再设 `CLCD_DIR` 运行 `gee/04`。派生数据遵循各源数据许可:GRW(CC BY 4.0)、GLC_FCS30D、TZ-SAM(CC BY-NC 4.0,`results/g1_tzsam_*` 仅限非商业使用)。`results/g1_visual/` 图集含经修改的 Copernicus Sentinel 数据(2018–2024)。

文字与图示:CC BY 4.0;代码:MIT。
