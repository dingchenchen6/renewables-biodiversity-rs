# 中国风光电扩张的生物多样性影响:遥感事后评估工作流

**在线页面**:https://dingchenchen6.github.io/renewables-biodiversity-rs/

研究工作流 v0.2(2026 年 10 月),丁晨晨,北京大学。

以遥感识别的真实设施足迹与季度建设日期为暴露,以不依赖观测努力的栖息地状况指标与物种暴露为结局,以晚建站点为反事实,评估中国地面光伏与陆上风电扩张的生物多样性代价,并检验 Zhou et al. (2026, *Nat. Ecol. Evol.*) 的选址情景预测。

| 文件 | 内容 |
|---|---|
| `index.html` | 在线页面:定位、证据台账、核心问题、七个模块、数据源与获取、质量关口、两周 pilot、GEE 起步脚本、文献 |
| `RS_Renewables_Biodiversity_Workflow.pptx` / `.pdf` | 20 页幻灯片 |
| `build_deck.js` | 幻灯片生成脚本(pptxgenjs) |
| `gee/01_preconstruction_landcover.py` | 2018 年后新建光伏的建设前一年 30 m 土地覆盖(GLC_FCS30D) |
| `gee/02_g1_date_check.py` | G1:抽 100 站取哨兵-2 季度反射率序列 |
| `gee/03_g1_seasonal_break.py` | G1:季节哑变量 + 阶跃模型定位断点,与 GRW 季度比对 |
| `results/` | 上述脚本输出(站点级与汇总表;GRW 与 GLC_FCS30D 的派生量) |

GEE 数据集 ID 已于 2026-10-20 在项目 renewable-conservation 中在线实测。Python 脚本运行:`HTTPS_PROXY=socks5h://127.0.0.1:7897 python gee/01_preconstruction_landcover.py`(国内网络需代理;需 earthengine-api、pandas、pysocks)。派生数据遵循 GRW(CC BY 4.0)与 GLC_FCS30D 的原始许可。

文字与图示:CC BY 4.0;代码:MIT。
