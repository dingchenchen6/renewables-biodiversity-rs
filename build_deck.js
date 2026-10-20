// ============================================================
// Objective / 目标:
// Build the slide deck for the remote-sensing workflow on biodiversity
// impacts of China's wind and solar expansion.
// 生成"中国风光电扩张生物多样性影响:遥感事后评估工作流"幻灯片。
// Output / 输出: RS_Renewables_Biodiversity_Workflow.pptx
// ============================================================
const pptxgen = require("pptxgenjs");
const pres = new pptxgen();
pres.layout = "LAYOUT_WIDE"; // 13.33 x 7.5 in
pres.author = "Chenchen Ding";
pres.title = "中国风光电扩张的生物多样性影响:遥感事后评估工作流";

// 配色:深青(主)、日照琥珀(强调)、草地绿(辅)/ palette
const C = {
  deep: "0E2F3C", teal: "1F6F78", amber: "E9A23B", sage: "6FA67A",
  ink: "1D2A33", mute: "5E6B75", tint: "EEF4F4", tintA: "FBF1DF", white: "FFFFFF",
  red: "B5523B",
};
const F = "PingFang SC"; // 中文字体 / CJK font
const W = 13.333;

// 标题 / slide title
function title(s, t, sub) {
  s.addText(t, { x: 0.6, y: 0.35, w: W - 1.2, h: 0.75, fontFace: F, fontSize: 30, bold: true, color: C.deep, margin: 0 });
  if (sub) s.addText(sub, { x: 0.6, y: 1.05, w: W - 1.2, h: 0.4, fontFace: F, fontSize: 14, color: C.mute, margin: 0 });
}
// 页脚页码 / page number
function pageNo(s, n) {
  s.addText(String(n), { x: W - 1.0, y: 7.0, w: 0.5, h: 0.3, fontFace: "Arial", fontSize: 10, color: C.mute, align: "right", margin: 0 });
}
// 带底色卡片 / tinted card with header
function card(s, x, y, w, h, head, body, opt = {}) {
  s.addShape(pres.shapes.ROUNDED_RECTANGLE, { x, y, w, h, fill: { color: opt.fill || C.tint }, line: { color: opt.fill || C.tint }, rectRadius: 0.08 });
  s.addText(head, { x: x + 0.2, y: y + 0.15, w: w - 0.4, h: 0.45, fontFace: F, fontSize: opt.hs || 17, bold: true, color: opt.hc || C.deep, margin: 0 });
  s.addText(body, { x: x + 0.2, y: y + 0.65, w: w - 0.4, h: h - 0.8, fontFace: F, fontSize: opt.bs || 13, color: C.ink, valign: "top", margin: 0, paraSpaceAfter: 4 });
}
// 圆形编号 / numbered circle motif
function badge(s, x, y, label, color) {
  s.addShape(pres.shapes.OVAL, { x, y, w: 0.6, h: 0.6, fill: { color: color || C.amber }, line: { color: color || C.amber } });
  s.addText(label, { x, y, w: 0.6, h: 0.6, fontFace: "Arial", fontSize: 14, bold: true, color: C.white, align: "center", valign: "middle", margin: 0 });
}
const bul = (arr, size) => arr.map((t, i) => ({ text: t, options: { bullet: true, breakLine: i < arr.length - 1, fontSize: size || 14 } }));

let n = 0;

// ---- 1 封面 / cover ------------------------------------------------------
{
  const s = pres.addSlide(); s.background = { color: C.deep }; n++;
  // 太阳 + 风机意象:同心圆 / concentric rings motif
  [3.2, 2.4, 1.6].forEach((d, i) => s.addShape(pres.shapes.OVAL, { x: 10.4 - d / 2, y: 2.2 - d / 2, w: d, h: d, fill: { color: C.amber, transparency: 70 + i * 8 }, line: { color: C.amber, transparency: 60 } }));
  s.addText("研究工作流 · v0.1", { x: 0.7, y: 1.1, w: 6, h: 0.4, fontFace: F, fontSize: 16, color: C.amber, margin: 0 });
  s.addText("中国风光电扩张的生物多样性影响", { x: 0.7, y: 1.7, w: 9, h: 0.9, fontFace: F, fontSize: 40, bold: true, color: C.white, margin: 0 });
  s.addText("遥感事后评估:占用了什么、改变了什么、与规划预测是否一致", { x: 0.7, y: 2.6, w: 9.5, h: 0.6, fontFace: F, fontSize: 20, color: "CFE3E6", margin: 0 });
  s.addText("真实设施足迹 × 季度建设日期 × 晚建站点反事实 × 不依赖观测努力的结局", { x: 0.7, y: 4.4, w: 11, h: 0.5, fontFace: F, fontSize: 16, color: C.white, margin: 0 });
  s.addText("丁晨晨 · 北京大学 · 2026 年 10 月", { x: 0.7, y: 6.4, w: 8, h: 0.4, fontFace: F, fontSize: 14, color: "9FB8BE", margin: 0 });
}

// ---- 2 一句话判断 / verdict --------------------------------------------
{
  const s = pres.addSlide(); n++; title(s, "一句话判断", "可以做,而且时机好;关键在于换掉暴露、结局与反事实");
  s.addShape(pres.shapes.ROUNDED_RECTANGLE, { x: 0.6, y: 1.8, w: 5.6, h: 4.6, fill: { color: C.deep }, line: { color: C.deep }, rectRadius: 0.08 });
  s.addText("2017Q4 – 2024Q2", { x: 0.9, y: 2.2, w: 5.0, h: 0.8, fontFace: "Arial", fontSize: 34, bold: true, color: C.amber, margin: 0 });
  s.addText("Global Renewables Watch 为全球每个光伏电站和陆上风机标注了季度精度的建设日期", { x: 0.9, y: 3.1, w: 5.0, h: 1.2, fontFace: F, fontSize: 15, color: C.white, margin: 0 });
  s.addText("这让\"建设前后、早建与晚建\"的交错双重差分第一次可以在全国尺度上做", { x: 0.9, y: 4.5, w: 5.0, h: 1.2, fontFace: F, fontSize: 15, color: "CFE3E6", margin: 0 });
  const rows = [
    ["暴露", "真实设施足迹与建设日期,而非县级政策指数"],
    ["结局", "栖息地状况的遥感指标与物种暴露,不依赖观测努力"],
    ["反事实", "晚建站点与匹配的未建点,按资源潜力匹配"],
    ["检验对象", "NEE 论文\"全国集中\"与\"省级分散\"两种情景的预测"],
  ];
  rows.forEach((r, i) => {
    const y = 1.85 + i * 1.15;
    badge(s, 6.7, y + 0.15, String(i + 1), i % 2 ? C.teal : C.amber);
    s.addText(r[0], { x: 7.5, y, w: 5.2, h: 0.45, fontFace: F, fontSize: 17, bold: true, color: C.deep, margin: 0 });
    s.addText(r[1], { x: 7.5, y: y + 0.45, w: 5.2, h: 0.55, fontFace: F, fontSize: 14, color: C.ink, margin: 0 });
  });
  pageNo(s, n);
}

// ---- 3 定位 / positioning ------------------------------------------------
{
  const s = pres.addSlide(); n++; title(s, "定位:两篇论文各缺什么", "Science 有事后视角但测错了;NEE 有正确尺度但只是潜在冲突");
  const hdr = { fill: { color: C.deep }, color: C.white, bold: true, fontFace: F, fontSize: 14 };
  const c = (t, o = {}) => ({ text: t, options: Object.assign({ fontFace: F, fontSize: 13, color: C.ink }, o) });
  const rows = [
    [{ text: "", options: hdr }, { text: "Zhang 等 2026 · Science", options: hdr }, { text: "Zhou…Li 2026 · NEE", options: hdr }, { text: "本工作流", options: Object.assign({}, hdr, { fill: { color: C.amber }, color: C.deep }) }],
    [c("时间视角", { bold: true }), c("事后,2014–2023"), c("事前,情景至 2060"), c("事后,2017Q4–2024Q2,可回溯至 2010", { bold: true })],
    [c("暴露", { bold: true }), c("县级政策强度指数;县内不预测实际光伏面积"), c("规划情景下的潜在选址"), c("遥感识别的真实足迹 + 季度建设日期", { bold: true })],
    [c("结局", { bold: true }), c("县-月观鸟香农指数,由观测努力主导"), c("与物种分布和保护优先区的重叠"), c("栖息地状况遥感指标 + 物种暴露 + 结构化监测", { bold: true })],
    [c("尺度", { bold: true }), c("县"), c("全国栅格"), c("站点及外围距离环", { bold: true })],
    [c("因果识别", { bold: true }), c("双向固定效应,存在预趋势"), c("不涉及,情景比较"), c("交错 DiD,晚建站点作对照", { bold: true })],
  ];
  s.addTable(rows, { x: 0.6, y: 1.75, w: W - 1.2, colW: [1.7, 3.6, 3.3, 3.53], rowH: 0.78, border: { type: "solid", pt: 1, color: "D5DEE2" }, fill: { color: C.white }, valign: "middle" });
  s.addText("缺口:真实发生的扩张到底占用了什么、造成了什么,目前没有全国性的因果证据。", { x: 0.6, y: 6.6, w: W - 1.2, h: 0.4, fontFace: F, fontSize: 15, bold: true, color: C.teal, margin: 0 });
  pageNo(s, n);
}

// ---- 4 证据台账 / evidence ledger -----------------------------------------
{
  const s = pres.addSlide(); n++; title(s, "证据台账", "区分有来源支撑、推断与待核实三类,只在第一类上立论");
  card(s, 0.6, 1.75, 4.0, 5.0, "有来源支撑", bul([
    "NEE:集中选址与丰富度优先区冲突低 4–7%,重叠物种少 11–33%,但与功能多样性和干旱生物群区冲突更大",
    "GRW:光伏多边形 + 风机点位,季度建设日期,PlanetScope 4.7 米",
    "光伏遥感研究普遍报告\"变绿\",但受气候混杂",
    "青海荒漠野外:板下生物量增加,物种丰富度与香农指数下降",
    "内蒙古风电场:夜间增温、生产力下降",
  ], 12), { hc: C.teal });
  card(s, 4.75, 1.75, 4.0, 5.0, "推断", bul([
    "大型基地向荒漠、戈壁集中,而那里恰是公民科学数据最稀疏、NEE 预测功能多样性冲突最大的地方",
    "\"劣质绿化\"若存在,应表现为绿度上升、异质性下降;站点尺度可检验,县级 MODIS 不可检验",
  ], 13), { fill: C.tintA, hc: "8A5A12" });
  card(s, 8.9, 1.75, 3.83, 5.0, "待核实", bul([
    "GRW 在中国的要素数量与日期准确度",
    "是否已有用建设日期构建反事实的全国风电研究(本次检索未见)",
    "China BON-Birds 的获取途径",
    "NEE 情景空间图层能否获取",
  ], 13), { fill: "F3E7E4", hc: C.red });
  pageNo(s, n);
}

// ---- 5 核心问题与假说 / question & hypotheses -------------------------------
{
  const s = pres.addSlide(); n++; title(s, "核心问题与三个竞争假说");
  s.addShape(pres.shapes.ROUNDED_RECTANGLE, { x: 0.6, y: 1.3, w: W - 1.2, h: 1.3, fill: { color: C.deep }, line: { color: C.deep }, rectRadius: 0.08 });
  s.addText("2017–2024 年中国新建的地面光伏与陆上风电,占用了哪些生境与物种栖息地;建设是否因果性地降低了站点及外围的栖息地状况;代价格局更接近 NEE 的\"全国集中\"还是\"省级分散\"情景?",
    { x: 0.9, y: 1.4, w: W - 1.8, h: 1.1, fontFace: F, fontSize: 16, color: C.white, valign: "middle", margin: 0 });
  const hs = [
    ["H1 生境置换", "建设转换自然生境,站点及外围状况下降,效应随距离衰减", C.red],
    ["H2 中性或改善", "站点多在退化地、裸地或农田,遮荫增湿带来变绿而无生境损失", C.sage],
    ["H3 同质化", "绿度上升但结构与光谱异质性下降,生境质量损失被 NDVI 掩盖", C.amber],
  ];
  hs.forEach((h, i) => {
    const x = 0.6 + i * 4.1;
    s.addShape(pres.shapes.ROUNDED_RECTANGLE, { x, y: 3.0, w: 3.9, h: 2.6, fill: { color: C.tint }, line: { color: C.tint }, rectRadius: 0.08 });
    s.addShape(pres.shapes.OVAL, { x: x + 0.25, y: 3.25, w: 0.35, h: 0.35, fill: { color: h[2] }, line: { color: h[2] } });
    s.addText(h[0], { x: x + 0.75, y: 3.2, w: 3.0, h: 0.45, fontFace: F, fontSize: 18, bold: true, color: C.deep, margin: 0 });
    s.addText(h[1], { x: x + 0.25, y: 3.85, w: 3.45, h: 1.6, fontFace: F, fontSize: 14, color: C.ink, valign: "top", margin: 0 });
  });
  s.addText("挑战的默认假设:① 遥感变绿即生态改善;② 荒漠是低代价选址;③ 政策强度可以代表生态暴露", { x: 0.6, y: 6.0, w: W - 1.2, h: 0.5, fontFace: F, fontSize: 14, color: C.teal, bold: true, margin: 0 });
  pageNo(s, n);
}

// ---- 6 判别矩阵 / discriminating predictions ------------------------------
{
  const s = pres.addSlide(); n++; title(s, "关键判别:同时看绿度与异质性", "绿度上升不等于多样性上升,这是整条工作流最核心的洞见");
  const hdr = { fill: { color: C.teal }, color: C.white, bold: true, fontFace: F, fontSize: 14, align: "center" };
  const c = (t, col) => ({ text: t, options: { fontFace: F, fontSize: 15, bold: true, align: "center", color: col || C.ink } });
  const rows = [
    [{ text: "预测(自然生境站点,建设后)", options: Object.assign({}, hdr, { align: "left" }) }, { text: "绿度 NDVI / GPP", options: hdr }, { text: "异质性 Rao's Q / 纹理", options: hdr }, { text: "夜间地表温度", options: hdr }],
    [{ text: "H1 生境置换", options: { fontFace: F, fontSize: 15, bold: true } }, c("↓", C.red), c("↓", C.red), c("↑", C.red)],
    [{ text: "H2 中性或改善", options: { fontFace: F, fontSize: 15, bold: true } }, c("0 或 ↑", C.sage), c("0", C.sage), c("0", C.sage)],
    [{ text: "H3 同质化", options: { fontFace: F, fontSize: 15, bold: true } }, c("↑", "8A5A12"), c("↓", C.red), c("±", C.mute)],
  ];
  s.addTable(rows, { x: 0.6, y: 1.75, w: 8.0, colW: [2.9, 1.7, 1.9, 1.5], rowH: 0.75, border: { type: "solid", pt: 1, color: "D5DEE2" }, valign: "middle" });
  card(s, 8.95, 1.75, 3.78, 3.0, "地面证据", "青海荒漠野外调查:板下生物量增加,但物种丰富度与香农指数显著下降。遥感只看绿度会把 H3 误判为 H2。", { fill: C.tintA, hc: "8A5A12", bs: 14 });
  card(s, 0.6, 5.0, W - 1.2, 1.6, "证伪条件", "预趋势平坦、功效充足,而自然生境站点各项状况指标的建设后效应都与零无差异,则 H1 被否定;绿度上升处异质性并未下降,则 H3 被否定。", { bs: 14 });
  pageNo(s, n);
}

// ---- 7 工作流总览 / workflow overview --------------------------------------
{
  const s = pres.addSlide(); n++; title(s, "工作流总览:七个模块", "M0 → M6,每一步都有明确产出与决策门");
  const mods = [
    ["M0", "准备与治理", "GEE 注册\n数据协议\n预注册"],
    ["M1", "设施清单", "GRW + 中国光伏专题\n+ GEM 容量状态"],
    ["M2", "转换与暴露", "建设前生境\nAOH 物种暴露\n对照 NEE"],
    ["M3", "因果效应", "交错 DiD\n距离环\n晚建对照"],
    ["M4", "机制", "绿度 vs\n异质性"],
    ["M5", "生物学响应", "BON 样线\n努力校正占域"],
    ["M6", "综合", "真实 vs 规划\n荒漠专题"],
  ];
  const bw = 1.62, gap = 0.18;
  mods.forEach((m, i) => {
    const x = 0.6 + i * (bw + gap);
    const col = [C.mute, C.teal, C.teal, C.deep, C.deep, C.sage, C.amber][i];
    s.addShape(pres.shapes.ROUNDED_RECTANGLE, { x, y: 2.0, w: bw, h: 3.0, fill: { color: C.tint }, line: { color: C.tint }, rectRadius: 0.08 });
    s.addShape(pres.shapes.OVAL, { x: x + bw / 2 - 0.4, y: 2.2, w: 0.8, h: 0.8, fill: { color: col }, line: { color: col } });
    s.addText(m[0], { x: x + bw / 2 - 0.4, y: 2.2, w: 0.8, h: 0.8, fontFace: "Arial", fontSize: 16, bold: true, color: C.white, align: "center", valign: "middle", margin: 0 });
    s.addText(m[1], { x: x + 0.08, y: 3.15, w: bw - 0.16, h: 0.5, fontFace: F, fontSize: 15, bold: true, color: C.deep, align: "center", margin: 0 });
    s.addText(m[2], { x: x + 0.1, y: 3.75, w: bw - 0.2, h: 1.15, fontFace: F, fontSize: 12.5, color: C.ink, align: "center", valign: "top", margin: 0 });
    if (i < mods.length - 1) s.addShape(pres.shapes.RIGHT_ARROW, { x: x + bw + 0.01, y: 3.68, w: 0.16, h: 0.22, fill: { color: C.amber }, line: { color: C.amber } });
  });
  s.addText("备选快报:只做 M1 + M2(真实选址更像哪个情景),无需因果设计;M5 可独立成文,用正确方法回应 Science 的问题。", { x: 0.6, y: 5.4, w: W - 1.2, h: 0.6, fontFace: F, fontSize: 14, color: C.teal, bold: true, margin: 0 });
  pageNo(s, n);
}

// ---- 8 M1 + M2 ---------------------------------------------------------
{
  const s = pres.addSlide(); n++; title(s, "M1 设施清单 · M2 生境转换与物种暴露");
  card(s, 0.6, 1.4, 5.95, 5.3, "M1 设施清单与建设日期", bul([
    "主数据:GRW 光伏与风电,筛选 country_code = CHN,保留 construction_date",
    "补 2018 年前:中国 2015/2020 矢量光伏数据集的建设年份字段",
    "复合利用:2010–2022 农光互补矢量数据",
    "容量与状态:Global Energy Monitor 光伏/风电追踪库;\"已宣布未建\"站点是天然反事实候选",
    "交叉验证:Dunnett 等协调化全球风电场位置",
    "日期验证:随机抽 100–200 站,在 Sentinel-2 时序上目视判读建设季度",
    "产出:统一设施表(几何、类型、容量、建设季度及其不确定性)",
  ], 13), { hc: C.teal });
  card(s, 6.78, 1.4, 5.95, 5.3, "M2 转换与暴露核算", bul([
    "建设前生境:建设前一年年度土地覆盖(GLC_FCS30D;中国专题用 CLCD)",
    "转换核算:按生境类型、生物气候区、海拔带统计占用面积",
    "物种暴露:IUCN/BirdLife 范围 × 生境偏好 × 海拔构建 AOH;受威胁物种用 SDM",
    "按分布区大小、生境专化度、功能群汇总被占用比例",
    "与 NEE 对照:用同一组指标计算真实选址冲突,判断更接近哪个情景",
    "保护地:WDPA 对中国覆盖不全,需注明;有条件换用国内边界",
  ], 13), { hc: C.teal });
  pageNo(s, n);
}

// ---- 9 M3 因果设计 / causal design ----------------------------------------
{
  const s = pres.addSlide(); n++; title(s, "M3 栖息地状况的因果效应", "交错双重差分:站点足迹 + 外围距离环,晚建站点作对照");
  // 距离环示意 / distance-ring diagram
  const cx = 3.0, cy = 4.3;
  [[3.6, "2–5 km"], [2.7, "1–2 km"], [1.9, "0.5–1 km"], [1.1, "0–0.5 km"]].forEach((r, i) => {
    s.addShape(pres.shapes.OVAL, { x: cx - r[0] / 2, y: cy - r[0] / 2, w: r[0], h: r[0], fill: { color: C.teal, transparency: 85 - i * 15 }, line: { color: C.teal, width: 1 } });
  });
  s.addShape(pres.shapes.RECTANGLE, { x: cx - 0.3, y: cy - 0.22, w: 0.6, h: 0.44, fill: { color: C.amber }, line: { color: C.deep, width: 1 } });
  s.addText("站点", { x: cx - 0.5, y: cy - 0.2, w: 1.0, h: 0.4, fontFace: F, fontSize: 11, bold: true, color: C.deep, align: "center", margin: 0 });
  s.addText("0–0.5 · 0.5–1 · 1–2 · 2–5 km\n距离环作剂量-反应", { x: 0.6, y: 6.25, w: 4.8, h: 0.6, fontFace: F, fontSize: 12, color: C.mute, align: "center", margin: 0 });
  s.addText([
    { text: "结局", options: { bold: true, color: C.deep, fontSize: 15, breakLine: true } },
    { text: "绿度与生产力 NDVI、EVI、GPP;结构与异质性 Rao's Q、GLCM 纹理;昼夜地表温度、蒸散、土壤水分;景观破碎度", options: { fontSize: 13, breakLine: true } },
    { text: " ", options: { fontSize: 6, breakLine: true } },
    { text: "识别", options: { bold: true, color: C.deep, fontSize: 15, breakLine: true } },
    { text: "Callaway–Sant'Anna 与 Sun–Abraham 互证;晚建站点作未处理对照;另以 CEM 匹配从未建设的对照点", options: { fontSize: 13, breakLine: true } },
    { text: " ", options: { fontSize: 6, breakLine: true } },
    { text: "匹配变量", options: { bold: true, color: C.deep, fontSize: 15, breakLine: true } },
    { text: "光伏潜力或风速(选址按它挑)、坡度、海拔、距道路/电网/居民点、建设前土地覆盖与绿度趋势", options: { fontSize: 13, breakLine: true } },
    { text: " ", options: { fontSize: 6, breakLine: true } },
    { text: "稳健性", options: { bold: true, color: C.deep, fontSize: 15, breakLine: true } },
    { text: "事件研究预趋势;HonestDiD 敏感性;ERA5-Land 气候协变量;站点聚类或 Conley 空间标准误;按生境、干旱度、设施类型分层", options: { fontSize: 13 } },
  ], { x: 6.0, y: 1.7, w: 6.73, h: 5.1, fontFace: F, color: C.ink, valign: "top", margin: 0 });
  pageNo(s, n);
}

// ---- 10 M4 M5 M6 --------------------------------------------------------
{
  const s = pres.addSlide(); n++; title(s, "M4 机制 · M5 生物学响应 · M6 综合");
  card(s, 0.6, 1.4, 3.9, 5.3, "M4 变绿还是同质化", bul([
    "同一批站点并列估计绿度与异质性的事件研究,直接检验 H3",
    "光学遥感看不到不透明面板下的植被:板间与外围用遥感,板下用野外样方补充,并在文中正面写明",
  ], 13), { hc: "8A5A12", fill: C.tintA });
  card(s, 4.72, 1.4, 4.1, 5.3, "M5 生物学响应", bul([
    "China BON-Birds 样线邻近设施时做 BACI",
    "公民科学:只用完整清单,保留时长、距离、人数",
    "合并同日同点多人清单以去伪重复;\"计数全为 1\"清单剔除或单独建模",
    "多物种占域模型估计\"距离 × 建设后\"交互,不做县级聚合",
    "先做功效分析,写明最小可检测效应",
  ], 12.5), { hc: C.sage });
  card(s, 9.03, 1.4, 3.7, 5.3, "M6 综合", bul([
    "主图:真实选址冲突落在 NEE 两个情景之间的哪个位置",
    "荒漠专题:扩张最快处是否也是数据最空处",
    "政策含义:选址规则需同时约束丰富度与功能多样性、森林与干旱生物群区",
  ], 13), { hc: C.amber });
  pageNo(s, n);
}

// ---- 11 数据源 GEE / data sources (GEE) -----------------------------------
{
  const s = pres.addSlide(); n++; title(s, "数据源与获取 ①:Google Earth Engine", "目录中已核实存在;注册 GEE 项目后需在线复核");
  const hdr = { fill: { color: C.deep }, color: C.white, bold: true, fontFace: F, fontSize: 12 };
  const c = (t, o = {}) => ({ text: t, options: Object.assign({ fontFace: F, fontSize: 11, color: C.ink }, o) });
  const m = (t) => c(t, { fontFace: "Courier New", fontSize: 9.5 });
  const rows = [
    [{ text: "用途", options: hdr }, { text: "数据集", options: hdr }, { text: "GEE ID", options: hdr }],
    [c("设施与建设日期"), c("Global Renewables Watch v1"), m("projects/sat-io/open-datasets/GRW/SOLAR_V1 · …/GRW/WIND_V1")],
    [c("风电场交叉验证"), c("Dunnett 等协调化风电场"), m("projects/sat-io/open-datasets/global_wind_farms_2020")],
    [c("年度土地覆盖"), c("GLC_FCS30D 1985–2022,30 m"), m("projects/sat-io/open-datasets/GLC-FCS30D/annual")],
    [c("10 m 土地覆盖"), c("Dynamic World;ESA WorldCover;ESRI LULC"), m("GOOGLE/DYNAMICWORLD/V1 · ESA/WorldCover/v200")],
    [c("光学影像"), c("Sentinel-2 SR;Landsat 8/9 L2"), m("COPERNICUS/S2_SR_HARMONIZED · LANDSAT/LC09/C02/T1_L2")],
    [c("地表温度与植被"), c("MODIS LST 8 天;MODIS VI 16 天"), m("MODIS/061/MOD11A2 · MODIS/061/MOD13Q1")],
    [c("气候协变量"), c("ERA5-Land"), m("ECMWF/ERA5_LAND/HOURLY")],
    [c("资源潜力"), c("Global Solar Atlas PVOUT;Global Wind Atlas"), m("…/global_solar_atlas/pvout_LTAy_AvgDailyTotals · …/global_wind_atlas/wind-speed")],
    [c("保护地 / 人类改造"), c("WDPA;TNC Human Modification v3"), m("WCMC/WDPA/current/polygons · TNC/HM/v3/300m_c")],
    [c("冠层高度"), c("GLAD GEDI-Landsat"), m("projects/sat-io/open-datasets/GLAD/GEDI_V27")],
  ];
  s.addTable(rows, { x: 0.6, y: 1.65, w: W - 1.2, colW: [1.9, 3.8, 6.43], rowH: 0.44, border: { type: "solid", pt: 0.75, color: "D5DEE2" }, valign: "middle" });
  s.addText("MODIS GPP 与蒸散产品、ERA5-Land 月聚合版本本次未核验,使用前确认。", { x: 0.6, y: 6.75, w: W - 1.2, h: 0.35, fontFace: F, fontSize: 11, color: C.mute, margin: 0 });
  pageNo(s, n);
}

// ---- 12 数据源 非 GEE / data sources (non-GEE) ------------------------------
{
  const s = pres.addSlide(); n++; title(s, "数据源与获取 ②:GEE 之外", "部分需下载上传,部分需授权或合作");
  const hdr = { fill: { color: C.teal }, color: C.white, bold: true, fontFace: F, fontSize: 12 };
  const c = (t, o = {}) => ({ text: t, options: Object.assign({ fontFace: F, fontSize: 11, color: C.ink }, o) });
  const rows = [
    [{ text: "数据", options: hdr }, { text: "内容", options: hdr }, { text: "获取方式", options: hdr }],
    [c("中国光伏专题", { bold: true }), c("2015/2020 矢量(含建设年份);2020 年 10 m;CPVPD-2024;农光互补 2010–2022"), c("Scientific Data 等论文的数据仓库,公开下载")],
    [c("CLCD", { bold: true }), c("中国逐年 30 m 土地覆盖(Yang & Huang 2021)"), c("Zenodo 下载后上传为 GEE 资产;先核对最新版本")],
    [c("Global Energy Monitor", { bold: true }), c("光伏与风电站点,装机容量,宣布/在建/运行状态"), c("globalenergymonitor.org 申请下载,核对许可")],
    [c("物种分布", { bold: true }), c("IUCN 红色名录范围;BirdLife 鸟类范围;自有 SDM"), c("IUCN 空间数据下载页;BirdLife DataZone 申请;不得再分发")],
    [c("China BON-Birds", { bold: true }), c("31 省 338 样区,1,822 条样线、1,343 个样点"), c("无公开门户,需通过合作接洽")],
    [c("公民科学", { bold: true }), c("eBird EBD(含努力字段);中国观鸟记录中心记录级数据"), c("eBird 数据申请;观鸟中心按平台条款;不再分发")],
    [c("NEE 情景图层", { bold: true }), c("全国集中与省级分散两种选址情景"), c("查论文数据可用性声明,或联系作者组")],
  ];
  s.addTable(rows, { x: 0.6, y: 1.65, w: W - 1.2, colW: [2.3, 5.3, 4.53], rowH: 0.62, border: { type: "solid", pt: 0.75, color: "D5DEE2" }, valign: "middle" });
  pageNo(s, n);
}

// ---- 13 质量关口 / quality gates ------------------------------------------
{
  const s = pres.addSlide(); n++; title(s, "质量与证伪关口", "看结果之前写定;任一关口不过,按对策调整后重跑");
  const gs = [
    ["G1", "日期", "目视判读与 GRW 建设季度相差不超过两个季度的比例达到预设门槛(建议 85%)", "改用年度时间窗,剔除日期模糊站点"],
    ["G2", "设计", "事件研究预趋势联合检验不显著,HonestDiD 敏感性可接受", "重新匹配或缩小比较组"],
    ["G3", "功效", "主结局的最小可检测效应小于文献中的合理效应", "合并区域或延长时窗"],
    ["G4", "构念", "遥感状况指标与有地面数据处的多样性指标方向一致", "结局只表述为栖息地状况,不外推"],
  ];
  gs.forEach((g, i) => {
    const y = 1.7 + i * 1.25;
    badge(s, 0.6, y + 0.2, g[0], [C.teal, C.deep, C.sage, C.amber][i]);
    s.addText(g[1], { x: 1.4, y: y + 0.22, w: 1.2, h: 0.5, fontFace: F, fontSize: 18, bold: true, color: C.deep, margin: 0 });
    s.addShape(pres.shapes.ROUNDED_RECTANGLE, { x: 2.6, y, w: 6.2, h: 1.05, fill: { color: C.tint }, line: { color: C.tint }, rectRadius: 0.06 });
    s.addText(g[2], { x: 2.8, y, w: 5.85, h: 1.05, fontFace: F, fontSize: 13, color: C.ink, valign: "middle", margin: 0 });
    s.addShape(pres.shapes.ROUNDED_RECTANGLE, { x: 9.0, y, w: 3.73, h: 1.05, fill: { color: C.tintA }, line: { color: C.tintA }, rectRadius: 0.06 });
    s.addText("不通过:" + g[3], { x: 9.2, y, w: 3.4, h: 1.05, fontFace: F, fontSize: 12.5, color: "6B4A12", valign: "middle", margin: 0 });
  });
  pageNo(s, n);
}

// ---- 14 两周 pilot / pilot timeline ----------------------------------------
{
  const s = pres.addSlide(); n++; title(s, "两周 pilot 与决策门", "G1 与 G2 都通过才全国铺开");
  const steps = [
    ["第 1–2 天", "注册 GEE,跑起步脚本", "中国光伏与风电按季度、按 2018 土地覆盖的统计"],
    ["第 3–5 天", "抽 100 站目视验证建设季度", "G1 一致率"],
    ["第 6–9 天", "西北旱区约 200 个 2019–2021 光伏站 + 匹配晚建站", "绿度与异质性事件研究,预趋势图"],
    ["第 10–12 天", "内蒙古草原约 100 个风电场", "MODIS 夜间地表温度,风电可行性"],
    ["第 13–14 天", "写决策备忘录", "继续、调整或放弃"],
  ];
  s.addShape(pres.shapes.LINE, { x: 0.9, y: 2.35, w: 11.5, h: 0, line: { color: C.teal, width: 2.5 } });
  steps.forEach((st, i) => {
    const x = 0.6 + i * 2.48;
    s.addShape(pres.shapes.OVAL, { x: x + 0.15, y: 2.1, w: 0.5, h: 0.5, fill: { color: i === 4 ? C.amber : C.teal }, line: { color: C.white, width: 2 } });
    s.addText(st[0], { x, y: 1.5, w: 2.3, h: 0.45, fontFace: F, fontSize: 15, bold: true, color: C.deep, margin: 0 });
    s.addShape(pres.shapes.ROUNDED_RECTANGLE, { x, y: 2.9, w: 2.3, h: 3.4, fill: { color: i === 4 ? C.tintA : C.tint }, line: { color: i === 4 ? C.tintA : C.tint }, rectRadius: 0.06 });
    s.addText(st[1], { x: x + 0.15, y: 3.05, w: 2.0, h: 1.4, fontFace: F, fontSize: 14, bold: true, color: C.ink, valign: "top", margin: 0 });
    s.addText("产出:" + st[2], { x: x + 0.15, y: 4.55, w: 2.0, h: 1.6, fontFace: F, fontSize: 12.5, color: C.mute, valign: "top", margin: 0 });
  });
  pageNo(s, n);
}

// ---- 15 风险 / risks -----------------------------------------------------
{
  const s = pres.addSlide(); n++; title(s, "主要风险与对策");
  const hdr = { fill: { color: C.deep }, color: C.white, bold: true, fontFace: F, fontSize: 13 };
  const c = (t, o = {}) => ({ text: t, options: Object.assign({ fontFace: F, fontSize: 12.5, color: C.ink }, o) });
  const rows = [
    [{ text: "风险", options: hdr }, { text: "对策", options: hdr }],
    [c("遥感结局被当成多样性", { bold: true }), c("构念限定为栖息地状况与暴露;M5 做生物学验证")],
    [c("选址非随机", { bold: true }), c("晚建站点作对照;按资源潜力与可达性匹配;预趋势检验")],
    [c("GRW 日期是检出时间,不等于并网时间", { bold: true }), c("目视验证;与 GEM 投运年份比对;时间窗稳健性")],
    [c("板下植被不可见", { bold: true }), c("板间与外围用遥感,板下用野外数据;正面写入局限")],
    [c("风电的主要影响(撞击、回避)不在地表", { bold: true }), c("遥感只回答足迹与地表效应;撞击风险以迁徙通道叠加作扩展")],
    [c("公民科学的努力混杂", { bold: true }), c("完整清单、显式努力、去伪重复、占域模型、功效分析")],
  ];
  s.addTable(rows, { x: 0.6, y: 1.4, w: W - 1.2, colW: [4.6, 7.53], rowH: 0.72, border: { type: "solid", pt: 0.75, color: "D5DEE2" }, valign: "middle" });
  pageNo(s, n);
}

// ---- 16 文献 / references --------------------------------------------------
{
  const s = pres.addSlide(); n++; title(s, "相关文献");
  const refs = [
    "Zhou Z., Sun S., Zhang J., Fang Y., Li B.V. (2026) Comparing potential biodiversity conflicts from renewable energy expansion in China at different centralization levels. Nat. Ecol. Evol. 10:1316–1328.",
    "Zhang H. et al. (2026) China's solar expansion policy reduces bird diversity. Science 393:831–836.",
    "Robinson C. et al. (2025) Global Renewables Watch: a temporal dataset of solar and wind energy derived from satellite imagery. arXiv:2503.14860.",
    "Vectorized solar photovoltaic installation dataset across China in 2015 and 2020. Scientific Data (s41597-024-04356-z).",
    "A 10-m national-scale map of ground-mounted photovoltaic power stations in China of 2020. Scientific Data (2024).",
    "Satellites reveal spatial heterogeneity in dryland photovoltaic plants' effects on vegetation dynamics. Earth's Future (2024).",
    "Diverse vegetation responses to solar farm installation are also driven by climate change. Commun. Earth Environ. (2025).",
    "Large-scale photovoltaic farms significantly change the vegetation diversity and biomass through influencing soil moisture. Vadose Zone J. (2025).",
    "Wind farms increase land surface temperature and reduce vegetation productivity in the Inner Mongolia. Geography and Sustainability (2024).",
    "Callaway B., Sant'Anna P.H.C. (2021) J. Econometrics 225:200–230;Sun L., Abraham S. (2021) J. Econometrics 225:175–199;Rambachan A., Roth J. (2023) Rev. Econ. Stud. 90:2555–2591.",
    "Brooks T.M. et al. (2019) Measuring terrestrial Area of Habitat (AOH) and its utility for the IUCN Red List. Trends Ecol. Evol. 34:977–986.",
  ];
  s.addText(refs.map((r, i) => ({ text: `[${i + 1}] ` + r, options: { breakLine: i < refs.length - 1 } })),
    { x: 0.6, y: 1.3, w: W - 1.2, h: 5.6, fontFace: F, fontSize: 12.5, color: C.ink, valign: "top", paraSpaceAfter: 7, margin: 0 });
  pageNo(s, n);
}

// ---- 17 下一步 / next steps ------------------------------------------------
{
  const s = pres.addSlide(); s.background = { color: C.deep }; n++;
  s.addText("下一步", { x: 0.7, y: 0.8, w: 8, h: 0.8, fontFace: F, fontSize: 36, bold: true, color: C.white, margin: 0 });
  const ns = [
    "注册 GEE 非商业项目,跑通起步脚本,统计中国站点的季度分布",
    "抽样目视验证建设日期,判断 G1",
    "联系 NEE 作者组索取情景图层",
    "通过鸟类学合作网络接洽 China BON-Birds",
  ];
  ns.forEach((t, i) => {
    const y = 2.0 + i * 1.05;
    badge(s, 0.8, y, String(i + 1), C.amber);
    s.addText(t, { x: 1.7, y, w: 10.5, h: 0.6, fontFace: F, fontSize: 18, color: C.white, valign: "middle", margin: 0 });
  });
  s.addText("在线版本与数据源清单:dingchenchen6.github.io/renewables-biodiversity-rs", { x: 0.7, y: 6.5, w: 11, h: 0.4, fontFace: F, fontSize: 13, color: "9FB8BE", margin: 0 });
}

pres.writeFile({ fileName: "RS_Renewables_Biodiversity_Workflow.pptx" }).then(f => console.log("wrote", f));
