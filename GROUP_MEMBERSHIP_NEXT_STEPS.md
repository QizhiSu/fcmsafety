# IARC 组别归属判定 — 现状与演进建议

## 一、当前实现现状

### 1.1 功能概述

`assign_group_membership()` 是一个将**单一化学物质**映射到 **IARC 组别条目**（如 "Cadmium and cadmium compounds"、"Chromium (VI) compounds" 这类"一大类物质总称"）的判定函数。

核心策略是**"先粗筛、后精判"**：
1. 从输入物质提取元素组成（分子式/SMILES 解析）
2. 反查 IARC 注册表中**含相同元素**的组条目（通常只命中 1~3 个组，无需遍历 859 行全表）
3. 对候选组按分层规则判定置信度

### 1.2 输入输出

**输入**：多格式自适应，接受以下任一形式
- `CAS` 号（如 `"7440-43-9"`）
- `InChIKey`（如 `"WLZRMCYVCSSEQC-UHFFFAOYSA-N"`）
- `SMILES`（如 `"[O-][Cr](=O)(=O)[O-]"`）
- 分子式（如 `"CdCl2"`）
- 物质名称（offline 时报错；online 时查 PubChem）

**输出**：`data.frame`（长表），每行一个命中组

| 列名 | 说明 | 示例 |
|---|---|---|
| `matched_agent` | IARC 组条目名称 | "Cadmium and cadmium compounds" |
| `iarc_group` | IARC 致癌分级 | "1" / "2A" / "2B" / "3" |
| `layer` | 判定层级 | element / valence / form / scenario |
| `element_hits` | 命中元素 | "Cd" |
| `evidence` | 判定依据 | "元素层命中: Cd" |
| `confidence` | 置信度 | auto_confirmed / probable / manual_review |
| `source_detail` | 输入溯源 | "formula→CdCl2" |

### 1.3 分层判定规则

| 层级 | 判定方式 | 典型例子 | 置信度 |
|---|---|---|---|
| **element** | 分子式含该元素即命中 | CdCl2 → Cadmium 组 | `auto_confirmed` |
| **valence** | SMILES 文本特征匹配价态 | 铬酸根 `[Cr](=O)` → Cr(VI) | `probable` |
| **form** | 结构无法区分形态，需人工 | Silica crystalline vs amorphous | `manual_review` |
| **scenario** | 纯结构不可判，需暴露信息 | 酒中乙醇、PUVA、衰变产物 | `manual_review` |

### 1.4 特殊处理规则

- **负向条件**：输入含 `W`（钨）时自动剔除 "Cobalt metal without tungsten carbide" 组
- **有机金属误报降级**：输入同时含 `C`（有机骨架）和金属元素时，置信度从 `auto_confirmed` 压到 `manual_review`（如二甲基镉 `C[Cd]C` 命中 Cadmium 组但标需人工复核）
- **SMILES 双字符元素优先**：解析时先匹配 `Cl`、`Br`、`Si`、`Se`、`As`、`Co` 等双字符符号，避免 `Co`（钴）被拆成 `C` + `o`

### 1.5 注册表机制

- **动态生成**：每次运行时从 `iarc` 表自动筛出组条目（24 条），解析元素、分层、SMILES 特征
- **可覆盖**：若存在 `inst/extdata/iarc_group_manual.xlsx`，则用其中规则覆盖动态生成的默认值
- **自动跟进**：IARC 数据更新后，新增组条目会自动出现在注册表中（layer 按名称规则初判）

### 1.6 测试覆盖

37 个测试，全部通过：
- 分子式/SMILES/名称元素解析（含 Co vs CO 区分）
- 注册表自动分层验证
- 元素层命中（CdCl2）
- 价态层启发式命中（铬酸根）
- 零命中（乙醇）
- 有机金属降级（二甲基镉）
- 负向条件剔除
- 集成 end-to-end 测试

---

## 二、已知边界与局限

1. **价态判定覆盖不全**：Cr(VI) 只靠 `[O-][Cr](=O)(=O)[O-]` 等少数铬酸根特征串，无法覆盖铬酸盐聚合物、杂多酸等结构
2. **形态/结晶态不可区分**：Silica crystalline vs amorphous、Talc 含/不含石棉纤维等，SMILES 里没有结晶度信息
3. **场景层永远需人工**：酒中乙醇、PUVA 联合暴露、放射性衰变产物等，纯化学结构无法推断暴露场景
4. **Cyclamates 元素映射待确认**：当前以 `C`（碳）为特征元素，但环氨酸钠的母体结构是否适合用元素层粗筛需领域专家复核
5. **IARC 表仅 24 条组条目**：覆盖范围有限，未纳入 CMR raw 群组行、SVHC UVCB 条目等

---

## 三、优化方向

### 方向 1：扩展数据源（优先级高）

当前仅覆盖 IARC 表中的 24 条组条目。可扩展至：
- **CMR `cmr_raw` 无 InChIKey 行**：CLP 法规中的群组条目（如 "lead compounds"、"borates" 等），约 20~30 条
- **SVHC `svhc_raw` 无 InChIKey 行**：UVCB 物质、多 CAS 合并行（如 MCCP、PDDP）
- **EU SML `eu_sml_group` 表**：已有的 38 行组条目，可统一纳入同一框架

**实施建议**：为每个数据源写独立的 `query_*_group_registry()` 函数，但共用 `screen_groups()` 核心逻辑。

### 方向 2：引入 SMARTS 规则（优先级中）

当前用 SMILES **文本正则**匹配结构特征（如 `[O-][Cr](=O)`），鲁棒性有限。可升级为 SMARTS（结构查询语言）：
- 在 `iarc_group_manual.xlsx` 中新增 `smarts_pattern` 列
- 纯文本级存储，不引入新依赖
- 未来迁移到 rcdk/RDKit 时直接可用

**示例**：
- Cr(VI) 铬酸根：`[O-][Cr](=O)(=O)[O-]` 的 SMARTS 等价形式可更精确地匹配不同质子化状态
- Co(II) 盐：可写 `[Co+2]` 或配位形式

### 方向 3：引入 rcdk 做真·结构匹配（优先级中，需评估代价）

`rcdk` 已在 `DESCRIPTION` Imports 中（Java 依赖），当前仅 Shiny 可视化可选加载。若提升为硬依赖：
- 用 `rcdk::parse.smiles()` + `rcdk::matches()` 做 SMARTS 子结构匹配
- 可精确判定价态、配位环境、有机/无机骨架
- 支持分子指纹相似度（用于 UVCB 近似匹配）

**代价**：Java 运行时、R CMD check 环境配置复杂、跨平台维护成本上升。

### 方向 4：引入 RDKit via reticulate（优先级低，能力最强但门槛最高）

通过 `reticulate` 调用 Python RDKit：
- SMARTS/SMILES 解析能力业界最强
- 支持子结构匹配、价态推断、分子指纹、相似度搜索
- 可处理 UVCB "代表性结构" 的模糊匹配

**代价**：需 Python 环境 + RDKit 安装（conda/pip），对非程序员用户门槛高。

### 方向 5：输入补齐增强（优先级中）

当前 `online=TRUE` 时仅支持 CAS 查 PubChem。可扩展：
- **名称搜索**：PubChem PUG REST 按名称搜索（`https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/name/{name}/property/...`）
- **批量查询**：支持向量输入（一次查多个物质）
- **缓存机制**：查询结果写入本地 SQLite，避免重复联网

### 方向 6：人工标注表完善（优先级高，零成本）

当前 `iarc_group_manual.xlsx` 尚未创建。建议：
1. 导出当前动态注册表作为初始模板
2. 领域专家逐条复核 24 条组条目的 layer 和 rationale
3. 补充更多 SMILES 特征串（如不同质子化状态的铬酸根）
4. 记录"代表性物质"清单（每组选 3~5 个典型 CAS，用于验证）

---

## 四、推荐路线图

| 阶段 | 目标 | 依赖 |
|---|---|---|
| **当前** | IARC 24 条组条目，纯 R 启发式，37 个测试 | 零新依赖 |
| **短期** | 完善 `iarc_group_manual.xlsx`；扩展 SMARTS 规则列 | 零新依赖 |
| **中期** | 纳入 CMR raw / SVHC raw / EU SML 组条目 | 零新依赖 |
| **长期** | 评估 rcdk 引入，用真·结构匹配替代文本正则 | Java 运行时 |
| **远期** | 若需 UVCB 模糊匹配，评估 RDKit/reticulate | Python + RDKit |

---

## 五、待人工确认的数据点

以下组条目的元素映射和分层标注需领域专家复核：

| 组条目 | 当前映射元素 | 当前 layer | 待确认问题 |
|---|---|---|---|
| Cyclamates (sodium cyclamate) | C | element | 是否应以 C 为特征元素？环氨酸钠的钠盐是否应限定无机？ |
| Silica dust, crystalline... | Si | form | 结晶态检测有无其他结构线索？ |
| Talc not containing asbestos... | Mg | form | 滑石组是否应加 Si 元素？ |
| Hypochlorite salts | Cl | element | 次氯酸盐是否应限定为无机盐（排除有机氯）？ |
| Cobalt and cobalt compounds | Co | element | 与 "Cobalt metal without WC" 和 "Cobalt sulfate..." 的区分规则是否足够？ |
