# 0009 - IARC 组条目的元素判据

- 状态：已采纳
- 日期：2026-09-10
- 相关：[0008](0008-20260910-report-export-and-failure-visibility.md)（组条目接入定级，默认关闭）、[0007](0007-20260910-toxicity-levels-and-sml-aggregation.md)（毒性等级 I–V）

## 背景

`assign_group_membership_table()` 用来回答"输入物质是否属于 IARC 的某个**组条目**"，
例如：

- `Cadmium and cadmium compounds`（组 1）
- `Arsenic and inorganic arsenic compounds`（组 1）×`Arsenobetaine and other organic arsenic compounds`（组 3）
- `Silica dust, crystalline, in the form of quartz or cristobalite`（组 1）
- `Cyclamates (sodium cyclamate)`（组 3）

24 条组条目全部靠 `parse_agent_elements()` 从条目名抽出"特征元素"，再由
`screen_iarc_groups()` 做元素交集 —— 输入含该元素即命中。ADR 0008 记录了这个
匹配器上线时发现的假阳性，并留下三个候选口径待决策。本轮把 24 条逐条审计后，
结论是三个候选都不适用，并且真正的问题在别处。

## 判据的适用范围

元素层做的是**存在性检验**（"这个物质含不含这个元素"）。它作为**必要条件**成立：
镉化合物必然含镉，这是镉的定义。但代码把它当**充分条件**用 —— 只要元素有交集
就给 `auto_confirmed`。

对金属族这没问题：Cd / Be / Ra / Th / Ni / As / Se 在普通有机分子里不出现，
"含砷"对判断砷化合物是有意义的。对有机族完全不成立：**有机族的定义是结构，
不可能是元素**。

## Decision 1：骨架元素不再作为特征元素

`parse_agent_elements()` 的映射表里有两条自证的错误映射：

```r
Talc = "Mg", Cyclamate = "C"   # 注释写的是"Cyclamate 以 C 为母体元素"
```

注释本身就是问题：碳是一切有机物的骨架，含碳不能说明是甜蜜素。实测后果是
**2210 / 2446 行（90%）被贴上 `Cyclamates` 标签**。两条映射删除。

甜蜜素与滑石改由 Decision 4 的 `layer = "manual"` 标记为"当前无自动判据"。

## Decision 2：骨架元素护栏

新增 `.skeletal_elements`（C/H/O/N/S/P、卤素、Na/K/Ca/Mg/Si/B，小写）。
`screen_iarc_groups()` 里，若命中的特征元素落在这张表里，`auto_confirmed`
一律压到 `manual_review`。

这是一道**数据驱动的护栏**，不是逐条人工标注：以后谁再往映射表里塞
`Xxx = "C"`，机制会自动兜住，不需要有人记得这件事。

当前它不改变任何结果（唯一受影响的 `Silica` 已经是 `manual_review`），
纯防御性。

`As`/`Se`/`Cd`/`Cr`/`Co`/`Hg`/`Ni` **不在**表内 —— 它们在普通有机物里不出现。

## Decision 3（本轮核心）：有机骨架判断只看碳

`screen_iarc_groups()` 里判断"输入是否含有机骨架"的代码是：

```r
organic_els <- c("C", "H", "O", "N")
has_organic <- any(tolower(input_els) %in% tolower(organic_els))
```

`any()` 是"任一命中"，而**氧和氢几乎出现在所有化合物里**。于是：

| 物质 | 元素 | 改前置信度 | 应该 |
|---|---|---|---|
| 砷酸 `O[As](=O)(O)O` | O, H, As | `manual_review` | 无机砷，`auto_confirmed` |
| 硫酸镉 `CdSO4` | Cd, S, O | `manual_review` | 无机镉，`auto_confirmed` |
| 氧化镍 `NiO` | Ni, O | `manual_review` | 无机镍，`auto_confirmed` |
| 氧化钴 `[O-2].[Co+2]` | Co, O | `manual_review` | 无机钴，`auto_confirmed` |
| 砷化镓 `[Ga]#[As]` | Ga, As | `auto_confirmed` | ✓ 侥幸通过 |

**只有不含 O / H 的物种幸存**（砷化镓、卤化物、金属单质），组条目层等于废掉 ——
这就是 ADR 0008 里"开了 group_membership 等级只变 5 行"的真正原因，而不是当时
推断的"元素层误报太多"。

改为只看碳。碳才是有机金属络合物与无机盐的分界：`CdCl2`、`CdSO4`、`CdO`
都是无机镉；`C[Cd]C`（二甲基镉）才是需要人工确认的有机金属。

## Decision 4：限定词靠准入条件落地，无判据的条目标 manual

条目名里的限定词，元素层看不见。24 条里此前只有 3 条补了精判
（`Chromium (III)` / `(VI)` 用价态正则，`Cobalt metal without tungsten carbide`
用负向条件）。

新增两个**元素级准入条件**字段：

- `negative_condition`：含这些元素就不属于本条（已有字段，本轮首次用于限定词）
- `require_condition`：必须含这些元素才属于本条（新增）

| 条目 | 条件 | 理由 |
|---|---|---|
| `Arsenic and inorganic arsenic compounds`（1） | 排除 `C` | 无机砷（砷酸、五氧化二砷、砷酸钙/铅/镍）都不含碳；有机砷（三乙基砷酸酯、甲基胂酸、二甲基胂酸）都含碳 |
| `Arsenobetaine and other organic arsenic compounds`（3） | 要求 `C` | 与上一条互为正反面。此前砷单质、砷化镓、砷酸盐同时挂在组 1 和组 3 两条下 |
| `Mercury and inorganic mercury compounds`（3） | 排除 `C` | 同砷 |
| `Silica dust, crystalline, …`（1） | 排除 `C` | 硅氧烷、硅烷偶联剂、含硅农药都是有机硅；无机硅（二氧化硅、硅酸盐、氟硅酸盐）一律不含碳 |

同时，`elements` 为空的条目（甜蜜素、糖精、次氮基三乙酸、次氯酸盐、滑石）
在注册表里标成 **`layer = "manual"`**，`screen_iarc_groups()` 直接跳过。
**宁可不报，也不靠错误判据硬报。** 这 5 条是将来接关键词层（复用
`BACKBONE_KEYWORDS` + `screen_uvcb_groups`）时的待办清单。

`Silica` 排除碳之后仍停在 `manual_review`：SMILES 无法区分石英/方石英（结晶）
与硅石（无定形），两者写法相同。取最严是安全侧。

## 实测

| 指标 | 改前 | 改后 |
|---|---|---|
| 组条目命中行数 | 2482 | **200** |
| 涉及物质数（全库 2446） | 2308 | 约 110 |
| 其中可参与定级（`auto_confirmed`/`probable`）的组 1/2B 命中 | 35 | **50** |
| `Cyclamates` 假阳性 | 2210 | **0** |
| `Silica` 命中 | 58（含环硅氧烷、硅烷偶联剂、含硅农药） | 23（全为无机硅酸盐） |
| `Arsenobetaine` 命中无机砷 | 7 | **0** |

端到端（全库 2446 行，`group_membership` 开 / 关对比）等级变化 5 行，**这 5 行
全部是新增的正确证据**：

| 物质 | 变化 | 依据 |
|---|---|---|
| `CdF6Si` 氟硅酸镉 | IV → V | 镉化合物，IARC 1 |
| `CdI2` 碘化镉 | IV → V | 镉化合物，IARC 1 |
| `CoO` 氧化钴 | `-` → IV | 钴化合物，IARC 2B |
| `Al2CoO4` 铝酸钴 | `-` → IV | 钴化合物，IARC 2B |
| `CoNiTiZn+10` | `-` → IV | 钴化合物，IARC 2B |

这 5 行改前都因含 O/I/F 而被误降级，拿不到任何组条目证据。

另有 95 行进入 `Group_review`（需人工复核），主要是 `Nickel, metallic`（27）、
`Silica`（23）、`Chromium, metallic`（22）、`Chromium (III)`（20）。

## Considered Options

**ADR 0008 的三个候选均被否：**

1. **排除 C/H/O/N** —— 不成立。`Silica = "Si"`、`Talc = "Mg"` 都不在 C/H/O/N 里，
   最需要拦的两条一条都拦不住。已作废。
2. **保留但降置信** —— 只解决噪音，不解决 Decision 3 那个让组条目失效的 bug。
   作为 Decision 2 的护栏被吸收。
3. **逐条人工标注** —— 24 条里真正需要精修的只有 6 条，全量人工维护性价比低。
   现有的 `inst/extdata/iarc_group_manual.xlsx` 覆盖机制保留，但不全量维护。

**为什么不用阈值法**（"命中特征元素数 / 组特征元素总数 > 阈值"）：甜蜜素只有 1 个
特征元素，金属族通常也只有 1 个，比例恒为 1，这个维度没有信息量。

**为什么不用结构相似性**：需要引入指纹/子结构匹配库，为 24 条条目引入一个
重依赖不划算；`negative_condition` / `require_condition` 已覆盖当前的实际情况。

## Consequences

- 组条目从"几乎无法参与定级"变为可用。`assign_toxicity(group_membership = TRUE)`
  现在能新增 5 行正确判定。
- **默认仍是 `FALSE`**，理由不变：本次改动尚未在真实筛查流程中长跑，
  且 95 行的 `Group_review` 提示需要人工消化。
- 5 条 `layer = "manual"` 的条目是**已知漏报**（首先是
  `Nitrilotriacetic acid and its salts`，IARC 2B）。当前库里这 5 条对应 0 个
  物质，属于机制缺口而非现实错误。
- `form` 层里 `Nickel, metallic`（27 行）、`Chromium, metallic`（22 行）、
  `Cobalt metal without tungsten carbide`（13 行）没有 `smiles_pattern`，
  因此恒为 `manual_review`。这三条其实可判 —— 金属态是裸金属单质（`[Ni]`、
  `[Cr]`），盐类有配体，一条正则即可分开。属后续增强。

## 教训：诊断工具的保真度决定结论的正确性

本轮最初的审计用 Python 复刻了 `parse_agent_elements()` /
`parse_formula_elements()` / `parse_smiles_elements()`，**但没有复刻
`layer` 判定与置信度降级**，于是得出"`Silica` 58 行误升 V 级、砷 4 行误升组 1"
的结论 —— 两者都不成立，那些命中本来就是 `manual_review`，不参与定级。

旁路复刻一旦与真实实现有偏差，偏差会直接变成错误结论，而且看起来同样
言之凿凿。此后审计一律改用**真实函数** —— `tools/audit_group_hits.R` 直接调用
`assign_group_membership_table()`，不做任何旁路复刻。Python 版
`tools/diagnose_group_element_layer.py` 保留，仅用于快速查看"每条条目的特征
元素是什么"，**其命中数与严重性判断不可信**，文件头部已标注这一点。
