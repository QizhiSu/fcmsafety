# 报告导出、失败可见化，以及组条目为什么先不接入定级

对应卡点 4（导出只有 CSV）、卡点 6（静默失败）、卡点 5（`assign_group_membership_table()` 未接入 `assign_toxicity()`）。

## 背景

用户对包的总体要求是：除了"用 R 代码导入检出数据"这一步手动，从匹配法规、跑 Toxtree 做 Cramer 分级到导出结果 Excel 全自动。前三个卡点已解决，剩下三个：

- **卡点 4**：`output_file` 走的是 `utils::write.csv`，从头到尾没有 Excel 输出。用户的原话要求是"导出结果的 Excel 表格"。
- **卡点 6**：每个 `query_*()` helper 都是 `tryCatch` 后 `message()` 一句警告、返回空，调用方把"空"当"没命中"，最终的 `result_data[is.na(result_data)] <- "-"` 再把空白统一渲染成 `-`。数据库出问题时，用户拿到的是一张格式完整、看起来"这些物质都干净"的表。
- **卡点 5**：`assign_group_membership_table()` 9/9 已修好并导出，但没有任何调用方。精确 InChIKey 匹配看不见"镉及镉化合物""氯化石蜡"这类族条目。

## Decision 1：`.xlsx` 走多表工作簿，`.csv` 保持原样

按扩展名分派：`.xlsx` → `export_toxicity_report()`（新导出函数），`.csv` → 原来的 `utils::write.csv`。这样既有 Excel 交付物，又不打断任何既有脚本。

工作簿四张表：

| 表 | 内容 | 为什么单独一张 |
|---|---|---|
| Results | 全表 | 冻结首行 + 自动筛选 + 列宽拟合；`Toxic_level` 按等级着色 |
| Summary | 等级分布、各库命中数、运行元信息 | 让人一眼看到"结论是什么、依据哪个库" |
| Unassigned | 只含 `Toxic_level == "-"` 的行 | 这批是需要人工跟进的，混在主表里 500 行不好挑 |
| Issues | 缺表 / 查挂 / CMR 无 H 码 / 组条目待复核 | 卡点 6 的落脚点 |

**着色用 V 深红 → I 绿**：与国内"涨红跌绿"的直觉一致（红=危险）。未定级的 `-` 用中性灰，**不**用绿色——否则"没查到证据"会被读成"安全"。

**加 `openxlsx` 依赖**：`writexl` 装起来更轻，但没有任何样式能力（无列宽、无冻结、无筛选），做出来的 xlsx 和 CSV 相比没有优势。`openxlsx` 4.2.9 有 Windows 二进制包，实测 19 秒装完。

## Decision 2：数据库路径与时间戳写进 Summary

`get_db_connection()` 的判断是 `dir.exists(file.path(getwd(), "inst"))` → 用 `inst/fcmsafety.db`，否则用 `tools::R_user_dir("fcmsafety", "data")`。也就是说**同一份代码从不同目录跑，可能读的是两个不同的库**，而且没有任何提示。加 `.resolve_db_path()` 把这条规则显式化，并把实际路径、文件大小、mtime 写进 Summary 的 Run 段。没有改 `get_db_connection()` 的选择逻辑，只是让它复用同一个函数，避免两处漂移。

## Decision 3：失败要带出调用栈，不能只 `message()`

改法：查询失败时把错误信息挂在返回对象上（`attr(x, "query_error")`），由 `assign_toxicity()` 收集成 `query_issues`，再：
- 在控制台单独打印一段 `‼️ N database quer(ies) FAILED`，逐条列出来源与原始错误；
- 挂到 `attr(result, "query_issues")`；
- 写进 Issues 表。

**另外区分"表不存在/表为空"与"查不到"**：前者说明库根本没建好（`check_database_status()` 的 `table_counts` 里是 `NA` 或 `0`），必须显式提示。后者是正常结果。之前两者在用户眼里长得一样。

**没有加的**：原本想"所有库都查不到就 `stop()`"，放弃了——测试夹具里只建 `chemicals` 一张表，那样会直接把测试打断；而且 `check_database_status()` 在 Step 2 已经拦了"库没初始化"。改成一条 `warning()`，强度够、不误伤。

## Decision 4：组条目接入，但默认关闭

`assign_toxicity(group_membership = TRUE)` 会调 `assign_group_membership_table()`，按 `input_index` 汇总成 `Group_hits` / `Group_IARC` / `Group_review` 三列，其中 IARC 组命中参与定级（依据标成 `IARC(group):1` 以区分来源），CMR / SVHC 的 UVCB 命中只记录不升级（它们本身不带危险码，升级等于编造证据）。只用 `auto_confirmed` / `probable` 两档置信度定级，`manual_review` 一律进 `Group_review`。

**默认 `FALSE`。** 理由是实测下来它现在还不能开——见下。

### 为什么先不开：两个实测问题

**问题 1（已修）：批量调用在 ≥100 行时崩溃。**
`assign_group_membership_table()` 用 `do.call(rbind, …)` 合并各来源结果，但 IARC 分支产 `matched_agent / layer / element_hits`，CMR 与 SVHC 分支产 `matched_entry / category / keyword_hits`，列名不一致 → `names do not match previous names`。

实测（220 行输入，逐档试）：

| 输入行数 | 结果 |
|---|---|
| 5 / 20 / 50 | OK |
| 100 | **ERR: names do not match previous names** |
| 220 | **ERR** |

小样本之所以能过，是因为恰好只命中一类来源；行数一多两类同时出现就炸。修法：新增 `.rbind_fill()`，合并前按列名并集补齐（缺列填 `NA`），`assign_group_membership_table()` 与 `.screen_identity()` 都用它。修后 220 行正常返回。修完顺带把 IARC 命中的 `matched_entry` / `name` 也填上（原来恒为 `NA`），长表里"条目叫什么"从此只有一个取法。

**问题 2（未修，需要决策）：元素层粗筛对有机族条目完全失效。**

修完崩溃后再跑 220 行，命中的条目排名：

| 条目 | 命中行数 |
|---|---|
| **Cyclamates (sodium cyclamate)** | **196 / 220** |
| Silica dust, crystalline, in the form of quartz or cristobalite | 5 |
| Chromium (III) compounds | 4 |
| Chromium (VI) compounds | 4 |
| Chromium, metallic | 4 |
| Cadmium and cadmium compounds | 2 |

`Cyclamates (sodium cyclamate)` 命中 89% 的输入，`evidence` 是 `元素命中: c`——因为组条目是按"特征元素"粗筛的，而这条有机族条目从甜蜜素的 SMILES/Formula 抽出来的特征元素里有 `C`，于是**任何含碳的化合物都能命中它**，置信度还给了 `auto_confirmed`。

对比之下铬、镉、二氧化硅这几条的命中数是合理的。所以问题不是整个匹配逻辑坏了，而是**元素层对"特征元素里有 C/H/O/N"的族条目没有区分力**。这一层本来是给 `Cadmium and cadmium compounds` 这种金属族设计的。

全库跑一遍（2446 行输入，`group_membership = TRUE`）把量级坐实：

| 指标 | 数值 |
|---|---|
| 有组条目命中的行 | **2309 / 2446（94%）** |
| 其中带 IARC 分组的 | 2233 |
| `manual_review`（只记录不定级） | 158 |
| 无法解析身份、整行跳过 | 13 |
| **等级因此改变的行** | **5**（3 行 `-`→IV，2 行 IV→V） |

也就是说：开了之后 94% 的行会被贴上一条组条目，而其中绝大多数是同一个"含碳就中"的假阳性；真正影响等级的只有 5 行。信噪比完全不可用。

## Considered Options

- **导出用 writexl / 自己拼 OOXML**（被否决）：writexl 无样式；自己拼 xlsx 是几百行容易出错的 zip + XML 代码，比不上一个成熟依赖。
- **导出成单表 CSV + 多写几个文件**（被否决）：用户要的是"一个 Excel"，不是一个文件夹。
- **所有库查询失败就 `stop()`**（被否决）：见 Decision 3，会误伤只建了部分表的测试夹具。
- **组条目定级默认 `TRUE`**（被否决）：实测 89% 的假阳性会大面积污染等级。默认关闭、需要时显式打开，先把"能不能用"交给用户判断。
- **顺手把元素层改成"排除 C/H/O/N"**（**本轮不做**）：这会改变一个已经测试过的匹配函数的行为，属于语义变更，需要先确认口径（见下）。

## Consequences

- `openxlsx` 成为硬依赖（`Imports`）。多了一个依赖，换来可交付的 Excel。
- `output_file` 的行为变成"按扩展名分派"。传 `.xlsx` 与传 `.csv` 的结果表内容一致，只是前者多了样表与分表。
- 数据库可读性问题的可见性大幅提高：缺表、空表、查询失败、CMR 无 H 码、组条目待复核，全部进 Issues 表并写进 `attr()`。
- **卡点 5 仍未闭合**。接线、汇总、置信度过滤、定级联通都做好了并有测试覆盖，但默认关闭，等元素层误报问题有结论后再决定是否默认开启。

## 待决策（已由 ADR 0009 结案，此处保留当时的三个候选）

元素层对有机族条目的误报，当时列了三种口径：

1. **排除 C/H/O/N**：特征元素只保留金属/非有机骨架元素（Cd、Cr、Ni、Si、Cl 等）。改动最小，能直接干掉甜蜜素这条；风险是某些以卤素/硫为特征的族条目行为会变。
2. **保留但降置信**：只要特征元素里含 C/H/O/N 就强制降到 `manual_review`，不参与定级、只进 `Group_review`。不动匹配范围，只动"能不能用来定级"。
3. **按条目标注**：给注册表加一列人工确认的"可用元素层判定"标记，逐条维护。

**三个都不对，最终采用的是第四种口径**，见
[ADR 0009](0009-20260910-iarc-group-element-judge.md)。其中方案 1 尤其不成立：
它排 C/H/O/N，而 `Silica = "Si"`、`Talc = "Mg"` 都不在 C/H/O/N 里，最需要拦的
两条一条都拦不住。

同时纠正本节上方 Decision 4 的一处误判：当时把"94% 的行被贴标签"当成主要问题，
并推断 `Silica dust, crystalline` 的 58 行会造成误升级。**后者不成立** ——
该条目 `layer = "form"`，置信度是 `manual_review`，本来就不参与定级。当时的审计
只复刻了元素层，没有复刻 `layer` 判定与置信度降级，因此高估了严重性。真正让组条目
失效的是另一处（`screen_iarc_groups()` 里"有机骨架"判断写成了含氧含氢），详见 ADR 0009。
