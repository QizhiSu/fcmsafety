# 毒性等级 I–V 落地：规则映射、多值取最严，以及顺带修掉的三个 SML/IARC 数据 bug

## 背景

`inst/toxicity_levels.png` 里的规则表一直没被实现——之前 `Toxic_level` 只是 `R/globals.R` 里一个 `globalVariables()` 的残留字符串，全仓库（含 git 历史）搜不到任何计算逻辑。同时，规则要用的输入数据有几处对不上：

- `assign_toxicity()` 对同一物质有多行时一律 `match()` 取数据库首行，等于看行序；
- `eu_sml_group` 表按 `group_no` 组织，但查询函数按 `InChIKey` 过滤。

## 规则映射

| 等级 | 触发条件 | 依据标签 |
|---|---|---|
| V | SVHC 命中 | `SVHC` |
| V | `CMR_H_codes` 含 H340 / H350 / H360 | `CMR:H360` |
| V | EDC 命中 | `EDC` |
| V | IARC 1 组 | `IARC:1` |
| V | SML ≤ 0.018 | `SML:0.018(EU)` |
| IV | `CMR_H_codes` 含 H341 / H351 / H361 | `CMR:H341` |
| IV | `CMR_suspect` 命中 | `CMR_suspect` |
| IV | IARC 2A / 2B | `IARC:2A` |
| IV | 0.018 < SML ≤ 0.09 | |
| IV | Cramer III（Toxtree `High (Class III)`） | `Cramer:III` |
| III | 0.09 < SML ≤ 0.54 · Cramer II | |
| II | 0.54 < SML ≤ 1.8 · Cramer I | |
| I | 1.8 < SML ≤ 60 | |

多条件命中取**最严**（等级数字最大）；`Toxic_level_basis` 只列最严那一档命中的规则，其余证据仍在 `SVHC` / `CMR` / `EDC` / `IARC` / `EU_SML` / `China_SML` / `Cramer_rules` 各列里。

## Decision

1. **新增两列**：`Toxic_level`（"I".."V"）与 `Toxic_level_basis`（依据）。放在毒性列块末尾——前面是证据，这两列是结论。`relocate()` 的范围同步从 `Cramer_rules:China_SML` 扩到 `Cramer_rules:Toxic_level_basis`，否则新列会被落在表格最末尾。
2. **完全没有证据的行留空**（渲染成 `"-"`），不冒充 I 级。等级 I 在规则表里唯一的来源是 `1.8 < SML ≤ 60`；没有任何证据的行不属于任何等级。运行日志单独报 `Not assigned (no evidence): N`。
3. **China SML 参与定级**，与 EU SML 取更严的那个，依据里写明来源（`SML:0.05(China)` / `(EU)` / `(EU+China)`）。两边单位实测都是 mg/kg（`china_sml.unit` 1182 行全是 `mg/kg`），可直接比较。
4. **`CMR_suspect` 按 IV 级参与**。`screen_clp()` 筛这张表的条件就是"含 H341/H351/H361"，正是规则表 IV 类的定义；依据标签写作 `CMR_suspect` 以便与正式 `cmr` 区分。
5. **同一物质多行时取最严**：SML 取最小值（EU 与中国一起比），IARC 取最严分组（1 > 2A > 2B > 3，未知分组排最后）。
6. **SML > 60 归入 I 级**。规则表上界就是 60，且现库中 EU 最大恰为 60、中国最大 48，该分支取不到，写下来只是为了不留未定义行为。

## 顺带修掉的三个 bug（都有实测证据）

### ① `eu_sml_group` 按 InChIKey 查永远返回 0 行

`eu_sml_group` 共 38 行，**`InChIKey` 全部为 NULL**——这张表实际按 `group_no` 组织，`substance_name` 列里存的是组内物质编号清单（如组 32 = "8, 72, 73, 138, …"）。而 `query_eu_sml_group_data()` 用的是 `WHERE InChIKey IN (...)`。

影响面不小：`eu_sml` 里 **126 行带组号，其中 118 行的个体 SML 是空的**，也就是这 118 个物质本该靠组限值定级，此前全部拿不到值。改法是整表取回（只有 38 行）再按 `group_no` 关联。

### ② 输出里凭空出现字面字符串 `NA*`

接上一条：组限值查不到 → `EU_SML` 是 NA，但代码随后无条件执行 `paste0(EU_SML, "*")` 给组限值打星号，`paste0(NA, "*")` 得到字符串 `"NA*"`。它是个字符串，所以最后的 `result_data[is.na(result_data)] <- "-"` 也接不住。

实测会走到这条路径的物质：`HBGGXOJOCNVPFY-UHFFFAOYSA-N`、`ZVFDTKUVRCTHQE-UHFFFAOYSA-N`（`sml_group` 是脏值 `"26\r\n                     32"`）、`CZLMRJZAHXYRIX-UHFFFAOYSA-N`（脏值 `"15\r\n                     30"`）。改法是先判 `!is.na(EU_SML)` 再打星号。

### ③ `match()` 取首行 = 结果看数据库行序

| 表 | 多行键 | 其中数值/分组真有差异的 |
|---|---|---|
| `iarc` | 28 | **11 个键分组冲突**（`2B,1` / `1,3` / `3,2B` / `2A,3` …） |
| `china_sml` | 209 | **4 个键两行数值不同**（0.05 vs 5.0、0.6 vs 3.0、0.01 vs 0.05） |
| `eu_sml` | 9 | 0（数值都相同） |

IARC 那 11 个键里，只要首行是 `3`，`na_if("3")` 就会把它变成 NA，**整条 IARC 证据凭空消失**。现在按最严取值。

### 附带

`eu_sml.sml_group` 的脏值（源表格两个单元格被读成一格，如 `"26\r\n                     32"`）用 `extract_group_nos()` 一律按数字拆分，一个格子可以给出多个组号，取其中更严的组限值。

## Considered Options

- **无证据留空（选定）** vs 一律给 I 级：后者表格没有空值、便于排序，但把"查不到数据"和"确有 1.8–60 的 SML"混成同一级。选了前者。
- **China SML 参与定级（选定）** vs 只算 EU：规则表只写 "SML" 未注来源；对中国用户 GB 9685 更相关，且单位一致。
- **`CMR_suspect` 按 IV 参与（选定）** vs 只保留标志位：suspect 表数据质量低于 `cmr` 表（只有物质名、无 H 码列），但它的筛选规则恰好就是 IV 类的定义。
- **三列各自出一个等级**（EU / China / Cramer 分开）：最透明，但输出列变多，且不一致时仍需人工裁决。否决。
- **把 Cramer 预测与法规证据区分开定级**：规则表把两者并列，本实现照表执行，但依据标签里标明来源（`Cramer:III` vs `SML:…`）以便复审。真要做成两套等级，需要先改规则表。未做。

## Consequences

- `Toxic_level` 是**规则表的机械执行**，不是风险评估结论。Cramer III 能把一个纯预测出来的物质抬到 IV 级，这是规则表本身的设计。
- 组限值现在能取到了，但 `eu_sml_group.sml` 的数值本身来自源表格，**精度未经二次核验**；`summarise_eu_sml()` 只负责取最严。
- 新增的内部函数（`compute_toxicity_levels` / `summarise_iarc_groups` / `summarise_eu_sml` / `summarise_china_sml` / `parse_cramer_class` / `extract_group_nos` / `toxicity_tier_from_sml` / `strictest_sml`）全部不依赖数据库，可脱离 SQLite 单独测试。
- `R/globals.R` 里补了 `Toxic_level_basis`；`China_SML_group` 已成死字符串，留着未删。
