# CMR 定级证据：把 H 码读出来，并明确 suspect 表的角色

## 背景

毒性等级 I–V 的规则表（`inst/toxicity_levels.png`）在 CMR 这一项上不是看"是否命中清单"，而是看**具体携带哪个危险说明代码**：

- 等级 V：H340 / H350 / H360（致癌、致突变、生殖毒性 1A 与 1B 类）
- 等级 IV：H341 / H351 / H361（对应 2 类）

但 `assign_toxicity()` 原先对 `cmr` 表只输出 `CMR = "Y"`，`cmr.hazard_statement_codes` 这一列从没被读过。等于定级所需的关键证据在流程里丢失了，只剩一个布尔标志。

## Decision

1. `assign_toxicity()` 新增输出列 **`CMR_H_codes`**：从 `cmr.hazard_statement_codes` 取出与定级有关的 H 码，按"V 类在前"排序、`"; "` 连接；无命中给 `"-"`。原有的 `CMR` / `CMR_suspect` 两个标志位语义不变。
2. H 码一律**按前 4 位归并**后再比对，不匹配完整字符串。
3. 同一 `InChIKey` 在 `cmr` 表里的多行（同族条目）先合并 H 码文本再提取，不让 `match()` 只取第一行。
4. `cmr_suspect` 表**不加** H 码列。该表只存了名称，其 `CMR_suspect = "Y"` 本身就是"携带 H341/H351/H361"的证据（由 `screen_clp()` 的筛选规则保证），定级时按 IV 类处理。
5. 顺带修正 `fetch_cmr_data()` / `fetch_cmr_suspect_data()`：H 码筛选原先只在 `download` 分支执行，现在两条路径一律筛选。

## 关键依据（2026-09-10 对 `inst/fcmsafety.db` 实测）

**归并是必需的，不是防御性写法**。`cmr.hazard_statement_codes` 共出现 73 个不同的码，其中只有 6 个与定级有关，且全部带后缀：

| 基础码 | 库内出现的形式 |
|---|---|
| H350 | `H350`、`H350i`（吸入途径） |
| H360 | `H360`、`H360D`、`H360D ***`、`H360Df`、`H360F`、`H360F ***`、`H360FD`、`H360Fd` |
| H361 | `H361d ***`、`H361f`、`H361f ***`、`H361fd` |
| H340 / H341 / H351 | 无后缀 |

`***` 是特定浓度限值标记，不是码的一部分。其余 67 个码（H302、H372 `**`、H400…）与 CMR 定级无关，必须排除。

**同键多行确实存在**：`cmr` 表 332 行 / 325 个唯一 InChIKey，7 个键各占 2 行（如 `lead powder` 与 `lead massive`、`lead diazide` 与 `lead diazide [≥20% phlegmatiser]`）。逐行核对后这 7 组的 CMR 相关码完全一致，但按"合并再提取"实现以免将来数据变化时静默漏码。

**`cmr_suspect` 表没有码列，且缺口不小**：`cmr_suspect` 356 个唯一键中，**267 个不在 `cmr` 表里**，也就是说这 267 个物质的 H 码在整库范围内查不到——尽管源文件 `inst/clp_cmr_meta.xlsx` 的 `cmr_suspect` 工作表里是有的（449 行全部含 H341/H351/H361），只是写入时因为目标表没有对应列而被丢弃。

**不变量成立**：全量跑 325 个 `cmr` 键，**每一个都携带 V 类码，0 例外**；`CMR = "Y"` 与 `CMR_H_codes != "-"` 完全等价。88 个键同时携带 IV 类码（如二丁基硼酸氢锡同时是 Carc./Repr. 与 Muta. 2）。两条规则叠加时取更严的 V 级，与规则表一致。

## Considered Options

- **只加 `CMR_H_codes`，不动 suspect 表（选定）**：改动局限在读取侧，无 schema 变更、无需重灌数据，风险最低。定级靠 `CMR`→V、`CMR_suspect`→IV 两条既有标志就能走通。代价：suspect 行看不到码，审计时只能看到标志位。
- **给 `cmr_suspect` 加 H 码列并回灌**：需要 `ALTER TABLE` + 从 `clp_cmr_meta.xlsx` 回填 356 行的数据迁移，还要同步改建表语句与更新流水线。收益是审计时能直接看到 H341/H351/H361，对定级结果没有影响。判定为"可选增强"，未做。
- **给 `cmr_suspect` 再加一张码表**：多一张表、多一次 join，收益与上一项相同，复杂度更高。否决。

## Consequences

- 定级逻辑（后续实现）不需要再依赖"cmr 表内容一定被 `screen_clp()` 筛过"这个隐式假设——`CMR` 标志与 `CMR_H_codes` 相互可校验；`cmr` 表里出现无 V 类码的行时，运行日志会明确报警而不是静默给 `CMR = "Y"`。
- `cmr_suspect` 的 H 码缺失是已知限制，写进 NEWS 与本 ADR；若将来需要逐条审计 IV 级判定，再走"加列 + 回灌"那条路。
- `fetch_cmr_data()` 的默认 `source` 是 `"local"`，即默认走的就是原先漏筛的那条路径。修好之后，`clp_cmr_meta.xlsx` 的 "cmr" 工作表（1087 行 CLP 全表）经筛选后返回 332 行，与库内 `cmr` 表一致。
- 代价：筛选现在是无条件的，因此源文件缺 `Hazard Statement Code(s)` 列时会直接报错（`find_hazard_code_col()` 的既有行为）而不是静默放行。这是有意为之——把整张总表当 CMR 入库比起报错危险得多。
