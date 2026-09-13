# FCMSafety 项目介绍与后续开发交接计划

本文档用于将 FCMSafety 当前未完成的数据库更新系统工作交接给后续开发者或实习生。请先完整阅读本文档，再开始修改代码。

## 1. 项目定位

FCMSafety 是一个用于食品接触材料（Food Contact Materials, FCM）相关化学物质安全筛查的 R 包。

项目的核心价值不是单独的可视化界面，也不是单纯的数据文件管理，而是：

1. 集成多个食品接触材料及化学风险相关数据来源；
2. 将用户检出的化学物质与这些来源派生出的本地数据库表进行匹配；
3. 判断某个物质是否出现在 SVHC、CLP 派生的 CMR/CMR suspect、IARC、EU 10/2011 派生的 EU SML/EU SML group 等表中；
4. 基于匹配结果和 Toxtree Cramer rules 给出风险/毒性相关标记；
5. 为用户提供可追溯、可更新、可审计的本地数据库。

当前包的主要使用流程是：

```r
library(fcmsafety)

# 用户导入自己的物质数据，数据中至少应包含 InChIKey；通常还会包含 CAS、SMILES、名称等字段。
data <- rio::import("my_detected_compounds.xlsx")

# 对未在监管数据库中出现的物质，可直接在 R 内跑 Toxtree Cramer 分类
# （首次运行会自动下载 Toxtree 到用户缓存目录，需本机装有 Java 8+）。
# run_toxtree() 会生成 "toxtree_results.csv"。
run_toxtree(data)

# 拿到 Cramer rules 结果后，执行风险数据库匹配。
result <- assign_toxicity(data, toxtree_result = "toxtree_results.csv")
```

## 2. 当前需要维护更新的核心数据来源

当前项目实际需要维护的不是 8 个彼此独立的核心数据库，而是 4 个核心数据来源。部分本地表是从同一个来源派生出来的子集或辅助表。

| 核心数据来源 | 派生出的本地表 | 主要用途 | 当前状态 |
| --- | --- | --- | --- |
| SVHC | `svhc` | ECHA 高关注物质清单 | 已有本地数据和部分更新函数，但自动更新未闭环 |
| CLP / Annex VI | `cmr`, `cmr_suspect` | 从欧盟 CLP 中筛选 CMR 和疑似 CMR 相关物质 | 已有 ECHA Annex VI 自动下载雏形，但仍是全量重写逻辑 |
| IARC | `iarc` | IARC 致癌性分类 | 已有本地数据；更新依赖人工文件或后续自动解析 |
| EU 10/2011 | `eu_sml`, `eu_sml_group` | 食品接触塑料材料特定迁移限量及 group restriction | 已有本地数据；EU SML 与 EU SML group 应同源更新 |

当前路线中不再把旧版 `edc` 表作为核心维护对象，因为它基于两篇文献形成的是静态证据表，不适合作为长期自动更新的权威监管来源。如果后续仍需要 EDC 证据层，优先重新设计为“ED assessment / endocrine disruptor evidence source”，并以 ECHA 的 endocrine disruptor assessment list、ECHA Candidate List 中因 ED properties 纳入的 SVHC 记录、欧盟委员会 ED 专题页和相关法规原文作为更权威、更可追溯的来源。旧 `edc` 表和数据可以暂时保留以避免破坏旧代码，但后续计划中应将其标记为 deprecated，并在确认没有兼容性需求后移除旧更新逻辑、文档和风险标记。

China SML 和未来可能增加的 China SML group 不应列为第一阶段自动更新来源。当前没有稳定、公开、可机器读取的官方在线表格可直接同步；如果继续支持，应按“人工维护的地区扩展数据源”处理：由维护者从现行 GB 9685、国家卫生健康委公告、国家标准全文公开系统等官方入口核对后更新，并保存标准版本、公告日期、页码/URL、人工复核记录和原始文件快照。

### 2.1 核心来源网址与更新方式

实习生在写 adapter 或测试前，必须先打开以下一手来源，确认当前网页结构、下载入口、字段和版本日期。不要直接相信旧 Excel 文件，也不要用第三方整理表替代官方来源。

| 数据来源 | 首选一手来源 | 更新方式建议 | 需要记录的版本信息 |
| --- | --- | --- | --- |
| SVHC | ECHA Candidate List table: <https://echa.europa.eu/candidate-list-table>；Candidate List page: <https://echa.europa.eu/candidate-list> | 优先自动或半自动下载表格快照；如下载入口变化，退回人工下载 | 抓取日期、页面显示的清单更新时间、EC/CAS、reason for inclusion、date of inclusion、原始文件 hash |
| CLP / Annex VI | ECHA Annex VI to CLP: <https://echa.europa.eu/information-on-chemicals/annex-vi-to-clp>；ECHA CLP legislation: <https://echa.europa.eu/regulations/clp/legislation>；ECHA C&L Inventory: <https://echa.europa.eu/information-on-chemicals/cl-inventory-database>；CLP 法规 EUR-Lex: <https://eur-lex.europa.eu/eli/reg/2008/1272/oj> | 以 CLP Annex VI / ATP / consolidated regulation 为准；ECHA C&L Inventory 只能作为补充检索源 | CELEX/ELI、ATP 编号、法规版本日期、下载文件 hash、派生出的 CMR/CMR suspect 条目数 |
| IARC | IARC List of Classifications: <https://monographs.iarc.who.int/list-of-classifications/>；IARC classification web app data: <https://webapi.iarc.who.int/loc/loc.app.js>；IARC Monographs: <https://monographs.iarc.who.int/>；Preamble: <https://monographs.iarc.who.int/iarc-monographs-preamble/> | IARC 页面可人工/半自动快照；classification web app 当前包含 agents 数据对象和 CSV/Excel 导出线索，但代码应把字段变化作为可能情况处理 | 抓取日期、页面 last_update、agent、CAS、Group、Volume、evaluation year、monograph URL |
| EU 10/2011 | EUR-Lex Regulation (EU) No 10/2011: <https://eur-lex.europa.eu/eli/reg/2011/10/oj>；当前 consolidated 入口需从 EUR-Lex 或 EC 页面确认；EC FCM legislation page: <https://food.ec.europa.eu/food-safety/chemical-safety/food-contact-materials/legislation_en> | 以 EUR-Lex 法律文本、consolidated text 和后续修订法规为准；从 Annex I 派生 EU SML / EU SML group。consolidated 版本便于使用，但要记录具体版本日期 | CELEX/ELI、法规版本日期、FCM substance No、group restriction No、SML、restriction text、原始文件 hash |
| EDC 证据层（如未来恢复） | ECHA ED assessment: <https://echa.europa.eu/ed-assessment>；ECHA endocrine disruptors topic: <https://echa.europa.eu/hot-topics/endocrine-disruptors>；EC Chemicals Strategy: <https://environment.ec.europa.eu/strategy/chemicals-strategy_en>；ED Lists: <https://edlists.org/>；OECD eChemPortal: <https://www.echemportal.org/echemportal/>；US EPA EDSP: <https://www.epa.gov/endocrine-disruption> | 不恢复旧两篇文献静态表；若需要，应新建 EDC evidence/assessment adapter，区分 EU regulatory assessment、JRC/ED Lists 候选清单、OECD 聚合检索、EPA EDSP 筛查体系和文献候选 | EC/CAS、assessment/list status、jurisdiction、conclusion、decision/date、evidence URL、是否为监管结论、是否为候选/筛查证据 |
| China SML（人工文件导入） | 以国家卫健委/市场监管总局官方 GB 9685 现行版本及公告为准；无稳定公开 API，需人工下载或通过官方检索入口获取 | 只做人工维护，用 `update_china_sml(file = "...xlsx")` 增量导入；原始文件快照存入 `inst/sources/china_sml/`；`GB 9685 添加剂清单-20241220.xlsx`（2897行）为当前基准文件 | 标准号与版本、公告/实施日期、CAS/FCA No、SML/单位、用途/材料限制、人工复核人；FCA No + CAS 复合键作为 source_key |

**China SML 输出列的已知 Bug。** `assign_toxicity()` 的输出结果表里有 `China_SML` 和 `China_SML_group` 两列，但当前代码中 `China_SML_group` 始终为 `NA_character_`（`direct_sql_toxicity.R` 第 204 行）。实际上 `china_sml` 表里有 `SML_group` 字段（数值型）和 `Identification No. of SML(T)` 字段（文本型），共 185 条记录包含 SML(T) 分组数据。Bug 原因是 `query_china_sml_data()` 只查询了 `sml_value`，没有同时查询分组字段。实习生在修复 China SML 更新函数时，也应顺便修复这个读取逻辑。

**China SML 更新函数设计（重要）。** GB 9685 没有稳定公开的在线来源，应设计为文件导入 + 增量对比模式，而非网络 adapter。

当前已有的 Excel：`GB 9685 添加剂清单-20241220.xlsx`（2897 行，含 GB9685+公告 Sheet 和附录D 材料缩写 Sheet）。其中 GB9685+公告 Sheet 对应 `china_sml` 表，附录D Sheet 是材料缩写参考表，不等同于 EU 那种 `SML(T)` 分组限制。

建议的 `update_china_sml()` 设计：

```r
update_china_sml <- function(
  file = NULL,           # Excel 文件路径，如 "GB 9685 添加剂清单-20241220.xlsx"
  source_dir = NULL,     # 或指定目录，从中选择最新文件
  db_path = NULL,
  dry_run = TRUE,
  apply = FALSE,
  reviewer = NULL,       # 人工复核人姓名
  notes = NULL           # 备注，如公告号、版本说明
) {
  # 1. 定位文件：优先用 file，否则在 source_dir 中选最新的
  # 2. 读取 Excel（支持多 Sheet：GB9685+公告 → china_sml；附录D → materials_reference）
  # 3. 规范化列名（处理换行、空格、大小写）
  # 4. 生成 source_key = paste(FCA No, CAS, sep = "||")
  # 5. 计算 row_hash（排除 CID/InChIKey 等运行时 metadata）
  # 6. diff：与数据库现有 china_sml 比对
  # 7. dry_run 报告：列出 added/removed/modified
  # 8. apply：增量写入，只变动 source_key 对应的行
  # 9. 写 database_metadata、update_history、change_log
  # 10. 保存原始文件快照到 inst/sources/china_sml/
}
```

推荐文件存储结构：

```text
inst/sources/china_sml/
  GB 9685 添加剂清单-20241220.xlsx   # 当前基准快照
  GB 9685 添加剂清单-YYYYMMDD.xlsx   # 未来新版
  manifest.csv                         # 每版文件的 source_url、版本、快照日期、hash
```

Manifest 建议字段：`file_name`、`gb_standard`、`announcement`、`snapshot_date`、`source_hash`、`reviewer`、`notes`。

`source_key` 建议：`FCA No` + `CAS` 复合键。`FCA No` 来自 GB 9685 编号体系（如 FCA0001），是 GB 9685 自己的物质编号，比 CAS 更权威；`CAS` 作为辅助唯一键。

当前 Excel 有 2897 行，数据库中 `china_sml` 只有 1182 行，说明现有数据库中的 china_sml 数据远不完整。更新时需要以新 Excel 为准，增量补入缺失记录，而不是整表替换。

附录D 材料缩写 Sheet 可以作为 `china_materials_reference` 表保存，也可以暂不处理，取决于后续是否有匹配需求。**GB 9685 不存在独立的 SML group 表**（不像 EU 10/2011 有 `SML(T)` 分组限制），因此不需要设计 `china_sml_group` 适配器。

重要边界：`source_url` 必须指向官方机构或法规原文；如果只能从网页人工下载，就把状态写成 `manual_required` 或 `manual_review_required`。网页结构变化、下载失败、返回 0 行时，绝不能把本地数据清空。

当前本地 SQLite 数据库基线计数如下，应作为后续迁移和测试的保护目标。注意这里列的是本地表，不等同于独立更新来源：

| 表 | 来源 | 当前记录数 |
| --- | --- | ---: |
| svhc | SVHC | 459 |
| cmr | CLP / Annex VI 派生 | 1139 |
| cmr_suspect | CLP / Annex VI 派生 | 491 |
| iarc | IARC | 1116 |
| eu_sml | EU 10/2011 派生 | 901 |
| eu_sml_group | EU 10/2011 派生 | 35 |
| edc | 计划废弃 | 94 |
| china_sml | 后续可选扩展/人工来源 | 1182 |
| chemicals | PubChem/LABTOOLS metadata | 0 |

注意：`chemicals = 0` 是一个重要问题。当前监管表中已有大量物质记录，但共享化学元数据表为空，说明 SQLite schema、迁移结果和实际匹配需求尚未完全对齐。

## 3. 当前代码状态概览

本地仓库相对于远程已有较大改动，主要集中在 SQLite 数据库迁移、数据库检查器和更新系统雏形。

关键文件如下：

### `R/direct_sql_toxicity.R`

当前定义了新版 `assign_toxicity()`，目标是直接查询 SQLite，而不是把所有数据库加载到全局变量。

目前问题：

1. `assign_toxicity()` 默认包含检查更新和自动更新逻辑，这不适合作为匹配函数的默认行为；
2. 匹配函数不应该在用户没有明确要求时联网或修改数据库；
3. 当前 SQL 查询通过拼接字符串构造 `IN (...)`，需要改为 DBI 参数化查询，避免 SQL 注入和特殊字符问题；
4. 该文件应只负责匹配和风险标记，不应承担数据库更新职责。

### `R/databases.R`

当前包含若干数据库更新函数，例如：

- `download_latest_clp()`
- `update_cmr()`
- `update_iarc()`
- `update_svhc()`
- `update_eusml()`

目前问题：

1. 多数 `update_*()` 函数依赖用户手动提供文件；
2. `update_cmr()` 虽然已有 ECHA CLP 自动下载雏形，但写入时使用 `DELETE FROM cmr` 后整表重写；
3. 函数直接写数据库，职责过重；
4. 不返回统一的 source version、source hash、diff、report 结构；
5. 不适合作为未来所有数据库的一键增量更新核心。

后续应将这里的函数改造成“source adapter / normalize adapter”，只负责获取和规范化来源数据，不直接做全表覆盖写入。

> ⚠️ **本节（`R/databases.R`）的 `download_latest_clp()` / `update_cmr()` 已过时，请勿再用。** 它们是全量重写逻辑（`DELETE FROM cmr` 后整表重写），且下载后不筛 H 代码。CLP 派生 CMR / CMR suspect 的正确入口见下一节 `R/update_other_dbs.R`。

### `R/update_other_dbs.R`（CLP / IARC / EU SML 增量更新的正确入口）

这是 CLP 派生 `cmr` / `cmr_suspect` 的**唯一正确更新入口**（配合 `R/incremental_update.R` 的通用增量核心）。不要再用 `R/databases.R` 里的旧函数。

- 统一委托：`update_cmr_auto()` / `update_cmr_suspect_auto()` / `update_iarc_auto()` / `update_eu_sml_auto()` 都委托 `run_incremental_update(db_name, key_col, cas_col)`，走「先 diff 后 enrich」的增量管线，不整表重写。
- CMR 与疑似 CMR 的下载后自动筛选（2026-09-01 修复）：
  - `fetch_cmr_data(source = "download")`：下载 CLP 总表 → 用 `screen_clp(df, "cmr")` 按 H 代码筛出确认 CMR（`H340|H350|H360`）。
  - `fetch_cmr_suspect_data(source = "download")`：下载 CLP 总表 → 用 `screen_clp(df, "cmr_suspect")` 筛出疑似 CMR（`H341|H351|H361`）。
  - 两者**非互斥**：一个物质可同时带确认 + 疑似代码，故用两个独立集合分别筛，不是 if/else 二选一。
  - H 代码列定位：`find_hazard_code_col()` 优先精确匹配 `Hazard Statement Code(s)`，兜底排除 `Alternative` / `Suppl` 备用列。
- 下载原语：`R/download_sources.R` 的 `download_clp()`（带校验：HTTP 200 + 文件非空 + xlsx 的 "PK" 魔数，失败自动删除并报错）。
- 参考基线（基于当前 `inst/clp.xlsx` 全表 4316 行的真实 H 代码）：确认 CMR ≈ 1087 行、疑似 CMR ≈ 449 行、两者同时 ≈ 115 行。注意与第 2 节「本地 SQLite 基线计数」（cmr=1139、cmr_suspect=491）不完全一致——库表可能是更早的全量重写口径或含 PubChem 富集差异，首次用新逻辑更新会看到 removed 差异，属预期，`removed > 0` 会强制人工确认（这是安全设计，不是 bug）。

### `R/enhanced_update_system.R`

这是当前一键更新流程的雏形，包含：

- `update_databases_interactive()`
- `check_available_updates()`
- `perform_database_updates()`
- `display_update_summary()`
- `update_sqlite_from_update_result()`

目前问题：

1. `check_available_updates()` 主要检查 `inst/` 中是否存在手工新文件，不是真正访问远程源；
2. 当前只覆盖部分表，而且没有按“核心数据来源 -> 派生表”的模型组织；
3. `update_sqlite_from_update_result()` 仍是 TODO；
4. 更新流程没有 dry-run / apply 分离；
5. 没有真正的增量 diff；
6. 没有完整写入 `update_history` 和 `change_log`。

这个文件适合作为新公共 API 和更新编排入口。

### `R/sqlite_database_manager.R`

当前负责：

- 获取 SQLite 连接；
- 初始化数据库；
- 检查数据库状态；
- 从 XLSX 迁移到 SQLite；
- 将不同数据库的列映射到 SQLite schema。

目前问题：

1. `initialize_database(force_recreate = TRUE)` 一类逻辑有潜在破坏性；
2. `process_database_for_migration()` 会按 `InChIKey` 过滤部分数据库，这可能丢失没有 PubChem 匹配但仍然有法规意义的原始条目；
3. SQLite schema 与实际数据库表结构不完全一致；
4. 缺少 schema version、idempotent migration、backup、restore、transactional apply 等关键能力。

### `R/update_history_audit.R`

当前可以读取和展示更新历史：

- `get_update_history()`
- `display_update_history()`
- `get_detailed_changes()`
- `display_detailed_changes()`
- `get_database_statistics()`
- `export_update_history()`

目前问题：

1. 主要是读取/展示函数；
2. 缺少真正写入 `update_history` / `change_log` 的生产逻辑；
3. 现有数据库中 `update_history` 和 `change_log` 为空；
4. 后续应支持 `run_id`，把一次多数据库更新作为一个审计 run。

### `inst/fcmsafety_schema.sql`

当前 schema 已经设计了多张本地表和审计表，其中 `edc` 是计划废弃的旧表，`china_sml` 是后续可选的地区扩展表：

- `chemicals`
- `svhc`
- `cmr`
- `cmr_suspect`
- `iarc`
- `eu_sml`
- `eu_sml_group`
- `edc`（旧表，计划废弃）
- `china_sml`（后续可选扩展/人工来源）
- `database_metadata`
- `update_history`
- `change_log`
- `view_svhc`
- `view_cmr`

目前问题：

1. 多个监管表要求 `InChIKey NOT NULL` 并外键关联 `chemicals`；
2. 实际 `chemicals` 表为空，这会导致新 schema 与当前数据不匹配；
3. 只有部分 raw 表，结构不对称；
4. 缺少稳定的 `source_key` 和 `row_hash`；
5. `database_metadata.total_records` 当前为 0，与真实表计数不一致；
6. `source_url` 为空，无法可靠判断来源版本。

## 4. 后续开发的核心目标

后续开发的核心目标是实现：

```r
update_databases_incremental(dry_run = TRUE)
```

或保留更简单的用户入口：

```r
update_databases(dry_run = TRUE)
```

这个函数应做到：

1. 默认检查全部核心数据来源：SVHC、CLP、IARC、EU 10/2011；
2. 自动访问可自动化的数据源；
3. 对无法自动检查的数据源返回 `manual_required`，而不是假装已更新；
4. 比较远程/新文件与本地 SQLite 中的旧数据；
5. 生成新增、删除、修改条目报告；
6. 默认 dry-run，不写数据库；
7. 用户显式 `apply = TRUE` 时才执行更新；
8. apply 前自动备份 SQLite；
9. 使用事务保证全部成功或全部回滚；
10. 只增量插入、更新、删除变动记录，不做整表重写；
11. 只对新增或 metadata 缺失的物质调用 LABTOOLS/PubChem；
12. 将成功/失败/变更细节写入审计表。

推荐公共 API：

```r
update_databases_incremental(
  databases = NULL,
  db_path = NULL,
  source_files = NULL,
  source_dir = NULL,
  dry_run = TRUE,
  apply = FALSE,
  backup = TRUE,
  strict = FALSE,
  retry_failed = FALSE,
  report_path = NULL
)
```

参数含义：

- `databases = NULL`：默认检查全部核心数据来源，实际包括 SVHC、CLP、IARC、EU 10/2011；
- `db_path`：允许测试或高级用户指定 SQLite 文件；
- `source_files` / `source_dir`：用于 IARC 等需要人工文件的来源；
- `dry_run = TRUE`：只生成计划和报告，不写入；
- `apply = TRUE`：显式执行更新；
- `backup = TRUE`：写入前备份数据库；
- `strict = FALSE`：默认 manual_required 不阻塞其他自动库；严格模式下任何库不可检查都可视为失败；
- `retry_failed = FALSE`：默认不反复查询此前 PubChem 失败的物质；
- `report_path`：可选保存 markdown/csv/json 更新报告。

## 5. 推荐的技术路线

不要在现有 `update_sqlite_from_update_result()` 里简单调用旧的 `update_*()` 函数。这样会把当前全量删除、全量重写、schema 不一致的问题固化下来。

推荐实现一条统一管线：

```text
source registry
  -> fetch/check source
  -> normalize canonical rows
  -> validate source
  -> compute diff
  -> enrich only new/missing metadata through LABTOOLS/PubChem
  -> produce dry-run report
  -> optional backup + transactional apply
  -> write database_metadata/update_history/change_log
```

### 5.1 Source registry

使用普通 R named list，不需要 R6 或复杂框架。

每个来源组定义：

```r
list(
  databases = c("cmr", "cmr_suspect"),
  source_url = "https://echa.europa.eu/information-on-chemicals/annex-vi-to-clp",
  key_columns = c("index_no", "cas_no"),
  fetch = fetch_clp_source,
  normalize = normalize_clp_source,
  manual_file_candidates = c("annex_vi_clp*.xlsx", "clp*.xlsx")
)
```

来源组应按实际来源划分，而不是按表机械划分：

| 来源组 | 派生数据库 |
| --- | --- |
| SVHC | `svhc` |
| CLP / Annex VI | `cmr`, `cmr_suspect` |
| EU 10/2011 / Eur-Lex | `eu_sml`, `eu_sml_group` |
| IARC | `iarc` |

这样可以避免同一个文件下载两次，也可以保证 CMR 和 CMR suspect、EU SML 和 EU SML group 在同一次事务中保持一致。EDC 不再作为核心来源；China SML 如需保留，应作为后续人工维护的地区扩展来源，而不是第一阶段核心更新对象。

### 5.2 Adapter 返回结构

每个 fetch adapter 返回统一结构：

```r
list(
  status = "changed", # changed/no_change/manual_required/unavailable/error
  payload = data_or_temp_file,
  source_url = "...",
  source_version = "...",
  source_hash = "...",
  checked_at = Sys.time(),
  message = "..."
)
```

状态语义：

| status | 含义 |
| --- | --- |
| `changed` | 发现来源内容与本地记录不同，可以进入 diff |
| `no_change` | 来源版本或 hash 与本地一致 |
| `manual_required` | 该数据库无法稳定自动下载，需要用户提供文件 |
| `unavailable` | 来源暂时不可访问 |
| `error` | 检查或解析失败 |

重要原则：

- `manual_required` 不是成功更新；
- `unavailable` 不是 up-to-date；
- 自动库失败时应进入报告；
- 默认情况下，某些手动库缺失不应阻塞其他自动库；
- strict 模式可以要求全部数据库都必须成功检查。

### 5.3 稳定 source_key 与 row_hash

不能用 SQLite 自增 `id` 做版本比较，因为不同版本导入后 ID 不稳定。

每个监管条目需要：

- `source_key`：由法规来源中的自然键生成；
- `row_hash`：由规范化后的法规字段生成。

初始自然键建议：

| 数据库表 | 来源 | source_key 建议 |
| --- | --- | --- |
| SVHC | SVHC | EC No + CAS No + normalized substance name |
| CMR | CLP / Annex VI | Index No + CAS No |
| CMR suspect | CLP / Annex VI | CAS No + substance name + classification |
| IARC | IARC | CAS + Agent |
| EU SML | EU 10/2011 | FCM substance No，必要时加 CAS |
| EU SML group | EU 10/2011 | Group restriction No + FCM substance No |

`row_hash` 应排除：

- SQLite `id`
- `created_at`
- `updated_at`
- PubChem metadata 字段
- 其他运行时生成字段。

### 5.4 真正的增量 diff

需要实现：

```r
diff_database_rows(old_rows, new_rows, key = "source_key")
```

输出：

```r
list(
  added = ...,
  removed = ...,
  modified = ...,
  unchanged = ...,
  field_changes = ...
)
```

注意：不能把所有交集记录都当作 modified。只有 `row_hash` 不同的记录才是 modified。modified 还应列出具体字段的 old/new 值。

### 5.5 删除保护

这是本项目最重要的数据完整性要求之一。

必须实现以下保护：

1. 来源返回 0 行时，默认禁止把本地表清空；
2. 必需字段缺失时，拒绝更新；
3. `source_key` 重复时，拒绝更新；
4. 来源记录数异常大幅下降时，进入 error 或需要显式确认；
5. dry-run 前后数据库文件 hash 必须不变；
6. apply 前必须备份；
7. apply 必须使用事务；
8. 任意一个选中库更新失败时，默认 rollback。

### 5.6 LABTOOLS / PubChem metadata 策略

PubChem metadata 查询是必要的，但不能每次全量重查。

只允许查询：

1. 新增法规条目；
2. 旧法规条目中缺失 CID、SMILES、InChIKey、Formula、IUPACName、ExactMass 等 metadata 的记录。

建议流程：

```text
added/missing metadata rows
  -> deduplicate by CAS/name
  -> query local chemicals table
  -> query metadata_lookup_cache
  -> labtools::extract_cid()
  -> LABTOOLS metadata extraction
  -> write chemicals/cache/report
```

规则：

- PubChem 查询失败不能导致法规条目丢失；
- 查询失败应写入 cache；
- not found 与 error 要区分；
- 默认不重复查询之前失败的物质，除非 `retry_failed = TRUE`；
- 只填补缺失字段，不覆盖已有可靠 metadata。

### 5.7 测试策略：不要依赖实时网站是否刚好有更新

更新系统的测试不能直接依赖真实网站“今天有没有新内容”。如果第一次运行已经把 IARC 从旧版更新到了当前版本，那么第二次再连真实来源当然会显示 no_change；这不是测试失败，而是实时来源测试本身不可重复。因此测试必须分层设计：

1. **单元测试只测纯函数。** `build_source_key()`、`build_row_hash()`、`diff_database_rows()`、normalize 函数都使用本地 fixture，不联网、不写真实数据库。
2. **集成测试使用临时 SQLite 和固定 fixture。** 例如准备两个 IARC fixture：`iarc_2020.csv` 代表旧本地数据，`iarc_2026_08_31.csv` 代表新来源数据。测试时先把 2020 fixture 写入临时数据库，再用 2026-08-31 fixture 作为“远程新数据”运行 update plan。这样无论真实 IARC 网站今天是否已更新，测试结果都稳定。
3. **幂等性要单独测试。** 同一个 2026-08-31 fixture 第一次 apply 应产生 added/removed/modified；第二次再 apply 或 dry-run 应返回 no_change 或 0 delta。这正好覆盖“已经更新后再运行没有新内容”的情况。
4. **真实网络只做手动 smoke test。** 可以有一个不在 CRAN/CI 默认运行的手动测试，例如 `test-live-sources.R`，用于检查 ECHA/IARC/EUR-Lex 页面是否还能访问、下载字段是否变了，但不能把它作为单元测试通过条件。
5. **中国 SML 只测人工文件流程。** China SML / China SML group 没有稳定公开 API，所以测试应使用人工整理的官方快照 fixture，验证人工文件导入、版本记录、差异报告和复核字段，而不是测试自动联网。

推荐 fixture 目录：

```text
tests/testthat/fixtures/update_sources/
  svhc/
    old_candidate_list.csv
    new_candidate_list.csv
  clp/
    annex_vi_old.csv
    annex_vi_new.csv
  iarc/
    iarc_2020.csv
    iarc_2026_08_31.csv
  eu10_2011/
    eu10_2011_old.csv
    eu10_2011_new.csv
  china_sml_manual/
    gb9685_manual_old.csv
    gb9685_manual_new.csv
```

每个 fixture 旁边建议放一个 `source_manifest.json` 或小型 CSV，记录：

- `source_name`
- `source_url`
- `source_version`
- `snapshot_date`
- `source_hash`
- `expected_added`
- `expected_removed`
- `expected_modified`
- `notes`

IARC 示例测试目标：

```r
# 1. 用旧 fixture 建临时数据库
seed_test_database(db_path, source = "iarc_2020.csv")

# 2. 用新 fixture 生成 dry-run plan
plan <- update_databases_incremental(
  databases = "iarc",
  db_path = db_path,
  source_files = list(iarc = "iarc_2026_08_31.csv"),
  dry_run = TRUE
)

# 3. 检查 dry-run 有预期 delta，且数据库没有变化
expect_equal(plan$iarc$added_count, expected_added)
expect_equal(plan$iarc$modified_count, expected_modified)
expect_database_unchanged(db_path, before_hash)

# 4. apply 到临时数据库
result <- update_databases_incremental(
  databases = "iarc",
  db_path = db_path,
  source_files = list(iarc = "iarc_2026_08_31.csv"),
  dry_run = FALSE,
  apply = TRUE
)

# 5. 再跑一次同样来源，应该没有新变化
second <- update_databases_incremental(
  databases = "iarc",
  db_path = db_path,
  source_files = list(iarc = "iarc_2026_08_31.csv"),
  dry_run = TRUE
)
expect_equal(second$iarc$added_count, 0)
expect_equal(second$iarc$modified_count, 0)
```

这个测试设计比“等真实网站出现更新”可靠得多，也能清楚证明增量更新、dry-run、apply 和重复运行都正确。

### 5.8 Toxtree 集成可行性：优先做 CLI wrapper，不要先做深度绑定

当前流程需要用户从 R 导出 SMILES，再打开 Toxtree 桌面软件预测 Cramer rules，最后把结果导回 R。这个流程确实麻烦，后续应探索把 Toxtree 作为外部命令集成进 R 包。

Toxtree 官方 FAQ 明确支持 headless / command line 运行，核心参数包括：

```bash
java -jar Toxtree-3.1.0.1851.jar -n -i test.csv -o out.csv
java -jar Toxtree-3.1.0.1851.jar -i test.csv -n -o out.csv -m toxTree.tree.cramer.CramerRules
```

官方文档入口：

- Toxtree FAQ / command line: <https://toxtree.sourceforge.net/faq.html>
- Toxtree homepage: <https://toxtree.sourceforge.net/>
- Toxtree downloads: <https://toxtree.sourceforge.net/download.html>

建议先实现一个外部进程 wrapper，而不是把 Java 类直接嵌入 R：

```r
run_toxtree <- function(data, smiles_col = "SMILES", jar_path = NULL,
                        module = "toxTree.tree.cramer.CramerRules",
                        java = "java", timeout = 300) {
  # 1. 校验 Java 和 jar_path
  # 2. 写入临时 CSV，必须包含 SMILES 列
  # 3. system2(java, c("-jar", jar_path, "-n", "-i", input, "-o", output, "-m", module))
  # 4. 检查退出码、stderr、输出文件
  # 5. 读回 Toxtree 结果并与原始 ID 合并
}
```

实习生需要先验证：

1. 当前 Toxtree JAR 是否能在本机 Java 版本下运行；
2. `java -jar <jar> --help` 是否可用；
3. 最小 CSV 是否必须包含名为 `SMILES` 的列；
4. Cramer rules 模块类名在当前 JAR 中是否为 `toxTree.tree.cramer.CramerRules`；
5. 输出 CSV 字段名是什么，是否稳定；
6. 非法 SMILES、空 SMILES、重复 ID、中文名称、逗号、换行是否会导致失败；
7. Java 缺失、JAR 不存在、运行超时、Toxtree 非零退出码时 R 函数是否给出清楚错误；
8. Toxtree 许可证为 GPL，若未来随 R 包分发 JAR，需要确认许可证和第三方依赖义务。更安全的初期做法是不随包内置 JAR，而是让用户配置 `toxtree.jar_path`。

Toxtree 预测结果应作为 `prediction` 数据层保存，不能与 SVHC、CLP、IARC、EU SML 等监管结论混在同一类字段中。报告中必须记录：Toxtree 版本、JAR hash、Java 版本、module、输入文件 hash、输出文件 hash 和运行时间。

## 6. 实习生执行路线

建议实习生按以下顺序推进，不要跳步。

### 第一步：只读理解和基线确认

目标：理解项目现状，不修改功能。

任务：

1. 阅读以下文件：
   - `DESCRIPTION`
   - `README.md` / `README.Rmd`
   - `R/direct_sql_toxicity.R`
   - `R/databases.R`
   - `R/enhanced_update_system.R`
   - `R/sqlite_database_manager.R`
   - `R/update_history_audit.R`
   - `inst/fcmsafety_schema.sql`
2. 打开第 2.1 节列出的每个官方来源网址，确认当前是否有下载入口、字段是否稳定、是否显示版本/更新时间；
3. 运行只读数据库计数检查，确认基线记录数；
4. 梳理当前导出的函数和用户使用流程；
5. 写一页简短 notes，说明自己理解的项目流程、每个来源的更新方式、哪些来源必须人工维护。

验收标准：

- 能解释 `assign_toxicity()` 的输入和输出；
- 能用 `tests/testthat/fixtures/example_input.csv` 成功运行一次完整的物质匹配，并解释每个物质的输出结果；
- 能列出 4 个核心更新来源，以及它们派生出的本地表；
- 能说明为什么当前更新机制不是增量更新；
- 能说明为什么 `chemicals = 0` 是问题；
- 能指出 China SML / China SML group 为什么只能先做人工更新；
- 能说明旧 EDC 表为什么不适合作为长期权威来源，以及未来如果恢复 EDC 应优先查看哪些官方来源。

**实习生的第一个测试示例（重要）。** 为帮助实习生快速理解 `assign_toxicity()` 的输入输出流程，提供了测试 fixture：

```text
tests/testthat/fixtures/example_input.csv
```

该文件包含 5 个物质，覆盖所有核心数据库：

| CAS | 名称 | 预期命中的数据库 |
|---|---|---|
| 115-86-6 | Triphenyl phosphate | SVHC |
| 7440-41-7 | beryllium | CMR |
| 10043-92-2 | Radon-222 and its decay products | IARC Group 1 |
| 50-00-0 | formaldehyde | EU SML |
| 25013-16-5 | 叔丁基羟基茴香醚(BHA) | China SML |

实习生应先确认这些物质确实存在于对应的数据库中（查询 SQLite 验证），然后运行：

```r
data <- read.csv("tests/testthat/fixtures/example_input.csv")
# 确认 InChIKey 列存在（若无则需先通过 CAS/SMILES 解析 InChIKey）
# 运行匹配
result <- assign_toxicity(data)

# 检查结果
# - 115-86-6 应有 SVHC = "Y"
# - 7440-41-7 应有 CMR = "Y"
# - 10043-92-2 应有 IARC = "1"
# - 50-00-0 应有 EU_SML 非空值
# - 25013-16-5 应有 China_SML 非空值
```

注意：当前 `assign_toxicity()` 需要输入数据包含 `InChIKey` 列。如果测试数据没有 InChIKey，需要先用 LABTOOLS/PubChem 补充，或直接用已知 InChIKey 的数据。

### 第二步：补测试框架和 fixtures

目标：先建立保护网。

任务：

1. 检查项目是否已有 `tests/testthat/`；如没有则建立；
2. 已有 `tests/testthat/fixtures/example_input.csv`（5 个物质的测试输入）；用 `tests/testthat/fixtures/` 存放所有测试 fixture；
3. 添加 diff 函数测试用例：
   - 新增一条；
   - 删除一条；
   - 修改一个字段；
   - 完全无变化；
   - 重复 key；
   - 空来源。
5. 添加更新测试用例时必须同时覆盖：
   - “旧版本 -> 新版本”产生 delta；
   - 同一个新版本重复运行返回 no_change / 0 delta；
   - dry-run 不改变数据库；
   - apply 只改变临时数据库；
   - China SML 人工文件导入和版本记录。

验收标准：

- 测试可以在本地运行；
- 测试不依赖网络；
- 测试不修改真实数据库；
- 重复运行同一个 fixture 的结果稳定；
- 人工来源 fixture 记录了 source URL、版本、快照日期和预期 diff。

### 第三步：实现 schema migration 和备份机制

目标：保证后续更新不会破坏用户数据。

任务：

1. 在 schema 中增加 `source_key`、`row_hash`、`run_id`、`source_hash` 等字段；
2. 实现幂等 migration：重复运行不会破坏已有数据；
3. 实现数据库备份函数；
4. 实现数据库恢复函数；
5. 实现 integrity check helper。

验收标准：

- migration 后核心派生表计数不减少；
- `database_metadata.total_records` 与真实表计数一致；
- 备份文件存在且可读；
- rollback/restore 路径至少有测试覆盖。

### 第四步：实现 registry 和 normalize

目标：统一不同数据库的来源处理。

任务：

1. 新建或整理 registry；
2. 为 4 个核心来源及其派生表实现 normalize 函数；
3. 实现 `build_source_key()`；
4. 实现 `build_row_hash()`；
5. 为每个 normalize 写测试。

验收标准：

- 每个数据库都能从 fixture 生成稳定 `source_key`；
- 同一条记录重复 normalize 得到相同 `row_hash`；
- 只改变空格/换行等无意义差异时，不误报 modified；
- 关键字段缺失或重复 key 会失败。

### 第五步：实现 diff 和 dry-run report

目标：用户可以先看到会发生什么，而不是直接写数据库。

任务：

1. 实现 `diff_database_rows()`；
2. 实现 `plan_database_update()`；
3. 实现 `update_databases_incremental(dry_run = TRUE)`；
4. 报告中展示每个数据库：
   - status
   - source version/hash
   - added count
   - removed count
   - modified count
   - manual_required/error 信息。

验收标准：

- dry-run 不修改数据库；
- dry-run 不创建备份；
- dry-run 报告结构化返回 list；
- 可选保存 markdown/json/csv 报告。

### 第六步：实现事务 apply 和审计日志

目标：把 dry-run 计划安全地应用到 SQLite。

任务：

1. 实现 `apply_database_update_plan()`；
2. 使用事务执行所有选中数据库更新；
3. 只对 added/removed/modified 的 `source_key` 做增量写入；
4. 更新 `database_metadata`；
5. 写入 `update_history`；
6. 写入 `change_log`；
7. 出错时 rollback；
8. rollback 后用独立连接写失败审计。

验收标准：

- 不使用 `DELETE FROM table` 做整表刷新；
- 添加、删除、修改都有测试覆盖；
- 失败时所有表回到旧状态；
- `get_update_history()` 能看到本次更新；
- `get_detailed_changes()` 能看到具体字段变化。

### 第七步：实现 LABTOOLS/PubChem 增量 metadata

目标：只查询新增或 metadata 缺失物质，避免全量重查。

任务：

1. 建立 `metadata_lookup_cache`；
2. 从 diff 中提取 metadata candidate；
3. 本地已有 metadata 时复用；
4. 只对未解析 candidate 调 LABTOOLS；
5. 失败写 cache 和 report；
6. 不因 PubChem 失败丢弃法规条目。

验收标准：

- 已有完整 metadata 的物质不会被重复查询；
- 新增物质会进入候选列表；
- 查询失败不会中断整个数据库更新；
- 失败原因在报告中可见。

### 第八步：清理用户 API 和文档

目标：让最终用户容易使用。

任务：

1. 保留或新增主函数：

```r
update_databases(dry_run = TRUE)
update_databases(apply = TRUE)
```

2. 保持旧函数兼容，但内部委托到新核心；
3. 更新 `README.Rmd`，再生成 `README.md`；
4. 更新 roxygen 注释并重新生成 `NAMESPACE` 和 `man/*.Rd`；
5. 明确说明哪些数据库可自动检查，哪些需要人工文件。

验收标准：

- 新用户能从 README 理解更新流程；
- `assign_toxicity()` 不再默认修改数据库；
- `update_databases(dry_run = TRUE)` 是安全默认入口；
- `R CMD check` 或 `devtools::check()` 通过。

## 7. 可以考虑新增的数据库

当前阶段建议先完成 4 个核心更新来源（SVHC、CLP、IARC、EU 10/2011）的一键增量更新。不要在核心机制没完成前加入太多新库。EDC 已从当前核心路线中移除；如未来重新需要，应作为可选 hazard evidence source 重新评估数据来源、维护成本和实际价值。

但 schema 和 registry 设计时应预留扩展能力。后续可优先考虑：

| 优先级 | 数据库 | 原因 |
| --- | --- | --- |
| 高 | US FDA Food Contact Notifications / FCS Inventory | 补齐美国食品接触材料法规维度 |
| 高 | EFSA OpenFoodTox | 增强食品相关化学危害证据层 |
| 高 | Swiss FCM / Printing Inks list | 对包装油墨、涂层、胶黏剂很有价值 |
| 中高 | BfR Recommendations | 补充非塑料食品接触材料场景 |
| 中高 | EU Endocrine Disruptor Lists | 如果未来需要恢复 EDC 证据层，可优先选择比当前 EDC 表更清晰、可维护的来源 |
| 中 | EPA CompTox | 作为 PubChem 之外的化学身份/毒理数据补充 |
| 中 | SIN List / ChemSec | 用于预测性关注物质筛查，但需标注非官方法规授权清单 |
| 中 | OECD eChemPortal | 更适合作为外部跳转或辅助 enrichment，不建议第一阶段全量本地化 |

## 8. 数据库分类建议

为了便于未来扩展，建议不要把所有数据库都硬编码进 `assign_toxicity()`。更好的逻辑是把数据库分层：

1. **FCM regulatory sources**
   - EU SML / EU SML group（EU 10/2011 派生）
   - FDA FCS/FCN
   - Swiss Printing Inks
   - BfR / EDQM
   - China SML（如后续恢复，应作为地区扩展来源）

2. **Hazard evidence sources**
   - CMR / CMR suspect（CLP 派生）
   - IARC
   - SVHC
   - EFSA OpenFoodTox
   - EDC（当前计划移除；如未来需要，重新作为可选来源评估）

3. **Identity metadata sources**
   - PubChem via LABTOOLS
   - EPA CompTox

4. **Watchlist sources**
   - SIN List
   - EU ED lists

每个数据库在 registry 中可以声明：

- 是否官方来源；
- 是否 FCM 专属；
- 风险类别；
- 匹配字段；
- 更新方式；
- source URL；
- 数据源状态；
- 是否需要人工文件。

## 9. 开发注意事项

### 数据完整性优先

禁止在未备份、未 dry-run、未确认 diff 的情况下直接覆盖 SQLite。

尤其禁止在新增增量更新核心中使用：

```sql
DELETE FROM table;
```

然后全量重写。这是当前旧函数的问题，后续应避免继续扩散。

### 不要丢弃没有 InChIKey 的法规条目

很多法规清单中存在混合物、物质组、UVCB、没有明确结构的条目。这些条目可能无法从 PubChem 获得 InChIKey，但它们仍然有法规意义。

正确做法：

- regulatory table 保留完整条目；
- `InChIKey` 可以为空；
- 匹配函数只对有 InChIKey 的记录进行结构匹配；
- 报告中说明有多少条记录 metadata 未解析。

### 匹配和更新要分离

`assign_toxicity()` 应该是确定性的匹配函数，不应默认联网、不应默认修改本地数据库。

数据库更新应由用户显式调用：

```r
update_databases(dry_run = TRUE)
update_databases(apply = TRUE)
```

### 网络测试要 mock

不要让单元测试依赖 ECHA、EFSA、FDA 或 PubChem 实时网络状态。实时网络可以放在手动集成测试中。

### 安装包路径要注意

安装后的 R 包目录通常不可写。不要假设可以写 `system.file("...", package = "fcmsafety")` 下面的 `inst/`。

默认可写数据库应放在：

```r
tools::R_user_dir("fcmsafety", "data")
```

## 10. 法规组物质匹配问题（待分析，不立即实施）

### 10.1 问题描述

当前 `assign_toxicity()` 的核心匹配逻辑依赖精确 `InChIKey`：

```r
SVHC <- case_when(InChIKey %in% svhc_keys ~ "Y")
CMR  <- case_when(InChIKey %in% cmr_keys  ~ "Y")
```

这对有唯一 PubChem 结构的单一物质有效，但会系统性漏掉法规清单中的**组物质（group substances）**。

典型例子是 SVHC 中的壬基酚条目：

| SVHC 条目名称 | CAS | 描述 | 实际结构 |
|---|---|---|---|
| 4-Nonylphenol, branched and linear | — | 对位壬基酚（C9 烷基链，允许直链和支链） | 无单一 PubChem 结构 |
| Nonylphenol | 25154-52-3 | 壬基酚（宽泛） | 无单一 PubChem 结构 |
| 4-heptylphenol, branched and linear | — | 对位庚基酚（C7） | 无单一 PubChem 结构 |
| Medium-chain chlorinated paraffins (C14-C17) | — | C14-C17 氯化石蜡，链长/氯化度范围 | 无单一 PubChem 结构 |

非靶向筛查检出的物质是**具体分子**，例如某个直链对壬基酚异构体或其支链异构体。PubChem 对"4-Nonylphenol, branched and linear"只能返回壬基酚的一个代表性 InChIKey，而不是所有合法异构体的 InChIKey。因此，用精确 InChIKey 匹配会把大量实际属于 SVHC 壬基酚组的检出化合物**系统性漏报**。

### 10.2 用户需求

用户需要的是**高召回**：只要检出的化合物在法规组物质范围内，就应该被标记为"疑似 SVHC/CMR/EDC"，而不是因为 PubChem 没有返回对应的组物质 InChIKey 就忽略。

### 10.3 实习生任务：先分析，不实施

**实习生不需要现在实施任何代码，只需要完成以下分析任务：**

#### 任务 1：统计各数据库中缺失 InChIKey 的条目数量

写一个 R 脚本（或在 R console 中运行），对每个数据库表执行：

```r
# 连接到现有 SQLite 数据库
con <- get_db_connection()

# 对每个表，统计：
# 1. 总行数
# 2. 缺失 InChIKey 的行数
# 3. 缺失 InChIKey 且有 substance_name 的行数
# 4. 缺失 InChIKey 且有 CAS 的行数

databases <- c("svhc", "cmr", "cmr_suspect", "iarc", "eu_sml", "eu_sml_group", "edc", "china_sml")

for (db in databases) {
  if (DBI::dbExistsTable(con, db)) {
    total <- DBI::dbGetQuery(con, paste0("SELECT COUNT(*) FROM ", db))[[1]]
    missing_key <- DBI::dbGetQuery(con, paste0("SELECT COUNT(*) FROM ", db, " WHERE InChIKey IS NULL OR InChIKey = ''"))[[1]]
    missing_key_with_name <- DBI::dbGetQuery(con, paste0("SELECT COUNT(*) FROM ", db, " WHERE (InChIKey IS NULL OR InChIKey = '') AND substance_name IS NOT NULL"))[[1]]
    missing_key_with_cas <- DBI::dbGetQuery(con, paste0("SELECT COUNT(*) FROM ", db, " WHERE (InChIKey IS NULL OR InChIKey = '') AND cas_no IS NOT NULL"))[[1]]
    cat(sprintf("%s: total=%d, missing_InChIKey=%d, with_name=%d, with_cas=%d\n",
                 db, total, missing_key, missing_key_with_name, missing_key_with_cas))
  }
}
```

或者直接查 XLSX 文件（如果 SQLite 数据不完整）：

```r
library(rio)
svhc <- import("inst/svhc.xlsx")
nrow(svhc)
sum(is.na(svhc$InChIKey) | svhc$InChIKey == "")
# 如果有 substance_name 列：
svhc$`Substance name`[is.na(svhc$InChIKey) | svhc$InChIKey == ""]
```

#### 任务 2：对缺失 InChIKey 的条目做类型分类

对每个数据库，列出缺失 InChIKey 的条目，分析其名称/描述，判断属于以下哪种类型：

| 类型 | 特征 | 能否自动匹配 |
|---|---|---|
| `structural_family` | 名称描述了一个结构族（如"nonylphenol, branched and linear"），有明确母核和碳链范围 | 可通过结构规则匹配 |
| `cas_group` | 有 CAS 号范围或组 CAS 列表 | 可通过 CAS 列表匹配 |
| `identifier_only` | 只有名称/CAS，无结构描述 | 仅名称相似度匹配，需人工复核 |
| `uvcb` | UVCB（未知或可变成分的复杂反应产物） | 通常无法自动判定 |
| `mixture` | 混合物 | 需成分信息才能判定 |
| `salt` | 盐类 | 通常可通过反离子判断 |
| `fiber` | 纤维类 | 通常无标准 SMILES |
| `unknown` | 无法从名称/描述判断类型 | 需人工复核 |

以 SVHC 为例，分析结果应类似：

```r
# 伪输出示例
svhc_missing_analysis <- data.frame(
  substance_name = c("4-Nonylphenol, branched and linear",
                     "Medium-chain chlorinated paraffins (C14-C17)",
                     "Nonylphenol",
                     "Lead compounds",
                     ...),
  cas_no = c(NA, NA, "25154-52-3", "7439-92-1", ...),
  group_type = c("structural_family",
                 "structural_family",
                 "structural_family",
                 "salt",
                 ...),
  possible_rule_type = c("para_alkylphenol_c9",
                         "chlorinated_alkane_c14_c17",
                         "alkylphenol_c9_broad",
                         "lead_salt",
                         ...),
  notes = c("需要对位C9链+酚OH+允许支化",
            "需要C14-17范围+氯化度范围",
            "过于宽泛，可能需人工复核",
            "可通过铅离子判断",
            ...)
)
```

#### 任务 3：统计各 structural_family 条目的规则复杂度

对每个 `structural_family` 条目，评估：

- **母核是否明确**：苯酚、全氟羧酸、邻苯二甲酸酯、氯化石蜡、胺类等 → 规则容易定义
- **侧链/取代基是否有明确范围**：碳链长（固定/范围）、支化是否允许、氯化度范围 → 规则需要参数化
- **需要排除的衍生物是否明确**：壬基酚乙氧基化物、TNPP 不是普通壬基酚 → 规则需要 exclusion list
- **是否可能有 PubChem 可解析的具体成员**：部分壬基酚异构体有独立 CAS 和 InChIKey → 可以精确 InChIKey 补充

给每条 `structural_family` 打一个复杂度评分（1-5）：

| 评分 | 含义 |
|---|---|
| 1 | 母核固定、链长精确、无排除项 → 规则简单，如对壬基酚 |
| 2 | 母核固定、链长有范围、有少量排除项 → 规则中等，如氯化石蜡 |
| 3 | 母核较明确但有较多例外 → 规则复杂，需多条件 |
| 4 | 母核模糊或覆盖多种不同结构 → 规则难以精确 |
| 5 | 几乎无法自动判定 → 仅做名称候选 |

#### 任务 4：输出分析报告

将以上分析整理成一份 Markdown 报告，保存在项目根目录：

`analysis_group_substances_YYYYMMDD.md`

报告应包含：

1. 各数据库缺失 InChIKey 条目的数量和百分比
2. 每个缺失条目的类型分类和规则复杂度评分
3. **高价值规则候选清单**：复杂度 ≤ 2 的 structural_family 条目，这些是优先可以后续实现结构匹配的
4. **CAS 列表候选清单**：有明确 CAS 范围的条目，这些只需维护 CAS 列表即可匹配
5. **需人工复核的条目**：复杂度 ≥ 4 的条目，这些应标记为 `requires_manual_review`
6. 初步评估：如果对所有 structural_family 条目建规则，工作量大约是多少

#### 任务 5：评估性能影响

如果最终实施组匹配，实习生应对性能影响做初步估算：

假设：
- 检测结果有 N 个化合物
- 法规组规则有 M 条
- 用三级过滤（anchor category → SMARTS → 图验证）vs 逐条规则 O(N×M) 暴力匹配

请估算：
- 如果 M = 100 条规则，N = 1000 个化合物，最坏情况需要多少次 SMARTS 匹配？
- 如果 anchor category 分类后，同一 category 最多只有 10 条规则，最坏情况需要多少次？
- 用 `rcdk` 做分子图解析的平均耗时大约是多少毫秒/分子？

### 10.4 实习生分析验收标准

1. 能列出每个数据库缺失 InChIKey 的条目数量和比例
2. 能对每个缺失条目做 group_type 分类
3. 能识别出 3-5 条高价值规则候选（complexity ≤ 2）
4. 能输出结构化的 `analysis_group_substances_YYYYMMDD.md` 报告
5. **不实施任何代码**，只做分析和报告

### 10.5 后续实施方向的初步建议（供实习生参考，不强制）

如果分析结果显示确实有足够多可建规则的 structural_family 条目（如 20 条以上），后续可以考虑：

**方案 A（推荐）：三级过滤 + 结构规则**

1. 按结构母核（anchor category）对规则分类
2. 用预编译 SMARTS automaton 对检测化合物做类别粗筛
3. 对候选化合物用分子图引擎（rcdk）做严格结构约束验证
4. 输出证据分级：exact_inchikey > group_structure > group_cas_list > candidate_name > unresolved

**方案 B（备选）：CAS 列表 + 名称相似度**

- 对有组 CAS 列表的条目，维护 `group_cas_list.csv`
- 对无结构的条目，用名称相似度做候选标记（不做自动判定）
- 优点是实现简单；缺点是覆盖不全（很多组物质没有完整 CAS 列表）

**不推荐的方案：**

- 纯名称子串匹配：容易误报（如把"nonylphenoxy ethanol"匹配为"nonylphenol"）
- 枚举所有异构体 InChIKey：实际上不可行（壬基酚 C9 异构体数量巨大）
- PubChem 批量查询组物质的成员：PubChem 不支持这种语义查询

### 10.6 与现有工作的关系

这条工作线和 INTERN_HANDOFF.md 其他工作是**相对独立**的。它不需要等增量更新系统完成，可以在任意阶段独立推进。但需要依赖 `assign_toxicity()` 的匹配逻辑框架，因此在实施前需要确保：

1. SQLite 数据库中保留了缺失 InChIKey 的原始法规条目（raw 表或不过滤的查询）
2. 匹配逻辑框架支持返回非布尔型的证据（不只是 Y/N，还需要 match_type、confidence 等）

建议先完成分析报告，再决定何时实施、实施哪个方案。

## 12. 建议验收命令

开发过程中建议常用：

```r
# 只做计划，不写数据库
update_databases_incremental(dry_run = TRUE)

# 在临时数据库上测试 apply，不要先动真实数据库
update_databases_incremental(
  db_path = tempfile(fileext = ".db"),
  source_dir = "tests/testthat/fixtures/update_sources",
  dry_run = FALSE,
  apply = TRUE
)

# 查看更新历史
get_update_history()

# 查看详细变更
get_detailed_changes()
```

最终检查：

```r
devtools::check()
```

或：

```bash
R CMD check .
```

## 13. 当前最重要的结论

这个项目下一步不要继续堆 Shiny UI 或单个数据库的临时下载脚本。应优先完成数据库更新底层能力：

1. SQLite schema 与实际数据一致；
2. 每条法规记录有稳定身份；
3. 每次更新能准确知道新增、删除、修改；
4. 更新默认 dry-run；
5. apply 有备份、事务和审计；
6. PubChem/LABTOOLS 只做增量 metadata；
7. `assign_toxicity()` 只负责匹配，不默认联网更新。

完成这些后，FCMSafety 才能成为一个可长期维护、可被用户信任的食品接触材料风险筛查工具。
