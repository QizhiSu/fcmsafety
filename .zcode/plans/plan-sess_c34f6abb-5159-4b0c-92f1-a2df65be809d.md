## fcmsafety R 包死代码清理计划（彻底清理档）

依据：两个探索代理的全仓审计（19 文件 / 14,732 行 / 190 函数 / 33 导出），每个删除项均有零引用证据。分 3 批执行，每批独立验证 + 提交，所有删除可在 git 历史找回。

### 批次 1 — 纯删（零行为变化）
1. 删 `R/simple_migration.R` 整文件（170 行，3 函数全仓零引用、未导出；真实现 `migrate_xlsx_to_sqlite()` 在 sqlite_database_manager.R 不动）
2. 删 `norm_col_name()`（sqlite_database_manager.R:524，内部函数零调用）
3. 删根目录 `test_four_sources.R`（硬编码实习生机器路径的一次性脚本）
4. 修 toxtree.R:267 运行时报错文案：指向已不存在的 `extract_cid()/extract_meta()` → 改为指引 `prepare_input()`
5. 删 `setup_fcmsafety_database()` 里的第二次冗余迁移调用（initialize_database 内部已委托 migrate，fcmsafety_main.R 再无条件跑一遍）

### 批次 2 — 导出面死代码 + 文档同步
1. 删 `export4toxtree()`（旧手动 GUI 流程，用户已确认不需要；其文档要求先跑已不存在的函数）。同步：删 test-toxtree 对应测试、改 README 与 INTERN_HANDOFF 的旧教程段落
2. 删 `load_databases_sqlite()`（旧线"灌全局环境"遗留，零引用）。先确认 view_svhc/view_cmr 无其他消费者（视图本身留在库里不动）
3. 删 `validate_database_integrity()`（导出但零测试零调用零文档，fix_issues 是空壳）
4. `update_history_audit.R` 瘦身：保留活的 `get_update_history()` 及其依赖；删 5 个顶层入口全死的展示/导出函数（display_* ×3、get_detailed_changes、export_update_history）+ `get_database_statistics`（fcmsafety_main.R 里只是字符串提及，一并清理）
5. 修 README 的 `load_databases()` 幽灵引用
6. roxygenise 重新生成 NAMESPACE/man（导出从 33 降到 ~28）

### 批次 3 — fetch 薄壳
1. 删 4 个 fetch 薄壳（`fetch_cmr_data/fetch_cmr_suspect_data/fetch_iarc_data/fetch_eu_sml_data`，注册表化后生产路径直连 `fetch_source_data`，薄壳只剩测试在调）
2. `test-cmr-screening.R` 改调 `fetch_source_data("cmr"/"cmr_suspect", ...)`
3. 同步 fetch_source_data 与 DB_SOURCES 注释里对薄壳的描述

### 每批门禁（相同）
- `NOT_CRAN=true Rscript tools/run_tests.R` → 0 失败 0 报错
- `LC_ALL=en_US.UTF-8 NOT_CRAN=true Rscript tools/run_check.R` → Status: 1 WARNING（基线）
- roxygenise 已跑、NEWS.md 记录、独立 commit（fcmsafety-v2 分支）

### 明确不做
- EDC 无更新线（功能缺口，不是死代码，另行拍板）
- prepare_input 的 PubChem 裸 HTTP 换公共客户端、GUI 账本层统一（用户未选重构档）
- 核心链路（update_* / run_screening / assign_toxicity / prepare_input）、group_membership、SQLite 通道一概不碰

预计净删 ~600–700 行，导出函数 33 → ~28。预计 3 个提交。