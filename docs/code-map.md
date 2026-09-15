# code-map（fcmsafety 代码地图）

给"想改点什么"的人用。**结论先行**：绝大多数需求只在 1-2 个文件里，
不必通读 17 个 R 文件（约 13,600 行）。

> 行号会随后续改动漂移。**优先按函数名搜索**，行号只用来估算距离。
> 每个文件内部都有 `# ---- 分区名 ----` 标记，用编辑器的"转到符号"或
> 直接搜分区名，比按行号跳转可靠。

---

## 1. 三个入口，覆盖 90% 的使用场景

| 我想… | 调这个 | 在 |
|---|---|---|
| **把一批物质（含 InChIKey）跑完匹配 + 定级 + 报告** | **`assign_toxicity()`** | **`assign_toxicity.R`** |
| **更新法规库**（一键或按库） | **`update_database_auto()`** / `update_*_auto()` | `update_other_dbs.R` |
| **看库里现在有什么 / GUI 筛查** | `launch_database_inspector()` | `shiny_launch.R` → `shiny_ui.R` + `shiny_server.R` |

```r
# 标准流程（数据先经 labtools::extract_meta() 拿到 InChIKey）
library(fcmsafety)
update_database_auto()                       # 可选：联网增量更新法规库
res <- assign_toxicity(data, output_file = "report.xlsx")
# 输出四张表：Results / Summary / Unassigned / Issues；等级 I–V 带 Toxic_level_basis
```

输入只要 `InChIKey` 列（SMILES 可选，供 Toxtree Cramer 分级）。
没有 InChIKey 时用 [labtools](https://github.com/QizhiSu/labtools) 的
`extract_meta()` 从名称/SMILES 提取（走 PubChem，需联网）。

---

## 2. 端到端数据流

```
   官方源（ECHA CHEM / ECHA CLP / EUR-Lex / IARC）
        │
        │  download_sources.R        只负责"抓下来存成 xlsx"
        ▼
   inst/*.xlsx
        │
        │  update_dbs.R              单一文件整合全部更新链路：
        │    DB_SOURCES 注册表       (CMR / IARC / EU_SML / SVHC)
        │    标准化函数              (normalize_cmr_df / normalize_svhc_df ...)
        │    通用流水线              (run_incremental_update)
        │    SVHC 专用流水线         (update_svhc_auto)
        ▼
   inst/fcmsafety.db（SQLite）
        │
        │  assign_toxicity.R        按 InChIKey 匹配 + 定级 I–V
        ▼
   report_export.R  →  report.xlsx
```

---

## 3. 文件地图（17 个文件，~13,600 行）

### 主链路（按数据流顺序）

| 文件 | 行数 | 职责 | 关键函数 |
|---|---:|---|---|
| `download_sources.R` | ~840 | 抓官方原始数据存 xlsx。**最易失效**（官网改版就废） | `download_svhc` `download_clp` `download_iarc` `download_eu_sml` |
| `update_dbs.R` | ~3370 | 单一文件整合三条更新链路：DB_SOURCES 注册表 + CMR/IARC/EU_SML 标准化与取数 + 通用增量流水线 + SVHC 专用流水线 | `update_database_auto()` `update_source_auto()` `run_incremental_update()` `update_svhc_auto()` |
| `database.R` | ~1240 | 底座：连接/建表/迁移/装载 | `get_db_connection()` `migrate_xlsx_to_sqlite()` |
| `assign_toxicity.R` | ~1160 | **核心**：查库匹配 + 定级 I–V + 组汇总 | `assign_toxicity()` `compute_toxicity_levels()` |
| `report_export.R` | ~250 | 导出带样式的 Excel 报告 | `export_toxicity_report()` |

### 筛查辅助

| 文件 | 行数 | 职责 |
|---|---:|---|
| `toxtree.R` | ~810 | Toxtree：rJava 快速路径 + CLI 回退 + jar 按需下载 |

### GUI（三个文件）

| 文件 | 行数 | 职责 |
|---|---:|---|
| `shiny_launch.R` | ~130 | `launch_database_inspector()`：端口/浏览器/启动 |
| `shiny_ui.R` | ~1480 | `fcm_app_ui()`：纯静态 UI 拼装（CSS/JS/布局） |
| `shiny_server.R` | ~1600 | `fcm_app_server()`：全部响应式逻辑（i18n/主表/一键操作/筛查面板） |

### 支撑

| 文件 | 行数 | 职责 |
|---|---:|---|
| `main.R` | ~220 | 建库 / 状态两个用户入口 |
| `manual_lists.R` | ~220 | 探测人工放进 `inst/` 的新清单并消费掉 |
| `update_guard.R` | ~220 | 一键更新的守卫与判读（纯函数，有测试覆盖） |
| `update_audit.R` | ~300 | 读更新账本（`get_update_history()`） |
| `globals.R` | ~35 | 声明全局变量消 check NOTE |

### 文档与决策

| 位置 | 内容 |
|---|---|
| docs/adr/0001–0012 | 架构决策记录。**每个 ADR 都写了"为什么不用另一种做法"** |
| `INTERN_HANDOFF.md` | 交接说明（历史版本，部分内容已随精简过时） |

---

## 4. 常见任务 → 去哪改

| 想做的事 | 改哪 | 注意 |
|---|---|---|
| 调整毒性等级规则（如 SML 阈值） | `assign_toxicity.R` 的 `compute_toxicity_levels()` 与顶部 `.cmr_*_h_codes` | 纯函数 |
| 报告加一列 / 换配色 | `report_export.R` 的样式区 | 只动 `.level_fills` / `.style_table()` |
| 加一个新法规库 | ① `inst/fcmsafety_schema.sql` 加表 ② `download_sources.R` 加抓取 ③ `update_dbs.R` 的 `DB_SOURCES` 注册表加一条 ④ `db_col_candidates` 登记列名差异（在同一文件内） | ③④ 漏了会静默不生效 |
| 某官网抓不到了 | `download_sources.R` 对应函数 | 先读函数头注释（回退顺序在里面） |
| Cramer 分级不对 | `toxtree.R` | **改完必须重装包** |
| 查为什么某物质"查不到" | `assign_toxicity.R` 的 `query_*_data()` + Issues 表 | 先看报告的 Issues sheet |
| Shiny 界面加个按钮 | UI 在 `shiny_ui.R`，逻辑在 `shiny_server.R` | 文案要同时进 i18n 文案表 |
| 报告说"数据库连接失败" | `database.R` 的 `.resolve_db_path()` | **必须在项目根目录跑** |

---

## 5. 改代码时不能破的约定

这几条都是踩过坑换来的，违反后表现为"测试还绿但结果错了"。

1. **回填靠 InChIKey 匹配，不是名字。** 别引入按名称匹配的主逻辑。
2. **"查不到" ≠ "安全"。** 全无证据的行 `Toxic_level` 留空 `-`，不冒充 I 级
   （I 级的唯一来源是 `1.8 < SML <= 60`）。
3. **查询失败必须可见。** `query_*()` 出错时挂 `attr(x, "query_error")`，
   由上层收进 Issues 表。
4. **IARC 组条目判定引擎与 (see X) 别名解析已整体删除（2026-09-15）**，
   历史版本在 wip/consolidation-draft 与 git 历史。
5. **元素层只是必要条件。** 元素命中不等于归属确认，护栏必须保留。
6. **新增 roxygen 块以 `#' @encoding UTF-8` 结尾，且必须在块尾**，
   护栏是 `tests/testthat/test-rd-docs.R`。
7. **不要用 `git stash` / `git rm`**（safe-delete 钩子会清空 `.git/objects`）。
8. **单库 `update_*_auto()` 默认 `source="local"`（离线），总调度
   `update_database_auto()` 默认 `source="download"`（联网）**——语义相反是有意的。
9. **SVHC 取数链只有 echa -> local**。Wikipedia 镜像源已整体删除，不要加回来。
10. **输入筛查必须有 InChIKey 列**（labtools::extract_meta() 提取）。
    包内已无离线 CDK 推导路径（2026-09-14 移除，见 wip/consolidation-draft 分支）。

---

## 6. 自检怎么跑

在项目根目录（含 inst/ 的目录）的 UTF-8 终端里：

```bash
# 测试（270 个用例）
NOT_CRAN=true Rscript -e "pkgload::load_all('.'); testthat::test_dir('tests/testthat')"

# R CMD check（只做静态检查，examples/tests 显示 SKIPPED 是正常的）
LC_ALL=en_US.UTF-8 R CMD check .

# 检查分区注释是否插进 roxygen 块
Rscript -e "source('R/check_section_markers.R')"   # 如果有这个脚本的话
```

**判读标准**：测试要 0 failed / 0 error；`R CMD check` 允许 1 个
WARNING（`code files for non-ASCII characters`，中文常量，接受不修），
**不允许 ERROR**。
