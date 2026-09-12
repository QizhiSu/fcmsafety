# 代码地图（code map）

给"想改点什么"的人用。**结论先行**：绝大多数需求只在 1-2 个文件里，
不必通读 16 个 R 文件。

> 行号会随后续改动漂移。**优先按函数名搜索**，行号只用来估算距离。
> 每个文件内部都有 `# ---- 分区名 ----` 标记，用编辑器的"转到符号"或
> 直接搜分区名，比按行号跳转可靠。

---

## 1. 三个入口，覆盖 90% 的使用场景

| 我想… | 调这个 | 在 |
|---|---|---|
| **把一批物质从「名称 + SMILES」一路跑到报告** | **`run_screening()`** | **`run_screening.R`** |
| 只要法规匹配那一步（已有 InChIKey） | `assign_toxicity()` | `direct_sql_toxicity.R` |
| 只要补结构标识那一步 | `prepare_input()` | `prepare_input.R` |
| 看库里现在有什么 | `launch_database_inspector()` | `database_inspector_app.R` |

```r
# 用法一：一句话跑完（推荐，P1-1 新增）
library(fcmsafety)
res <- run_screening(my_data, output_file = "report.xlsx",
                     online = FALSE)          # 默认离线；不联网、不改库
print_prepare_report(res)                     # 看哪些行没补上，为什么

# 用法二：手工两步（想在中途检查补全结果时用）
x <- prepare_input(my_data)        # 名称+SMILES -> 补 InChIKey/CID/分子式
print_prepare_report(x)
res <- assign_toxicity(x, output_file = "report.xlsx")
# 两种用法都输出 4 个 sheet：Results / Summary / Unassigned / Issues
```

---

## 2. 端到端数据流

```
   官方源（ECHA / EUR-Lex / IARC / Wikipedia）
        │
        │  download_sources.R        只负责"抓下来存成 xlsx"
        ▼
   inst/*.xlsx
        │
        │  update_other_dbs.R        CMR / IARC / EU_SML 的调度
        │  auto_update_svhc.R        SVHC 单独一条线（InChIKey 不唯一，特殊处理）
        ▼
   incremental_update.R  ←── 四个库共用的公共流水线
        │   对齐库表列 → 回填老物质 → 只对新增查 PubChem → diff → 确认 → 入库
        ▼
   inst/fcmsafety.db（SQLite）
        │
        │  direct_sql_toxicity.R     按 InChIKey 匹配 + 定级 I–V
        ▼
   toxicity_report_export.R  →  report.xlsx
```

**记住一件事**：`incremental_update.R` 是四个库共用的。改它的行为会同时
影响 SVHC 之外的三个库 —— 这是本仓库最容易"改一处炸一片"的地方。

---

## 3. 文件地图

### 主链路（按数据流顺序）

| 文件 | 行数 | 职责 | 关键函数 |
|---|---:|---|---|
| `download_sources.R` | ~760 | 抓官方原始数据存 xlsx。**最易失效**（官网改版就废） | `download_svhc` `download_clp` `download_eu_sml` `download_iarc` |
| `update_other_dbs.R` | ~700 | CMR / CMR_suspect / IARC / EU_SML 四条更新线 + 总调度 | `update_*_auto()` `update_database_auto()` |
| `auto_update_svhc.R` | ~820 | SVHC 单独一条线（主键三级回退，不迁公共流水线） | `update_svhc_auto()` |
| `incremental_update.R` | ~990 | 公共流水线：回填 / 补新增 / diff / 入库 / 变更明细 | `run_incremental_update()` |
| `sqlite_database_manager.R` | ~900 | 库底座：连接 / 建表 / xlsx 迁移 / 装载 | `get_db_connection()` `migrate_xlsx_to_sqlite()` |
| `direct_sql_toxicity.R` | ~1190 | **核心**：查库匹配 + 定级 I–V + 组条目汇总 | `assign_toxicity()` `compute_toxicity_levels()` |
| `run_screening.R` | ~175 | **总控入口**：把上游补全与下游匹配串成一次调用。只调度，不含逻辑 | `run_screening()` |
| `group_membership.R` | ~1600 | 组条目判定（IARC 类条目 / CMR·SVHC 的 UVCB） | `assign_group_membership()` |
| `prepare_input.R` | ~720 | 补全结构标识（离线 CDK 算 InChIKey） | `prepare_input()` |
| `toxtree.R` | ~510 | 调 Toxtree CLI 出 Cramer 分类 | `run_toxtree()` |
| `toxicity_report_export.R` | ~240 | 导出带样式的 Excel 报告 | `export_toxicity_report()` |

### 支撑

| 文件 | 行数 | 职责 |
|---|---:|---|
| `database_inspector_app.R` | ~2960 | Shiny 查看器。**整个包在一个函数里**，只有 1 个导出函数。一键操作面板见 ADR 0011 |
| `update_run_guard.R` | ~180 | 一键更新的守卫与判读：防积压点击 / 预演结果判读 / 逐库跑一轮。**纯函数，有测试覆盖**，改更新按钮先看这里 |
| `fcmsafety_main.R` | ~350 | 建库 / 状态 / 体检三个面向用户的入口 |
| `update_history_audit.R` | ~520 | 读更新账本，回答"这条数据什么时候进来的" |
| `manual_list_check.R` | ~240 | 探测人工放进 `inst/` 的新清单并消费掉 |
| `simple_migration.R` | ~160 | 旧线兼容壳，转发给 `migrate_xlsx_to_sqlite()` |
| `globals.R` | ~36 | 只声明全局变量消 check NOTE，无运行时作用 |

### 文档与决策

| 位置 | 内容 |
|---|---|
| `CONTEXT.md` | 术语表。改代码前先对齐术语（如"法规库"不叫"数据库"） |
| `docs/adr/0001`–`0011` | 架构决策记录。**每个 ADR 都写了"为什么不用另一种做法"**，改相关代码前必读 |
| `GROUP_MEMBERSHIP_NEXT_STEPS.md` | 组条目判定的待办清单 |
| `INTERN_HANDOFF.md` | 交接说明 |

---

## 4. 常见任务 → 去哪改

| 想做的事 | 改哪 | 注意 |
|---|---|---|
| 调整毒性等级规则（如 SML 阈值） | `direct_sql_toxicity.R` 的 `compute_toxicity_levels()` 与顶部 `.cmr_*_h_codes` | 纯函数，先看同文件的等级注释块 |
| 报告加一列 / 换配色 | `toxicity_report_export.R` 的样式区 | 只动 `.level_fills` / `.style_table()` |
| 加一个新法规库 | ① `inst/fcmsafety_schema.sql` 加表 ② `download_sources.R` 加抓取 ③ `update_other_dbs.R` 的 `DB_SOURCES` 注册表加一条（名单与总调度分发自动派生）④ `incremental_update.R` 的 `db_col_candidates` 登记列名差异 | ③④ 漏了会静默不生效；`manual_list_check.R` 的 file_map 与 inspector 的 `UPDATE_DBS` 仍是独立登记点，见架构评审候选 3 |
| 某官网抓不到了 | `download_sources.R` 对应函数 | 先读函数头注释，里面写了当前可用路径和回退顺序 |
| Cramer 分级不对 | `toxtree.R` | **改完必须重装包**；Toxtree 以 cwd 定位 `ext/` |
| 查为什么某物质"查不到" | `direct_sql_toxicity.R` 的 `query_*_data()` + Issues 表 | 先看 Issues sheet 有没有查询失败记录 |
| IARC 组条目判定太严/太松 | `group_membership.R` 的 `screen_iarc_groups()` | 两条护栏只能作用于 `element` 层，别扩大范围 |
| Shiny 界面加个按钮 | `database_inspector_app.R` 搜 `# ---- UI` 与 `# ---- 一键操作面板` | 文案要同时进 i18n 文案表 |
| 报告说"数据库连接失败" | `sqlite_database_manager.R` 的 `.resolve_db_path()` | **必须在项目根目录跑**，否则会连到别的库或建空库 |

---

## 5. 改代码时不能破的约定

这几条都是踩过坑换来的，违反后表现为"测试还绿但结果错了"。

1. **回填靠 InChIKey 匹配，不是名字。** 别引入按名称匹配的主逻辑。
2. **"查不到" ≠ "安全"。** 全无证据的行 `Toxic_level` 留空 `-`，不冒充 I 级
   （I 级的唯一来源是 `1.8 < SML <= 60`）。
3. **查询失败必须可见。** `query_*()` 出错时挂 `attr(x, "query_error")`，
   由上层收进 Issues 表。**不要改成"查不到就 `stop()`"** —— 测试夹具只建
   `chemicals` 表，会被直接打断。
4. **`group_membership` 默认 FALSE。** 开启会改变既有结果，不能默默默认打开。
5. **元素层只是必要条件。** 元素命中不等于归属确认，护栏必须保留。
6. **新增毒性列必须落在 `Cramer_rules:Toxic_level_basis` 区间内**
   （`relocate()` 的范围），否则列顺序会乱。
7. **新增 roxygen 块以 `#' @encoding UTF-8` 结尾，且必须在块尾**，
   护栏是 `tests/testthat/test-rd-docs.R`。
8. **不要用 `git stash` / `git rm`**（safe-delete 钩子会清空 `.git/objects`）。
9. **单库 `update_*_auto()` 默认 `source="local"`（读 inst/ 本地文件，不联网），
   总调度 `update_database_auto()` 默认 `source="download"`（联网下载）**——
   语义相反是有意的：一键更新就该联网，单独跑某个库则默认离线。别"顺手统一"。
10. **SVHC 的 auto 回退链只有 echa -> local**。Wikipedia 镜像非官方源，
    仅显式 `source="wikipedia"` 时可用，不要加回自动链。

---

## 6. 自检怎么跑

在项目根目录（`C:\Users\13432\WorkBuddy\2026-08-31-14-16-57\fcmsafety`），
用 VSCode 的 Git Bash 终端：

```bash
# 测试（153 个用例，约 20 秒）
NOT_CRAN=true "/c/Program Files/R/R-4.6.1/bin/Rscript.exe" tools/run_tests.R

# R CMD check（只做静态检查，examples/tests 显示 SKIPPED 是正常的）
LC_ALL=zh_CN.UTF-8 "/c/Program Files/R/R-4.6.1/bin/Rscript.exe" tools/run_check.R

# 分区注释没插进 roxygen 块（改完注释跑一下，很快）
"/c/Program Files/R/R-4.6.1/bin/Rscript.exe" tools/check_section_markers.R
```

**判读标准**：`run_tests.R` 要 0 failed / 0 error；`run_check.R` 允许 1 个
WARNING（`code files for non-ASCII characters`，中文常量，接受不修），
**不允许 ERROR**。

> 本机 R 在 WorkBuddy 终端里退出时常报 exit 139 / 0xC0000005，那是沙箱
> 注入的 `tsbx.dll` 干的，不是代码问题。**看落盘的产物和打印的结果判成败，
> 不看退出码。**
