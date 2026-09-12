# 0011 - 联网更新按钮改成两段式，以及同步长任务下「输出通、输入不通」的实测结论

- 状态：已采纳
- 日期：2026-09-11
- 相关：[0008](0008-20260910-report-export-and-failure-visibility.md)（失败可见化）、[0010](0010-20260910-iarc-group-entry-identification.md)（仍在交接的组条目线）

## 背景

用户点数据库页面的「🚀 联网更新全部」后，**整页按钮全部没反应，且再也没恢复**，
最后只能关掉 R。

先说结论：**数据没坏，页面也没坏，是 R 被占死了，而且一占就是好几轮。**

排查到的三件事：

| 检查项 | 结果 |
|---|---|
| `update_history` | 15:02:51 / 15:03:17 / 15:05:26 三条 `incremental_auto`，`success = 1` |
| `pragma integrity_check` / 外键 | ok / 0 条违规 |
| 各表行数 | cmr 443 / cmr_suspect 451 / iarc 896 / eu_sml 681 / svhc 294 / chemicals 2606 —— 与当天提交时一致 |
| `inst/clp.xlsx` mtime | **15:30**（DB 最后写入 15:05）→ 之后又有一轮更新从头上跑起来了 |

### 那三条写库记录不是网页按钮写的（纠正一个误判）

`backups/` 里有 `...150251.db` / `150317` / `150525`，看着像按钮在写。但
`.workbuddy/apply_real_db.log` 的 mtime 是 **15:08**，而写这些日志的
`tools/_tmp_apply81.R` 显式传的是 `max_auto_changes = Inf`；网页按钮走的是
`update_database_auto()` 的默认 `max_auto_changes = 20`，
**+101 / ~349 这个量级根本过不了闸门**，不可能写进去。

结论：网页按钮那一轮**什么都没写成**（全被 `max_auto_changes` 拦下），
只是把页面卡死、白跑了一轮下载。先前"按钮静默跳过了安全阀"的说法是错的 ——
安全阀在 `interactive = FALSE` 下对任何调用方都生效，浏览器翻不过去。

### 真正的机制：输出通、输入不通

用两个探针 app 实测（`Rscript --vanilla` 起 Shiny，浏览器端每 250ms 采样进度条宽度，
落盘时间线比对）：

| 浏览器采样 | 进度条 | R 侧日志 |
|---|---|---|
| 221ms | 10% | 21:04:55 第 1 步 |
| 1224ms | 20% | 21:04:56 第 2 步 |
| 5223ms | 60% | 21:05:00 第 6 步 |
| 9473ms | 100% | 21:05:04 第 10 步 |

纯 `Sys.sleep(1)` 的同步阻塞循环里，`incProgress()` 和 `shinyjs::html()` 的消息
**每 1 秒就送到浏览器一次**，与 R 侧时间戳逐点对齐。

- **出站**消息不依赖 R 主线程：httpuv 的 libuv 事件循环跑在**后台线程**，
  写 websocket 不需要 R 空下来。
- **入站**消息必须回到 R 主线程才能变成 `input$xxx`。R 在跑更新时，点击全在排队，
  任务结束后才集中补送。

于是「按钮全哑」不是按钮坏了，是**没人接电话**；而任务一结束，排队的点击立刻
又触发一轮 —— 所以用户永远等不到"页面恢复"的那一刻。
这也解释了为什么同一个按钮点一次却跑了两轮。

## Decision 1：写库走两段式（预演 → 确认写入）

| 阶段 | 调用 | 作用 |
|---|---|---|
| ① 预演 | `update_database_auto(auto_apply = FALSE)` | 只算不写，返回按库汇总表 |
| ② 写入 | `update_database_auto(auto_apply = TRUE)` | 点「确认写入」后才跑 |

**为什么 `auto_apply = FALSE` 是真 dry**：`run_incremental_update()` 在
`write_changes_to_db()` 之前就返回 `done(FALSE, NULL, "Dry run (no apply)")`
（`incremental_update.R:1575`），业务表和账本都不写。

**代价（明确接受）**：第二段会重跑。dry-run 阶段新增行没进库，
`backfill_meta_from_db()` 只查同一张业务表、对新增行无效，所以第二段的 PubChem
补结构只能重查一次；下载也一样重来。

**为什么不做成"复用第一段下载的文件"**：`source = "local"` 并不是"读我刚下载的文件"，
它读的是 `inst/clp_cmr_meta.xlsx`（旧 meta）/ `inst/candidate_list.xlsx`（人工清单）这类
**另一套文件**；真正想复用得靠 `fetch_*_data(new_file=)`，而 `update_database_auto()`
没暴露这个参数（要动核心 API）。而实测下载并不慢：15:02:51 写 cmr → 15:03:17 写
cmr_suspect（这一步含**再下一次 CLP**），即 CLP 整包下载+解析 ≈ 20 秒，大头是 enrich。
为省 20 秒动核心 API 不划算。

**顺带的好处**：预演表能提前说清"这个库为什么写不了"。原来 `interactive = FALSE` 下
安全阀（有移除条目 / 变更数超上限）返回的 `Cancelled` / `Too many changes` 只写在
日志里，而日志要等整轮结束才显示 —— 表现就是"点了半天什么也没发生"。

## Decision 2：三道防连点，且**不做**「取消」按钮

1. 任务一开始就用 `fcmSetBusy` 自定义消息把 6 个按钮 `disabled`，结束再放开；
2. 服务端按「上一轮结束到现在多久」丢弃请求（`should_drop_run_request`，宽限期 10 秒）——
   这是关键：积压点击在任务结束后才到达，那一刻 `quick_busy` 早已复位，**只看忙不忙挡不住**；
3. `run_quick_task` 结束时用 `on.exit` 复位，而不是顺序执行 ——
   R 控制台 Ctrl+C 抛的是 interrupt，`tryCatch(error = )` 接不住，
   顺序执行会让 `quick_busy` 永久停在 TRUE。

**不做取消按钮**：入站消息进不来，运行中按「取消」根本按不下去，加了就是骗人。
中止只能到 R 控制台 Ctrl+C —— 好在 `on.exit` 保证页面能干净恢复（历史 bug：中断后
四个按钮永久回"上一个操作还在运行中"）。

## Decision 3：判读逻辑抽成包内纯函数并测试

`R/update_run_guard.R`：`should_drop_run_request()` / `summarise_update_run()` /
`translate_run_message()` / `run_db_update_round()`，测试 19 例
（`tests/testthat/test-update-run-guard.R`）。

**为什么必须抽出来**：这些逻辑原本写在 app 的 `server` 闭包里，闭包内部没法单独调用 ——
而"单库失败不中断整轮""各库汇总表要能拼成一张表""预演的 status 恒为 failed 但计数是全的"
恰恰最容易写错、错了又只在页面上表现为"表里少一行"。

其中一条测试专门钉住一个坑：**dry-run 时每个库的 `status` 都是 `"failed"`**
（上游 `done(FALSE, ...)`），用 `status` 判失败会让每个库都显示"运行失败"。
真失败是 `tryCatch` 兜底路径 —— 三个计数列全 NA。

## Decision 4：写入沿用 `max_auto_changes = 20`，不放宽

浏览器里的「确认」只用来放行**已经看过的那份 diff**，不放宽自动上限；
有移除条目时安全阀依然硬拦。理由：这正是"不静默写真库"红线的落点，
放宽等于让一次点击能写进去几百条 `modified`（iarc 常态就是 ~846 条 `modified`，
而这类大规模 modified 十有八九是列映射对不上造成的假变更，该看报告而不是直接写）。

## Considered Options

| 方案 | 为什么没选 |
|---|---|
| 后台进程跑更新（`callr`），页面全程可用 | 本机 `callr/processx/later/promises` 都有，技术上可行。但实测出站消息本来就是实时刷的，同步跑已经能给真进度 —— 引入子进程要处理"后台读的是装进库的旧代码"、日志轮询转发、取消=杀进程，收益不抵复杂度 |
| 在循环里 `later::run_now()` 泵事件循环，换取真取消 | 在 observer 内泵事件循环会引发重入式 reactive flush，属于未定义行为，风险不可控 |
| dry-run 跑在库副本上（`db_path=`），确认后换文件 | 一次跑完、diff 更精确，但要在运行中的 Shiny 下关连接→换文件→重连，失败面太大 |
| 干脆去掉按钮只给命令行 | 用户明确要页面上能更新 |

## Consequences

- **好的**：进度条与滚动日志全程实时；重复点击不再重跑；写库前一定看得到 diff；
  Ctrl+C 后页面能干净恢复；"为什么没写进去"在页面上有明确中文说明。
- **代价**：预演 + 写入两轮，总耗时约翻倍（无变更时第二轮会自动跳过，耗时与原来一致）。
- **已知限制**：运行中无法从页面取消；进度粒度是"库"级（5 步），单库内部的长阶段
  （如 enrich）只能靠滚动日志判断是否还活着。

## 未决问题

是否允许"浏览器里看过 diff 并点确认"之后写入超过 `max_auto_changes = 20` 的变更？
现在是不允许（超上限的库在预演表里标成"不会被写入，需到命令行处理"）。
放宽能让按钮真正好用，但等于把"人看过一次"当作批量写入的授权。
**这是产品语义决策，需要用户拍板后再改。**
