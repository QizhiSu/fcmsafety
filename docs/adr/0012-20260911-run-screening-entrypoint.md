# 0012 - 新增 `run_screening()` 总控入口，以及它**刻意不暴露**的那两个参数
> **Status: Superseded（2026-09-14）** — run_screening() 已于 2026-09-14 移除；入口回归 assign_toxicity()（数据先经 labtools 提取）。本文保留为历史记录。

- 状态：已采纳
- 日期：2026-09-11
- 相关：[0004](0004-20260910-offline-identity-completion.md)（离线补全结构标识）、[0008](0008-20260910-report-export-and-failure-visibility.md)（失败可见化）

## 背景

核心链路（输入 → 补结构 → 法规匹配 → 定级 → Excel 报告）早就打通了，但**没有一个入口**。
用户要手写两步，还得自己把第一步的返回值接给第二步：

```r
x <- prepare_input(my_data)     # 名称 + SMILES → 补 InChIKey
res <- assign_toxicity(x, output_file = "report.xlsx")
```

`0911项目后续工作交接表.md` 把这条列为 P1-1，并点了名：
> 这是**用户体验上最大的缺口**。新增一个薄封装即可，把两步串起来 + 传参透传。
> **注意**：`assign_toxicity()` 是确定性匹配函数，**不应默认联网、不应默认改库**。

## Decision 1：只做调度，不复制逻辑

`run_screening()` 内部就只有三件事：调 `prepare_input()`、判读补全报告、调
`assign_toxicity()`。补全逻辑仍在 `prepare_input.R`，匹配与定级仍在
`direct_sql_toxicity.R`。

**没有把两步合并进 `assign_toxicity()`**，虽然那样调用更短。理由是
`assign_toxicity()` 的契约是"输入必须含 `InChIKey` 列"（有测试守着：
缺列时报错信息必须提 `InChIKey`），已经有外部调用方在按这个契约用。改它等于
把"补结构"这个可能联网的动作塞进一个被声明为确定性的函数里，方向反了。

同理，也没有在 `run_screening()` 里重写一遍列名识别之类的逻辑 —— 两份实现
必然漂移，而漂移出来的偏差会变成"看起来同样有说服力的错误结论"
（交接表红线「审计一律调真实函数，不要旁路复刻」的同一个道理）。

## Decision 2：**不暴露** `check_updates` / `auto_update`

`assign_toxicity()` 有这两个参数，会调用 `check_manual_lists()` 探测并应用
人工放进 `inst/` 的新清单文件 —— 那是**写库语义**。

`run_screening()` 把它们**完全从签名里去掉**，不设成"暴露但默认 FALSE"。

| 方案 | 结果 | 为什么不选 |
|---|---|---|
| **不暴露**（采纳） | 调用方无论如何都碰不到写库路径 | — |
| 暴露但默认 FALSE | 签名更长，且给误用开了口子：翻一次 `?run_screening` 就会看到那俩参数，然后 `auto_update = TRUE` 一行就能改库 | 安全边界应该是"够不着"，不是"要记得别按" |
| 暴露并文档警告 | 同上，且依赖文档被读到 | 依赖人的记忆做安全护栏，历史上没成功过 |

有测试守着这条：`test-run-screening.R` 的「安全边界」用例直接检查
`formals(run_screening)` 里不含这三个名字（`enrich` 一并挡掉）。

## Decision 3：只在一个极端情况下告警

**全部行都没解析出 `InChIKey` 时** `warning()`，部分未解析时不响。

原因：全行未解析时，`assign_toxicity()` 会照常返回一张**全是 `"-"` 的表**。
而 `-` 的含义是"无证据"，在报告里跟"查过了、确实干净"长得一模一样 ——
用户会把"工具没认出这些物质"读成"这些物质都安全"。这是交接表反复强调的
静默失败风险（同 0008 的「失败可见化」）。

反过来，部分未解析是常态（`prepare_input()` 本来就有六级阶梯，末级才联网），
天天喊狼来了，真出事那次就没人看了。所以阈值卡在 `unresolved >= total`。

## Considered Options

- **合并成一步 `assign_toxicity(data_with_smiles)`**：破坏既有契约，且把联网
  可能塞进确定性函数。拒绝。
- **在 Shiny 页面加一个「开始筛查」按钮**：当前优先解决的是**脚本/命令行
  用户**走通全流程。页面上的入口在图库查看器里本来就少见，且按钮的输入是
  文件上传，是另一个话题。暂缓。
- **返回 list 把中间结果也交出去**：调用方要改写法（`res$result` 而不是
  `res`），对"最小改动"目标是负收益。改成把补全报告挂在
  `attr(res, "prepare_report")`，有 `print_prepare_report()` 现成可读。
  拒绝返回 list。

## Consequences

- 用户从"两步 + 手工接力"变成一次调用；手工两步的写法仍然可用、未被废弃。
- 补全报告不占返回表的列，但要通过 `attr()` 或 `print_prepare_report()` 取，
  不能直接 `res$prepare_report`。
- **本函数不解决**：CMR A 类组条目漏报（P1-4）、UVCB 错误结构键（P1-2）、
  同一 InChIKey 共用导致的误分级（P1-3）。这三件事都在匹配层，改法是另一
  话题，不因为有了总控入口就消失。
- `online = TRUE` 时仍会逐行 `Sys.sleep(delay)`，100 行大约多出半分钟以上
  的网络等待。默认 `FALSE` 就是要让这个代价必须被显式选择。

## 未决问题

- `output_file = NULL` 时只返回不落盘，但**没有任何进度提示**。批量跑几百行
  时用户不知道还要多久。要不要加进度输出，等有人真的这么用再说。
