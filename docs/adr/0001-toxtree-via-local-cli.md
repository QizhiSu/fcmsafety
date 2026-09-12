# 通过本地 headless CLI 集成 Toxtree，而非 REST API 或 rJava 内嵌

工作流需要在 R 内完成 Cramer 分类（此前依赖手动操作 Toxtree GUI）。我们选择用 `system2()` 调用 Toxtree 官方支持的无界面模式（`java -jar Toxtree.jar -n -i in.csv -o out.csv -m <module>`），而非调 OpenTox/AMBIT REST 服务或经 rJava 进程内加载 Toxtree 类。理由：包的设计哲学是本地 SQLite + 确定性匹配、不默认联网（见 INTERN_HANDOFF），REST 依赖一个年头已久的外部服务且要把化学清单发到外部；rJava 内嵌依赖 Toxtree 内部 Java API，脆弱且难排错。CLI 是官方文档明确支持的调用面。

## Considered Options

- **本地 headless CLI（选定）**：离线、确定性、官方文档支持；代价是本机须装 Java。
- **OpenTox/AMBIT REST API**：免装 Java，但服务可用性无保证、数据外发、断网不可用。
- **rJava 进程内调用**：无进程开销，但依赖内部 API，升级 Toxtree 版本即碎。

## Consequences

- Toxtree 以**当前工作目录**定位 `ext/` 模块目录（不是 jar 所在目录），因此 CLI 必须在应用目录下启动（代码中用临时 `setwd()` 实现，见 `R/toxtree.R` 的 `.run_toxtree_cli()`）。
- CLI 输出的结果列名是 `Cramer rules`（空格）；GUI 工作流中的 `Cramer.rules` 其实是 `read.csv(check.names=TRUE)` 的自动改名。`run_toxtree()` 显式归一化为 `Cramer.rules`，两种读取方式下均兼容。
