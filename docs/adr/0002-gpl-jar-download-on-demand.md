# GPL 的 Toxtree 不随 MIT 包分发，改为按需下载到用户缓存

Toxtree 以 GPL 2.0 分发（zip 约 81 MB），fcmsafety 是 MIT license——把 jar 打进 `inst/` 随包分发会造成 license 冲突且体积失控。决策：jar 不入包、不入 git，首次运行 `run_toxtree()` 时自动从 SourceForge 官方源下载并解压到包外用户缓存目录（`tools::R_user_dir("fcmsafety", "cache")`），之后复用；用户也可用 `jar_path` 指向自己已有的 Toxtree 安装。用户从官方源自取 GPL 程序、包只做自动化，不构成"分发"。

## Consequences

- 首次运行需要联网；下载失败时给手动下载指引 + `jar_path` 兜底。
- 缓存目录在包外，升级/重装 R 包不会丢 jar，但换机器需重新下载。
