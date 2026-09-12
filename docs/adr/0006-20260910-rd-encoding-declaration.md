# Rd 文档编码声明：为什么每个 roxygen 块都带 `@encoding UTF-8`

## 背景

2026-09-10 把 `R CMD check` 的 locale 从 `LC_ALL=C` 改成 `LC_ALL=zh_CN.UTF-8` 之后，check 冒出 5 个 WARNING + 2 个 NOTE，其中 4 WARNING + 2 NOTE 全部来自 Rd：

```
* checking Rd files ... WARNING
* checking Rd metadata ... NOTE
* checking for missing documentation entries ... WARNING
* checking for code/documentation mismatches ... WARNING
* checking Rd \usage sections ... WARNING
* checking Rd contents ... NOTE
```

`tools::checkRd()` 逐个扫 116 个 `man/*.Rd`，**83 个有问题、共 709 条**，全部是同一类：

```
Non-ASCII contents without declared encoding
```

即 Rd 里是中文，但没有 `\encoding{UTF-8}` 声明。这是**既有问题**：此前一直用 `LC_ALL=C` 跑 check，ASCII locale 下 checkRd 不做这个校验，把问题掩盖了。

另有一类只有 2 条、是**本轮新引入的**：

```
prepare_Rd: man/extract_cmr_h_codes.Rd:19: unknown macro '\r'
prepare_Rd: man/extract_cmr_h_codes.Rd:19: unknown macro '\n'
```

根因：`extract_cmr_h_codes()` 的 `@description` 里直接写了 `"\r\r\n"` 这个分隔符字面量。Rd 把 `\r` / `\n` 当成宏名解析。

## Decision

1. **每个 roxygen 文档块末尾统一加 `#' @encoding UTF-8`**（共 120 处、14 个 R 文件）。`@noRd` 块跳过（不生成 Rd）。
2. **不加在块中间或块首**。roxygen 把标签之后的纯文本当作该标签的续行，插在中间会让紧随其后的描述文字被吞进 `@encoding` 的值里。追加到块尾是唯一安全位置。
3. **不用「生成后批量改写 `man/*.Rd`」的后处理方案**。那样每次 `devtools::document()` 都会把补的声明冲掉，需要额外记得跑一次脚本。
4. `\r\r\n` 改为用文字描述（"两个回车符加一个换行符"），不再在 Rd 里出现裸反斜杠。**R 代码注释里描述控制字符一律用文字，不写字面量转义。**
5. 本机跑 check 固定用 `LC_ALL=zh_CN.UTF-8`（见 `tools/run_check.R` 头部注释）。

## 关键依据（2026-09-10 实验）

| 实验 | 结果 |
|---|---|
| `man/*.Rd` 的实际字节编码 | UTF-8（`charToRaw("可")` 的 3 字节序列命中），`DESCRIPTION` 也声明了 `Encoding: UTF-8` |
| 给单个 Rd 补 `\encoding{UTF-8}` 后重跑 `checkRd` | 返回 `character(0)`，问题消失 |
| 在 roxygen 块里写 `@encoding UTF-8` 后生成 Rd | Rd 里出现 `\encoding{UTF-8}`，且排在 `\name{}` **之前** |
| 不写 `@encoding` | Rd 里完全不出现编码声明 |
| roxygen2 是否有「包级默认编码」开关 | 没有。命名空间里只有 `format.rd_section_encoding` / `roxy_tag_parse.roxy_tag_encoding` / `roxy_tag_rd.roxy_tag_encoding` 三个对象，全是按块处理的标签 |

## 改后的 check 结果

| | 改前 | 改后 |
|---|---|---|
| STATUS | 5 WARNINGs, 2 NOTEs | **1 WARNING** |
| Rd files / metadata / contents | WARNING / NOTE / NOTE | OK |
| missing documentation entries | WARNING | OK |
| code/documentation mismatches | WARNING | OK |
| Rd \usage sections | WARNING | OK |
| code files for non-ASCII characters | WARNING | WARNING（保留，见下） |

## Considered Options

- **按块加 `@encoding UTF-8`（选定）**：改动在 R 源码里，`devtools::document()` 后自动保持，一次性解决。代价是 120 行样板，且每个新写的文档块都要记得带（漏了 check 会立刻报）。
- **后处理 `man/*.Rd`**：Rd 文件数更少（83 个 vs 120 个块），但生成物会被 `document()` 覆盖，属于"必须记得再跑一步"的脆弱方案。否决。
- **接受这 5 个 WARNING**：项目规则只要求 0 ERROR，可以交付。但 5 个 WARNING 会把真正的回归（如这次的 `\r` 宏错误）淹没在噪音里。否决。
- **把文档全改成英文**：能根治，但整个包的文档就是给中文使用者看的。否决。

## Consequences

- 新增文档块**必须**以 `#' @encoding UTF-8` 结尾，否则 check 会报 `Non-ASCII contents without declared encoding`。这是刻意的摩擦：报错比静默好。
- 剩余的 1 条 WARNING 是 `checking code files for non-ASCII characters`，点名 9 个 `R/*.R`。**实测确认它不是注释问题**——R CMD check 只看字符串字面量，纯注释里的中文不触发（对照：`R/update_other_dbs.R` 有 200 处中文注释但未被点名，`R/incremental_update.R` 有 215 处同样未被点名；被点名的文件字符串里都有中文）。
- 因此这条 WARNING **接受不修**：中文全在 `message()` / `stop()` 的用户可见文案、Shiny UI 文案，以及 `prepare_input.R` 里用于匹配中文列头的候选列名常量（"物质名" / "结构式" / "CAS 号" 等）。改成 `\uXXXX` 转义会让这些常量彻底不可读，而本包是私有仓库、不上 CRAN，可移植性不是目标。`database_inspector_app.R` 里那 3 个非 ASCII "符号名"只是 `df$\`Database / 数据库\`` 这类列名引用，不是中文变量名。
