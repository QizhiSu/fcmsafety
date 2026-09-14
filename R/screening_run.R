# =============================================================================
# 总控入口：run_screening() —— 一次调用跑完「补结构 → 查法规 → 定级 → 出报告」
#
# 为什么要有这个文件：核心链路本来就通，但用户得自己手写两步
#   prepare_input()  →  assign_toxicity(output_file = )
# 中间还要自己接住 prepare_input() 的返回值。交接表把"总控入口没做"列为
# 用户体验上最大的缺口（P1-1），本文件就是补这一层薄封装。
#
# 边界（重要，别在这里越界）：
#   1) **只做调度，不复制逻辑**。补全在 prepare_input.R，匹配与定级在
#      direct_sql_toxicity.R。想改"怎么补""怎么判"，去那两个文件，
#      不要在本文件里再写一套。
#   2) **不联网、不改库**。online 默认 FALSE；本函数刻意不暴露
#      assign_toxicity() 的 check_updates / auto_update 参数 —— 那两个会
#      检测并应用人工清单文件，属于"写库"范畴，不该挂在确定性匹配入口上。
#      有测试守着这条（test-run-screening.R「安全边界」用例）。
#   3) **唯一一处主动告警**：全部行都没解析出 InChIKey 时。这时结果表会
#      全是 "-"，最容易被误读成"这些物质都干净"——静默失败比崩溃更危险。
#
# 注意：文件头说明用普通注释（而非 roxygen #'），否则会被 roxygen2
# 误配给其后第一个函数而抢占它的文档 title。
# =============================================================================

# ---- 主入口 ----------------------------------------------------------------

#' 一句话跑完筛查：补全结构标识 → 法规匹配与毒性定级 → 导出报告
#'
#' 把 [prepare_input()] 与 [assign_toxicity()] 串成一个入口，省掉手工接力的两步。
#' 内部只做调度与参数透传，不复制两者的任何逻辑。
#'
#' 输入通常只需**名称 + SMILES** 两列：`SMILES` 用来本地推导 `InChIKey`
#' （离线，见 ADR 0004），`NAME` 用来在库内做名称匹配补齐元数据。
#' 若数据已自带 `InChIKey` 列，则直接采用，跳过推导。
#'
#' **本函数不联网、不改数据库。** 结构推导默认全离线；法规匹配只读本地
#' SQLite。`assign_toxicity()` 上那两个会触碰"人工清单文件"的参数
#' （`check_updates` / `auto_update`）刻意没有暴露出来 —— 它们是写库语义，
#' 不属于确定性匹配入口。
#'
#' @param data `data.frame`，待筛查物质。至少要有名称列与 SMILES 列。
#' @param name_col,smiles_col,cas_col,inchikey_col 字符标量。显式指定列名；
#'   `NULL`（默认）时按常见列名自动识别。`cas_col` / `inchikey_col` 可为空。
#' @param online 逻辑值。本地推导不出 `InChIKey` 时是否联网 PubChem 兜底。
#'   **默认 `FALSE`**（全离线）。打开后每行会按 `delay` 间隔请求，较慢。
#' @param delay 数值。联网兜底时每行之间的间隔秒数，默认 0.35。
#' @param output_file 字符标量或 `NULL`。报告输出路径，**按扩展名分派**：
#'   `.xlsx` 走 [export_toxicity_report()]（Results / Summary / Unassigned /
#'   Issues 四张表），`.csv` 走 `write.csv`。`NULL` 时只返回结果不落盘。
#' @param toxtree_result 字符标量。Toxtree（Cramer 规则）结果 CSV 的路径。
#'   文件已存在则直接读取；不存在但数据含 `SMILES` 列时会现场调用
#'   [run_toxtree()] 生成（首次需要联网下载 jar，约 81MB）。默认
#'   `"toxtree_results.csv"`（当前工作目录）。
#' @param group_membership 逻辑值。是否额外做 IARC 等**组条目**归属判定
#'   （见 ADR 0009 / 0010）。默认 `FALSE`，因为组条目匹配是启发式的、
#'   且会显著加长运行时间。
#' @param db_path 字符标量或 `NULL`。SQLite 数据库路径；`NULL` 用包内默认库。
#' @param verbose 逻辑值。是否打印补全过程的进度信息，默认 `TRUE`。
#'
#' @return `data.frame`，即 [assign_toxicity()] 的结果表（在输入列基础上追加
#'   `SVHC` / `CMR` / `Cramer_rules` / `Toxic_level` 等列）。
#'   `prepare_input()` 的补全报告挂在 `attr(result, "prepare_report")` 上，
#'   可用 [print_prepare_report()] 查看。
#'
#' @seealso [prepare_input()]、[assign_toxicity()]、[export_toxicity_report()]
#'
#' @examples
#' \dontrun{
#' # 最小用法：只要有名称和 SMILES
#' substances <- data.frame(
#'   NAME   = c("Bisphenol A", "Ethanol"),
#'   SMILES = c("CC(C)(c1ccc(O)cc1)c1ccc(O)cc1", "CCO")
#' )
#' result <- run_screening(substances, output_file = "screening.xlsx")
#'
#' # 看补全情况（哪些行没解析出结构）
#' print_prepare_report(result)
#'
#' # 本地解析不了的，联网兜底
#' result <- run_screening(substances, online = TRUE)
#' }
#' @export
#' @export
run_screening <- function(data, name_col = NULL, smiles_col = NULL,
                          cas_col = NULL, inchikey_col = NULL,
                          online = FALSE, delay = 0.35,
                          output_file = NULL,
                          toxtree_result = "toxtree_results.csv",
                          group_membership = FALSE,
                          db_path = NULL, verbose = TRUE) {

  if (!is.data.frame(data) || nrow(data) == 0) {
    stop("data 必须是非空 data.frame。", call. = FALSE)
  }
  say <- function(...) if (verbose) message(...)
  say("🚀 开始筛查：共 ", nrow(data), " 行物质")
  say(paste(rep("=", 60), collapse = ""))

  # ---- 第 1 步：补全结构标识（prepare_input 自己会打印分步进度）----
  prepared <- prepare_input(
    data,
    name_col = name_col, smiles_col = smiles_col,
    cas_col = cas_col, inchikey_col = inchikey_col,
    online = online, delay = delay,
    db_path = db_path, verbose = verbose
  )

  # 报告要先取出来：prepare_input() 的返回对象在后续传递中可能被重建，
  # attr 不保证跟着走。
  prepare_report <- attr(prepared, "prepare_report")

  # 全行未解析 = 结果表必然全 "-"。这看起来像"都没问题"，必须显式告警。
  # 只在这个极端情况告警：部分未解析是常态，天天喊狼来了会让人忽略它。
  if (!is.null(prepare_report) &&
      isTRUE(prepare_report$total > 0) &&
      isTRUE(prepare_report$unresolved >= prepare_report$total)) {
    warning(
      "全部 ", prepare_report$total,
      " 行都未能解析出 InChIKey，法规匹配结果将全部为空（\"-\"）。\n",
      "  这不代表这些物质安全，只代表工具没能识别它们。\n",
      "  可检查 SMILES 是否有效，或改用 online = TRUE 联网查 PubChem 补齐。",
      call. = FALSE
    )
  }

  # ---- 第 2 步：法规匹配 + 毒性定级 + 出报告 ----
  # 注意从 data = prepared 传下去（不是原始 data）：第 1 步补出来的
  # InChIKey / Formula 等列就在这里被消费。
  result <- assign_toxicity(
    prepared,
    toxtree_result   = toxtree_result,
    output_file      = output_file,
    db_path          = db_path,
    group_membership = group_membership
  )

  attr(result, "prepare_report") <- prepare_report

  say(paste(rep("=", 60), collapse = ""))
  say("✅ 筛查完成：", nrow(result), " 行")
  result
}
