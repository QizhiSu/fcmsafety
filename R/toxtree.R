# =============================================================================
# Toxtree 集成（Cramer 分类）
#
# 本文件收纳所有 Toxtree 相关接口：
#   - run_toxtree()       R 内直接调用 Toxtree CLI 完成 Cramer 分类，
#                         产出 assign_toxicity() 可直接消费的 toxtree_results.csv
#   - ensure_toxtree_jar() jar 查找/按需下载（内部函数）
#
# 设计决策见 docs/adr/0001-toxtree-via-local-cli.md 与
# docs/adr/0002-gpl-jar-download-on-demand.md（GPL jar 不随 MIT 包分发）。
#
# 实测要点（2026-09-08 冒烟测试，Toxtree 3.1.0.1851 + Temurin 17）：
#   - CLI 输出列名为 "Cramer rules"（空格），GUI 工作流的 "Cramer.rules"
#     其实是 read.csv(check.names=TRUE) 的自动改名；这里显式归一化。
#   - Toxtree 以【当前工作目录】定位 ext/ 模块目录，而非 jar 所在目录，
#     因此必须 cd 到应用目录再启动 java（system2 不支持设 cwd，用 system()）。
# =============================================================================

# ---- R 内直接调用 Toxtree CLI ----------------------------------------------

# ---- jar 管理：查找 / 下载 / 解压 ------------------------------------------

# 固定版本：Toxtree 3.1.0（SourceForge 最新，2018-05-04 发布）
.toxtree_version <- "3.1.0.1851"
.toxtree_zip_url <- "https://downloads.sourceforge.net/project/toxtree/toxtree/Toxtree-v.3.1.0/Toxtree-v3.1.0.1851.zip"
.toxtree_zip_size <- 81045024  # 期望字节数，下载后校验
.toxtree_app_dirname <- "toxtree_app"  # 缓存内的应用目录名（jar + ext/）

# 空白 NAME / CAS 字段的占位符。
# Toxtree 3.1.0 的已知缺陷（2026-09-10 复现）：输入行 CAS 字段为空时，
# 该行输出会整行左移一列——CAS 字段被整个丢弃，其后所有值前移，导致
# Cramer 分级值落进 CRAMERFLAGS 列，按列名读回时得到 NA。
# 实测给空字段填任意非空占位符即可避免错位（行号 / CID / "N/A" 均可），
# 故这里统一填 "N/A"，且在归一化后回填为输入原值，不让占位符泄进结果。
.toxtree_blank_fill <- "N/A"

#' Ensure the Toxtree application is available (internal)
#'
#' 查找顺序：① 用户显式指定的 jar_path（须为标准安装布局，jar 旁边有
#' ext/ 模块目录）；② 包外缓存目录里已解好的应用目录；③ 从 SourceForge
#' 下载官方 zip 并解出应用目录。
#'
#' Toxtree 以 GPL 2.0 分发，fcmsafety 是 MIT，因此 jar 不随包分发，
#' 只在用户机器上按需下载（用户自取官方源，包只做自动化）。
#'
#' @return 主 jar 的绝对路径（其同级目录含 ext/）。
#' @noRd
ensure_toxtree_jar <- function(jar_path = NULL, download = TRUE) {
  # ① 显式路径（用户自己的 Toxtree 安装）
  if (!is.null(jar_path)) {
    if (!file.exists(jar_path)) {
      stop("jar_path points to a non-existent file: ", jar_path,
           call. = FALSE)
    }
    jar_path <- normalizePath(jar_path)
    if (!dir.exists(file.path(dirname(jar_path), "ext"))) {
      warning("No 'ext' module directory found next to ", jar_path,
              " - Toxtree may fail to load plugins. jar_path should point ",
              "to the main jar of a standard Toxtree installation.")
    }
    return(jar_path)
  }

  # ② 缓存目录
  cache_dir <- tools::R_user_dir("fcmsafety", "cache")
  app_dir <- file.path(cache_dir, .toxtree_app_dirname)
  jar_file <- .find_main_jar(app_dir)
  if (!is.null(jar_file)) {
    return(jar_file)
  }
  if (!download) {
    stop("Toxtree not found in cache. Run run_toxtree() once to download, ",
         "or pass jar_path explicitly.", call. = FALSE)
  }

  # ③ 下载官方 zip 并解出应用目录
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  zip_file <- file.path(cache_dir, paste0("Toxtree-v", .toxtree_version, ".zip"))
  if (!file.exists(zip_file) || file.size(zip_file) != .toxtree_zip_size) {
    message("Downloading Toxtree ", .toxtree_version,
            " from SourceForge (~81 MB, one-time setup)...")
    ok <- tryCatch({
      .download_toxtree_zip(.toxtree_zip_url, zip_file)
      TRUE
    }, error = function(e) {
      message("Download failed: ", conditionMessage(e))
      FALSE
    })
    if (!ok || !file.exists(zip_file) ||
        file.size(zip_file) != .toxtree_zip_size) {
      stop("Failed to download Toxtree zip. Please download manually from:\n  ",
           .toxtree_zip_url, "\nextract it, and pass the main jar path via ",
           "jar_path.", call. = FALSE)
    }
  }
  message("Extracting Toxtree application directory...")
  .extract_toxtree_app(zip_file, cache_dir)
  jar_file <- .find_main_jar(app_dir)
  if (is.null(jar_file)) {
    stop("Toxtree jar could not be extracted from the zip archive.",
         call. = FALSE)
  }
  jar_file
}

#' Locate the main (largest) jar inside the app directory (internal)
#' @noRd
.find_main_jar <- function(app_dir) {
  if (!dir.exists(app_dir)) return(NULL)
  jars <- list.files(app_dir, pattern = "\\.jar$", ignore.case = TRUE,
                     full.names = TRUE)
  if (length(jars) == 0) return(NULL)
  jars[which.max(file.size(jars))]
}

#' Download the Toxtree zip, handling SourceForge's HTML interstitial (internal)
#' @noRd
.download_toxtree_zip <- function(url, dest) {
  tmp <- tempfile(fileext = ".zip")
  resp <- httr::GET(url, httr::user_agent("R (fcmsafety)"),
                    httr::write_disk(tmp, overwrite = TRUE))
  httr::stop_for_status(resp)

  # downloads.sourceforge.net 可能返回带 meta refresh 的 HTML 跳转页而非文件
  ct <- httr::headers(resp)$`content-type`
  if (!is.null(ct) && grepl("text/html", ct, ignore.case = TRUE)) {
    html <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
    m <- regmatches(html, regexpr('content="[0-9]+;\\s*url=([^"]+)"', html))
    if (length(m) == 0 || !nzchar(m)) {
      stop("SourceForge returned an unexpected HTML page (no mirror redirect).")
    }
    redirect <- sub('content="[0-9]+;\\s*url=', "", m)
    redirect <- gsub("&amp;", "&", redirect, fixed = TRUE)
    resp <- httr::GET(redirect, httr::user_agent("R (fcmsafety)"),
                      httr::write_disk(tmp, overwrite = TRUE))
    httr::stop_for_status(resp)
  }
  ok <- file.copy(tmp, dest, overwrite = TRUE)
  if (!ok) stop("Could not write the downloaded zip to: ", dest)
  invisible(dest)
}

#' Extract the Toxtree application directory from the official zip (internal)
#'
#' zip 内布局：Toxtree-v3.1.0.1851/Toxtree/{主jar, ext/*.jar,
#' toxtree-plugins.properties}。只解 Toxtree 应用子目录（跳过 doc/src）。
#' @noRd
.extract_toxtree_app <- function(zip_file, cache_dir) {
  listing <- utils::unzip(zip_file, list = TRUE)
  # 应用子目录 = 主 jar（最大的 .jar 条目）所在的目录
  jar_entries <- listing[grepl("\\.jar$", listing$Name, ignore.case = TRUE), ,
                         drop = FALSE]
  if (nrow(jar_entries) == 0) {
    stop("No jar found inside the Toxtree zip archive.")
  }
  app_entry_dir <- dirname(jar_entries$Name[which.max(jar_entries$Length)])

  exdir <- file.path(cache_dir, "toxtree_extract")
  if (dir.exists(exdir)) unlink(exdir, recursive = TRUE)
  utils::unzip(zip_file,
               files = listing$Name[startsWith(listing$Name, app_entry_dir)],
               exdir = exdir)

  app_src <- file.path(exdir, basename(app_entry_dir))
  app_dst <- file.path(cache_dir, .toxtree_app_dirname)
  if (dir.exists(app_dst)) unlink(app_dst, recursive = TRUE)
  if (!file.rename(app_src, app_dst)) {
    # rename 跨盘失败时退化为复制
    if (!dir.create(app_dst, recursive = TRUE)) {
      stop("Could not create the Toxtree app directory in cache.")
    }
    if (!file.copy(app_src, app_dst, recursive = TRUE)) {
      stop("Could not copy the Toxtree app directory into cache.")
    }
  }
  unlink(exdir, recursive = TRUE)
  invisible(app_dst)
}


# ---- CLI 调用：跑 classify / 归一输出 --------------------------------------

#' Run Toxtree Cramer classification from R
#'
#' 在 R 内直接调用 Toxtree 的无界面（headless）模式完成 Cramer 分类，
#' 不再需要手动打开 Toxtree GUI 做批处理。产出与 GUI 工作流同构的
#' \code{toxtree_results.csv}，可直接传给 \code{\link{assign_toxicity}()}。
#'
#' 首次运行会自动从 SourceForge 下载 Toxtree（约 81 MB，一次性），
#' 之后复用缓存。也可以用 \code{jar_path} 指向本机已有的 Toxtree 安装
#' （标准布局：主 jar 旁边有 \code{ext/} 模块目录）。需要本机安装 Java 8+
#' （\url{https://adoptium.net}）。
#'
#' @param data 你的数据表，必须包含 \code{SMILES} 列（通常是
#'   \code{extract_cid()} / \code{extract_meta()} 之后的产物）。
#' @param module Toxtree 插件的完整类名。默认经典 Cramer 规则
#'   \code{"toxTree.tree.cramer.CramerRules"}（与 GUI 默认一致）；
#'   可选 \code{"cramer2.CramerRulesWithExtensions"}（扩展版）、
#'   \code{"toxtree.tree.cramer3.RevisedCramerDecisionTree"}（修订版）。
#' @param output 结果文件路径，默认 \code{"toxtree_results.csv"}，
#'   供 \code{assign_toxicity()} 直接消费。
#' @param jar_path 可选，本机已有的 Toxtree 主 jar 路径；缺省时自动
#'   查找缓存并按需下载。
#' @param cas_col CAS 列（列名或列序号），默认自动找 \code{"CAS"}。
#' @param name_col 化学名列（列名或列序号），默认自动找 \code{"NAME"}。
#' @param timeout 单次 CLI 运行的超时秒数，默认 600。
#'
#' @return 一个 data.frame：输入的 NAME / CAS / SMILES 加上归一化后的
#'   \code{Cramer.rules} 结果列（及决策路径列）。同时写入 \code{output}。
#'
#' @importFrom dplyr mutate select filter
#' @export
#' @encoding UTF-8
run_toxtree <- function(data,
                        module = "toxTree.tree.cramer.CramerRules",
                        output = "toxtree_results.csv",
                        jar_path = NULL,
                        cas_col = "CAS",
                        name_col = "NAME",
                        timeout = 600) {
  message("Toxtree CLI runner (module: ", module, ")")
  message(paste(rep("-", 60), collapse = ""))

  # --- 输入校验 ---
  if (!"SMILES" %in% names(data)) {
    stop("Input data must contain a 'SMILES' column. ",
         "Run prepare_input() first to derive one from chemical names.",
         call. = FALSE)
  }
  java_bin <- Sys.which("java")
  if (!nzchar(java_bin)) {
    stop("Java not found on this machine. Install Java 8+ from ",
         "https://adoptium.net and retry.", call. = FALSE)
  }

  # --- 定位 NAME / CAS 列（列名或序号均可） ---
  resolve_col <- function(col, fallback_search) {
    if (is.numeric(col)) {
      if (col < 1 || col > ncol(data)) {
        stop("Column index out of range: ", col, call. = FALSE)
      }
      return(col)
    }
    if (col %in% names(data)) return(match(col, names(data)))
    hit <- match(fallback_search, names(data))
    hit <- hit[!is.na(hit)]
    if (length(hit) > 0) return(hit[1])
    NA_integer_
  }
  name_idx <- resolve_col(name_col, c("NAME", "name", "Chemical.Name"))
  cas_idx <- resolve_col(cas_col, c("CAS", "cas", "CAS_retrieved"))

  # --- jar 准备 ---
  jar <- ensure_toxtree_jar(jar_path)
  app_dir <- dirname(jar)
  message("Toxtree jar: ", jar)

  # --- 组装 Toxtree 输入（NAME / CAS / SMILES，SMILES 缺失的行丢弃） ---
  tox_input <- data.frame(
    NAME = if (is.na(name_idx)) NA_character_ else as.character(data[[name_idx]]),
    CAS = if (is.na(cas_idx)) NA_character_ else as.character(data[[cas_idx]]),
    SMILES = as.character(data$SMILES),
    stringsAsFactors = FALSE
  )
  n_before <- nrow(tox_input)
  tox_input <- tox_input[!is.na(tox_input$SMILES) & nzchar(tox_input$SMILES), ,
                         drop = FALSE]
  message("Compounds to classify: ", nrow(tox_input),
          " (dropped ", n_before - nrow(tox_input), " rows without SMILES)")
  if (nrow(tox_input) == 0) {
    stop("No compounds with a valid SMILES to classify.", call. = FALSE)
  }

  # --- 剔除无法解析的 SMILES ---
  # Toxtree 遇到解析不了的 SMILES 会静默丢掉该行，导致输出行数少于输入，
  # 按行序对齐随即失败 —— 一行坏数据就能让整批 Cramer 结果全丢。
  # 这里先用 CDK 在本地下同样的判断，把不能解析的行挡在批处理之外，
  # 跑完再按原顺序把它们的各列置 NA 合并回去（行数、行序都与输入一致）。
  # rcdk 不可用时跳过这一步，此时仍由 .normalize_toxtree_output 的行数校验兜底。
  n_all <- nrow(tox_input)
  parsable <- rep(TRUE, n_all)
  if (requireNamespace("rcdk", quietly = TRUE)) {
    parsable <- .smiles_is_parsable(tox_input$SMILES)
    n_bad <- sum(!parsable)
    if (n_bad > 0) {
      message("Skipping ", n_bad, " row(s) whose SMILES cannot be parsed; ",
              "their Cramer result will be NA.")
    }
  }
  tox_run <- tox_input[parsable, , drop = FALSE]
  if (nrow(tox_run) == 0) {
    stop("No compounds with a parsable SMILES to classify.", call. = FALSE)
  }

  # --- 空白 NAME/CAS 占位符（规避 Toxtree 输出错位，见 .toxtree_blank_fill）---
  # 归一化后会用 orig_identity 把这两列还原成输入原值，占位符不会出现在结果里。
  orig_identity <- tox_run[, c("NAME", "CAS"), drop = FALSE]
  n_filled <- 0L
  for (col in c("NAME", "CAS")) {
    blank <- is.na(tox_run[[col]]) |
      !nzchar(trimws(as.character(tox_run[[col]])))
    blank[is.na(blank)] <- TRUE
    if (any(blank)) {
      tox_run[[col]] <- as.character(tox_run[[col]])
      tox_run[[col]][blank] <- .toxtree_blank_fill
      n_filled <- n_filled + sum(blank)
    }
  }
  if (n_filled > 0L) {
    message("Filled ", n_filled, " blank NAME/CAS field(s) with \"",
            .toxtree_blank_fill, "\" to avoid Toxtree's output column shift.")
  }

  # --- 调用 CLI ---
  # 注意：Toxtree 以【当前工作目录】定位 ext/ 模块目录，必须让 java 的
  # 工作目录 = 应用目录。system2() 不支持设 cwd、Windows 的 system()
  # 又不认 cd 内建命令，故用临时 setwd()（子进程继承 cwd），helper 的
  # on.exit 保证必然还原。
  in_csv <- tempfile(fileext = ".csv")
  out_csv <- tempfile(fileext = ".csv")
  utils::write.csv(tox_run, in_csv, row.names = FALSE, na = "")

  message("Running Toxtree headless (this may take a while)...")
  cli_out <- .run_toxtree_cli(java_bin, jar, app_dir, in_csv, out_csv,
                              module, timeout)
  status <- attr(cli_out, "status")
  if (is.null(status)) status <- 0
  if (!file.exists(out_csv) || status != 0) {
    stop("Toxtree CLI failed (exit status ", status, ").\n",
         "CLI output (last 30 lines):\n",
         paste(utils::tail(cli_out, 30), collapse = "\n"),
         call. = FALSE)
  }

  # --- 读取并归一化输出 ---
  result <- .normalize_toxtree_output(out_csv, module, tox_run)

  # 占位符还原：把 NAME / CAS 写回输入原值（含 NA），不让 "N/A" 泄进结果。
  # .normalize_toxtree_output 保证输出行序与 tox_run 一致，可直接按位置回填。
  result$NAME <- orig_identity$NAME
  result$CAS  <- orig_identity$CAS

  # --- 把被剔除的非法 SMILES 行按原位置合并回来（结果列 NA）---
  # 目的是让返回值的行数、行序与输入（非空 SMILES 的行）完全一致，
  # 调用方既能按位置对齐、也能按 SMILES 匹配，不会因个别坏行错位。
  if (nrow(tox_run) < n_all) {
    extra_cols <- setdiff(names(result), names(tox_input))
    full <- tox_input
    for (cn in extra_cols) full[[cn]] <- result[[cn]][NA_integer_]
    full[parsable, names(result)] <- result
    result <- full[, c(names(tox_input), extra_cols), drop = FALSE]
  }

  # --- 写出结果 ---
  utils::write.csv(result, output, row.names = FALSE)
  message("Results written to: ", normalizePath(output))
  message("Next step: assign_toxicity(data, toxtree_result = \"", output, "\")")
  message(paste(rep("-", 60), collapse = ""))

  invisible(result)
}

#' Invoke the Toxtree headless CLI with cwd set to the app dir (internal)
#' @noRd
.run_toxtree_cli <- function(java_bin, jar, app_dir, in_csv, out_csv,
                             module, timeout) {
  old_wd <- getwd()
  on.exit(if (!is.null(old_wd)) setwd(old_wd), add = TRUE)
  setwd(app_dir)
  suppressWarnings(system2(
    java_bin,
    args = c("-jar", shQuote(jar), "-n",
             "-i", shQuote(in_csv),
             "-o", shQuote(out_csv),
             "-m", module),
    stdout = TRUE, stderr = TRUE, timeout = timeout
  ))
}

#' Normalize Toxtree CLI output to the toxtree_results.csv convention (internal)
#'
#' CLI 输出的结果列名为 "Cramer rules"（空格）；GUI 工作流里的
#' "Cramer.rules" 其实是 read.csv(check.names = TRUE) 的自动改名。
#' 这里显式重命名为 Cramer.rules，保证 assign_toxicity() 的
#' tox$Cramer.rules 在任何读取方式下都能命中。
#'
#' 防错位（P1-①，2026-09-09 实测）：当输入行 CAS 为空时，Toxtree CLI
#' 会把该行整行左移一列（NAME 顶进 CAS 位、SMILES 顶进 NAME 位…），
#' Cramer 分级值落进 CRAMERFLAGS 列。此处不信任输出身份列——行数校验后
#' 一律按输入行序重建 NAME/CAS/SMILES，并对检测到错位的行把结果列置 NA，
#' 避免分级值被静默错配到别的物质。
#' @noRd
.normalize_toxtree_output <- function(out_csv, module, tox_input) {
  out <- utils::read.csv(out_csv, check.names = FALSE, stringsAsFactors = FALSE)

  # 行数硬校验：Toxtree 按输入行序逐行输出（SMILES 过滤后），
  # 行数不符即无法按行对齐，直接拒绝而非静默错配。
  if (nrow(out) != nrow(tox_input)) {
    stop("Cannot align Toxtree output with input: output has ", nrow(out),
         " rows but input (after dropping rows without SMILES) has ",
         nrow(tox_input), " rows. Please check the input CSV or rerun ",
         "run_toxtree().", call. = FALSE)
  }

  # SMILES 列兜底：CLI 正常应原样保留输入列；若没有则按行序回填
  if (!"SMILES" %in% names(out)) {
    warning("Toxtree output has no SMILES column; falling back to input order.")
    out$SMILES <- tox_input$SMILES
  }

  # 结果列定位（实测列名 "Cramer rules"；排除 FLAGS 与决策路径列）
  result_col <- NULL
  candidates <- grep("cramer", names(out), ignore.case = TRUE, value = TRUE)
  candidates <- candidates[!grepl("flags", candidates, ignore.case = TRUE)]
  candidates <- candidates[!grepl("treeresult", candidates, ignore.case = TRUE)]
  # 优先与 module 类名尾段对应的列（CramerRules -> "Cramer rules"），
  # 否则取剩下的第一个
  if (module %in% names(out)) {
    result_col <- module
  } else if (length(candidates) > 0) {
    result_col <- candidates[1]
  }
  if (is.null(result_col)) {
    stop("Could not locate the Cramer result column in Toxtree output. ",
         "Columns found: ", paste(names(out), collapse = ", "),
         call. = FALSE)
  }

  # 身份列错位检测：在重建之前比对输出与输入共有的身份列。
  # NA/空值不参与比较；任一列不一致即视为该行错位。
  id_cols <- intersect(c("NAME", "CAS", "SMILES"), names(out))
  id_ok <- rep(TRUE, nrow(out))
  if (length(id_cols) > 0) {
    for (col in id_cols) {
      a <- out[[col]]
      b <- tox_input[[col]]
      a_miss <- is.na(a) | !nzchar(as.character(a))
      b_miss <- is.na(b) | !nzchar(as.character(b))
      both_present <- !a_miss & !b_miss
      diff <- both_present & as.character(a) != as.character(b)
      id_ok <- id_ok & !diff
    }
  }
  n_shift <- sum(!id_ok)
  if (n_shift > 0) {
    warning("Detected ", n_shift, " Toxtree output row(s) whose identity ",
            "columns do not match the input (likely caused by empty CAS ",
            "values shifting output columns). Identity columns were rebuilt ",
            "from the input in row order; Cramer results on those rows were ",
            "set to NA to avoid silent mis-assignment. For a full Cramer ",
            "result, provide CAS values or rerun with corrected input.")
  }

  # 行序对齐：身份列一律以输入为准重建（覆盖或补建 NAME/CAS/SMILES），
  # 不信任 CLI 输出的身份列。
  for (col in c("NAME", "CAS", "SMILES")) {
    out[[col]] <- tox_input[[col]]
  }

  # 保守：错位行的结果/FLAGS/决策路径列置 NA，宁缺毋滥
  if (n_shift > 0) {
    flag_cols <- grep("flags", names(out), ignore.case = TRUE, value = TRUE)
    tree_cols <- grep("treeresult", names(out), ignore.case = TRUE, value = TRUE)
    for (col in unique(c(result_col, flag_cols, tree_cols))) {
      out[[col]][!id_ok] <- NA_character_
    }
  }

  keep <- setdiff(names(out), "cdk:Title")  # cdk:Title 恒为空，丢弃
  out <- out[, keep, drop = FALSE]
  names(out)[names(out) == result_col] <- "Cramer.rules"

  out
}
