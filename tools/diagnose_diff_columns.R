# =============================================================================
# 增量 diff 列级诊断：某个库为什么被判一大片 modified？
#
# 何时用：tools/test_auto_update_line.R 报出的 modified 数量明显不合理
#   （例如 iarc 859 行里 846 行都"变了"），需要知道到底是哪一列在变、
#   是真变化、库缺值，还是两条路径的归一化规则不一致导致的假 modified。
#
# 做法：忠实复刻 run_incremental_update() 的步骤顺序
#   fetch_*_data -> map_to_db_columns -> 取库表 -> backfill_unmapped_cols -> diff_incremental
#   （只调 diff_incremental 会误判，见 2026-09-03 的教训），
#   然后对每个被判 modified 的 key 逐列比对，并把差异归成五类：
#     库空源有 / 库有源空 / 仅顺序不同 / 仅括号不同 / 派生等价(ND->0.01 等) / 真不同
#   只有"真不同"才说明数据确实落后；其余四类都是可修的假 modified。
#
# 前置：先跑 test_auto_update_line.R 生成 %TEMP%/fcm_auto_test/（副本库 + dl/）。
# 只读，不改任何库。
#
# 用法（Git Bash，cwd = 包根）：
#   LC_ALL=zh_CN.UTF-8 "/c/Program Files/R/R-4.6.1/bin/Rscript.exe" --vanilla tools/diagnose_diff_columns.R
# 结果落盘：.workbuddy/diff_diag_result.txt
# =============================================================================

LOG <- file.path(".workbuddy", "diff_diag_result.txt")
if (file.exists(LOG)) file.remove(LOG)
log_line <- function(...) { m <- paste0(...); cat(m, "\n"); cat(m, "\n", file = LOG, append = TRUE) }
section <- function(t) log_line("\n", strrep("=", 78), "\n## ", t, "\n", strrep("=", 78))

pkgload::load_all(".", quiet = TRUE)

TMP <- file.path(Sys.getenv("TEMP"), "fcm_auto_test")
DL  <- file.path(TMP, "dl")
DB  <- file.path(TMP, "fcmsafety.db")

key_of <- function(df, key_col, fallback_col) {
  k <- as.character(df[[key_col]])
  k <- ifelse(is_blank_key(k), NA_character_, trimws(k))
  if (!is.null(fallback_col)) {
    f <- as.character(df[[fallback_col]])
    f <- ifelse(is_blank_key(f), NA_character_, trimws(f))
    k <- ifelse(is.na(k) & !is.na(f), paste0(fallback_col, ":", f), k)
  }
  k
}

# 单格归一：与 diff_incremental 内部 canon_row 保持一致（含空白/换行/占位符）
canon <- function(x) {
  x <- as.character(x); x[is.na(x)] <- ""; Encoding(x) <- "UTF-8"
  x <- gsub("\r\r\n", "\n", x); x <- gsub("\r\n", "\n", x); x <- gsub("\r", "\n", x)
  x <- gsub("\n+", "\n", x); x <- trimws(x); x[x %in% key_placeholder] <- ""
  x
}
# 多行单元格按行集合比较（顺序无关）
cell_sorted <- function(x) paste(sort(strsplit(canon(x), "\n", fixed = TRUE)[[1]]), collapse = "\n")
# 派生等价：EU SML 迁移时把 ND 当 0.01、逗号当小数点，数值统一成规范写法。
# 多行值（H 码 / 象形图 / 名称串）不走这条路：对它们做"截断/数值化"会把
# 全部多行代码列误判成等价（首行相同即相等），掩盖真差异。
derived_norm <- function(x) {
  v <- canon(x)
  if (grepl("\n", v, fixed = TRUE)) return(v)
  v <- gsub(",", ".", v, fixed = TRUE)
  v <- gsub("ND", "0.01", v, fixed = TRUE)
  v <- trimws(v)
  n <- suppressWarnings(as.numeric(v))
  if (!is.na(n)) v <- format(n, scientific = FALSE, trim = TRUE)
  v
}
# 括号等价：sml_group 迁移时剥掉 ( )
paren_norm <- function(x) gsub("[()]", "", canon(x))

classify <- function(a, b) {
  a <- canon(a); b <- canon(b)
  if (identical(a, b)) return("same")
  if (!nzchar(a) && nzchar(b)) return("db_blank")
  if (nzchar(a) && !nzchar(b)) return("src_blank")
  if (identical(cell_sorted(a), cell_sorted(b))) return("order_only")
  if (identical(paren_norm(a), paren_norm(b))) return("paren_only")
  if (identical(derived_norm(a), derived_norm(b))) return("derived_only")
  "real"
}

VERDICT <- c(
  same         = "",
  db_blank     = "库缺值（迁移未写入）",
  src_blank    = "源缺值",
  order_only   = "归一化缺口：多行顺序",
  paren_only   = "归一化缺口：括号",
  derived_only = "归一化缺口：派生值 (ND/逗号)",
  real         = "真变化"
)

diag_one <- function(db, file, key_col, fallback_col, cas_col, name_col, sheet = NULL) {
  section(paste0(db, "  (源文件: ", basename(file), ")"))
  new_df <- tryCatch({
    if (db == "iarc") fetch_iarc_data(source = "local", new_file = file)
    else if (db == "cmr") fetch_cmr_data(source = "local", new_file = file)
    else if (db == "cmr_suspect") fetch_cmr_suspect_data(source = "local", new_file = file)
    else fetch_eu_sml_data(source = "local", new_file = file)
  }, error = function(e) { log_line("读取源失败: ", conditionMessage(e)); NULL })
  if (is.null(new_df)) return(invisible(NULL))
  log_line("源行数: ", nrow(new_df))

  mapped <- map_to_db_columns(new_df, db, DB)
  mf <- attr(mapped, "mapped_from", exact = TRUE)
  con <- get_db_connection(DB); on.exit(DBI::dbDisconnect(con), add = TRUE)
  cur <- DBI::dbGetQuery(con, paste("SELECT * FROM", db))
  log_line("库行数: ", nrow(cur))

  mapped <- backfill_unmapped_cols(mapped, cur, key_col, fallback_col)
  changes <- diff_incremental(mapped, cur, key_col, fallback_col, NULL, cas_col = cas_col)
  log_line(sprintf("diff: added=%d removed=%d modified=%d",
                   changes$total_added, changes$total_removed, changes$total_modified))

  content_cols <- setdiff(intersect(names(mapped), names(cur)),
                          c(chem_cols, "id", "created_at", "updated_at"))

  # 映射来源：这一列的值取自哪个源列（NA = 新源没提供）
  log_line("\n列映射（库列 <- 源列；(未映射) 表示新源不提供该列，按库旧值回填）:")
  for (cc in content_cols) {
    src <- if (!is.null(mf) && cc %in% names(mf)) mf[[cc]] else NA_character_
    log_line(sprintf("   %-40s <- %s", cc, if (is.na(src)) "(未映射)" else src))
  }

  mk <- changes$modified_keys
  cur_k <- key_of(cur, key_col, fallback_col)
  new_k <- key_of(mapped, key_col, fallback_col)

  tally <- matrix(0L, nrow = length(content_cols), ncol = length(VERDICT),
                  dimnames = list(content_cols, names(VERDICT)))
  samples <- list(); multi <- 0L
  for (k in mk) {
    ci <- which(cur_k == k); ni <- which(new_k == k)
    if (length(ci) != 1 || length(ni) != 1) { multi <- multi + 1L; next }
    for (cc in content_cols) {
      v <- classify(cur[[cc]][ci], mapped[[cc]][ni])
      tally[cc, v] <- tally[cc, v] + 1L
      if (v == "real" && length(samples) < 6 && is.null(samples[[as.character(k)]])) {
        samples[[as.character(k)]] <- sprintf("    %s | %s: [库] %s  ->  [源] %s", k, cc,
          substr(gsub("\n", " ; ", canon(cur[[cc]][ci])), 1, 55),
          substr(gsub("\n", " ; ", canon(mapped[[cc]][ni])), 1, 55))
      }
    }
  }

  log_line("\n多行键（同键多行，无法逐行比对，已跳过）: ", multi)
  log_line("\n列级判定 —— 被判 modified 的行里，各成因命中的行数:")
  hdr <- sprintf("   %-40s %7s %7s %7s %7s %7s %7s", "列", "库空源有", "库有源空",
                 "仅顺序", "仅括号", "派生等价", "真不同")
  log_line(hdr)
  ord <- order(-rowSums(tally[, c("real", "db_blank", "src_blank", "order_only",
                                  "paren_only", "derived_only"), drop = FALSE]))
  for (i in ord) {
    if (sum(tally[i, ]) == 0) next
    log_line(sprintf("   %-40s %7d %7d %7d %7d %7d %7d", content_cols[i],
                     tally[i, "db_blank"], tally[i, "src_blank"], tally[i, "order_only"],
                     tally[i, "paren_only"], tally[i, "derived_only"], tally[i, "real"]))
  }

  eff <- vapply(content_cols, function(cc) {
    nz <- setdiff(names(VERDICT), c("same", "real"))
    if (tally[cc, "real"] > 0) VERDICT[["real"]]
    else if (any(tally[cc, nz] > 0)) VERDICT[[nz[which.max(tally[cc, nz])]]]
    else ""
  }, character(1))
  log_line("\n逐列定性:")
  for (cc in content_cols) {
    if (sum(tally[cc, ]) == 0) next
    log_line(sprintf("   %-40s %s", cc, eff[[cc]]))
  }

  # added 行是否带 InChIKey：业务表设计上只留带键的行，无键入库会污染表
  if (changes$total_added > 0) {
    nk <- if ("InChIKey" %in% names(changes$added)) {
      sum(is.na(changes$added$InChIKey) | !nzchar(trimws(as.character(changes$added$InChIKey))))
    } else nrow(changes$added)
    log_line(sprintf("\nadded 行中无 InChIKey 的: %d / %d（业务表设计只留带键行）",
                     nk, nrow(changes$added)))
  }
  if (changes$total_removed > 0) {
    log_line(sprintf("removed 行: %d，样例键: %s", nrow(changes$removed),
                     paste(utils::head(changes$removed_keys, 5), collapse = ", ")))
  }

  log_line("\n真差异样例（只列真变化）:")
  if (length(samples) == 0) log_line("    （无）") else for (s in utils::head(samples, 6)) log_line(s)
  invisible(NULL)
}

diag_one("iarc",        file.path(DL, "iarc.xlsx"),      "cas_no",            "agent",            "cas_no", "agent")
diag_one("cmr",         file.path(DL, "clp.xlsx"),       "index_no",          "cas_no",           "cas_no", "international_chemical_identification")
diag_one("cmr_suspect", file.path(DL, "clp.xlsx"),       "substance_name",    "cas_no",           "cas_no", "substance_name")
diag_one("eu_sml",      file.path(DL, "eu10_2011.xlsx"), "fcm_substance_no",  "cas_no",           "cas_no", "substance_name")

log_line("\n完成: ", format(Sys.time()))
