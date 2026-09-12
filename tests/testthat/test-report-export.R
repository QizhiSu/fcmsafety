# 报告导出（Excel / CSV）、查询失败可见化、组条目汇总
#
# 这一组测试都不碰真实数据库：导出与汇总函数只吃 data.frame，可以在内存里造。

# 造一张最小的结果表（列名与 assign_toxicity() 的输出一致）
make_result <- function() {
  data.frame(
    InChIKey = c("AAAA-1", "BBBB-2", "CCCC-3", "DDDD-4"),
    NAME = c("丙烯酸", "甲醛", "某个未知物", "乙酸"),
    Cramer_rules = c("Low (Class I)", "High (Class III)", NA, "Low (Class I)"),
    SVHC = c("-", "Y", "-", "-"),
    CMR = c("-", "Y", "-", "-"),
    CMR_H_codes = c("-", "H350", "-", "-"),
    CMR_suspect = c("-", "-", "-", "-"),
    EDC = c("-", "-", "-", "-"),
    IARC = c("-", "1", "-", "-"),
    EU_SML = c("6", "-", "-", "60"),
    China_SML = c("6", "-", "-", "-"),
    Group_hits = c("-", "-", "-", "-"),
    Group_IARC = c("-", "-", "-", "-"),
    Group_review = c("-", "-", "-", "-"),
    Toxic_level = c("I", "V", "-", "I"),
    Toxic_level_basis = c("SML:6(EU+China)", "CMR:H350; IARC:1", NA, "SML:60(EU)"),
    stringsAsFactors = FALSE
  )
}

test_that("导出 xlsx：四张表都在，内容对得上", {
  skip_if_not_installed("openxlsx")
  res <- make_result()
  path <- file.path(tempdir(), "fcmsafety-test-report.xlsx")
  on.exit(unlink(path), add = TRUE)

  out <- export_toxicity_report(res, path)
  expect_true(file.exists(path))
  expect_identical(out, path)   # 返回路径（invisibly）

  sheets <- openxlsx::getSheetNames(path)
  expect_identical(sheets, c("Results", "Summary", "Unassigned", "Issues"))

  back <- openxlsx::read.xlsx(path, sheet = "Results")
  expect_identical(nrow(back), nrow(res))
  expect_identical(sort(names(back)), sort(names(res)))

  # Unassigned 只留没有等级的哪一行
  un <- openxlsx::read.xlsx(path, sheet = "Unassigned")
  expect_identical(nrow(un), 1L)
  expect_identical(as.character(un$InChIKey), "CCCC-3")
})

test_that("导出 csv：与旧行为一致，返回路径", {
  res <- make_result()
  path <- file.path(tempdir(), "fcmsafety-test-report.csv")
  on.exit(unlink(path), add = TRUE)
  export_toxicity_report(res, path)
  expect_true(file.exists(path))
  back <- utils::read.csv(path, stringsAsFactors = FALSE)
  expect_identical(nrow(back), nrow(res))
  expect_identical(as.character(back$Toxic_level), res$Toxic_level)
})

test_that("不支持的扩展名给出可操作的报错", {
  res <- make_result()
  expect_error(export_toxicity_report(res, file.path(tempdir(), "x.txt")),
               "Unsupported output extension")
})

test_that("非 data.frame 输入被拒绝", {
  expect_error(export_toxicity_report(list(a = 1), file.path(tempdir(), "a.xlsx")),
               "must be a data.frame")
})

test_that("Issues 表把 failed / missing 标红，但没有问题时也有一行说明", {
  skip_if_not_installed("openxlsx")
  res <- make_result()
  path <- file.path(tempdir(), "fcmsafety-test-issues.xlsx")
  on.exit(unlink(path), add = TRUE)

  issues <- data.frame(
    Source = c("svhc", "iarc"),
    Status = c("ok", "failed"),
    Rows = c(3L, NA_integer_),
    Message = c("", "no such table: iarc"),
    stringsAsFactors = FALSE
  )
  export_toxicity_report(res, path, issues = issues)
  back <- openxlsx::read.xlsx(path, sheet = "Issues")
  expect_identical(nrow(back), 2L)
  expect_true("no such table: iarc" %in% back$Message)

  # 不传 issues 时也要有一行，而不是空表（空表看起来像"没写成功"）
  path2 <- file.path(tempdir(), "fcmsafety-test-issues-empty.xlsx")
  on.exit(unlink(path2), add = TRUE)
  export_toxicity_report(res, path2)
  back2 <- openxlsx::read.xlsx(path2, sheet = "Issues")
  expect_identical(nrow(back2), 1L)
  expect_identical(as.character(back2$Status), "ok")
})

test_that("Summary 按 Section 分段并统计等级分布", {
  res <- make_result()
  s <- fcmsafety:::.report_summary_table(res)
  expect_identical(names(s), c("Section", "Metric", "Value"))
  get_val <- function(metric) s$Value[s$Metric == metric]
  expect_identical(get_val("Rows in input"), "4")
  expect_identical(get_val("Level V"), "1")
  expect_identical(get_val("Level I"), "2")
  expect_identical(get_val("Not assigned (no rule matched)"), "1")
  expect_identical(get_val("SVHC"), "1")
})

test_that("Summary 带上 run_info 时排在最前", {
  res <- make_result()
  ri <- data.frame(Key = c("database", "package_version"),
                   Value = c("fcmsafety.db", "0.1.6"),
                   stringsAsFactors = FALSE)
  s <- fcmsafety:::.report_summary_table(res, run_info = ri)
  expect_identical(s$Section[1], "Run")
  expect_identical(s$Metric[1], "database")
  expect_identical(s$Metric[2], "package_version")
})

test_that("列宽拟合：中文按两格算，长文本截到上限，空列不炸", {
  w_cn <- fcmsafety:::.autofit_width(c("短", "一二三四五"), "列")
  expect_gt(w_cn, 8)
  w_long <- fcmsafety:::.autofit_width(paste(rep("x", 500), collapse = ""), "h")
  expect_identical(w_long, 48)
  expect_identical(fcmsafety:::.autofit_width(character(0), "h"), 8)
  expect_identical(fcmsafety:::.autofit_width(c(NA, NA), "h"), 8)
})

test_that("等级配色覆盖 I–V 与未定级，且未定级不是绿色", {
  expect_setequal(names(fcmsafety:::.level_fills), c("V", "IV", "III", "II", "I", "-"))
  # "-" 用中性灰：绿色会被读成"安全"
  expect_identical(unname(fcmsafety:::.level_fills[["-"]]), "#EFEFEF")
  expect_false(unname(fcmsafety:::.level_fills[["-"]]) ==
                 unname(fcmsafety:::.level_fills[["I"]]))
})

# ---------------------------------------------------------------------------
# 查询失败可见化
# ---------------------------------------------------------------------------

test_that("查询错误能从 helper 里带出来，不会被当成空结果", {
  x <- fcmsafety:::.attach_query_error(data.frame(), "no such table: svhc")
  expect_identical(attr(x, "query_error"), "no such table: svhc")
  # 没挂错误的对象不影响判断
  expect_null(attr(data.frame(), "query_error"))
})

test_that(".query_issue_table 收空 list 返回空表而不是报错", {
  empty <- fcmsafety:::.query_issue_table(list())
  expect_identical(nrow(empty), 0L)
  expect_identical(names(empty), c("Source", "Status", "Rows", "Message"))

  one <- fcmsafety:::.query_issue_table(list(
    data.frame(Source = "svhc", Status = "failed", Rows = NA_integer_,
               Message = "boom", stringsAsFactors = FALSE),
    data.frame(Source = "iarc", Status = "empty", Rows = 0L,
               Message = "no rows", stringsAsFactors = FALSE)
  ))
  expect_identical(nrow(one), 2L)
  expect_identical(one$Source, c("svhc", "iarc"))
})

test_that("同一个库只记一条：缺表就不会再补一条查挂", {
  # 这条逻辑写在 assign_toxicity() 内部，这里用等价的最小复现守住语义：
  # 已经登记过的 source 不再重复登记。
  issues <- list(data.frame(Source = "svhc", Status = "missing",
                            Rows = NA_integer_, Message = "table absent",
                            stringsAsFactors = FALSE))
  already_noted <- function(source) {
    any(vapply(issues, function(d) identical(d$Source, source), logical(1)))
  }
  expect_true(already_noted("svhc"))
  expect_false(already_noted("iarc"))
})

# ---------------------------------------------------------------------------
# 组条目汇总
# ---------------------------------------------------------------------------

make_group_hits <- function() {
  data.frame(
    source_db = c("iarc", "iarc", "iarc", "svhc", "cmr"),
    matched_entry = c("Cadmium and cadmium compounds", "Cadmium and cadmium compounds",
                      "Chromium (VI) compounds", "nonylphenol", "chlorinated paraffins"),
    iarc_group = c("1", "2A", "3", NA, NA),
    confidence = c("auto_confirmed", "probable", "auto_confirmed",
                   "probable", "manual_review"),
    input_index = c(1L, 1L, 1L, 2L, 3L),
    stringsAsFactors = FALSE
  )
}

test_that("组条目汇总：同一条目去重，IARC 取最严", {
  out <- fcmsafety:::summarise_group_hits(make_group_hits())
  expect_identical(nrow(out), 3L)
  row1 <- out[out$input_index == 1L, ]
  # 两个条目：镉（去重后一条）+ 铬
  expect_identical(row1$Group_hits, "iarc: Cadmium and cadmium compounds; iarc: Chromium (VI) compounds")
  # 1 与 2A 取 1；3 不参与竞争
  expect_identical(row1$Group_IARC, "1")
  expect_identical(row1$Group_review, NA_character_)
})

test_that("组条目汇总：manual_review 只进 Group_review，不参与定级", {
  out <- fcmsafety:::summarise_group_hits(make_group_hits())
  row3 <- out[out$input_index == 3L, ]
  expect_true(grepl("chlorinated paraffins", row3$Group_hits))
  expect_true(grepl("chlorinated paraffins", row3$Group_review))
  expect_identical(row3$Group_IARC, NA_character_)
})

test_that("组条目汇总：CMR / SVHC 命中不产生 IARC 分组", {
  out <- fcmsafety:::summarise_group_hits(make_group_hits())
  row2 <- out[out$input_index == 2L, ]
  expect_true(grepl("nonylphenol", row2$Group_hits))
  expect_identical(row2$Group_IARC, NA_character_)
})

test_that("组条目汇总：空输入与缺列都返回 0 行", {
  expect_identical(nrow(fcmsafety:::summarise_group_hits(NULL)), 0L)
  empty <- data.frame(source_db = character(0), matched_entry = character(0),
                      iarc_group = character(0), confidence = character(0),
                      input_index = integer(0), stringsAsFactors = FALSE)
  expect_identical(nrow(fcmsafety:::summarise_group_hits(empty)), 0L)
  # 缺 confidence 列时不能猜，直接返回空
  bad <- data.frame(source_db = "iarc", matched_entry = "x",
                    input_index = 1L, stringsAsFactors = FALSE)
  expect_identical(nrow(fcmsafety:::summarise_group_hits(bad)), 0L)
})

test_that("命中条目多于 3 条时截断并标出剩余数量", {
  expect_identical(fcmsafety:::.paste_capped(c("a", "b")), "a; b")
  expect_identical(fcmsafety:::.paste_capped(c("a", "b", "c", "d", "e")),
                   "a; b; c (+2 more)")
  # 去重后再判断是否超限
  expect_identical(fcmsafety:::.paste_capped(c("a", "a", "b")), "a; b")
})

# ---------------------------------------------------------------------------
# 组条目批量 API 的 rbind 崩溃（回归）
# ---------------------------------------------------------------------------

test_that(".rbind_fill 按列名并集补齐后合并", {
  a <- data.frame(matched_agent = "Cadmium and cadmium compounds",
                  layer = "element", iarc_group = "1",
                  stringsAsFactors = FALSE)
  b <- data.frame(matched_entry = "nonylphenol", source_db = "svhc",
                  category = "uvcb", stringsAsFactors = FALSE)
  out <- fcmsafety:::.rbind_fill(list(a, b))
  expect_identical(nrow(out), 2L)
  expect_true(all(c("matched_agent", "layer", "iarc_group",
                    "matched_entry", "source_db", "category") %in% names(out)))
  # 缺的列填 NA，而不是整批失败
  expect_true(is.na(out$matched_entry[1]))
  expect_true(is.na(out$matched_agent[2]))
})

test_that(".rbind_fill 丢掉空块，全空时返回 NULL", {
  empty <- data.frame(a = character(0), stringsAsFactors = FALSE)
  one <- data.frame(a = "x", stringsAsFactors = FALSE)
  expect_identical(nrow(fcmsafety:::.rbind_fill(list(empty, one))), 1L)
  expect_null(fcmsafety:::.rbind_fill(list()))
  expect_null(fcmsafety:::.rbind_fill(list(empty, empty)))
})

test_that("复杂列（list / factor）也能合并", {
  a <- data.frame(a = 1, stringsAsFactors = FALSE)
  a$lst <- list(1:3)
  b <- data.frame(b = "x", stringsAsFactors = FALSE)
  out <- fcmsafety:::.rbind_fill(list(a, b))
  expect_identical(nrow(out), 2L)
  expect_true(is.list(out$lst))
})

# ---------------------------------------------------------------------------
# iarc_extra：组条目提供的 IARC 分组参与定级
# ---------------------------------------------------------------------------

tier_with_extra <- function(iarc = NA_character_, extra = NA_character_) {
  fcmsafety:::compute_toxicity_levels(
    svhc = FALSE, cmr_h_codes = NA_character_, cmr_suspect = FALSE, edc = FALSE,
    iarc = iarc, sml_eu = NA_real_, sml_cn = NA_real_,
    cramer_rules = NA_character_, iarc_extra = extra)
}

test_that("组条目 IARC 分组可以用来定级，来源标成 IARC(group)", {
  r <- tier_with_extra(extra = "1")
  expect_identical(r$Toxic_level, "V")
  expect_identical(r$Toxic_level_basis, "IARC(group):1")

  r2 <- tier_with_extra(extra = "2B")
  expect_identical(r2$Toxic_level, "IV")
  expect_identical(r2$Toxic_level_basis, "IARC(group):2B")
})

test_that("精确 IARC 与组条目 IARC 冲突时取更严的一方", {
  # 精确命中 3 组（无证据），组条目给 2A -> 用组条目
  r <- tier_with_extra(iarc = "3", extra = "2A")
  expect_identical(r$Toxic_level, "IV")
  expect_identical(r$Toxic_level_basis, "IARC(group):2A")

  # 精确命中 1 组，组条目给 2B -> 保留精确命中
  r2 <- tier_with_extra(iarc = "1", extra = "2B")
  expect_identical(r2$Toxic_level, "V")
  expect_identical(r2$Toxic_level_basis, "IARC:1")
})

test_that("不给 iarc_extra 时行为与从前完全一致", {
  r <- tier_with_extra(iarc = "2A")
  expect_identical(r$Toxic_level, "IV")
  expect_identical(r$Toxic_level_basis, "IARC:2A")

  r2 <- fcmsafety:::compute_toxicity_levels(
    svhc = FALSE, cmr_h_codes = NA_character_, cmr_suspect = FALSE, edc = FALSE,
    iarc = "1", sml_eu = NA_real_, sml_cn = NA_real_, cramer_rules = NA_character_)
  expect_identical(r2$Toxic_level, "V")
  expect_identical(r2$Toxic_level_basis, "IARC:1")
})
