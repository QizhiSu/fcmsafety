# CLP 复合列拆分（Pictogram+Signal Word / SCL+M-factor）
#
# 背景：CLP 官方导出把两个字段塞进一列。
#   "Labelling Pictogram, Signal Word Code(s)" -> "GHS06\r\nGHS08\r\nDgr"
#   "M, SCL, ATE" / "Specific Conc. Limits, M-factors" -> "Repr. 1B; H360FD: C ≥ 3,1 %\r\nM=10\r\n"
# 旧迁移把整串同时写进 pictogram 和 signal_word_codes（库里 332/332 行两列完全
# 相同）、同时写进 specific_conc_limits 和 m_factors（同样 332/332 相同），
# signal_word_codes 里没有一行是纯 Dgr/Wng。
#
# 取值实测（用于定边界）：
#   - token 只有 GHS01..GHS09 / Dgr / Wng，外加 "?"（无信息）与 "****"/"*"（脚注标记）
#   - 官方偶发把两个码写在一行："GHS08 GHS07"
#   - M 的两种写法：meta "M=1000"、增量源 "M = 10" —— 必须归一到同一形式，
#     否则 canon_cell 判为不同（它只折换行、不折中间空格），diff 永不收敛

test_that("Pictogram/Signal Word 复合列按 token 拆开", {
  x <- c("GHS06\nGHS08\nDgr",
         "GHS08\nWng",
         "GHS06\r\r\nGHS08\r\r\nDgr")
  sp <- fcmsafety:::split_clp_label_cell(x)

  expect_equal(sp$pictogram, c("GHS06\nGHS08", "GHS08", "GHS06\nGHS08"))
  expect_equal(sp$signal_word_codes, c("Dgr", "Wng", "Dgr"))
})

test_that("一行里写两个 GHS 码也能拆出来", {
  sp <- fcmsafety:::split_clp_label_cell("GHS08 GHS07")
  expect_equal(sp$pictogram, "GHS08\nGHS07")
  expect_equal(sp$signal_word_codes, NA_character_)
})

test_that("占位符 ? 与脚注 **** 不落进任何一列", {
  sp <- fcmsafety:::split_clp_label_cell(c("?\nGHS08\nGHS06\nDgr",
                                           "****\nGHS09\nWng",
                                           "?"))
  expect_equal(sp$pictogram, c("GHS08\nGHS06", "GHS09", NA_character_))
  expect_equal(sp$signal_word_codes, c("Dgr", "Wng", NA_character_))
})

test_that("NA 与空白值拆出来是空", {
  sp <- fcmsafety:::split_clp_label_cell(c(NA, "", "   "))
  expect_equal(sp$pictogram, rep(NA_character_, 3))
  expect_equal(sp$signal_word_codes, rep(NA_character_, 3))
})

test_that("SCL/M-factor 复合列按行拆，M 行归一到 'M = n'", {
  x <- c("Repr. 1B; H360FD: C ≥ 3,1 %\nM=10\n",
         "M = 1\nM = 10",
         "inhalation: ATE = 0.75 mg/L dusts or mists",
         NA)
  sp <- fcmsafety:::split_clp_limit_cell(x)

  expect_equal(sp$specific_conc_limits,
               c("Repr. 1B; H360FD: C ≥ 3,1 %",
                 NA_character_,
                 "inhalation: ATE = 0.75 mg/L dusts or mists",
                 NA_character_))
  expect_equal(sp$m_factors, c("M = 10", "M = 1\nM = 10", NA_character_, NA_character_))
})

test_that("两个源对 M 的写法不同，拆完必须一致（否则 diff 永不收敛）", {
  a <- fcmsafety:::split_clp_limit_cell("M=1000")
  b <- fcmsafety:::split_clp_limit_cell("M = 1000")
  expect_equal(a$m_factors, b$m_factors)
  expect_equal(a$specific_conc_limits, b$specific_conc_limits)
})

test_that("normalize_cmr_df 从复合列派生出 pictogram / signal_word_codes", {
  df <- data.frame(`Labelling Pictogram, Signal Word Code(s)` =
                     c("GHS06\nGHS08\nDgr", "GHS08\nWng"),
                   check.names = FALSE, stringsAsFactors = FALSE)
  out <- fcmsafety:::normalize_cmr_df(df)

  expect_equal(out$pictogram, c("GHS06\nGHS08", "GHS08"))
  expect_equal(out$signal_word_codes, c("Dgr", "Wng"))
})

test_that("旧 meta 表头（无 Labelling 前缀）同样能派生", {
  df <- data.frame(`Pictogram, Signal Word Code(s)` = c("GHS08\nDgr", "****\nGHS09\nWng"),
                   check.names = FALSE, stringsAsFactors = FALSE)
  out <- fcmsafety:::normalize_cmr_df(df)

  expect_equal(out$pictogram, c("GHS08", "GHS09"))
  expect_equal(out$signal_word_codes, c("Dgr", "Wng"))
})

test_that("normalize_cmr_df 刻意不派生 specific_conc_limits / m_factors", {
  # 新增量源（ATP23）对 91 个 Index No 的 "M, SCL, ATE" 是空的，而老 meta 有值
  # （如 005-008-00-8 的 "Repr. 1B; H360FD: C ≥ 3,1 %"，见 clp.xlsx 原始第 22 行）。
  # 在这里派生会让这两列变成"已映射"，写库时把那 91 个限值擦成 NULL 且回不来。
  df <- data.frame(`M, SCL, ATE` = "Repr. 1B; H360FD: C ≥ 3,1 %\nM=10",
                   check.names = FALSE, stringsAsFactors = FALSE)
  out <- fcmsafety:::normalize_cmr_df(df)

  expect_false("specific_conc_limits" %in% names(out))
  expect_false("m_factors" %in% names(out))
})

test_that("自愈：两列完全相同的行，M 行搬去 m_factors", {
  df <- data.frame(
    specific_conc_limits = "Repr. 1B; H360FD: C ≥ 0,01 %\nM=10\n",
    m_factors = "Repr. 1B; H360FD: C ≥ 0,01 %\nM=10\n",
    stringsAsFactors = FALSE)
  out <- fcmsafety:::heal_cmr_split_cols(df)

  expect_equal(out$specific_conc_limits, "Repr. 1B; H360FD: C ≥ 0,01 %")
  expect_equal(out$m_factors, "M = 10")
})

test_that("自愈：两列相同但没有 M 行时，m_factors 清空、scl 保留", {
  df <- data.frame(
    specific_conc_limits = "Repr. 1B; H360FD: C ≥ 3,1 %\r\r\n",
    m_factors = "Repr. 1B; H360FD: C ≥ 3,1 %\r\r\n",
    stringsAsFactors = FALSE)
  out <- fcmsafety:::heal_cmr_split_cols(df)

  expect_equal(out$specific_conc_limits, "Repr. 1B; H360FD: C ≥ 3,1 %")
  expect_equal(out$m_factors, NA_character_)
})

test_that("自愈只动两列完全相同的行，已独立的值一律不碰", {
  df <- data.frame(
    specific_conc_limits = c("M = 5", "Repr. 1B\nM=10\n"),
    m_factors = c("M = 10", "Repr. 1B\nM=10\n"),
    stringsAsFactors = FALSE)
  out <- fcmsafety:::heal_cmr_split_cols(df)

  # 第 1 行两列不同（已独立）→ 原样不动，scl 里的 "M = 5" 不丢
  expect_equal(out$specific_conc_limits[1], "M = 5")
  expect_equal(out$m_factors[1], "M = 10")
  # 第 2 行两列相同（迁移留下的复制品）→ 拆开
  expect_equal(out$specific_conc_limits[2], "Repr. 1B")
  expect_equal(out$m_factors[2], "M = 10")
})

test_that("自愈是幂等的", {
  df <- data.frame(
    specific_conc_limits = c("Repr. 1B; H360FD: C ≥ 0,01 %\nM=10\n",
                             "Carc. 1B; H350: C ≥ 0,001 %",
                             NA_character_),
    m_factors = c("Repr. 1B; H360FD: C ≥ 0,01 %\nM=10\n",
                  "Carc. 1B; H350: C ≥ 0,001 %",
                  NA_character_),
    stringsAsFactors = FALSE)
  once <- fcmsafety:::heal_cmr_split_cols(df)
  twice <- fcmsafety:::heal_cmr_split_cols(once)

  expect_equal(once$specific_conc_limits,
               c("Repr. 1B; H360FD: C ≥ 0,01 %", "Carc. 1B; H350: C ≥ 0,001 %", NA_character_))
  expect_equal(once$m_factors, c("M = 10", NA_character_, NA_character_))
  expect_equal(twice$specific_conc_limits, once$specific_conc_limits)
  expect_equal(twice$m_factors, once$m_factors)
})

test_that("缺少这两列时自愈原样返回", {
  df <- data.frame(x = 1:2)
  expect_equal(fcmsafety:::heal_cmr_split_cols(df), df)
})
