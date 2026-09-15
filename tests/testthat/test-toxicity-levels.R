# 毒性等级 I–V（规则表 inst/toxicity_levels.png）
#
# 规则：
#   V  ：SVHC / CMR(H340,350,360) / EDC / IARC 1 组 / SML <= 0.018
#   IV ：CMR(H341,351,361) / CMR_suspect / IARC 2A、2B / 0.018 < SML <= 0.09 / Cramer III
#   III：0.09 < SML <= 0.54 / Cramer II
#   II ：0.54 < SML <= 1.8 / Cramer I
#   I  ：1.8 < SML <= 60
#
# 全无证据的行留空（渲染成 "-"），不冒充 I 级。

# 造一个"只填了某些参数"的调用
tier_call <- function(...) {
  args <- list(svhc = FALSE, cmr_h_codes = NA_character_, cmr_suspect = FALSE,
               edc = FALSE, iarc = NA_character_, sml_eu = NA_real_,
               sml_cn = NA_real_, cramer_rules = NA_character_)
  over <- list(...)
  args[names(over)] <- over
  fcmsafety:::compute_toxicity_levels(args$svhc, args$cmr_h_codes, args$cmr_suspect,
                                      args$edc, args$iarc, args$sml_eu, args$sml_cn,
                                      args$cramer_rules)
}
tier_of <- function(...) tier_call(...)$Toxic_level
basis_of <- function(...) tier_call(...)$Toxic_level_basis

test_that("每条规则各自映射到正确的等级", {
  expect_identical(tier_of(svhc = TRUE), "V")
  expect_identical(tier_of(cmr_h_codes = "H350"), "V")
  expect_identical(tier_of(cmr_h_codes = "H341"), "IV")
  expect_identical(tier_of(cmr_suspect = TRUE), "IV")
  expect_identical(tier_of(edc = TRUE), "V")
  expect_identical(tier_of(iarc = "1"), "V")
  expect_identical(tier_of(iarc = "2A"), "IV")
  expect_identical(tier_of(iarc = "2B"), "IV")
  # Cramer III 给 IV，II 给 III，I 给 II（规则表就是这么写的）
  expect_identical(tier_of(cramer_rules = "High (Class III)"), "IV")
  expect_identical(tier_of(cramer_rules = "Intermediate (Class II)"), "III")
  expect_identical(tier_of(cramer_rules = "Low (Class I)"), "II")
})

test_that("IARC 3 组与缺失都不构成证据", {
  expect_identical(tier_of(iarc = "3"), NA_character_)
  expect_identical(tier_of(iarc = NA_character_), NA_character_)
  expect_identical(tier_of(iarc = "  "), NA_character_)
})

test_that("SML 分档的每个边界都落在正确一侧", {
  # 闭区间在右侧：<= 才算属于本档
  expect_identical(tier_of(sml_eu = 0.018), "V")
  expect_identical(tier_of(sml_eu = 0.019), "IV")
  expect_identical(tier_of(sml_eu = 0.09), "IV")
  expect_identical(tier_of(sml_eu = 0.091), "III")
  expect_identical(tier_of(sml_eu = 0.54), "III")
  expect_identical(tier_of(sml_eu = 0.541), "II")
  expect_identical(tier_of(sml_eu = 1.8), "II")
  expect_identical(tier_of(sml_eu = 1.81), "I")
  expect_identical(tier_of(sml_eu = 60), "I")
  # > 60 在现库中取不到（EU 最大 60、中国最大 48），按 I 级收口
  expect_identical(tier_of(sml_eu = 600), "I")
  expect_identical(tier_of(sml_eu = 0.0005), "V")
})

test_that("EU 与 China SML 取更严的那个，依据里写明来源", {
  expect_identical(tier_of(sml_eu = 5, sml_cn = 0.05), "IV")
  expect_identical(basis_of(sml_eu = 5, sml_cn = 0.05), "SML:0.05(China)")
  expect_identical(basis_of(sml_eu = 0.05, sml_cn = 5), "SML:0.05(EU)")
  expect_identical(basis_of(sml_eu = 0.05, sml_cn = 0.05), "SML:0.05(EU+China)")
  expect_identical(basis_of(sml_eu = NA, sml_cn = 0.05), "SML:0.05(China)")
  expect_identical(basis_of(sml_eu = 30, sml_cn = NA), "SML:30(EU)")
})

test_that("多条件命中取最严，依据只列最严那一档的规则", {
  expect_identical(tier_of(svhc = TRUE, cmr_h_codes = "H341"), "V")
  expect_identical(basis_of(svhc = TRUE, cmr_h_codes = "H341"), "SVHC")
  expect_identical(tier_of(cmr_h_codes = "H350", sml_eu = 0.05), "V")
  # CMR 同时带 V 类与 IV 类码：取 V，依据只列 V 类的那个码
  expect_identical(basis_of(cmr_h_codes = "H360; H341"), "CMR:H360")
  # 同一档有多条并列时全部列出
  expect_identical(basis_of(svhc = TRUE, edc = TRUE), "SVHC; EDC")
})

test_that("全无证据的行返回 NA，不冒充 I 级", {
  res <- tier_call()
  expect_identical(res$Toxic_level, NA_character_)
  expect_identical(res$Toxic_level_basis, NA_character_)
})

test_that("cramer 分类字符串解析", {
  expect_identical(fcmsafety:::parse_cramer_class("High (Class III)"), "III")
  expect_identical(fcmsafety:::parse_cramer_class("Intermediate (Class II)"), "II")
  expect_identical(fcmsafety:::parse_cramer_class("Low (Class I)"), "I")
  expect_identical(fcmsafety:::parse_cramer_class("III"), "III")
  expect_identical(fcmsafety:::parse_cramer_class("I"), "I")
  expect_identical(fcmsafety:::parse_cramer_class(NA_character_), NA_character_)
  expect_identical(fcmsafety:::parse_cramer_class("-"), NA_character_)
  # 不能把 "Low (Class I)" 误判成 III
  expect_identical(fcmsafety:::parse_cramer_class("Low (Class I)"), "I")
})

# ---------------------------------------------------------------------------
# 同一物质多行：取最严，不看数据库行序
# ---------------------------------------------------------------------------

test_that("IARC 多分组冲突时取最严（1 > 2A > 2B > 3）", {
  d <- data.frame(
    InChIKey = c("K1", "K1", "K2", "K2", "K3", "K4", "K5", "K6"),
    group_classification = c("2B", "1", "1", "3", "3", "2A", NA, "4"),
    stringsAsFactors = FALSE
  )
  s <- fcmsafety:::summarise_iarc_groups(d)
  g <- function(k) s$group_classification[s$InChIKey == k]

  expect_identical(g("K1"), "1")    # 若取行序会得到 2B
  expect_identical(g("K2"), "1")    # 若取行序会得到 1，但反序就会丢成 3
  expect_identical(g("K3"), "3")
  expect_identical(g("K4"), "2A")
  expect_identical(g("K5"), NA_character_)
  expect_identical(g("K6"), "4")    # 未知分组在没有别的可选时保留
  expect_identical(nrow(s), 6L)

  # 未知分组不参与最严竞争
  d2 <- data.frame(InChIKey = c("Z", "Z"), group_classification = c("4", "2B"),
                   stringsAsFactors = FALSE)
  expect_identical(
    fcmsafety:::summarise_iarc_groups(d2)$group_classification, "2B")
})

test_that("IARC 空输入返回 0 行而不是报错", {
  d <- data.frame(InChIKey = character(0), group_classification = character(0),
                  stringsAsFactors = FALSE)
  expect_identical(nrow(fcmsafety:::summarise_iarc_groups(d)), 0L)
  expect_identical(nrow(fcmsafety:::summarise_iarc_groups(NULL)), 0L)
})

test_that("China SML 多行取最小值", {
  d <- data.frame(InChIKey = c("X", "X", "Y", "Z"),
                  sml_value = c("5", "0.05", "0.3", NA),
                  stringsAsFactors = FALSE)
  s <- fcmsafety:::summarise_china_sml(d)
  expect_identical(s$sml[s$InChIKey == "X"], 0.05)
  expect_identical(s$sml[s$InChIKey == "Y"], 0.3)
  expect_identical(s$sml[s$InChIKey == "Z"], NA_real_)
  expect_identical(nrow(s), 3L)

  empty <- data.frame(InChIKey = character(0), sml_value = character(0),
                      stringsAsFactors = FALSE)
  expect_identical(nrow(fcmsafety:::summarise_china_sml(empty)), 0L)
})

test_that("EU SML 把个体值与组限值一起取最小，并标记来源", {
  eu <- data.frame(
    InChIKey = c("A", "B", "C", "D", "E"),
    sml = c(0.05, NA, 5, NA, NA),
    # C：源表格两个单元格被读成一格，脏值；D：同一个物质属于 26 与 32 两组
    sml_group = c(NA, "26", "26", "26\r\n                     32", "999"),
    stringsAsFactors = FALSE
  )
  grp <- data.frame(group_no = c("26", "32"), sml = c(1.8, 60),
                    InChIKey = c(NA, NA), stringsAsFactors = FALSE)

  s <- fcmsafety:::summarise_eu_sml(eu, grp)
  pick <- function(k, col) s[[col]][s$InChIKey == k]

  expect_identical(pick("A", "sml"), 0.05)          # 个体值
  expect_false(pick("A", "from_group"))
  expect_identical(pick("B", "sml"), 1.8)           # 只有组号 -> 用组限值
  expect_true(pick("B", "from_group"))
  # C：个体 5 比组限值 1.8 松 -> 取 1.8，且标记来自组
  expect_identical(pick("C", "sml"), 1.8)
  expect_true(pick("C", "from_group"))
  # D：脏组号 "26\r\n 32" 应解析出两个组，取更严的 1.8
  expect_identical(pick("D", "sml"), 1.8)
  expect_identical(pick("D", "groups"), "26; 32")
  # E：组号在表里不存在 -> NA，不能变成 "NA*"
  expect_identical(pick("E", "sml"), NA_real_)
  expect_false(pick("E", "from_group"))

  empty <- data.frame(InChIKey = character(0), sml = numeric(0),
                      sml_group = character(0), stringsAsFactors = FALSE)
  expect_identical(nrow(fcmsafety:::summarise_eu_sml(empty, grp)), 0L)
})

test_that("组号文本按数字拆分（含脏值）", {
  x <- fcmsafety:::extract_group_nos(c("26\r\n                     32", "15", NA, ""))
  expect_identical(x[[1]], c("26", "32"))
  expect_identical(x[[2]], "15")
  expect_identical(x[[3]], character(0))
  expect_identical(x[[4]], character(0))
})

# ---------------------------------------------------------------------------
# 端到端：临时库里跑 assign_toxicity()
# ---------------------------------------------------------------------------

# eu_sml_group 的 InChIKey 一律为 NULL —— 与真实库一致（38 行全部如此）。
# 这条 fixture 专门用来钉住"按 InChIKey 查组限值永远查不到"那个 bug。
make_tier_fixture <- function() {
  db_path <- tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))

  for (sql in c(
    "CREATE TABLE chemicals (InChIKey TEXT PRIMARY KEY)",
    "CREATE TABLE svhc (InChIKey TEXT)",
    "CREATE TABLE cmr (InChIKey TEXT, hazard_statement_codes TEXT)",
    "CREATE TABLE cmr_suspect (InChIKey TEXT, substance_name TEXT)",
    "CREATE TABLE edc (InChIKey TEXT)",
    "CREATE TABLE iarc (InChIKey TEXT, group_classification TEXT)",
    "CREATE TABLE eu_sml (InChIKey TEXT, sml REAL, sml_group TEXT)",
    "CREATE TABLE eu_sml_group (group_no TEXT, sml REAL, InChIKey TEXT)",
    "CREATE TABLE china_sml (InChIKey TEXT, sml_value TEXT, unit TEXT)"
  )) DBI::dbExecute(con, sql)

  DBI::dbExecute(con, "INSERT INTO svhc (InChIKey) VALUES ('SVHCHITAAAAAA-UHFFFAOYSA-N')")
  DBI::dbExecute(con, "INSERT INTO edc (InChIKey) VALUES ('EDCHITAAAAAAA-UHFFFAOYSA-N')")
  DBI::dbExecute(con, "INSERT INTO cmr (InChIKey, hazard_statement_codes) VALUES ('CMRTIERIVAAAA-UHFFFAOYSA-N', 'H341\r\r\nH302\r\r\n')")
  DBI::dbExecute(con, "INSERT INTO cmr_suspect (InChIKey, substance_name) VALUES ('SUSPECTONLYAA-UHFFFAOYSA-N', 'suspect only')")

  # IARC 同一物质两行冲突，且"3"排在前面 —— 取行序会丢掉整条证据
  DBI::dbExecute(con, "INSERT INTO iarc (InChIKey, group_classification) VALUES ('IARCCONFLICT1-UHFFFAOYSA-N', '3')")
  DBI::dbExecute(con, "INSERT INTO iarc (InChIKey, group_classification) VALUES ('IARCCONFLICT1-UHFFFAOYSA-N', '2A')")

  # 只有组号、没有个体 SML 的 EU 物质；组限值表里 InChIKey 是 NULL
  DBI::dbExecute(con, "INSERT INTO eu_sml (InChIKey, sml, sml_group) VALUES ('EUSMLGROUPAA-UHFFFAOYSA-N', NULL, '26')")
  DBI::dbExecute(con, "INSERT INTO eu_sml_group (group_no, sml, InChIKey) VALUES ('26', 0.05, NULL)")

  # 中国 SML 两行数值不同（0.05 更严）
  DBI::dbExecute(con, "INSERT INTO china_sml (InChIKey, sml_value, unit) VALUES ('CHINASMLMULTI-UHFFFAOYSA-N', '5', 'mg/kg')")
  DBI::dbExecute(con, "INSERT INTO china_sml (InChIKey, sml_value, unit) VALUES ('CHINASMLMULTI-UHFFFAOYSA-N', '0.05', 'mg/kg')")

  db_path
}

test_that("assign_toxicity 端到端产出 Toxic_level 与依据", {
  db_path <- make_tier_fixture()
  d <- data.frame(
    NAME = c("svhc", "cmr tier IV", "edc", "iarc conflict", "eu group sml",
             "china sml", "cramer III", "no hits"),
    InChIKey = c("SVHCHITAAAAAA-UHFFFAOYSA-N", "CMRTIERIVAAAA-UHFFFAOYSA-N",
                 "EDCHITAAAAAAA-UHFFFAOYSA-N", "IARCCONFLICT1-UHFFFAOYSA-N",
                 "EUSMLGROUPAA-UHFFFAOYSA-N", "CHINASMLMULTI-UHFFFAOYSA-N",
                 "CRAMERIIIONLY-UHFFFAOYSA-N", "NOHITSAAAAAAA-UHFFFAOYSA-N"),
    SMILES = c(rep("CCO", 6), "CCN", "CCC"),
    stringsAsFactors = FALSE
  )
  # Mock run_toxtree to return Cramer III for "CCN"
  mock_result <- data.frame(
    SMILES = "CCN",
    Cramer.rules = "High (Class III)",
    stringsAsFactors = FALSE
  )
  testthat::local_mocked_bindings(
    run_toxtree = function(data, ...) mock_result,
    .package = "fcmsafety"
  )

  res <- assign_toxicity(d, db_path = db_path)

  expect_true(all(c("Toxic_level", "Toxic_level_basis") %in% names(res)))
  expect_identical(res$Toxic_level, c("V", "IV", "V", "IV", "IV", "IV", "IV", "-"))
  expect_identical(res$Toxic_level_basis,
                   c("SVHC", "CMR:H341", "EDC", "IARC:2A",
                     "SML:0.05(EU)", "SML:0.05(China)", "Cramer:III", "-"))

  # 毒性列块按 append 顺序连成一片。本 fixture 里没有 Flavornet / CAS_retrieved /
  # ExactMass 三个 relocate 锚点，所以不会被搬动，直接追加在末尾。
  block <- c("Cramer_rules", "SVHC", "CMR", "CMR_H_codes", "CMR_suspect",
             "EDC", "IARC", "EU_SML", "China_SML",
             "Toxic_level", "Toxic_level_basis")
  expect_identical(names(res)[seq_along(block) + 3L], block)

  # 回归：组限值必须真的取到（此前 eu_sml_group 按 InChIKey 过滤，永远 0 行）
  expect_identical(res$EU_SML[5], "0.05*")
  # 回归：取不到值时不能写出字面量 "NA*"
  expect_false(any(grepl("NA\\*", as.matrix(res), perl = TRUE)))
  expect_identical(res$EU_SML[1], "-")

  # 中国 SML 多行取更严的
  expect_identical(res$China_SML[6], "0.05")

  unlink(db_path)
})

test_that("有 relocate 锚点时整块毒性列被搬走，等级两列跟着一起走", {
  db_path <- make_tier_fixture()
  # ExactMass 后面还跟着别的列，才能看出 relocate 到底有没有生效
  d <- data.frame(NAME = "svhc",
                  InChIKey = "SVHCHITAAAAAA-UHFFFAOYSA-N",
                  ExactMass = 46.04,
                  extra = "keep me",
                  stringsAsFactors = FALSE)
  res <- assign_toxicity(d, db_path = db_path)

  block <- c("Cramer_rules", "SVHC", "CMR", "CMR_H_codes", "CMR_suspect",
             "EDC", "IARC", "EU_SML", "China_SML",
             "Toxic_level", "Toxic_level_basis")
  expect_identical(names(res)[4:14], block)      # 紧跟 ExactMass
  expect_identical(names(res)[15], "extra")      # 被挤到后面
  expect_identical(res$Toxic_level, "V")
  unlink(db_path)
})

test_that("没有 cmr / iarc / eu_sml 等表时定级退化为 '-' 而不是报错", {
  db_path <- tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbExecute(con, "CREATE TABLE chemicals (InChIKey TEXT PRIMARY KEY)")
  DBI::dbExecute(con, "CREATE TABLE svhc (InChIKey TEXT)")
  DBI::dbExecute(con, "INSERT INTO svhc (InChIKey) VALUES ('AAAABBBBCCCCDD-UHFFFAOYSA-N')")
  DBI::dbDisconnect(con)

  d <- data.frame(NAME = c("hit", "miss"),
                  InChIKey = c("AAAABBBBCCCCDD-UHFFFAOYSA-N",
                               "ZZZZYYYYXXXXWW-VVHHHHHHHH-N"),
                  stringsAsFactors = FALSE)
  res <- assign_toxicity(d, db_path = db_path)

  expect_identical(res$Toxic_level, c("V", "-"))
  expect_identical(res$Toxic_level_basis, c("SVHC", "-"))
  expect_identical(res$EU_SML, c("-", "-"))
  unlink(db_path)
})
