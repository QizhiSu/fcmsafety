# prepare_input: 从"名称 + SMILES"补全结构标识（卡点 3）
# 全部离线：临时 SQLite fixture；自带 InChIKey / 骨架降级 / 名称匹配三条路径
# 都不碰网络。依赖 CDK InChI 模块的用例在 rcdk/rJava 不可用时自动 skip。
#
# 断言一律用 identity_method（ASCII 码）而不是 identity_source（中文）：
# 测试进程在 C locale 下读取测试文件时中文常量编码不可靠，会把断言变成假失败。

make_prepare_fixture <- function() {
  db_path <- tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)

  DBI::dbExecute(con, "CREATE TABLE chemicals (
    InChIKey TEXT PRIMARY KEY, CID TEXT, Formula TEXT, SMILES TEXT, ExactMass TEXT)")
  # 苯酚：精确匹配用例
  DBI::dbExecute(con, "INSERT INTO chemicals VALUES (?,?,?,?,?)",
                 params = list("ISWSIDIOOBJBQZ-UHFFFAOYSA-N", "996", "C6H6O",
                               "C1=CC=C(C=C1)O", "94.0419"))
  # 同一骨架的两个立体异构体：歧义用例
  DBI::dbExecute(con, "INSERT INTO chemicals VALUES (?,?,?,?,?)",
                 params = list("BITAPBDLHJQAID-MDZDMXLPSA-N", "1", "C20H41NO2",
                               "CCCCCCCC/C=C/CCCCCCCCN(CCO)CCO", "327.3137"))
  DBI::dbExecute(con, "INSERT INTO chemicals VALUES (?,?,?,?,?)",
                 params = list("BITAPBDLHJQAID-KTKRTIGZSA-N", "2", "C20H41NO2",
                               "CCCCCCCC/C=C\\CCCCCCCCN(CCO)CCO", "327.3137"))

  DBI::dbExecute(con, "CREATE TABLE svhc (InChIKey TEXT, substance_name TEXT)")
  DBI::dbExecute(con, "INSERT INTO svhc VALUES (?,?)",
                 params = list("ISWSIDIOOBJBQZ-UHFFFAOYSA-N", "Phenol"))

  # 其余业务表建成空表，避免名称汇总查询报错
  for (t in c("cmr", "cmr_suspect", "iarc", "eu_sml", "china_sml", "edc")) {
    if (t == "cmr") {
      DBI::dbExecute(con,
        "CREATE TABLE cmr (InChIKey TEXT, international_chemical_identification TEXT)")
    } else if (t == "iarc") {
      DBI::dbExecute(con, "CREATE TABLE iarc (InChIKey TEXT, agent TEXT)")
    } else {
      DBI::dbExecute(con, sprintf("CREATE TABLE %s (InChIKey TEXT, substance_name TEXT)", t))
    }
  }

  DBI::dbDisconnect(con)
  db_path
}

has_cdk <- function() {
  requireNamespace("rcdk", quietly = TRUE) &&
    requireNamespace("rJava", quietly = TRUE) &&
    !is.null(tryCatch(fcmsafety:::.fcm_inchi_factory(), error = function(e) NULL))
}

test_that("column requirements: missing SMILES or missing name errors out", {
  db_path <- make_prepare_fixture()
  on.exit(unlink(db_path))

  d1 <- data.frame(NAME = "Phenol", stringsAsFactors = FALSE)
  expect_error(prepare_input(d1, db_path = db_path, verbose = FALSE),
               "SMILES")

  d2 <- data.frame(SMILES = "C1=CC=C(C=C1)O", stringsAsFactors = FALSE)
  expect_error(prepare_input(d2, db_path = db_path, verbose = FALSE),
               "NAME")
})

test_that("column detection handles explicit names and non-ASCII headers", {
  db_path <- make_prepare_fixture()
  on.exit(unlink(db_path))

  d <- data.frame(
    name_raw = "Phenol", structure = "C1=CC=C(C=C1)O",
    InChIKey = "ISWSIDIOOBJBQZ-UHFFFAOYSA-N",
    stringsAsFactors = FALSE
  )
  res <- prepare_input(d, name_col = "name_raw", smiles_col = "structure",
                       db_path = db_path, verbose = FALSE)

  expect_true(all(c("NAME", "SMILES", "InChIKey") %in% names(res)))
  expect_identical(res$NAME, "Phenol")
  expect_identical(res$SMILES, "C1=CC=C(C=C1)O")
})

test_that("given InChIKey is kept as-is and metadata is backfilled", {
  db_path <- make_prepare_fixture()
  on.exit(unlink(db_path))

  d <- data.frame(
    NAME = "Phenol",
    SMILES = "C1=CC=C(C=C1)O",
    InChIKey = "ISWSIDIOOBJBQZ-UHFFFAOYSA-N",
    stringsAsFactors = FALSE
  )
  res <- prepare_input(d, db_path = db_path, verbose = FALSE)

  expect_identical(res$InChIKey, "ISWSIDIOOBJBQZ-UHFFFAOYSA-N")
  expect_identical(res$identity_method, "given")
  expect_identical(res$CID, "996")
  expect_identical(res$Formula, "C6H6O")
  expect_equal(res$ExactMass, 94.0419, tolerance = 1e-4)
})

test_that("skeleton fallback: unique skeleton hit adopts the library key", {
  db_path <- make_prepare_fixture()
  on.exit(unlink(db_path))

  d <- data.frame(
    NAME = "Phenol",
    SMILES = "C1=CC=C(C=C1)O",
    InChIKey = "ISWSIDIOOBJBQZ-AAAAAAAAA-N",   # bogus stereo block
    stringsAsFactors = FALSE
  )
  res <- prepare_input(d, db_path = db_path, verbose = FALSE)

  expect_identical(res$InChIKey, "ISWSIDIOOBJBQZ-UHFFFAOYSA-N")
  expect_identical(res$identity_method, "db_skeleton")
  expect_identical(res$CID, "996")
})

test_that("skeleton fallback: ambiguous skeleton is flagged, never silently picked", {
  db_path <- make_prepare_fixture()
  on.exit(unlink(db_path))

  d <- data.frame(
    NAME = "Oleyl diethanolamine",
    SMILES = "CCCCCCCC=CCCCCCCCN(CCO)CCO",
    InChIKey = "BITAPBDLHJQAID-AAAAAAAAA-N",
    stringsAsFactors = FALSE
  )
  res <- prepare_input(d, db_path = db_path, verbose = FALSE)

  expect_identical(res$identity_method, "db_skeleton_ambiguous")
  expect_true(grepl("BITAPBDLHJQAID", res$identity_note))
  expect_equal(attr(res, "prepare_report")$skeleton_ambiguous, 1)
})

test_that("skeleton fallback can be turned off", {
  db_path <- make_prepare_fixture()
  on.exit(unlink(db_path))

  d <- data.frame(
    NAME = "Phenol",
    SMILES = "C1=CC=C(C=C1)O",
    InChIKey = "ISWSIDIOOBJBQZ-AAAAAAAAA-N",
    stringsAsFactors = FALSE
  )
  res <- prepare_input(d, db_path = db_path, skeleton_fallback = FALSE,
                       verbose = FALSE)
  # 输入自带的键原样保留，且不会被骨架降级改写、也不会补出库内元数据
  expect_identical(res$InChIKey, "ISWSIDIOOBJBQZ-AAAAAAAAA-N")
  expect_identical(res$identity_method, "given")
  expect_true(is.na(res$CID))
  expect_equal(attr(res, "prepare_report")$skeleton_hit, 0)
})

test_that("unresolvable rows do not abort the run and land in the report", {
  db_path <- make_prepare_fixture()
  on.exit(unlink(db_path))

  d <- data.frame(
    NAME = c("Phenol", "Unknown substance"),
    SMILES = c("C1=CC=C(C=C1)O", "not-a-smiles"),
    InChIKey = c("ISWSIDIOOBJBQZ-UHFFFAOYSA-N", NA),
    stringsAsFactors = FALSE
  )
  res <- prepare_input(d, db_path = db_path, verbose = FALSE)

  expect_equal(nrow(res), 2)
  expect_identical(res$identity_method[1], "given")
  expect_true(is.na(res$InChIKey[2]))

  rep <- attr(res, "prepare_report")
  expect_equal(rep$total, 2)
  expect_equal(rep$unresolved, 1)
  expect_equal(rep$unresolved_rows$row, 2)
})

test_that("name fallback: matches local business tables when SMILES fails", {
  db_path <- make_prepare_fixture()
  on.exit(unlink(db_path))

  d <- data.frame(
    NAME = c("Phenol", "phenol"),
    SMILES = c("not-a-smiles", "still not smiles"),
    stringsAsFactors = FALSE
  )
  res <- prepare_input(d, db_path = db_path, verbose = FALSE)

  expect_identical(res$InChIKey, rep("ISWSIDIOOBJBQZ-UHFFFAOYSA-N", 2))
  expect_identical(res$identity_method, rep("db_name", 2))
  expect_identical(res$CID, rep("996", 2))
  expect_equal(attr(res, "prepare_report")$name_hit, 2)
})

test_that("name + SMILES only: InChIKey is derived locally (needs CDK)", {
  skip_if_not(has_cdk(), "rcdk / rJava not available")
  db_path <- make_prepare_fixture()
  on.exit(unlink(db_path))

  d <- data.frame(NAME = "Phenol", SMILES = "Oc1ccccc1", stringsAsFactors = FALSE)
  res <- prepare_input(d, db_path = db_path, verbose = FALSE)

  expect_identical(res$InChIKey, "ISWSIDIOOBJBQZ-UHFFFAOYSA-N")
  expect_identical(res$identity_method, "smiles_cdk")
  expect_identical(res$CID, "996")
  expect_identical(attr(res, "prepare_report")$from_smiles, 1L)
})

test_that("canonical SMILES keeps stereochemistry (needs CDK)", {
  skip_if_not(has_cdk(), "rcdk / rJava not available")
  db_path <- make_prepare_fixture()
  on.exit(unlink(db_path))

  d <- data.frame(
    NAME = c("L-alanine", "D-alanine", "alanine"),
    SMILES = c("C[C@@H](N)C(=O)O", "C[C@H](N)C(=O)O", "CC(N)C(=O)O"),
    stringsAsFactors = FALSE
  )
  res <- prepare_input(d, db_path = db_path, verbose = FALSE)

  ika <- res$InChIKey
  expect_false(is.na(ika[1]))
  expect_false(is.na(ika[2]))
  expect_false(identical(ika[1], ika[2]))                        # stereo must differ
  expect_false(identical(res$SMILES_canonical[1], res$SMILES_canonical[2]))
  expect_identical(substr(ika[1], 1, 14), substr(ika[3], 1, 14)) # same skeleton
})
