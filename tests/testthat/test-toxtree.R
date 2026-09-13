# Toxtree 集成测试：归一化单测 + CLI 集成冒烟

test_that("normalize: CLI 列名 Cramer rules（空格）归一化为 Cramer.rules", {
  # 模拟 2026-09-08 实测的 CLI 输出列结构
  cli_out <- data.frame(
    CAS = c("64-17-5", "71-43-2"),
    CRAMERFLAGS = c(NA, NA),
    `Cramer rules` = c("Low (Class I)", "High (Class III)"),
    NAME = c("Ethanol", "Benzene"),
    SMILES = c("CCO", "c1ccccc1"),
    `cdk:Title` = c(NA, NA),
    `toxTree.tree.cramer.CramerTreeResult` = c("1Y", "1N,2N"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  csv <- tempfile(fileext = ".csv")
  utils::write.csv(cli_out, csv, row.names = FALSE, na = "")
  input <- data.frame(
    NAME = c("Ethanol", "Benzene"),
    CAS = c("64-17-5", "71-43-2"),
    SMILES = c("CCO", "c1ccccc1"),
    stringsAsFactors = FALSE
  )
  res <- fcmsafety:::.normalize_toxtree_output(
    csv, "toxTree.tree.cramer.CramerRules", input)

  expect_true("Cramer.rules" %in% names(res))
  expect_identical(res$Cramer.rules, c("Low (Class I)", "High (Class III)"))
  expect_true(all(c("NAME", "CAS", "SMILES") %in% names(res)))
  expect_false("cdk:Title" %in% names(res))          # 恒空列被丢弃
  expect_true("toxTree.tree.cramer.CramerTreeResult" %in% names(res))  # 决策路径保留
})

test_that("normalize: 输出缺 SMILES 列时按行序回填并给出警告", {
  cli_out <- data.frame(
    CAS = "64-17-5",
    NAME = "Ethanol",
    `Cramer rules` = "Low (Class I)",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  csv <- tempfile(fileext = ".csv")
  utils::write.csv(cli_out, csv, row.names = FALSE, na = "")
  input <- data.frame(
    NAME = "Ethanol", CAS = "64-17-5", SMILES = "CCO",
    stringsAsFactors = FALSE
  )
  expect_warning(
    res <- fcmsafety:::.normalize_toxtree_output(
      csv, "toxTree.tree.cramer.CramerRules", input),
    "SMILES"
  )
  expect_identical(res$SMILES, "CCO")
})

test_that("normalize: 找不到结果列时报错并列出实际列名", {
  cli_out <- data.frame(
    NAME = "Ethanol", CAS = "64-17-5", SMILES = "CCO",
    stringsAsFactors = FALSE
  )
  csv <- tempfile(fileext = ".csv")
  utils::write.csv(cli_out, csv, row.names = FALSE, na = "")
  input <- cli_out
  expect_error(
    fcmsafety:::.normalize_toxtree_output(
      csv, "toxTree.tree.cramer.CramerRules", input),
    "Cramer result column"
  )
})

test_that("run_toxtree 集成冒烟（需 Java 与缓存中的 Toxtree，否则跳过）", {
  skip_on_cran()
  skip_if(!nzchar(Sys.which("java")), "Java not available")
  cache_dir <- tools::R_user_dir("fcmsafety", "cache")
  skip_if(!dir.exists(file.path(cache_dir, "toxtree_app")),
          "Toxtree app not in cache")

  d <- data.frame(
    NAME = c("Ethanol", "Benzene"),
    CAS = c("64-17-5", "71-43-2"),
    SMILES = c("CCO", "c1ccccc1"),
    stringsAsFactors = FALSE
  )
  out <- tempfile(fileext = ".csv")
  res <- run_toxtree(d, output = out)

  expect_true("Cramer.rules" %in% names(res))
  expect_equal(nrow(res), 2)
  expect_true(res$Cramer.rules[1] == "Low (Class I)")
  expect_true(res$Cramer.rules[2] == "High (Class III)")

  # assign_toxicity 的消费视角：read.csv 后 Cramer.rules 可用
  tox <- utils::read.csv(out)
  expect_true("Cramer.rules" %in% names(tox))
  expect_identical(tox$Cramer.rules, c("Low (Class I)", "High (Class III)"))
})

test_that("normalize: 身份列错位（空 CAS 致左移）被检测、按输入行序重建、结果置 NA", {
  # P1-① 回归：输入第 3 行 CAS 为空，是触发 Toxtree CLI 整行左移的典型场景
  input <- data.frame(
    NAME = c("Ethanol", "Benzene", "No-CAS compound"),
    CAS  = c("64-17-5", "71-43-2", NA),
    SMILES = c("CCO", "c1ccccc1", "CCN"),
    stringsAsFactors = FALSE
  )
  # 模拟 CLI 输出：前两行正常；第 3 行整行左移一列
  # （CAS 位=原 NAME、NAME 位=原 SMILES、SMILES 位=原空 CAS、
  #   CRAMERFLAGS 位=真实分级、Cramer rules 位=空）
  cli_out <- data.frame(
    CAS = c("64-17-5", "71-43-2", "No-CAS compound"),
    NAME = c("Ethanol", "Benzene", "CCN"),
    SMILES = c("CCO", "c1ccccc1", NA),
    CRAMERFLAGS = c(NA, NA, "Low (Class I)"),
    `Cramer rules` = c("Low (Class I)", "High (Class III)", NA),
    `toxTree.tree.cramer.CramerTreeResult` = c("1Y", "1N,2N", "1Y,3N"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  csv <- tempfile(fileext = ".csv")
  utils::write.csv(cli_out, csv, row.names = FALSE, na = "")

  expect_warning(
    res <- fcmsafety:::.normalize_toxtree_output(
      csv, "toxTree.tree.cramer.CramerRules", input),
    "identity columns do not match"
  )
  expect_identical(res$NAME, input$NAME)     # 身份列按输入行序重建
  expect_identical(res$CAS, input$CAS)
  expect_identical(res$SMILES, input$SMILES)
  expect_true(is.na(res$Cramer.rules[3]))    # 错位行结果置 NA，不静默错配
  expect_identical(res$Cramer.rules[1:2],
                   c("Low (Class I)", "High (Class III)"))
})

test_that("normalize: 输出行数与输入不符时报错拒绝对齐", {
  input <- data.frame(
    NAME = c("Ethanol", "Benzene"),
    CAS = c("64-17-5", "71-43-2"),
    SMILES = c("CCO", "c1ccccc1"),
    stringsAsFactors = FALSE
  )
  cli_out <- data.frame(
    CAS = c("64-17-5", "71-43-2", "50-00-0"),
    NAME = c("Ethanol", "Benzene", "Formaldehyde"),
    SMILES = c("CCO", "c1ccccc1", "C=O"),
    `Cramer rules` = rep("Low (Class I)", 3),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  csv <- tempfile(fileext = ".csv")
  utils::write.csv(cli_out, csv, row.names = FALSE, na = "")

  expect_error(
    fcmsafety:::.normalize_toxtree_output(
      csv, "toxTree.tree.cramer.CramerRules", input),
    "rows"
  )
})

test_that("空白 NAME/CAS 用占位符填充：无 CAS 的行也能拿到 Cramer 分级", {
  # 2026-09-10 定性：Toxtree CLI 在输入行 CAS 为空时会把该行输出整行左移一格
  # （CAS 字段被整个丢弃），导致 Cramer 值落进 CRAMERFLAGS 列而读回为 NA。
  # run_toxtree 现在先把空字段填成占位符，实测可完全避免错位。
  skip_on_cran()
  skip_if(!nzchar(Sys.which("java")), "Java not available")
  cache_dir <- tools::R_user_dir("fcmsafety", "cache")
  skip_if(!dir.exists(file.path(cache_dir, "toxtree_app")),
          "Toxtree app not in cache")

  d <- data.frame(
    NAME = c("Phenol", "Bisphenol A", "Benzene", "L-alanine"),
    CAS  = c("108-95-2", NA, "71-43-2", NA),   # 混合：两行无 CAS，正是触发条件
    SMILES = c("Oc1ccccc1", "CC(C)(c1ccc(O)cc1)c1ccc(O)cc1",
               "c1ccccc1", "C[C@@H](N)C(=O)O"),
    stringsAsFactors = FALSE
  )
  out <- tempfile(fileext = ".csv")
  res <- suppressWarnings(run_toxtree(d, output = out))

  # 关键断言：无 CAS 的行不再丢分级
  expect_false(any(is.na(res$Cramer.rules)))
  expect_equal(nrow(res), 4)

  # 分级值与物质正确对应（不是错位后的错配）
  expect_identical(res$Cramer.rules[1], "Low (Class I)")     # phenol
  expect_identical(res$Cramer.rules[3], "High (Class III)")  # benzene

  # 占位符不得泄进结果：原 CAS 仍为 NA
  expect_true(is.na(res$CAS[2]))
  expect_true(is.na(res$CAS[4]))
  expect_false(any(res$CAS %in% "N/A", na.rm = TRUE))

  # 落盘文件同样干净
  tox <- utils::read.csv(out)
  expect_false(any(tox$CAS %in% "N/A", na.rm = TRUE))
  expect_false(any(is.na(tox$Cramer.rules)))
})

test_that("非法 SMILES 不会毁掉整批：该行结果 NA，其余行照常分级", {
  # Toxtree 会静默丢弃解析不了的 SMILES，导致输出行数少于输入，
  # 按行序对齐随即失败。run_toxtree 现在先把这类行挡在批处理之外，
  # 跑完再按原位置合并回来（行数/行序不变）。
  skip_on_cran()
  skip_if(!nzchar(Sys.which("java")), "Java not available")
  cache_dir <- tools::R_user_dir("fcmsafety", "cache")
  skip_if(!dir.exists(file.path(cache_dir, "toxtree_app")),
          "Toxtree app not in cache")
  skip_if_not(requireNamespace("rcdk", quietly = TRUE), "rcdk not available")

  d <- data.frame(
    NAME = c("Phenol", "Broken one", "Benzene"),
    CAS  = c("108-95-2", NA, "71-43-2"),
    SMILES = c("Oc1ccccc1", "not-a-real-smiles", "c1ccccc1"),
    stringsAsFactors = FALSE
  )
  out <- tempfile(fileext = ".csv")
  res <- suppressWarnings(run_toxtree(d, output = out))

  expect_equal(nrow(res), 3)                                  # 行数不变
  expect_identical(res$NAME, c("Phenol", "Broken one", "Benzene"))
  expect_identical(res$Cramer.rules[1], "Low (Class I)")
  expect_true(is.na(res$Cramer.rules[2]))                     # 坏行 -> NA
  expect_identical(res$Cramer.rules[3], "High (Class III)")
})
