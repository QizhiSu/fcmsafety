# Tests for IARC group membership assignment
# Uses temp SQLite dbs so they run offline and fast.

# ---- helpers: temp db with minimal iarc + chemicals ----

make_chemicals_table <- function(con) {
  DBI::dbExecute(con, 'CREATE TABLE chemicals (
    InChIKey TEXT PRIMARY KEY,
    CID INTEGER,
    Formula TEXT,
    SMILES TEXT,
    IUPACName TEXT,
    ExactMass REAL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  )')
}

make_iarc_table <- function(con) {
  DBI::dbExecute(con, 'CREATE TABLE iarc (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    InChIKey TEXT,
    cas_no TEXT,
    agent TEXT,
    group_classification TEXT,
    volume INTEGER,
    volume_publication_year INTEGER,
    evaluation_year INTEGER,
    additional_information TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  )')
}

insert_chemical <- function(con, inchikey, formula, smiles) {
  DBI::dbExecute(con,
    'INSERT INTO chemicals (InChIKey, Formula, SMILES) VALUES (?, ?, ?)',
    params = list(inchikey, formula, smiles))
}

insert_iarc <- function(con, inchikey, cas, agent, grp) {
  DBI::dbExecute(con,
    'INSERT INTO iarc (InChIKey, cas_no, agent, group_classification) VALUES (?, ?, ?, ?)',
    params = list(inchikey, cas, agent, grp))
}

# ---- parse_formula_elements ----

test_that("parse_formula_elements extracts elements correctly", {
  expect_equal(fcmsafety:::parse_formula_elements("CdCl2"), c("Cd", "Cl"))
  expect_equal(fcmsafety:::parse_formula_elements("C6H12O6"), c("C", "H", "O"))
  expect_equal(fcmsafety:::parse_formula_elements("Cr+6"), c("Cr"))
  expect_equal(fcmsafety:::parse_formula_elements(NA_character_), character(0))
  expect_equal(fcmsafety:::parse_formula_elements(""), character(0))
})

# ---- parse_smiles_elements ----

test_that("parse_smiles_elements distinguishes Co vs CO", {
  # Co = cobalt
  expect_true("Co" %in% fcmsafety:::parse_smiles_elements("[Co]"))
  # CO = carbon + oxygen (organic)
  els <- fcmsafety:::parse_smiles_elements("C=O")
  expect_true("C" %in% els)
  expect_true("O" %in% els)
  expect_false("Co" %in% els)
})

test_that("parse_smiles_elements detects Cr in chromate", {
  els <- fcmsafety:::parse_smiles_elements("[O-][Cr](=O)(=O)[O-]")
  expect_true("Cr" %in% els)
  expect_true("O" %in% els)
})

test_that("parse_smiles_elements handles aromatic lower case", {
  els <- fcmsafety:::parse_smiles_elements("c1ccccc1")
  expect_true("C" %in% els)
})

# ---- parse_agent_elements ----

test_that("parse_agent_elements returns element symbols", {
  expect_equal(
    fcmsafety:::parse_agent_elements("Cadmium and cadmium compounds"),
    "Cd"
  )
  expect_equal(
    fcmsafety:::parse_agent_elements("Chromium (VI) compounds"),
    "Cr"
  )
  expect_equal(
    fcmsafety:::parse_agent_elements("Ethanol in alcoholic beverages"),
    character(0)
  )
})

# ---- query_iarc_group_registry ----

test_that("registry excludes see-rows and non-group entries", {
  db_path <- tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  make_chemicals_table(con)
  make_iarc_table(con)

  insert_chemical(con, "KEYCD", "Cd", "[Cd]")
  insert_chemical(con, "KEYAS", "As", "[As]")
  insert_chemical(con, "KEYBENZ", "C6H6", "c1ccccc1")

  insert_iarc(con, "KEYCD", "7440-43-9", "Cadmium and cadmium compounds", "1")
  insert_iarc(con, "KEYAS", "7440-38-2", "Arsenic and inorganic arsenic compounds", "1")
  insert_iarc(con, "KEYBENZ", "71-43-2", "Benzene", "1")
  # see-row (group_classification NULL)
  insert_iarc(con, "KEYBENZ", "71-43-2", "Benzene (see Monographs on Benzene)", NA_character_)

  DBI::dbDisconnect(con)

  reg <- fcmsafety:::query_iarc_group_registry(db_path = db_path)
  expect_equal(nrow(reg), 2)
  expect_true("Cadmium and cadmium compounds" %in% reg$agent)
  expect_true("Arsenic and inorganic arsenic compounds" %in% reg$agent)
  expect_false("Benzene" %in% reg$agent)
  expect_false("Benzene (see Monographs on Benzene)" %in% reg$agent)

  unlink(db_path)
})

test_that("registry auto-assigns layer by name features", {
  db_path <- tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  make_chemicals_table(con)
  make_iarc_table(con)

  insert_chemical(con, "KEYCR6", "Cr+6", "[Cr+6]")
  insert_chemical(con, "KEYCO", "Co", "[Co]")
  insert_chemical(con, "KEYETOH", "C2H6O", "CCO")
  insert_chemical(con, "KEYSIO2", "O2Si", "O=[Si]=O")

  insert_iarc(con, "KEYCR6", "18540-29-9", "Chromium (VI) compounds", "1")
  insert_iarc(con, "KEYCO", "7440-48-4", "Cobalt and cobalt compounds", "2B")
  insert_iarc(con, "KEYETOH", "64-17-5", "Ethanol in alcoholic beverages", "1")
  insert_iarc(con, "KEYSIO2", "14808-60-7", "Silica dust, crystalline, in the form of quartz or cristobalite", "1")

  DBI::dbDisconnect(con)

  reg <- fcmsafety:::query_iarc_group_registry(db_path = db_path)
  expect_equal(reg$layer[reg$agent == "Chromium (VI) compounds"], "valence")
  expect_equal(reg$layer[reg$agent == "Cobalt and cobalt compounds"], "element")
  # Ethanol in alcoholic beverages 不是组条目（无组词），不会出现在 registry 中
  expect_false("Ethanol in alcoholic beverages" %in% reg$agent)
  expect_equal(reg$layer[reg$agent == "Silica dust, crystalline, in the form of quartz or cristobalite"], "form")

  unlink(db_path)
})

# ---- screen_iarc_groups ----

test_that("CdCl2 hits Cadmium group with auto_confirmed", {
  db_path <- tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  make_chemicals_table(con)
  make_iarc_table(con)

  insert_chemical(con, "KEYCD", "Cd", "[Cd]")
  insert_iarc(con, "KEYCD", "7440-43-9", "Cadmium and cadmium compounds", "1")

  DBI::dbDisconnect(con)

  id <- list(elements = c("Cd", "Cl"), formula = "CdCl2", smiles = "[Cd]Cl",
             inchikey = NA_character_, input_type = "formula",
             source_note = "test")
  reg <- fcmsafety:::query_iarc_group_registry(db_path = db_path)
  res <- fcmsafety:::screen_iarc_groups(id, reg)

  expect_equal(nrow(res), 1)
  expect_equal(res$matched_agent, "Cadmium and cadmium compounds")
  expect_equal(res$confidence, "auto_confirmed")
  expect_true(grepl("cd", res$element_hits, ignore.case = TRUE))

  unlink(db_path)
})

test_that("chromate SMILES hits Chromium (VI) with probable", {
  db_path <- tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  make_chemicals_table(con)
  make_iarc_table(con)

  insert_chemical(con, "KEYCR6", "Cr+6", "[Cr+6]")
  insert_chemical(con, "KEYCR3", "Cr+3", "[Cr+3]")
  insert_iarc(con, "KEYCR6", "18540-29-9", "Chromium (VI) compounds", "1")
  insert_iarc(con, "KEYCR3", "16065-83-1", "Chromium (III) compounds", "3")

  DBI::dbDisconnect(con)

  id <- list(elements = c("Cr", "O"), formula = "CrO4", smiles = "[O-][Cr](=O)(=O)[O-]",
             inchikey = NA_character_, input_type = "smiles",
             source_note = "test")
  reg <- fcmsafety:::query_iarc_group_registry(db_path = db_path)
  res <- fcmsafety:::screen_iarc_groups(id, reg)

  expect_true("Chromium (VI) compounds" %in% res$matched_agent)
  vi_row <- res[res$matched_agent == "Chromium (VI) compounds", ]
  expect_equal(vi_row$confidence, "probable")

  unlink(db_path)
})

test_that("ethanol returns zero rows (no group hit)", {
  db_path <- tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  make_chemicals_table(con)
  make_iarc_table(con)

  # 必须至少有一个组条目，否则 registry 会报空
  insert_chemical(con, "KEYCD", "Cd", "[Cd]")
  insert_iarc(con, "KEYCD", "7440-43-9", "Cadmium and cadmium compounds", "1")

  insert_chemical(con, "KEYETOH", "C2H6O", "CCO")
  insert_iarc(con, "KEYETOH", "64-17-5", "Ethanol in alcoholic beverages", "1")

  DBI::dbDisconnect(con)

  id <- list(elements = c("C", "H", "O"), formula = "C2H6O", smiles = "CCO",
             inchikey = NA_character_, input_type = "smiles",
             source_note = "test")
  reg <- fcmsafety:::query_iarc_group_registry(db_path = db_path)
  res <- fcmsafety:::screen_iarc_groups(id, reg)

  expect_equal(nrow(res), 0)

  unlink(db_path)
})

test_that("organic-metal complex is manual_review not auto_confirmed", {
  db_path <- tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  make_chemicals_table(con)
  make_iarc_table(con)

  insert_chemical(con, "KEYCD", "Cd", "[Cd]")
  insert_iarc(con, "KEYCD", "7440-43-9", "Cadmium and cadmium compounds", "1")

  DBI::dbDisconnect(con)

  # 模拟一个有机镉化合物（如二甲基镉）
  id <- list(elements = c("C", "H", "Cd"), formula = "C2H6Cd", smiles = "C[Cd]C",
             inchikey = NA_character_, input_type = "smiles",
             source_note = "test")
  reg <- fcmsafety:::query_iarc_group_registry(db_path = db_path)
  res <- fcmsafety:::screen_iarc_groups(id, reg)

  expect_equal(nrow(res), 1)
  expect_equal(res$confidence, "manual_review")
  # 有机金属复合物被正确降级为 manual_review（evidence 含中文提示，locale 不稳定不测内容）

  unlink(db_path)
})

# ---- assign_group_membership integration ----

test_that("assign_group_membership returns structured result with attributes", {
  db_path <- tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  make_chemicals_table(con)
  make_iarc_table(con)

  insert_chemical(con, "KEYCD", "Cd", "[Cd]")
  insert_iarc(con, "KEYCD", "7440-43-9", "Cadmium and cadmium compounds", "1")

  DBI::dbDisconnect(con)

  res <- assign_group_membership("CdCl2", source = "iarc", db_path = db_path)
  expect_equal(nrow(res), 1)
  expect_equal(res$matched_agent, "Cadmium and cadmium compounds")
  expect_equal(attr(res, "input_type"), "formula")
  # note 包含命中计数（避免中文 locale 问题，直接检查数字）
  expect_true(grepl("1", attr(res, "note")))

  unlink(db_path)
})

test_that("assign_group_membership rejects data.frame with pointer to table version", {
  df <- data.frame(CAS = "7440-43-9", stringsAsFactors = FALSE)
  expect_error(
    assign_group_membership(df, source = "iarc"),
    "assign_group_membership_table"
  )
})

# ---- assign_group_membership_table (P1-② 批量版) ----

test_that("table version screens each row and keeps input_index mapping", {
  db_path <- tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  make_chemicals_table(con)
  make_iarc_table(con)
  insert_chemical(con, "KEYCD", "Cd", "[Cd]")
  insert_iarc(con, "KEYCD", "7440-43-9", "Cadmium and cadmium compounds", "1")
  DBI::dbDisconnect(con)

  d <- data.frame(
    NAME   = c("Cadmium metal", "Formaldehyde", "Cadmium chloride"),
    CAS    = c("7440-43-9", "50-00-0", NA),
    SMILES = c(NA, "C=O", NA),
    Formula = c(NA, NA, "CdCl2"),
    stringsAsFactors = FALSE
  )
  res <- assign_group_membership_table(d, source = "iarc", db_path = db_path)

  expect_equal(nrow(res), 2)                       # 行1 与 行3 命中，行2 苯不命中
  expect_setequal(res$input_index, c(1L, 3L))      # 能对回原输入行
  expect_true(all(c("input_index", "input") %in% names(res)))
  expect_equal(nrow(attr(res, "errors")), 0)
  expect_true(all(res$input %in% c("7440-43-9", "CdCl2")))

  unlink(db_path)
})

test_that("table version keeps going when a row has no resolvable identifier", {
  db_path <- tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  make_chemicals_table(con)
  make_iarc_table(con)
  insert_chemical(con, "KEYCD", "Cd", "[Cd]")
  insert_iarc(con, "KEYCD", "7440-43-9", "Cadmium and cadmium compounds", "1")
  DBI::dbDisconnect(con)

  d <- data.frame(
    NAME = c("Cadmium metal", "Some totally random substance name"),
    CAS  = c("7440-43-9", NA),
    SMILES = c(NA, NA),
    stringsAsFactors = FALSE
  )
  # 中文 message 在 C locale 下不可靠，用 message 前缀里的 ASCII 片段匹配
  expect_message(
    res <- assign_group_membership_table(d, source = "iarc", db_path = db_path),
    "assign_group_membership_table"
  )
  expect_equal(nrow(res), 1)                        # 行1 正常命中
  expect_equal(res$input_index, 1L)
  errs <- attr(res, "errors")
  expect_equal(nrow(errs), 1)                       # 行2 记录错误，不中断
  expect_equal(errs$input_index, 2L)

  unlink(db_path)
})

# ---- UVCB / 复杂物质扩展测试 ----

test_that("is_uvcb_name detects UVCB indicators", {
  expect_true(fcmsafety:::is_uvcb_name("Nonylphenol, branched and linear, ethoxylated"))
  expect_true(fcmsafety:::is_uvcb_name("reaction mass of ..."))
  expect_true(fcmsafety:::is_uvcb_name("polyethylene polymer"))
  expect_true(fcmsafety:::is_uvcb_name("Distillates (petroleum), heavy paraffinic"))
  expect_false(fcmsafety:::is_uvcb_name("Cadmium chloride"))
  expect_false(fcmsafety:::is_uvcb_name("Benzene"))
})

test_that("categorize_uvcb assigns correct labels", {
  expect_true("isomer_mixture" %in% fcmsafety:::categorize_uvcb("branched and linear"))
  expect_true("polymer" %in% fcmsafety:::categorize_uvcb("polymer"))
  expect_true("reaction_mass" %in% fcmsafety:::categorize_uvcb("reaction mass"))
  expect_true("distillate" %in% fcmsafety:::categorize_uvcb("distillate"))
  expect_true("mixture" %in% fcmsafety:::categorize_uvcb("complex combination"))
})

test_that("extract_backbone_keywords finds backbone terms", {
  kws <- fcmsafety:::extract_backbone_keywords("Nonylphenol, branched and linear, ethoxylated")
  expect_true("nonylphenol" %in% tolower(kws))
  expect_true("phenol" %in% tolower(kws))

  kws2 <- fcmsafety:::extract_backbone_keywords("Chlorinated paraffin C10-C13")
  expect_true("chlorinated paraffin" %in% tolower(kws2) || "paraffin" %in% tolower(kws2))
})

test_that("query_cmr_uvcb_registry returns expected columns", {
  db_path <- tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbExecute(con, 'CREATE TABLE cmr_raw (
    id INTEGER PRIMARY KEY, international_chemical_identification TEXT,
    cas_no TEXT, ec_no TEXT, hazard_class_and_category_codes TEXT,
    notes TEXT, Formula TEXT, SMILES TEXT, InChIKey TEXT
  )')
  DBI::dbExecute(con, 'INSERT INTO cmr_raw (id, international_chemical_identification, cas_no) VALUES (1, ?, ?)',
                 params = list("Nonylphenol, branched and linear", "-"))
  DBI::dbExecute(con, 'INSERT INTO cmr_raw (id, international_chemical_identification, cas_no) VALUES (2, ?, ?)',
                 params = list("Cadmium chloride", "10108-64-2"))
  DBI::dbDisconnect(con)

  reg <- fcmsafety:::query_cmr_uvcb_registry(db_path = db_path)
  expect_equal(nrow(reg), 1)
  expect_equal(reg$name, "Nonylphenol, branched and linear")
  expect_true("nonylphenol" %in% tolower(strsplit(reg$keywords, ";")[[1]]))

  unlink(db_path)
})

test_that("query_svhc_uvcb_registry filters SVHC UVCB entries", {
  db_path <- tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbExecute(con, 'CREATE TABLE svhc_raw (
    id INTEGER PRIMARY KEY, substance_name TEXT, cas_no TEXT,
    ec_no TEXT, reason_for_inclusion TEXT, description TEXT,
    remarks TEXT, Formula TEXT, SMILES TEXT, InChIKey TEXT
  )')
  DBI::dbExecute(con, 'INSERT INTO svhc_raw (id, substance_name, cas_no, description) VALUES (1, ?, ?, ?)',
                 params = list("4-Nonylphenol, branched and linear", "-", "UVCB substance"))
  DBI::dbExecute(con, 'INSERT INTO svhc_raw (id, substance_name, cas_no, description) VALUES (2, ?, ?, ?)',
                 params = list("Cadmium", "7440-43-9", "Carcinogen"))
  DBI::dbDisconnect(con)

  reg <- fcmsafety:::query_svhc_uvcb_registry(db_path = db_path)
  expect_equal(nrow(reg), 1)
  expect_equal(reg$name, "4-Nonylphenol, branched and linear")

  unlink(db_path)
})

test_that("screen_uvcb_groups matches by keyword", {
  db_path <- tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbExecute(con, 'CREATE TABLE chemicals (
    InChIKey TEXT PRIMARY KEY, CID INTEGER, Formula TEXT, SMILES TEXT,
    IUPACName TEXT, ExactMass REAL
  )')
  DBI::dbExecute(con, 'INSERT INTO chemicals (InChIKey, IUPACName) VALUES (?, ?)',
                 params = list("KEYNP", "4-nonylphenol"))
  DBI::dbDisconnect(con)

  reg <- data.frame(
    source_db = "svhc", entry_id = "svhc_1",
    name = "4-Nonylphenol, branched and linear",
    cas_no = "-", category = "isomer_mixture;derivative_mixture",
    keywords = "nonylphenol;phenol", reason = "Equivalent level of concern",
    notes = NA_character_, stringsAsFactors = FALSE
  )

  identity <- list(
    elements = c("C", "H", "O"), formula = "C15H24O",
    smiles = NA_character_, inchikey = "KEYNP",
    input_type = "formula", source_note = "test"
  )
  attr(identity, "raw_input") <- "nonylphenol"

  res <- fcmsafety:::screen_uvcb_groups(identity, reg, "svhc")
  expect_equal(nrow(res), 1)
  expect_equal(res$confidence, "probable")
  expect_true("nonylphenol" %in% tolower(strsplit(res$keyword_hits, ";")[[1]]))

  unlink(db_path)
})

test_that("assign_group_membership with source='all' returns combined results", {
  db_path <- tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)

  # chemicals
  DBI::dbExecute(con, 'CREATE TABLE chemicals (
    InChIKey TEXT PRIMARY KEY, CID INTEGER, Formula TEXT, SMILES TEXT,
    IUPACName TEXT, ExactMass REAL
  )')
  DBI::dbExecute(con, 'INSERT INTO chemicals (InChIKey, Formula, SMILES) VALUES (?, ?, ?)',
                 params = list("KEYCD", "Cd", "[Cd]"))

  # iarc
  DBI::dbExecute(con, 'CREATE TABLE iarc (
    id INTEGER PRIMARY KEY, InChIKey TEXT, cas_no TEXT,
    agent TEXT, group_classification TEXT
  )')
  DBI::dbExecute(con, 'INSERT INTO iarc (InChIKey, cas_no, agent, group_classification) VALUES (?, ?, ?, ?)',
                 params = list("KEYCD", "7440-43-9", "Cadmium and cadmium compounds", "1"))

  # cmr_raw
  DBI::dbExecute(con, 'CREATE TABLE cmr_raw (
    id INTEGER PRIMARY KEY, international_chemical_identification TEXT,
    cas_no TEXT, ec_no TEXT, hazard_class_and_category_codes TEXT,
    notes TEXT, Formula TEXT, SMILES TEXT, InChIKey TEXT
  )')
  DBI::dbExecute(con, 'INSERT INTO cmr_raw (id, international_chemical_identification, cas_no) VALUES (1, ?, ?)',
                 params = list("Nonylphenol, branched and linear", "-"))

  # svhc_raw
  DBI::dbExecute(con, 'CREATE TABLE svhc_raw (
    id INTEGER PRIMARY KEY, substance_name TEXT, cas_no TEXT,
    ec_no TEXT, reason_for_inclusion TEXT, description TEXT,
    remarks TEXT, Formula TEXT, SMILES TEXT, InChIKey TEXT
  )')
  DBI::dbExecute(con, 'INSERT INTO svhc_raw (id, substance_name, cas_no, description) VALUES (1, ?, ?, ?)',
                 params = list("4-Nonylphenol, branched and linear", "-", "UVCB"))

  DBI::dbDisconnect(con)

  # 输入含 Cd，应命中 IARC；同时名称 fallback 可能命中 UVCB
  res <- assign_group_membership("Cd", source = "all", db_path = db_path)
  # 至少命中 IARC Cadmium 组
  expect_true(any(res$source_db == "iarc"))

  unlink(db_path)
})
# ---- 元素层判据：骨架元素护栏与限定词处理 ----
# 背景：parse_agent_elements() 的映射表里曾有 Cyclamate -> "C" 与 Talc -> "Mg"，
# 于是 90% 的行（含碳即命中）被贴上 Cyclamates 标签；同时 screen_iarc_groups()
# 判断"有机骨架"时用 c("C","H","O","N") 配 any()，导致所有含氧或含氢的金属
# 化合物（砷酸、硫酸镉、氧化镍）被一律降级为 manual_review，组条目几乎废掉。

test_that("parse_agent_elements no longer maps backbone elements", {
  # 碳是一切有机物的骨架，镁是通用元素，都不构成条目特征
  expect_equal(fcmsafety:::parse_agent_elements("Cyclamates (sodium cyclamate)"),
               character(0))
  expect_equal(
    fcmsafety:::parse_agent_elements("Talc not containing asbestos or asbestiform fibres"),
    character(0))
  # 金属映射不受影响
  expect_equal(fcmsafety:::parse_agent_elements("Cadmium and cadmium compounds"), "Cd")
  expect_equal(
    fcmsafety:::parse_agent_elements(
      "Silica dust, crystalline, in the form of quartz or cristobalite"),
    "Si")
})

test_that(".skeletal_elements holds backbone and counter-ion elements only", {
  skel <- fcmsafety:::.skeletal_elements
  expect_true(all(c("c", "h", "o", "n", "s", "p", "cl", "br",
                    "na", "k", "ca", "mg", "si") %in% skel))
  # 特征金属不在表内：含砷对判断砷化合物是有意义的
  expect_false(any(c("as", "cd", "cr", "co", "hg", "se", "ni", "be", "pb") %in% skel))
  # 必须是小写，因为 common_els 恒为小写
  expect_false(any(skel != tolower(skel)))
})

# 手工构造一行注册表，便于单独测试元素层
make_reg_row <- function(agent, grp, layer, elements,
                         smiles_pattern = NA_character_,
                         negative_condition = NA_character_,
                         require_condition = NA_character_,
                         rationale = "") {
  data.frame(
    agent = agent, group_classification = grp, layer = layer, elements = elements,
    heuristic_type = "none", smiles_pattern = smiles_pattern,
    negative_condition = negative_condition, require_condition = require_condition,
    rationale = rationale,
    representative_formula = NA_character_, representative_smiles = NA_character_,
    stringsAsFactors = FALSE
  )
}

make_identity <- function(elements, formula, smiles) {
  list(elements = elements, formula = formula, smiles = smiles,
       inchikey = NA_character_, input_type = "smiles", source_note = "test")
}

test_that("backbone element as characteristic element is capped at manual_review", {
  reg <- make_reg_row("Test silicon compounds", "1", "element", "Si")
  res <- fcmsafety:::screen_iarc_groups(
    make_identity(c("O", "Si"), "O2Si", "O=[Si]=O"), reg)
  expect_equal(nrow(res), 1)
  expect_equal(res$confidence, "manual_review")
  expect_true(grepl("骨架", res$evidence))
})

test_that("characteristic metal element is unaffected by the backbone guard", {
  reg <- make_reg_row("Cadmium and cadmium compounds", "1", "element", "Cd")
  res <- fcmsafety:::screen_iarc_groups(make_identity("Cd", "Cd", "[Cd]"), reg)
  expect_equal(res$confidence, "auto_confirmed")
})

test_that("manual-layer entries never produce a hit", {
  # elements 为空的条目（甜蜜素、糖精、次氮基三乙酸、次氯酸盐）由注册表标成
  # layer = "manual"；这类条目不该产出任何命中。
  reg <- make_reg_row("Cyclamates (sodium cyclamate)", "3", "manual", "")
  res <- fcmsafety:::screen_iarc_groups(
    make_identity(c("C", "H", "O"), "C6H12O3S", "CS(=O)(=O)O"), reg)
  expect_equal(nrow(res), 0)
})

# 回归：这个 bug 让组条目几乎无法自动定级
test_that("inorganic metal salts containing oxygen stay auto_confirmed", {
  reg <- make_reg_row("Cadmium and cadmium compounds", "1", "element", "Cd")
  # 硫酸镉含 O，但显然是无机镉
  res <- fcmsafety:::screen_iarc_groups(
    make_identity(c("Cd", "S", "O"), "CdSO4", "[Cd+2].[O-]S(=O)(=O)[O-]"), reg)
  expect_equal(nrow(res), 1)
  expect_equal(res$confidence, "auto_confirmed")

  # 氧化镍同理
  reg_ni <- make_reg_row("Nickel, metallic", "2B", "element", "Ni")
  res2 <- fcmsafety:::screen_iarc_groups(
    make_identity(c("Ni", "O"), "NiO", "O=[Ni]"), reg_ni)
  expect_equal(res2$confidence, "auto_confirmed")
})

test_that("carbon-bearing metal complex is still demoted", {
  reg <- make_reg_row("Cadmium and cadmium compounds", "1", "element", "Cd")
  # 二甲基镉含碳骨架
  res <- fcmsafety:::screen_iarc_groups(
    make_identity(c("C", "H", "Cd"), "C2H6Cd", "C[Cd]C"), reg)
  expect_equal(res$confidence, "manual_review")
  expect_true(grepl("碳骨架", res$evidence))
})

# ---- 限定词：条目名里的 inorganic / crystalline 靠负向条件落地 ----

test_that("registry marks inorganic entries with a carbon exclusion", {
  db_path <- tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  make_chemicals_table(con)
  make_iarc_table(con)
  insert_chemical(con, "KEYAS", "As", "[As]")
  insert_chemical(con, "KEYHG", "Hg", "[Hg]")
  insert_chemical(con, "KEYSI", "O2Si", "O=[Si]=O")
  insert_iarc(con, "KEYAS", "7440-38-2", "Arsenic and inorganic arsenic compounds", "1")
  insert_iarc(con, "KEYHG", "7439-97-6", "Mercury and inorganic mercury compounds", "3")
  insert_iarc(con, "KEYSI", "14808-60-7",
              "Silica dust, crystalline, in the form of quartz or cristobalite", "1")
  DBI::dbDisconnect(con)

  reg <- fcmsafety:::query_iarc_group_registry(db_path = db_path)
  for (a in c("Arsenic and inorganic arsenic compounds",
              "Mercury and inorganic mercury compounds",
              "Silica dust, crystalline, in the form of quartz or cristobalite")) {
    expect_equal(reg$negative_condition[reg$agent == a], "C")
  }

  unlink(db_path)
})

test_that("arsenic: inorganic salt confirmed, organic arsenate excluded", {
  db_path <- tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  make_chemicals_table(con)
  make_iarc_table(con)
  insert_chemical(con, "KEYAS", "As", "[As]")
  insert_iarc(con, "KEYAS", "7440-38-2", "Arsenic and inorganic arsenic compounds", "1")
  DBI::dbDisconnect(con)

  reg <- fcmsafety:::query_iarc_group_registry(db_path = db_path)

  # 砷酸含 O 和 H，但它是实实在在的无机砷，必须能给 auto_confirmed
  res <- fcmsafety:::screen_iarc_groups(
    make_identity(c("O", "H", "As"), "H3AsO4", "O[As](=O)(O)O"), reg)
  expect_equal(nrow(res), 1)
  expect_equal(res$confidence, "auto_confirmed")

  # 三乙基砷酸酯含碳，被排除后交给"有机砷"条目（IARC 3 组）
  res2 <- fcmsafety:::screen_iarc_groups(
    make_identity(c("C", "H", "O", "As"), "C6H15AsO4", "CCO[As](=O)(OCC)OCC"), reg)
  expect_equal(nrow(res2), 0)

  unlink(db_path)
})

test_that("silica: organic silicon excluded, inorganic silicon kept", {
  db_path <- tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  make_chemicals_table(con)
  make_iarc_table(con)
  insert_chemical(con, "KEYSI", "O2Si", "O=[Si]=O")
  insert_iarc(con, "KEYSI", "14808-60-7",
              "Silica dust, crystalline, in the form of quartz or cristobalite", "1")
  DBI::dbDisconnect(con)

  reg <- fcmsafety:::query_iarc_group_registry(db_path = db_path)

  # 无机硅保留（仍停在 manual_review：SMILES 无法区分结晶与无定形）
  res <- fcmsafety:::screen_iarc_groups(
    make_identity(c("O", "Si"), "O2Si", "O=[Si]=O"), reg)
  expect_equal(nrow(res), 1)

  # 环硅氧烷 D4 含碳，与结晶二氧化硅无关
  res2 <- fcmsafety:::screen_iarc_groups(
    make_identity(c("C", "H", "O", "Si"), "C8H24O4Si4",
                  "C[Si]1(O[Si](O[Si](O[Si](O1)(C)C)(C)C)(C)C)C"), reg)
  expect_equal(nrow(res2), 0)

  # 硅烷偶联剂同理
  res3 <- fcmsafety:::screen_iarc_groups(
    make_identity(c("C", "H", "O", "Si"), "C8H18O4Si", "COCCO[Si](C=C)(OCCOC)OCCOC"), reg)
  expect_equal(nrow(res3), 0)

  unlink(db_path)
})

test_that("require_condition demands a specific element", {
  # 正向条件：条目只该收某一类。有机砷条目要求含碳。
  reg <- make_reg_row("Arsenobetaine and other organic arsenic compounds", "3",
                      "element", "As", require_condition = "C")
  # 含碳的有机砷 → 命中
  res <- fcmsafety:::screen_iarc_groups(
    make_identity(c("C", "H", "As", "O"), "C5H11AsO2", "C[As+](C)(C)CC(=O)[O-]"), reg)
  expect_equal(nrow(res), 1)
  # 不含碳的无机砷 → 不收
  res2 <- fcmsafety:::screen_iarc_groups(
    make_identity(c("O", "H", "As"), "H3AsO4", "O[As](=O)(O)O"), reg)
  expect_equal(nrow(res2), 0)
})

test_that("a registry without require_condition still works", {
  # 老版注册表没有这一列时不该报错（row[["require_condition"]] 返回 NULL）
  reg <- data.frame(
    agent = "Cadmium and cadmium compounds", group_classification = "1",
    layer = "element", elements = "Cd", heuristic_type = "none",
    smiles_pattern = NA_character_, negative_condition = NA_character_,
    rationale = "", representative_formula = NA_character_,
    representative_smiles = NA_character_, stringsAsFactors = FALSE)
  res <- fcmsafety:::screen_iarc_groups(make_identity("Cd", "Cd", "[Cd]"), reg)
  expect_equal(nrow(res), 1)
})

test_that("registry separates inorganic and organic arsenic entries", {
  db_path <- tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  make_chemicals_table(con)
  make_iarc_table(con)
  insert_chemical(con, "KEYAS", "As", "[As]")
  insert_iarc(con, "KEYAS", "7440-38-2", "Arsenic and inorganic arsenic compounds", "1")
  insert_iarc(con, "KEYAS", "7440-38-2",
              "Arsenobetaine and other organic arsenic compounds that are not metabolized in humans", "3")
  DBI::dbDisconnect(con)

  reg <- fcmsafety:::query_iarc_group_registry(db_path = db_path)
  expect_equal(reg$negative_condition[reg$agent == "Arsenic and inorganic arsenic compounds"], "C")
  expect_equal(
    reg$require_condition[
      reg$agent == "Arsenobetaine and other organic arsenic compounds that are not metabolized in humans"],
    "C")

  # 无机砷：只该命中组 1 那条，不再同时挂在"有机砷"条目下
  res <- fcmsafety:::screen_iarc_groups(
    make_identity(c("O", "H", "As"), "H3AsO4", "O[As](=O)(O)O"), reg)
  expect_equal(nrow(res), 1)
  expect_equal(res$matched_agent, "Arsenic and inorganic arsenic compounds")

  # 有机砷：只该命中组 3 那条
  res2 <- fcmsafety:::screen_iarc_groups(
    make_identity(c("C", "H", "O", "As"), "C6H15AsO4", "CCO[As](=O)(OCC)OCC"), reg)
  expect_equal(nrow(res2), 1)
  expect_equal(res2$iarc_group, "3")

  unlink(db_path)
})

test_that("registry flags entries without an element-layer judge as manual", {
  db_path <- tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  make_chemicals_table(con)
  make_iarc_table(con)
  insert_chemical(con, "KEYCD", "Cd", "[Cd]")
  insert_iarc(con, "KEYCD", "7440-43-9", "Cadmium and cadmium compounds", "1")
  insert_iarc(con, "KEYCD", "139-05-9", "Cyclamates (sodium cyclamate)", "3")
  insert_iarc(con, "KEYCD", "81-07-2", "Saccharin and its salts", "3")
  insert_iarc(con, "KEYCD", "14808-60-7",
              "Silica dust, crystalline, in the form of quartz or cristobalite", "1")
  DBI::dbDisconnect(con)

  reg <- fcmsafety:::query_iarc_group_registry(db_path = db_path)
  expect_equal(reg$layer[reg$agent == "Cyclamates (sodium cyclamate)"], "manual")
  expect_equal(reg$layer[reg$agent == "Saccharin and its salts"], "manual")
  # 有元素判据的条目不受影响
  expect_equal(reg$layer[reg$agent == "Cadmium and cadmium compounds"], "element")
  expect_equal(
    reg$layer[reg$agent == "Silica dust, crystalline, in the form of quartz or cristobalite"],
    "form")

  unlink(db_path)
})

# ---- (see X) 交叉引用解析 ----

test_that("parse_iarc_see_target handles plain, also, and nested-paren targets", {
  p <- fcmsafety:::parse_iarc_see_target

  expect_equal(p("Mustard gas (see Sulfur mustard)"), "Sulfur mustard")
  expect_equal(p("Lindane (see also Hexachlorocyclohexanes)"),
               "Hexachlorocyclohexanes")
  # 目标名自带括号，必须保留内层括号、只剥掉最外层收尾括号
  expect_equal(
    p("Chloromethyl methyl ether (see Bis(chloromethyl)ether; chloromethyl methyl ether)"),
    "Bis(chloromethyl)ether; chloromethyl methyl ether")
  expect_equal(p("2,4-D (2,4-dichlorophenoxyacetic acid) (See also Chlorophenols)"),
               "Chlorophenols")
  # 没有 (see) 的行返回 NA，不能误抓名字里的普通括号
  expect_true(is.na(p("Benzene")))
  expect_true(is.na(p("1-(2-Chloroethyl)-3-(4-methylcyclohexyl)-1-nitrosourea")))
  expect_true(is.na(p(NA_character_)))
  expect_equal(length(p(c("Benzene", "Mustard gas (see Sulfur mustard)"))), 2)
})

test_that("norm_iarc_name ignores spacing and punctuation differences", {
  n <- fcmsafety:::.norm_iarc_name
  expect_identical(n("Di(2-ethylhexyl) phthalate"),
                   n("Di(2-ethylhexyl)phthalate"))
  expect_identical(n("Acid mists"), n("acid  mists"))
  expect_identical(n("3,3'-Dimethoxybenzidine"), n("3,3'-Dimethoxybenzidine"))
})

test_that("group detection also covers cas-less and manually listed class entries", {
  db_path <- tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  make_chemicals_table(con)
  make_iarc_table(con)
  insert_chemical(con, "KEYCD", "Cd", "[Cd]")

  # 原有正则命中的那类
  insert_iarc(con, "KEYCD", "7440-43-9", "Cadmium and cadmium compounds", "1")
  # 无 CAS 的类条目 —— Progestins 就是这么被旧正则漏掉的
  insert_iarc(con, "KEYPRG", NA_character_, "Progestins", "2B")
  # 无 CAS 但是具体物质，不能被当成组条目
  insert_iarc(con, "KEYARE", NA_character_, "Arecoline", "2B")
  # 有 CAS、不含任何组词，靠人工表补进来
  insert_iarc(con, "KEYAFL", "1402-68-2", "Aflatoxins", "1")
  # 普通单一物质，不进注册表
  insert_iarc(con, "KEYBZ", "71-43-2", "Benzene", "1")
  DBI::dbDisconnect(con)

  reg <- fcmsafety:::query_iarc_group_registry(db_path = db_path)

  expect_true("Progestins" %in% reg$agent)
  expect_equal(reg$group_classification[reg$agent == "Progestins"], "2B")
  # 抽不出特征元素 -> 显式标成 manual，而不是隐形
  expect_equal(reg$layer[reg$agent == "Progestins"], "manual")
  expect_true("Aflatoxins" %in% reg$agent)
  expect_false("Arecoline" %in% reg$agent)
  expect_false("Benzene" %in% reg$agent)
  expect_true("Cadmium and cadmium compounds" %in% reg$agent)

  unlink(db_path)
})

test_that("see-alias map fills only rows whose key carries no group at all", {
  db_path <- tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  make_chemicals_table(con)
  make_iarc_table(con)
  insert_chemical(con, "KEYCD", "Cd", "[Cd]")

  # 目标在库：GaAs 分组为空，目标条目是组 1 -> 可补
  insert_iarc(con, "KEYGA", "1303-00-0",
              "Gallium arsenide (see Arsenic and inorganic arsenic compounds)",
              NA_character_)
  insert_iarc(con, "KEYAS", "7440-38-2",
              "Arsenic and inorganic arsenic compounds", "1")

  # 同一 InChIKey 上另有一条已带分组 -> 不许补，避免给全部同键物质硬套分级
  insert_iarc(con, "KEYTALC", "14807-96-6",
              "Talc containing asbestiform fibres (see Asbestos)", NA_character_)
  insert_iarc(con, "KEYTALC", "14807-96-6",
              "Talc not containing asbestos or asbestiform fibres", "3")
  insert_iarc(con, "KEYASB", NA_character_, "Asbestos", "1")

  # 目标不在库 -> 不补（宁可不报）
  insert_iarc(con, "KEYIOD", "10043-66-0", "Iodine-131 (see Radioiodines)",
              NA_character_)
  DBI::dbDisconnect(con)

  am <- fcmsafety:::query_iarc_see_alias_map(db_path = db_path)
  expect_equal(nrow(am), 1)
  expect_equal(am$InChIKey, "KEYGA")
  expect_equal(am$group_classification, "1")
  expect_equal(am$alias_of, "Arsenic and inorganic arsenic compounds")
  # 已被同键兄弟行覆盖 / 目标缺失的两条都不在其中
  expect_false("KEYTALC" %in% am$InChIKey)
  expect_false("KEYIOD" %in% am$InChIKey)

  unlink(db_path)
})

test_that("apply_iarc_see_aliases fills gaps but never overwrites evidence", {
  am <- data.frame(
    InChIKey = c("KEYGA", "KEYDEHP"),
    group_classification = c("1", "1"),
    alias_of = c("Arsenic and inorganic arsenic compounds", "X"),
    source_agent = c("Gallium arsenide (see ...)", "Y"),
    stringsAsFactors = FALSE)

  s <- data.frame(
    InChIKey = c("KEYGA", "KEYDEHP", "KEYNONE"),
    group_classification = c(NA, "2B", NA),
    stringsAsFactors = FALSE)
  out <- fcmsafety:::apply_iarc_see_aliases(s, am)

  expect_equal(out$group_classification[1], "1")      # 空缺补上
  expect_equal(out$group_classification[2], "2B")     # 已有证据不被别名覆盖
  expect_true(is.na(out$group_classification[3]))     # 别名里没有的仍留空
  expect_equal(attr(out, "see_alias_added"), 1L)

  # 空别名表 -> 原样返回，计数为 0
  out2 <- fcmsafety:::apply_iarc_see_aliases(s, NULL)
  expect_equal(attr(out2, "see_alias_added"), 0L)
  expect_equal(out2$group_classification, s$group_classification)
})
