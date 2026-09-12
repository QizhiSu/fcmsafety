# Regression tests for EU 10/2011 consolidated page parsing.
# These tests use minimal HTML fixtures so they run offline and fast.

make_eu10_html <- function(sml_rows, group_rows, extra_sml = "") {
  # Build a minimal EUR-Lex-like page with two centered tables.
  # Each row vector becomes one <tr>; empty strings become empty <td>s.
  row_html <- function(cells, attrs = "") {
    cells <- vapply(cells, function(x) {
      if (is.na(x) || x == "") "<td></td>" else sprintf("<td>%s</td>", x)
    }, character(1), USE.NAMES = FALSE)
    sprintf("<tr%s>%s</tr>", attrs, paste(cells, collapse = ""))
  }

  sml_body <- paste(vapply(sml_rows, row_html, character(1)), collapse = "\n")
  group_body <- paste(vapply(group_rows, row_html, character(1)), collapse = "\n")

  sprintf(
    "<!DOCTYPE html><html><body>
    <div class=\"centered\">
      <table><tbody>
        %s
        %s
      </tbody></table>
    </div>
    <div class=\"centered\">
      <table><tbody>
        %s
      </tbody></table>
    </div>
    </body></html>
    ",
    extra_sml, sml_body, group_body
  )
}

# Old layout: title row, header row, data rows.
old_sml <- list(
  c("Chapter I - Positive list"),
  c("FCM substance No", "Substance name", "CAS No", "Ref No", "Restrictions and specifications"),
  c("1", "Substance A", "123-45-6", "10001", "T = 1 mg/kg"),
  c("2", "Substance B", "0000123-45-6", "10002", "")
)
old_group <- list(
  c("Chapter II - Group restrictions"),
  c("Group Restriction No", "FCM substance No", "SML (T) [mg/kg]", "Group restriction specification"),
  c("G1", "100", "10", "Group one")
)

test_that("parse_eu_sml_page handles old layout", {
  html <- make_eu10_html(old_sml, old_group)
  res <- fcmsafety:::parse_eu_sml_page(html)

  expect_named(res, c("SML", "SML_group"))
  expect_equal(nrow(res$SML), 2L)
  expect_equal(nrow(res$SML_group), 1L)
  expect_equal(res$SML[["FCM substance No"]], c("1", "2"))
  expect_equal(res$SML[["CAS No"]], c("123-45-6", "0000123-45-6"))
})

# New layout: column-number row, header row, data rows, amendment marker row.
new_sml <- list(
  c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)", "(9)", "(10)", "(11)", "(12)", "(13)"),
  c("FCM substance No", "Substance name", "CAS No", "Ref No", "Restrictions and specifications", "", "", "", "", "", "", "", ""),
  c("1", "Substance A", "123-45-6", "10001", "T = 1 mg/kg", "", "", "", "", "", "", "", ""),
  c("2", "Substance B", "0000123-45-6", "10002", "", "", "", "", "", "", "", "", ""),
  # Deletion marker row: the marker spreads across many columns via colspan
  c("3", "\u25bcM16 \u2014\u2014\u2014\u2014\u2014", "\u25bcM16", "", "", "", "", "", "", "", "", "", "")
)
new_group <- list(
  c("(1)", "(2)", "(3)", "(4)"),
  c("Group Restriction No", "FCM substance No", "SML (T) [mg/kg]", "Group restriction specification"),
  c("G1", "100", "10", "Group one")
)

test_that("parse_eu_sml_page handles new layout with column numbers and marker rows", {
  html <- make_eu10_html(new_sml, new_group)
  res <- fcmsafety:::parse_eu_sml_page(html)

  expect_equal(nrow(res$SML), 2L)
  expect_equal(nrow(res$SML_group), 1L)
  # Empty trailing columns should be dropped, not left as ...12 / _1
  expect_false(any(grepl("^\\.+[0-9]+", names(res$SML))))
  expect_false(any(names(res$SML) == "_1"))
  expect_false(any(names(res$SML_group) == "_1"))
  expect_equal(res$SML[["FCM substance No"]], c("1", "2"))
})

# Deduplication: duplicate substance numbers should keep the first occurrence.
dup_sml <- list(
  c("FCM substance No", "Substance name", "CAS No", "Ref No", "Restrictions and specifications"),
  c("1", "First", "123-45-6", "10001", ""),
  c("1", "Duplicate", "123-45-6", "10001", "")
)
dup_group <- list(
  c("Group Restriction No", "FCM substance No", "SML (T) [mg/kg]", "Group restriction specification"),
  c("G1", "100", "10", "Group one")
)

test_that("parse_eu_sml_page deduplicates substance numbers", {
  html <- make_eu10_html(dup_sml, dup_group)
  res <- fcmsafety:::parse_eu_sml_page(html)
  expect_equal(nrow(res$SML), 1L)
  expect_equal(res$SML[["Substance name"]][1], "First")
})

# Error handling: missing SML table should throw.
test_that("parse_eu_sml_page errors when SML table is missing", {
  html <- make_eu10_html(
    list(c("No relevant header here")),
    old_group
  )
  expect_error(fcmsafety:::parse_eu_sml_page(html), "could not locate the SML table")
})

# Error handling: missing group table should throw.
test_that("parse_eu_sml_page errors when group table is missing", {
  html <- make_eu10_html(
    old_sml,
    list(c("No relevant header here"))
  )
  expect_error(fcmsafety:::parse_eu_sml_page(html), "could not locate the group restrictions table")
})

# Version discovery (offline): extract consolidated CELEX versions from the
# base act page HTML, newest first and deduplicated.
test_that("extract_eu_sml_versions returns versions newest-first, deduplicated", {
  html <- paste0(
    "<a href=\".../CELEX:02011R0010-20200101\">2020</a>",
    "<a href=\".../CELEX:02011R0010-20230101\">2023</a>",
    "<a href=\".../CELEX:02011R0010-20221231\">2022</a>",
    "<a href=\".../CELEX:02011R0010-20230101\">2023 duplicate</a>"
  )
  expect_equal(
    fcmsafety:::extract_eu_sml_versions(html),
    c("02011R0010-20230101", "02011R0010-20221231", "02011R0010-20200101")
  )
})

test_that("extract_eu_sml_versions errors when no version link is found", {
  expect_error(
    fcmsafety:::extract_eu_sml_versions("<html><body>no version here</body></html>"),
    "no consolidated version"
  )
})

# Cellar (SPARQL) version discovery (offline).
test_that("parse_cellar_versions extracts CELEX ids newest-first, deduplicated", {
  json <- paste0(
    '{"head":{"vars":["celex"]},"results":{"bindings":[',
    '{"celex":{"type":"literal","value":"02011R0010-20200101"}},',
    '{"celex":{"type":"literal","value":"02011R0010-20260714"}},',
    '{"celex":{"type":"literal","value":"02011R0010-20200101"}}',
    ']}}'
  )
  expect_equal(
    fcmsafety:::parse_cellar_versions(json),
    c("02011R0010-20260714", "02011R0010-20200101")
  )
})

test_that("parse_cellar_versions returns an empty vector when there are no bindings", {
  json <- '{"head":{"vars":["celex"]},"results":{"bindings":[]}}'
  expect_equal(fcmsafety:::parse_cellar_versions(json), character(0))
})

test_that("cellar_eu_sml_query pins the base CELEX as an xsd:string literal", {
  q <- fcmsafety:::cellar_eu_sml_query("32011R0010")
  # An untyped literal matches nothing in the Cellar store, so the type matters.
  expect_true(grepl('"32011R0010"^^<http://www.w3.org/2001/XMLSchema#string>', q,
                    fixed = TRUE))
  expect_true(grepl("act_consolidated_based_on_resource_legal", q, fixed = TRUE))
})
