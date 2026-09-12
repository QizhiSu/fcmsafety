# Regression tests for multi-value CAS extraction.
#
# The official CLP / IARC / EU SML exports write several CAS numbers into one
# cell. CLP marks each with a running index:
#     "10102-44-0 [1]\r\r\n10544-72-6 [2]\r\r\n12267-73-1 [3]"
# IARC separates with ", ", EU SML with a newline padded by spaces.
#
# The old enrichment path did `canonicalize_cas()` and kept the first
# segment. Two independent defects made every one of those rows fail:
#
#   1. canonicalize_cas() stripped leading zeros from the *last* segment
#      without checking it was all digits: "10102-44-0 [1]" -> "10102-44- [1]",
#      so the number became unusable after the bracket was removed.
#   2. The "[n]" marker was never removed inside canonicalize_cas(), and a
#      comma-separated list was not split at all.
#
# Measured on the live sources: 607 cmr rows + 115 eu_sml rows + 16 iarc rows
# were unusable. A PubChem sample of 19 such rows scored 0 hits with the old
# logic and 15 with the new one.

# ---- canonicalize_cas() must drop the [n] index marker ----

test_that("canonicalize_cas removes the [n] index marker", {
  expect_equal(fcmsafety:::canonicalize_cas("10102-44-0 [1]"), "10102-44-0")
  expect_equal(fcmsafety:::canonicalize_cas("1332-77-0 [1]"), "1332-77-0")
  expect_equal(fcmsafety:::canonicalize_cas("0266309-43-7 [12]"), "266309-43-7")
})

test_that("canonicalize_cas splits a multi-CAS cell and drops every marker", {
  expect_equal(
    fcmsafety:::canonicalize_cas("10102-44-0 [1]\n10544-72-6 [2]"),
    "10102-44-0;10544-72-6"
  )
  # \r\r\n is what the live CLP export actually contains
  expect_equal(
    fcmsafety:::canonicalize_cas("10102-44-0 [1]\r\r\n10544-72-6 [2]"),
    "10102-44-0;10544-72-6"
  )
})

test_that("canonicalize_cas still strips leading zeros on a plain last segment", {
  # guard against over-correcting: a bare "0" last segment must stay "0"
  expect_equal(fcmsafety:::canonicalize_cas("50-00-0"), "50-00-0")
  expect_equal(fcmsafety:::canonicalize_cas("0000050-00-0"), "50-00-0")
  expect_equal(fcmsafety:::canonicalize_cas("50-0-0"), "50-00-0")
})

# ---- extract_cas_candidates() ----

test_that("extract_cas_candidates returns all candidates in source order", {
  v <- "10102-44-0 [1]\n10544-72-6 [2]\n12267-73-1 [3]"
  expect_equal(fcmsafety:::extract_cas_candidates(v),
               list(c("10102-44-0", "10544-72-6", "12267-73-1")))
})

test_that("extract_cas_candidates handles every separator the sources use", {
  # newline (clp), \r\r\n (clp raw), comma (iarc), semi (manual), space (eu_sml)
  expect_equal(fcmsafety:::extract_cas_candidates("10043-35-3 [1]\n11113-50-1 [2]")[[1]],
               c("10043-35-3", "11113-50-1"))
  expect_equal(fcmsafety:::extract_cas_candidates("16543-55-8, 64091-91-4")[[1]],
               c("16543-55-8", "64091-91-4"))
  expect_equal(fcmsafety:::extract_cas_candidates("266309-43-7;50-00-0")[[1]],
               c("266309-43-7", "50-00-0"))
  expect_equal(fcmsafety:::extract_cas_candidates("0068515-48-0 0028553-12-0")[[1]],
               c("68515-48-0", "28553-12-0"))
})

test_that("extract_cas_candidates canonicalises each candidate", {
  # eu_sml zero-pads every CAS; candidates must come back normalised
  expect_equal(fcmsafety:::extract_cas_candidates("0000059-02-9\n0010191-41-0")[[1]],
               c("59-02-9", "10191-41-0"))
})

test_that("extract_cas_candidates skips placeholders and non-CAS text", {
  # the live data has rows whose first slot is a placeholder
  expect_equal(fcmsafety:::extract_cas_candidates("- [1]\n131929-60-7 [2]")[[1]],
               "131929-60-7")
  expect_equal(fcmsafety:::extract_cas_candidates("n/a")[[1]], character(0))
  expect_equal(fcmsafety:::extract_cas_candidates("")[[1]], character(0))
  expect_equal(fcmsafety:::extract_cas_candidates(NA_character_)[[1]], character(0))
  expect_equal(fcmsafety:::extract_cas_candidates("no cas here")[[1]], character(0))
})

test_that("extract_cas_candidates de-duplicates repeated numbers", {
  expect_equal(fcmsafety:::extract_cas_candidates("7085-19-0 [1]\n7085-19-0 [2]")[[1]],
               "7085-19-0")
})

test_that("extract_cas_candidates is vectorised and returns one list element per input", {
  out <- fcmsafety:::extract_cas_candidates(c("50-00-0", NA, "266309-43-7\n50-00-0"))
  expect_length(out, 3)
  expect_equal(out[[1]], "50-00-0")
  expect_equal(out[[2]], character(0))
  expect_equal(out[[3]], c("266309-43-7", "50-00-0"))
})

test_that("extract_cas_candidates ignores numbers that are not CAS-shaped", {
  # "12345" has no dashes, "10043-35" has only two segments
  expect_equal(fcmsafety:::extract_cas_candidates("12345")[[1]], character(0))
  expect_equal(fcmsafety:::extract_cas_candidates("10043-35")[[1]], character(0))
})

# ---- enrich_new_compounds() must fall through to the next candidate ----

enrich_test_df <- function(cas) {
  data.frame(
    cas_no = cas,
    substance_name = paste0("sub", seq_along(cas)),
    CID = NA_character_, Formula = NA_character_, SMILES = NA_character_,
    InChIKey = NA_character_, IUPACName = NA_character_, ExactMass = NA_character_,
    stringsAsFactors = FALSE
  )
}

test_that("enrich_new_compounds tries the next CAS when the first is not found", {
  df <- enrich_test_df("10102-44-0 [1]\n10544-72-6 [2]")
  seen <- character(0)
  testthat::local_mocked_bindings(
    pubchem_lookup_cas = function(cas, timeout = 30) {
      seen <<- c(seen, cas)
      if (identical(cas, "10544-72-6")) {
        list(CID = "12345", Formula = "N2O4", SMILES = "O=N(=O)ON=O",
             InChIKey = "KEY10544", IUPACName = "x", ExactMass = "92.0")
      } else {
        list(CID = NA_character_, Formula = NA_character_, SMILES = NA_character_,
             InChIKey = NA_character_, IUPACName = NA_character_,
             ExactMass = NA_character_)
      }
    },
    .package = "fcmsafety"
  )
  out <- fcmsafety:::enrich_new_compounds(df, "cas_no", "substance_name",
                                          delay = 0, verbose = FALSE)
  expect_equal(seen, c("10102-44-0", "10544-72-6"))
  expect_equal(out$InChIKey, "KEY10544")
})

test_that("enrich_new_compounds stops at the first hit", {
  df <- enrich_test_df("50-00-0\n266309-43-7")
  seen <- character(0)
  testthat::local_mocked_bindings(
    pubchem_lookup_cas = function(cas, timeout = 30) {
      seen <<- c(seen, cas)
      list(CID = "1", Formula = "CH2O", SMILES = "C=O",
           InChIKey = "KEYFORM", IUPACName = "x", ExactMass = "30.0")
    },
    .package = "fcmsafety"
  )
  out <- fcmsafety:::enrich_new_compounds(df, "cas_no", "substance_name",
                                          delay = 0, verbose = FALSE)
  expect_equal(seen, "50-00-0")          # 266309-43-7 never requested
  expect_equal(out$InChIKey, "KEYFORM")
})

test_that("enrich_new_compounds leaves a row untouched when no candidate resolves", {
  df <- enrich_test_df("- [1]\nnot a cas")
  n_calls <- 0L
  testthat::local_mocked_bindings(
    pubchem_lookup_cas = function(cas, timeout = 30) {
      n_calls <<- n_calls + 1L
      list(CID = NA_character_, Formula = NA_character_, SMILES = NA_character_,
           InChIKey = NA_character_, IUPACName = NA_character_,
           ExactMass = NA_character_)
    },
    .package = "fcmsafety"
  )
  out <- fcmsafety:::enrich_new_compounds(df, "cas_no", "substance_name",
                                          delay = 0, verbose = FALSE)
  expect_equal(n_calls, 0L)              # nothing worth requesting
  expect_true(is.na(out$InChIKey))
})

test_that("enrich_new_compounds caps how many candidates it tries per row", {
  # 一行 31 个 CAS 在 CLP 里真实存在；不封顶就会连发 31 次请求
  df <- enrich_test_df("50-00-0\n266309-43-7\n10043-35-3\n11113-50-1")
  seen <- character(0)
  testthat::local_mocked_bindings(
    pubchem_lookup_cas = function(cas, timeout = 30) {
      seen <<- c(seen, cas)
      list(CID = NA_character_, Formula = NA_character_, SMILES = NA_character_,
           InChIKey = NA_character_, IUPACName = NA_character_,
           ExactMass = NA_character_)
    },
    .package = "fcmsafety"
  )
  fcmsafety:::enrich_new_compounds(df, "cas_no", "substance_name",
                                   delay = 0, verbose = FALSE, max_cas_try = 2)
  expect_equal(seen, c("50-00-0", "266309-43-7"))
})

test_that("enrich_new_compounds skips rows that already have an InChIKey", {
  df <- enrich_test_df("50-00-0")
  df$InChIKey <- "ALREADY"
  n_calls <- 0L
  testthat::local_mocked_bindings(
    pubchem_lookup_cas = function(cas, timeout = 30) {
      n_calls <<- n_calls + 1L
      list(CID = NA_character_, Formula = NA_character_, SMILES = NA_character_,
           InChIKey = NA_character_, IUPACName = NA_character_,
           ExactMass = NA_character_)
    },
    .package = "fcmsafety"
  )
  out <- fcmsafety:::enrich_new_compounds(df, "cas_no", "substance_name",
                                          delay = 0, verbose = FALSE)
  expect_equal(n_calls, 0L)
  expect_equal(out$InChIKey, "ALREADY")
})
