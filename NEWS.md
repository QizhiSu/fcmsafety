# fcmsafety 0.1.6 (unreleased)

## Rename R/ files to follow the data flow
File names now tell the pipeline story instead of mixing three unrelated
"app_*" meanings:
- update family: update_dbs.R (was update_other_dbs.R), update_svhc.R
  (was auto_update_svhc.R), update_pipeline.R (was incremental_update.R),
  update_guard.R (was run_guard.R); update_audit.R unchanged
- screening: assign_toxicity.R (was screening.R - renamed to reflect the main exported function)
- support: database.R (was sqlite_database_manager.R),
  report_export.R (was toxicity_report_export.R), main.R (was app_main.R),
  manual_lists.R (was app_manual.R)
- GUI trio: shiny_launch.R / shiny_ui.R / shiny_server.R (were
  app_launch.R / app_ui.R / app_server.R)
Pure renames - no code moved or changed; update_history ledger labels for
new writes use the new file names.

## Remove the group-entry matching subsystem and IARC aliases
- group_membership.R (1,621 lines: element judges, UVCB matching, the
  screen_* engine and its registry) is deleted, along with
  assign_group_membership() / assign_group_membership_table() exports and
  the group_membership / Group_hits / Group_IARC / Group_review surface of
  assign_toxicity(). The subsystem was default-off and not part of the
  owner's core flow.
- iarc_see_aliases.R (~370 lines: IARC (see X) cross-reference resolution,
  element extraction, group registry) is also deleted as it provided no
  practical value. ADRs 0009/0010 deleted.
  Historical versions on wip/consolidation-draft branch and in git history.

## PubChem metadata extraction returns to labtools; run_screening removed
- prepare_input() (offline CDK InChIKey derivation, ~700 lines plus the
  rcdk/rJava-only identity machinery) and run_screening() are removed per
  owner decision. The documented input path is now:
  labtools::extract_meta(data) -> assign_toxicity(data). labtools moves to
  Suggests with Remotes: QizhiSu/labtools; the GUI screening panel now
  uploads files that already carry InChIKey and calls assign_toxicity
  directly. ADRs 0004/0012 marked Superseded.
- All man/ pages regenerated from roxygen source; every block now carries
  @encoding UTF-8 in source (the reorganize commit had patched the .Rd
  files directly, which roxygenise overwrote). test-rd-docs guard: 0
  failures across 136 pages.

## Split the Shiny monolith into ui / server / launcher

## Split the Shiny monolith into ui / server / launcher
- `database_inspector_app.R` (3191 lines, the whole app inside one
  function) is now three files: `app_ui.R` (`fcm_app_ui()`, static UI
  assembly), `app_server.R` (`fcm_app_server()`, all reactive logic) and
  `database_inspector_app.R` (the exported `launch_database_inspector()`
  entry with port/browser handling). Behavior verified byte-identical:
  the served app HTML from before and after the split is the same.

## Drop one-off diagnostic scripts
- tools/archive/ (10 CMR-migration-era diagnostics) and the Sept troubleshooting
  scripts (audit_group_hits, compare_group_membership, probe_substance,
  diagnose_diff_columns, test_auto_update_line, rehearse_full_alignment,
  diagnose_group_element_layer.py) are deleted; git history keeps them.
  Kept: run_tests / run_one_test / run_check / check_section_markers /
  echacl_download.cjs / install_fcmsafety.sh.

## Remove the Wikipedia SVHC source entirely
- `fetch_svhc_wikipedia()` is deleted and the `"wikipedia"` source option is
  gone from `fetch_svhc_data()` / `update_svhc_auto()` /
  `update_database_auto(svhc_source = )`. The SVHC fetch chain is now
  ECHA CHEM (authoritative) -> local files in inst/, with no non-official
  source at any tier. The date/column-alias parsers keep tolerating
  Wikipedia-style values already stored in existing databases.

## The Shiny inspector can now run the full screening workflow
- New "Screen substances" panel in `launch_database_inspector()`: upload an
  xlsx/csv substance list, run the whole pipeline (structure completion ->
  regulatory matching -> toxicity grading), preview the Toxic_level
  distribution and regulatory-hit counts, and download the styled xlsx report
  or a CSV. The complete update -> import -> screen -> report workflow is now
  available without leaving the GUI.
- `assign_toxicity()` no longer aborts the whole screening when the automatic
  Toxtree run fails (no Java, jar download failure, ...): regulatory matching
  continues with blank Cramer columns - the same degradation policy already
  used when the input has no SMILES column. Regression-tested with a mocked
  failing run_toxtree.

## Share the seams between the SVHC line and the common pipeline
- `canon_cell()` gains an explicit `sort_multiline` switch and the drifted
  private `canon()` copy inside `diff_svhc_data()` is gone: one
  canonicalization implementation, with the incremental line sorting
  multi-line code lists (GHS/H codes, reorder = no change) and the SVHC line
  not sorting (remarks is prose where line order carries meaning). The old
  copy silently lacked the sort step - the same copy-and-drift pattern behind
  the cmr_suspect false-removal incident.
- Backup + update_history + change_log bookkeeping is now one shared
  `record_update_ledger()` / `backup_db_file()` pair used by both
  `write_changes_to_db()` and `write_svhc_to_db()`; the per-line differences
  (source_file, user_notes, key style) are parameters.
- `resolve_svhc_db_path()` (a verbatim copy of `.resolve_db_path()`) is
  deleted; its callers use the canonical helper.

## One source of truth for per-source manual-list detection
- `manual_candidates` (the file names probed as hand-dropped new lists) now
  live in the `DB_SOURCES` registry next to the fetch fallback lists, with the
  svhc_meta.xlsx "backup, not a new list" exclusion recorded once instead of
  twice. `check_manual_lists()` derives its probe set and its per-source
  dispatch from the registry, and the Shiny app's `UPDATE_DBS` queue is derived
  from `ALL_AUTO_DBS` - three more hand-maintained copies of the database list
  are gone, so adding a source cannot silently miss the manual-list probe.

## Deepen the update-line: one registry instead of nine touchpoints
- New internal `DB_SOURCES` registry holds every per-source difference (label,
  key columns, fetch file names/sheets/header layers, normalize fn, CLP H-code
  screening) as data. The four `fetch_*_data()` twins collapse into one
  `fetch_source_data()`; the four exported `update_*_auto()` entries keep their
  exact signatures and become thin shells over a registry-driven
  `update_source_auto()`. `ALL_AUTO_DBS` and the `update_database_auto()`
  dispatch are now derived from the registry, so adding a source no longer
  means editing a name list and a `switch()` by hand.
- Fixed a live drift bug this duplication had already caused: SVHC's safety
  valve message ("Cancelled: removed entries require manual review") was never
  registered in `translate_run_message()`, so the Shiny run panel showed that
  one row in English while every other database was translated.

## Harden the regulatory-database downloaders
- `download_clp()` plain-HTTP path now sends the full browser-like header set
  (ECHA's Azure WAF keys on `Accept: text/html`; the bare package UA was 403'd
  on every attempt) and retries with backoff, mirroring what already works for
  EUR-Lex. The Annex VI xlsx link is picked by highest ATP revision instead of
  "last link on the page", and absolute hrefs no longer produce a broken URL.
- The headless-browser fallback resolves `node` across platforms
  (`FCMSAFETY_NODE_BIN` -> `PATH` -> the original dev machine's bundled path);
  the Node script likewise tries common Chrome locations per OS and a
  `require.resolve("playwright-core")` before falling back to the bundled path.
- SVHC fetch chain "auto" is now ECHA -> local file. The Wikipedia mirror is no
  longer an automatic fallback (non-authoritative source, unreachable on some
  networks) and remains available as an explicit `source = "wikipedia"`.
- `download_iarc()` now validates the parsed table beyond the 500-row floor:
  required columns present, `Group` values restricted to 1/2A/2B/3, basic CAS
  format sampling, and a loud warning when the row count deviates >10% from
  the last good export.

## Stop missing group entries that carry no group word in their name
- Group entries were selected by a single name regex (`compounds`, `salts`,
  `metallic`, `dust`, …), so every class entry written as a plain plural noun
  was invisible. `Progestins` (IARC group 2B) is one: asking about progesterone
  returned "no evidence" even though the entry sat in the table.
  A manual audit of the `iarc` table found **15 such entries**, including
  `Aflatoxins` (group 1), `Polychlorinated biphenyls` (group 1),
  `Polybrominated biphenyls` (2A), `Hexachlorocyclohexanes` (2B),
  `Bleomycins` (2B), `Nodularins` (3), `Sulfites` (3) and several copolymer
  and foam entries. The registry held 24 entries; it now holds 39.
- Selection is now the union of three sources: the existing name regex, an
  empty `cas_no` (class entries rarely have a single CAS), and an explicit
  `.iarc_extra_group_entries` list. The list is deliberately explicit rather
  than a fuzzy heuristic — a "plural noun" rule also swallows `Dichlorvos`
  and `Tetrachlorvinphos`, which are ordinary substances.
  `Arecoline` is excluded by name: it is a single substance whose CAS column
  is simply blank in the source table.
- Most of the 15 do not yield characteristic elements, so they are registered
  as `layer = "manual"` and contribute nothing to grading. The point is that
  they are now **visible** instead of silently absent — this is the checklist
  that a member list or a scaffold layer has to work from.

## Read the IARC "(see X)" cross-references
- 36 rows in `iarc` are written `X (see Y)`, IARC's own statement that X's
  evaluation sits under entry Y. Nothing read them, so their
  `group_classification` stayed empty. Of the 24 such rows with no
  classification, 14 were already rescued by accident — they share an InChIKey
  with a classified row. The remaining 10 were silent.
- Aliases are now resolved by normalised name (`Di(2-ethylhexyl) phthalate`
  and `Di(2-ethylhexyl)phthalate` differ only by a space in this table).
  **Gallium arsenide** picks up group 1 from
  `Arsenic and inorganic arsenic compounds` and grades V.
- Aliases only **fill gaps**. A row whose InChIKey already carries a
  classification is skipped: one key can hold several distinct entries (three
  talc forms, three carbon nanotube entries share carbon's key), and an alias
  belonging to one of them must not be stamped onto all of them.
- The other nine resolve to nothing because their target entries are absent
  from the table altogether — `Asbestos`, `Dieldrin, and aldrin metabolized to
  dieldrin`, `Benzidine, dyes metabolized to`, `Radioiodines`, `Fission
  products`, `Acid mists`, `Bis(chloromethyl)ether`, and
  `Monographs on 1,3-Butadiene` (a pointer to a volume, not an entry). Those
  need the upstream list completed before anything can be done in code.

## Fix the IARC group entry matcher, which graded almost nothing
- Group entries such as "Cadmium and cadmium compounds" are matched by the
  elements they are named after. Two problems made that layer unreliable.
- **A backbone element was treated as a distinguishing feature.** The element
  map contained `Cyclamate -> C` and `Talc -> Mg`, so any compound containing
  carbon — 90% of the database — was labelled a cyclamate. Both mappings are
  gone. A new `.skeletal_elements` guard caps any entry whose characteristic
  element is part of every organic backbone (C/H/O/N/S/P, halogens, Na/K/Ca/Mg/Si)
  at `manual_review`: a hit on "contains carbon" says nothing about which group
  a compound belongs to.
- **The organic-skeleton check tested for oxygen and hydrogen, not for carbon.**
  It used `c("C", "H", "O", "N")` with `any()`, so every metal compound carrying
  an oxygen or a hydrogen — arsenate, cadmium sulfate, nickel oxide — was demoted
  to `manual_review`. Only species without O or H (gallium arsenide, the
  halides) survived, which is why the group layer changed almost no grades.
  The check now looks for carbon, which is what actually separates an organic
  complex from an inorganic salt.
- Result: hits fall from 2482 to 207 rows, from 2308 to 115 compounds, and
  entries that can contribute to grading rise from 35 to 50 rows.
- Entries with no usable element judge — cyclamates, saccharin,
  nitrilotriacetic acid, hypochlorite salts, talc — are marked
  `layer = "manual"` and skipped rather than matched on a wrong criterion.

## Honour the qualifiers in group entry names
- `Arsenic and inorganic arsenic compounds` (group 1) and `Silica dust,
  crystalline, …` (group 1) are narrowed by their names, but the element layer
  cannot see "inorganic" or "crystalline". Added a `negative_condition` of
  carbon: inorganic arsenic, inorganic mercury and inorganic silica contain no
  carbon, while organic arsenates, methylmercury and silicones all do. This
  keeps the four organic arsenic species (triethyl arsenate, methyl- and
  dimethylarsonic acid) out of the group 1 entry that they never belonged to,
  and drops silica from 58 to 23 rows — the 23 being genuine inorganic silicates.
- Added the symmetric `require_condition` field for the reverse case.
  `Arsenobetaine and other organic arsenic compounds` (group 3) now requires
  carbon, so arsenic metal, gallium arsenide and the arsenate salts no longer
  show up under an entry that is explicitly about organic species.
- Crystal form of silica remains undecidable from a structure, so those rows
  stay at `manual_review` rather than being auto-confirmed.

## Write a styled Excel report instead of a flat CSV
- `assign_toxicity(output_file = "….xlsx")` now writes a workbook, not a CSV.
  Passing a `.csv` path keeps the old behaviour, so existing scripts are
  unaffected. New export `export_toxicity_report()` does the writing and can
  re-export an existing result data.frame on its own.
- Four sheets: **Results** (the full table, frozen header, autofilter, fitted
  column widths, `Toxic_level` shaded V dark red → I green with `-` grey so a
  blank is not mistaken for "safe"); **Summary** (grade distribution, per-source
  match counts, and the provenance of the run); **Unassigned** (only the rows
  with no rule matched, for follow-up); **Issues** (see below).
- The Summary sheet names the database file that was actually read, its
  timestamp, and the package version. `get_db_connection()` picks `inst/…` or the
  user-data copy depending on the working directory, so two runs started from
  different folders could silently read two different databases. The choice is
  now reported instead of guessed at.
- Adds `openxlsx` to `Imports`.

## Make database failures visible instead of silent
- Each query helper caught its own error, printed a `message()` and returned an
  empty result, which the caller then treated as "no match". A database problem
  therefore produced a complete, well-formatted table in which every compound
  looked clean.
- Errors are now carried out of the query helpers on the returned object and
  collected into `query_issues`. `assign_toxicity()` prints them in a dedicated
  block, attaches them as `attr(result, "query_issues")`, and writes them to the
  **Issues** sheet. Tables that exist but hold zero rows are flagged too — that
  means the database was never populated, which is not the same as "no match".
- Run metadata is attached as `attr(result, "run_info")`.

## Fix a crash in the group-membership batch API
- `assign_group_membership_table()` combined its per-source results with
  `do.call(rbind, …)`, but the IARC branch returns `matched_agent / layer /
  element_hits` while the CMR and SVHC branches return `matched_entry / category
  / keyword_hits`. Any batch in which a row hit both kinds failed outright with
  `names do not match previous names`. It only showed up on larger inputs —
  small samples happened to hit one kind only (a 50-row sample passed, 100 rows
  did not). Columns are now aligned to their union before binding, in both the
  batch function and `.screen_identity()`.
- IARC hits now also populate `matched_entry` and `name`, so "what is this entry
  called" has a single answer regardless of source. `matched_agent` is unchanged.

## Grade compounds into toxicity levels I–V
- `assign_toxicity()` now returns `Toxic_level` ("I".."V") and `Toxic_level_basis`
  (the rule(s) behind the grade), implementing the table in
  `inst/toxicity_levels.png`. Until now the tier was never computed — `Toxic_level`
  only existed as a leftover string in `globalVariables()`. When several rules hit,
  the strictest wins and the basis lists the winning tier's rules only.
- A compound with no evidence at all gets `"-"`, not level I: the table awards
  tier I only for `1.8 < SML <= 60`, so a blank means "nothing found", not "found
  harmless". The run reports `Not assigned (no evidence): N` separately.
- China SML (GB 9685) counts alongside EU SML and the stricter of the two wins,
  with the source named in the basis (`SML:0.05(China)` / `(EU)` / `(EU+China)`).
  Both columns are mg/kg, so they compare directly.
- `CMR_suspect` counts as tier IV: `screen_clp()` selects that table for
  H341/H351/H361, which is exactly the tier IV definition.
- Cramer is a prediction, not a regulatory finding. The rule table puts it on the
  same footing as SVHC/CMR, so it is applied as written, but the basis tags its
  origin (`Cramer:III`) so a reviewer can tell the two apart.
- `Toxic_level` / `Toxic_level_basis` sit at the end of the toxicity block, and the
  `relocate()` range was widened from `Cramer_rules:China_SML` to
  `Cramer_rules:Toxic_level_basis` so they are not stranded at the far right.
- `SML > 60` is folded into tier I. The table stops at 60, and the bundled data
  holds no such value (EU tops out at exactly 60, China at 48).

## Take the strictest value when a substance has several rows
- `iarc`: 28 InChIKeys have more than one row and **11 of them carry conflicting
  groups** (`2B,1`, `1,3`, `3,2B`, `2A,3`, ...). `match()` took whatever row came
  first; when that row was `3`, `na_if("3")` turned it into NA and the IARC
  evidence vanished. The most severe group now wins (1 > 2A > 2B > 3), with
  unrecognised groups ranked last.
- `china_sml`: 209 InChIKeys have several rows and **4 have genuinely different
  values** (0.05 vs 5.0, 0.6 vs 3.0, 0.01 vs 0.05). The smallest now wins.
- `eu_sml`: 9 InChIKeys have several rows (identical values), so nothing changes
  in practice — it now takes the minimum for the same reason.

## Fix the EU group SML lookup, which never returned anything
- `eu_sml_group` is keyed by `group_no`, not by substance: **all 38 rows have a
  NULL `InChIKey`**, and `substance_name` holds a list of ref numbers instead. The
  query nevertheless filtered on `WHERE InChIKey IN (...)`, so it always returned
  zero rows.
- 126 rows of `eu_sml` carry a group number and **118 of them have no individual
  SML**, so those substances silently lost their SML evidence. The table is now
  read whole (it is tiny) and joined on `group_no`.
- Related: the group-limit marker was appended unconditionally
  (`paste0(EU_SML, "*")`), which printed the literal string `NA*` whenever the
  lookup came back empty. Being a string, it survived the final `NA -> "-"` pass
  and reached the output. The asterisk is now added only when a value is present.
- `sml_group` also holds merged cells (`"26\r\n 32"` — the substance belongs to
  both groups); group numbers are parsed out of the string and the strictest
  applicable limit wins.

## Declare the encoding of every Rd file
- Running `R CMD check` under a UTF-8 locale (required on this machine, see
  `tools/run_check.R`) reported `Non-ASCII contents without declared encoding` for
  83 Rd files / 709 occurrences, surfacing as 4 WARNINGs and 2 NOTEs. The package
  documents everything in Chinese, and roxygen2 writes `\encoding{UTF-8}` only when
  a block asks for it. A `LC_ALL=C` run never checked this, which is why it went
  unnoticed.
- Every documented roxygen block (120 blocks across 14 files) now ends with
  `#' @encoding UTF-8`; `@noRd` blocks are skipped. The tag has to go at the *end*
  of the block — roxygen treats plain text after a tag as a continuation of it.
- `R CMD check` goes from `5 WARNINGs, 2 NOTEs` to `1 WARNING`. The remaining one is
  `checking code files for non-ASCII characters`, which flags 9 `R/*.R`. Verified it
  is *not* about comments — Chinese in comments does not trigger it — but about
  string literals: Chinese `message()` / `stop()` text, Shiny UI labels, and the
  Chinese column-header candidates in `prepare_input()` ("\u7269\u8d28\u540d",
  "\u7ed3\u6784\u5f0f", ...). Those are deliberate and stay; escaping them into
  `\uXXXX` would make the constants unreadable.
- Fixed a bug in the new `extract_cmr_h_codes()` documentation: its `@description`
  carried a literal `"\r\r\n"`, which the Rd parser read as the unknown macros `\r`
  and `\n`. The separator is now described in words, and the rule is recorded:
  never write control-character escapes into roxygen prose.
- See `docs/adr/0006-20260910-rd-encoding-declaration.md`.

## Expose the CMR hazard statement codes that decide the toxicity tier
- `assign_toxicity()` used to report only `CMR = "Y"` for a substance listed in the
  `cmr` table. The tier rules (`inst/toxicity_levels.png`) key off *which* code it
  carries — H340/H350/H360 mean tier V, H341/H351/H361 mean tier IV — so a bare flag
  was not enough to grade a compound.
- New output column `CMR_H_codes` holds exactly those codes, taken from
  `cmr.hazard_statement_codes`, joined with `"; "` and listed tier-V first, or `"-"`
  when there are none. The existing `CMR` / `CMR_suspect` flags keep their meaning.
- CLP codes carry suffixes that must be folded before comparing: `H350i` (inhalation
  route), `H360FD` / `H360Df` / `H360F` / `H360D` (combined reproductive and
  developmental codes) and `H361f ***` (the asterisks mark a specific concentration
  limit). Codes are matched on their first four characters; a token like `H372 **`
  is ignored. Mixed lists still end up tier-V first regardless of input order.
- The same `InChIKey` can appear on several `cmr` rows (group entries such as
  `lead powder` and `lead massive`). All rows are merged before extraction so no
  code is lost to `match()` picking the first row only.
- When a compound is listed in `cmr` but carries none of H340/H350/H360, the run now
  says so on the console instead of silently reporting `CMR = "Y"` with no evidence.
  On the bundled database this never fires (325/325 matched keys carry a tier-V code).
- Note `cmr_suspect` has no hazard code column, so a compound present only there
  gets `CMR_suspect = "Y"` and `CMR_H_codes = "-"`; the flag itself is the tier-IV
  evidence in that case. See `docs/adr/0005-*.md`.

## Screen the local CLP source for CMR rows too (fetch_cmr_data)
- `fetch_cmr_data()` / `fetch_cmr_suspect_data()` applied the H-code screen only on
  the `download` branch. The `local` branch returned the source table unchanged, so
  dropping a full CLP export in as `clp_new.xlsx` would have written every row with
  a non-empty InChIKey into `cmr` as if it were CMR.
- Both functions now screen unconditionally after normalisation. Re-screening an
  already-screened file is a no-op, so nothing is lost, and a missing
  `Hazard Statement Code(s)` column now fails loudly instead of passing rows through.

## Add `prepare_input()`: go from "name + SMILES" to a screening-ready table, fully offline
- New exported `prepare_input()` fills in the identity columns a screening run
  needs (InChIKey / CID / Formula / ExactMass) from just **name + SMILES**, which
  is what a detection lab actually has. CAS and InChIKey are now optional inputs.
- Identity resolution is a priority ladder that stays offline wherever possible:
  given InChIKey -> CDK InChI computed locally -> exact `chemicals` lookup ->
  skeleton (first 14 chars) fallback -> local business-table name match ->
  PubChem (only when `online = TRUE`, off by default).
- Local SMILES-to-InChIKey uses CDK's InChI module via rJava. rcdk does not
  export any InChI function, but `rcdklibs` ships `cdk-inchi` plus the native
  `jna-inchi` library on the classpath. Accuracy was self-validated against the
  bundled database: recomputing InChIKey from the stored SMILES reproduced the
  stored key for 80/80 and 500/500 rows, 0 mismatches, ~27 ms per row, no network.
- SMILES canonicalization uses the `Absolute` flavor on purpose: `Canonical`
  collapses L-alanine, D-alanine and stereo-free alanine into one string, which
  would silently cross-match stereoisomers.
- Skeleton fallback only adopts a library key on a unique hit. Measured ambiguity
  is low (20 of 2419 skeletons, 0.83%); ambiguous rows are flagged, never silently
  resolved. The whole step can be disabled with `skeleton_fallback = FALSE`.
- Every row records `identity_method` (ASCII code: `given`, `smiles_cdk`,
  `db_skeleton`, `db_skeleton_ambiguous`, `db_name`, `pubchem_smiles`,
  `pubchem_name`) plus a human-readable `identity_source`. Unresolvable rows never
  abort the run; they are listed in `attr(x, "prepare_report")` and printed by
  `print_prepare_report()`.
- Column detection accepts common English and Chinese header variants, with
  `name_col` / `smiles_col` / `cas_col` / `inchikey_col` overrides. Chinese
  headers work in a Chinese/UTF-8 session locale; under a C locale R escapes
  non-ASCII source characters, so the function errors and points at `name_col`.
- `rcdk` stays in `Suggests`: when unavailable, `prepare_input()` warns and
  processes only rows that already carry an InChIKey instead of failing.

## Fix Toxtree dropping Cramer results for rows without CAS (replaces the P1-① guard)
- Toxtree 3.1.0 discards the whole CAS field when it is empty, shifting that
  output row one field left; the Cramer value then lands in the `CRAMERFLAGS`
  column and reads back as NA. The previous mitigation detected the shift and set
  those rows to NA, i.e. CAS-less rows simply lost their classification.
- `run_toxtree()` now writes a placeholder (`"N/A"`) into blank NAME/CAS fields
  before calling the CLI and restores the original values afterwards. Empirically
  any non-empty placeholder removes the shift, so rows without CAS now get a full
  Cramer result while the result table stays free of placeholders.

## Fix a single invalid SMILES aborting the whole Toxtree batch
- Toxtree silently drops SMILES it cannot parse, so the output has fewer rows than
  the input and the row-order alignment fails hard, losing Cramer results for the
  entire batch.
- `run_toxtree()` now screens unparsable SMILES locally with CDK first and
  re-inserts them afterwards in their original positions with NA results, keeping
  row count and row order identical to the input.
- Note when screening: `rcdk::parse.smiles()` returns a length-1 list holding
  `NULL` for bad input, so `length()` alone misclassifies a broken row as valid.

## Make the EU 10/2011 update robust when the EUR-Lex front-end is down (Cellar provider)
- `download_eu_sml()` now discovers consolidated versions and downloads the
  consolidated text through the Publications Office **Cellar** service
  (`publications.europa.eu`) instead of relying only on `eur-lex.europa.eu`:
  versions come from the Cellar SPARQL endpoint (predicate
  `cdm:act_consolidated_based_on_resource_legal`), and the text is fetched as
  XHTML via `Accept: application/xhtml+xml` on `/resource/celex/<ver>.ENG`.
- EUR-Lex stays as the fallback for both discovery and download. This fixes the
  failure where EUR-Lex's `?uri=CELEX:` front-end redirected every request to the
  "OJ of the day" page (service degradation), so `extract_eu_sml_versions()`
  found no `02011R0010-YYYYMMDD` link and aborted the EU SML update.
- New internal helpers (offline-tested): `cellar_eu_sml_query()`,
  `parse_cellar_versions()`, `fetch_cellar_sparql()`,
  `discover_eu_sml_versions_cellar()`, `fetch_cellar_consolidated()`.
- Verified end-to-end: the EU SML download now fetches the latest consolidated
  version `02011R0010-20260714` (904 SML rows / 38 group rows) where the previous
  run errored out.

## Fix Toxtree Cramer mis-assignment on empty CAS rows (P1-①)
- `.normalize_toxtree_output()` now aligns Toxtree CLI output to the input by
  row order instead of trusting the CLI's identity columns: empty-CAS rows (which
  made the CLI shift columns one step left) previously had their Cramer class
  silently lost or mis-assigned; now such rows are detected, warned about, rebuilt
  from the input identity columns, and their result columns set to NA.
- Hard error (instead of silent mis-alignment) when output row count differs from
  the SMILES-filtered input row count.

## Add batch group-membership screening (P1-②)
- New `assign_group_membership_table()` screens a whole data.frame (one substance
  per row) for "group entry" hits (e.g. cadmium compounds, nonylphenol family).
  Identifier priority per row: InChIKey > CAS > SMILES > Formula > NAME (NAME only
  with `online=TRUE`). Rows that cannot be resolved never abort the run; they are
  recorded in `attr(result, "errors")`. Output carries `input_index` so results
  merge back to the input rows.
- The scalar `assign_group_membership()` now rejects data.frame input with a clear
  pointer to the batch function instead of failing with a cryptic
  `'length = 6' in coercion to 'logical(1)'`.

## Make Toxtree optional in assign_toxicity() (P1-③)
- `assign_toxicity()` no longer stops when the Toxtree result file is missing and
  the data has no SMILES column: Cramer classification is skipped with a message
  and regulatory-list matching proceeds (the `Cramer_rules` column is still
  emitted, all "-"). Existing behavior (auto-rerun Toxtree when SMILES present,
  use existing file as-is) is unchanged.
- The `InChIKey` column check now runs before Toxtree handling, so data without an
  InChIKey column fails with the right error.
- `assign_toxicity()` gains `db_path = NULL` (passthrough) for tests and custom
  database deployments.

## Adopt ECHA ATP23 CMR export (schema drift + full re-import)
- `normalize_cmr_df()` maps the new ATP23 official column names
  (`Chemical Name` / `M, SCL, ATE` / `ATP`) onto the DB schema; the former
  fallback H-code column no longer exists in the new export.
- `map_to_db_columns()` records which source column fed each DB column
  (`attr(, "mapped_from")`); `backfill_unmapped_cols()` restores
  legacy-only columns from the DB by key before diffing, so a source that
  dropped a column no longer reports a whole-table spurious `modified`.
- `canon_row()` now trims leading/trailing whitespace, so historical xlsx
  cells ending with CRLF no longer diff against fresh official exports.
- PubChem enrichment strips the `[n]` index suffix ECHA appends to CAS
  numbers and tolerates missing property fields (NULL -> NA).
- DB upgraded to ATP23: `cmr` 1150 rows / `cmr_suspect` 495 rows, PubChem
  CIDs backfilled to 442, titanium dioxide removed from both tables.
- `tools/` adds the ATP23 dry-run / apply (preflight + transaction) /
  verify / diagnostic scripts used for the re-import.

## Add SVHC auto-update pipeline
- New `update_svhc_auto()`: fetch SVHC candidate list (Wikipedia / local file fallback),
  enrich chemical metadata via PubChem for new entries only, diff against the SQLite
  `svhc` table by InChIKey (CAS fallback), confirm, then write with backup + transaction.
- Background: ECHA's site is now behind Azure WAF, so the old `download_svhc()` POST
  channel returns 403. Use `source="local"` after manually saving an ECHA export to
  `inst/candidate_list.xlsx`, or `source="wikipedia"` when network allows.

## Fix CAS format mismatch in SVHC diff / backfill
- `svhc_key_of()` now canonicalizes the CAS fallback key, so `0266309-43-7` and
  `266309-43-7` (leading-zero variants) identify the same substance instead of being
  split into added + removed pairs.
- `diff_svhc_data()` content comparison canonicalizes the CAS column before signing,
  so format differences no longer report spurious `modified` rows.
- `write_svhc_to_db()` CAS-key deletion now matches rows in R after canonicalization,
  so legacy rows whose stored CAS differs in format are still removed.
- Regression tests added in `tests/testthat/test-cas-canonicalization.R`.

## Add fallback-key mechanism to all incremental updates
- All four generic databases (CMR, CMR_suspect, IARC, EU SML) now use a
  two-level identity key: official key first (`Index No` / `FCM substance No`
  / `CAS No.`), falling back to `CAS No` / `Agent` when the official key is
  missing. This matches the SVHC "InChIKey -> CAS -> Name" philosophy and
  prevents blank official keys from being misreported as removed + added.
- `write_changes_to_db()` accepts a `fallback_col` argument and deletes
  fallback rows by the fallback column when the primary key is blank.
- `run_incremental_update()` passes `fallback_col` through to the writer.
- Regression tests added in `tests/testthat/test-fallback-keys.R`.

# fcmsafety 0.1.5
## Fix the extract_meta() bug about CAS retrieval

# fcmsafety 0.1.4
## Add a function to assign meta data


# fcmsafety 0.1.2

## Enhancement

1. extract_cid() now support any of the following keys, "InChIKey", "CAS", 
or "Name", as well as their combinations. 
2. Add a default value to the cas_col argument in evaluate_compound().


## Bug fixes

1. evaluate_compound() "Error: Argument 1 must have names." error fixed.
2. Remove duplicates results if you have duplicate InChIKey values.
3. Remove warnings when load_databases().

## Others

1. Retrieval CAS number in the extract_meta() function instead of in evaluate_compound().

