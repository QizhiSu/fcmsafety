fcmsafety
================

true

<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

<!-- badges: end -->

# Introduction

The goal of fcmsafety is to screen compounds from food contact materials
against regulatory hazard lists and assign a toxicity level (I–V) to
each substance. It takes into account the toxicity data from:

1.  Substances of Very High Concern (SVHC) from ECHA
    (<https://echa.europa.eu/candidate-list-table>);
2.  Carcinogenic, Mutagenic, and Reprotoxic (CMR) and CMR-suspect
    entries from the Classification, Labelling, and Packaging (CLP)
    regulation, Annex VI
    (<https://echa.europa.eu/information-on-chemicals/annex-vi-to-clp>);
3.  Carcinogenic substances from IARC
    (<https://monographs.iarc.who.int/list-of-classifications>);
4.  Endocrine Disrupting Chemicals (EDC) from The International Panel on
    Chemical Pollution (IPCP) commissioned by UN Environment
    (<https://www.unep.org/explore-topics/chemicals-waste/what-we-do/emerging-issues/scientific-knowledge-endocrine-disrupting>);
5.  Specific Migration Limit (SML) from EU 10/2011 regulation
    (<https://eur-lex.europa.eu/legal-content/EN/TXT/HTML/?uri=CELEX:02011R0010-20200923&qid=1636402301680&from=en>);
6.  Specific Migration Limit (SML) from China GB 9685 regulation
    (provided by IQTC).

Every row of the output carries a `Toxic_level` (I–V, strictest evidence
wins) plus a `Toxic_level_basis` column that states *why* the level was
assigned. An empty level (`-`) means “no evidence found” — it never
means “safe”. Group entries (e.g. IARC group entries such as
*Aflatoxins*, lead and its compounds) are covered by a group-entry
registry; pass `group_membership = TRUE` to include group-level
judgment.

All regulatory tables live in one SQLite file (`inst/fcmsafety.db`) that
ships with the package, so screening works fully offline.

# Installation

``` r
install.packages("remotes")
# Development happens on the fcmsafety-v2 branch:
remotes::install_github("QizhiSu/fcmsafety", ref = "fcmsafety-v2")
```

# Quick start: one call

Your input needs at least a name column and a `SMILES` column. If it
already contains an `InChIKey` column (e.g. produced by
[***labtools***](https://github.com/QizhiSu/labtools)), it is used as-is
and no structure derivation happens.

``` r
library(fcmsafety)

substances <- data.frame(
  NAME   = c("Bisphenol A", "Ethanol"),
  SMILES = c("CC(C)(c1ccc(O)cc1)c1ccc(O)cc1", "CCO")
)

res <- run_screening(substances, output_file = "screening.xlsx")
print_prepare_report(res)   # which rows could not be resolved, and why
```

`run_screening()` derives missing structure identifiers offline (local
CDK, no network), matches the regulatory databases, assigns the toxicity
level, and writes a styled Excel report with four sheets (Results /
Summary / Unassigned / Issues). Use `online = TRUE` to fall back to
PubChem for rows that cannot be resolved locally.

# Update the regulatory databases

``` r
update_database_auto()   # refresh all sources (online, incremental)
```

Each source can also be updated individually, and a dry run that shows
the diff without writing is the default for the individual functions:

``` r
update_svhc_auto()                  # SVHC candidate list
update_cmr_auto(enrich = TRUE)      # CMR from CLP Annex VI (H340/H350/H360)
update_cmr_suspect_auto()           # CMR suspects from CLP (H341/H351/H361)
update_iarc_auto()                  # IARC classifications
update_eu_sml_auto(enrich = TRUE)   # EU 10/2011 positive list
```

Additions are applied automatically (bounded by `max_auto_changes`);
removals always require manual confirmation. China SML has no stable
online source and is updated by dropping a file into `inst/` — see
`check_manual_lists()`.

# Step by step

`run_screening()` is a thin wrapper around two steps that you can also
call directly.

## 1. Prepare the input (only if InChIKey is missing)

``` r
prepared <- prepare_input(data)   # name + SMILES -> InChIKey/CID/Formula (offline CDK)
print_prepare_report(prepared)
```

Skip this step when your data already carries an `InChIKey` column.

## 2. Match and grade

``` r
res <- assign_toxicity(prepared, output_file = "report.xlsx")
```

Matching is by `InChIKey` against SVHC / CMR / CMR-suspect / EDC / IARC
/ EU SML / China SML. For compounds absent from all lists, Cramer rules
are predicted by calling Toxtree headless from R (`run_toxtree()`); the
Toxtree application (about 81 MB) is downloaded once on first use and
Java 8+ must be installed (<https://adoptium.net>). If Toxtree cannot
run, the screening degrades gracefully: regulatory matching is kept and
Cramer columns stay blank.

## 3. Report

`output_file` dispatches on the extension: `.xlsx` produces the styled
four-sheet report, `.csv` a plain export. `rio::export(res, ...)` also
works — the data frame is plain data.

# Graphical interface

``` r
launch_database_inspector()
```

A Shiny app to browse every regulatory table, run the online update with
a two-phase preview-then-write flow, and run the full screening workflow
(upload a substance list, preview level distribution, download the
report).

# Note on group entries

Group entries (nonylphenols, lead compounds, …) are visible to the
package through an explicit registry.
`assign_toxicity(group_membership = TRUE)` additionally reports group
hits per row; grading from group evidence is deliberately conservative
and flags conservative cases for manual review instead of guessing.
