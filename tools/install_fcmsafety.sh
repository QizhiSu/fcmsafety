#!/usr/bin/env bash
# ---------------------------------------------------------------------------
# Install fcmsafety into the local R library, working around the
# "lazy loading failed" bug on this machine.
#
# Background
#   On this machine (R 4.6.1 ucrt, Windows), loading rlang's R namespace makes
#   the R process segfault during shutdown (exit 139). Because `R CMD INSTALL`
#   must load rlang while building the package's lazy-load DB, ANY package that
#   imports rlang fails with:
#
#       ** byte-compile and prepare package for lazy loading
#       ERROR: lazy loading failed for package 'fcmsafety'
#
#   The lazy-load DB (R/fcmsafety.rdb) is in fact written correctly - the
#   process just dies on exit afterwards, so R CMD INSTALL aborts and deletes
#   the freshly built package.
#
# Workaround (two passes + splice)
#   1. `--no-R`  : installs help/, inst/, Meta/, INDEX - never loads rlang, so
#                  it succeeds.
#   2. full pass with `--no-clean-on-error`: fails, but leaves a complete R/
#                  directory (loader stub + .rdb + .rdx) behind.
#   3. copy that R/ into the library installed in pass 1.
#
# Usage (Git Bash):
#   bash tools/install_fcmsafety.sh
#   bash tools/install_fcmsafety.sh "C:/path/to/fcmsafety"
# ---------------------------------------------------------------------------
set -u

PKG="${1:-C:/Users/13432/WorkBuddy/2026-08-31-14-16-57/fcmsafety}"
R="/c/Program Files/R/R-4.6.1/bin/R.exe"
RS="/c/Program Files/R/R-4.6.1/bin/Rscript.exe"
LIB="C:/Users/13432/AppData/Local/R/win-library/4.6"
TMP="C:/Users/13432/AppData/Local/Temp/fcmsafety_install_tmp"

rm -rf "$TMP"
mkdir -p "$TMP"

echo "[1/3] installing help/inst/Meta (R code skipped - this pass never loads rlang)"
LC_ALL=C "$R" CMD INSTALL --no-R --no-test-load -l "$LIB" "$PKG" > "$TMP/s1.log" 2>&1
if [ $? -ne 0 ]; then
  echo "  FAILED - see $TMP/s1.log"
  tail -20 "$TMP/s1.log"
  exit 1
fi
echo "  ok: $(tail -1 "$TMP/s1.log")"

echo "[2/3] building the lazy-load DB (expected to report a failure; the DB is still written)"
LC_ALL=C "$R" CMD INSTALL --no-test-load --no-clean-on-error --no-staged-install \
  -l "$TMP" "$PKG" > "$TMP/s2.log" 2>&1
if [ ! -f "$TMP/fcmsafety/R/fcmsafety.rdb" ]; then
  echo "  FAILED - R/fcmsafety.rdb was not produced - see $TMP/s2.log"
  tail -20 "$TMP/s2.log"
  exit 1
fi
echo "  ok: R/ = $(ls "$TMP/fcmsafety/R" | tr '\n' ' ')"

echo "[3/3] splicing R/ into the installed package"
rm -rf "$LIB/fcmsafety/R"
cp -r "$TMP/fcmsafety/R" "$LIB/fcmsafety/R"

echo "verify:"
LC_ALL=C "$RS" -e "library(fcmsafety); cat('  version =', as.character(packageVersion('fcmsafety')), '\n'); cat('  assign_group_membership_table present =', exists('assign_group_membership_table'), '\n')" 2>/dev/null
echo "done (a trailing segfault / exit 139 at the very end is expected and harmless)"
