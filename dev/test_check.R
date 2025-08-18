# test_check.R
# Purpose: run tests, linters and R CMD check locally.
# Usage: source('dev/test_check.R') or call functions directly.

# Notes:
# - R CMD check can be slow locally; run on CI for PRs.
# - Linting requires 'lintr' if you want style checks.

library(devtools)

# Run unit tests (testthat)
run_tests <- function() {
  message("==> Running tests via devtools::test()")
  devtools::test()
}

# Run lintr across the package (optional)
run_linters <- function() {
  if (!requireNamespace("lintr", quietly = TRUE)) {
    message("lintr not installed. Install with install.packages('lintr') to run linters.")
    return(invisible(NULL))
  }
  message("==> Linting package code with lintr")
  lintr::lint_package()
}

# Run full package check (wrapped)
run_check <- function() {
  message("==> Running devtools::check() (this may take a while)")
  devtools::check()
}

# Convenience: tests -> linters -> check
test_lint_check <- function(run_lintr = FALSE) {
  run_tests()
  if (run_lintr) run_linters()
  run_check()
}

message("Loaded dev/test_check.R: use run_tests(), run_linters(), run_check(), test_lint_check()")
