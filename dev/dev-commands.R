# Development workflow helpers for NMCleaner
# Run from package root (use Projects or setwd to package root)
# This script is intended to be sourced interactively, not run in CI.

# Usage examples:
# source("dev/dev-commands.R")
# dev_document_all()
# dev_test_all()

library(devtools)
library(usethis)

# 1. Document and update NAMESPACE and Rd files
dev_document_all <- function() {
  message("Running devtools::document()")
  devtools::document()
}

# 2. Load package into current R session for interactive testing
dev_load <- function() {
  message("Loading package with devtools::load_all()")
  devtools::load_all()
}

# 3. Run tests
dev_test_all <- function() {
  message("Running testthat tests")
  devtools::test()
}

# 4. Run package checks (recommended before release)
dev_check <- function() {
  message("Running devtools::check() - this may take a while")
  devtools::check()
}

# 5. Build package tarball
dev_build_pkg <- function() {
  message("Building package tar.gz")
  devtools::build()
}

# 6. Convenience: run document, test, check sequentially
dev_all <- function() {
  dev_document_all()
  dev_load()
  dev_test_all()
  dev_check()
}

# Helpful reminders printed when sourcing
message("Loaded dev helper functions: dev_document_all, dev_load, dev_test_all, dev_check, dev_build_pkg, dev_all")
message("Use these interactively. Avoid sourcing in non-interactive CI environments.")
