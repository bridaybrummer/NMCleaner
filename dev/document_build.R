# document_build.R
# Purpose: small script to document the package and build a source tarball.
# Intended to be run interactively from the package root, e.g.
# source("dev/document_build.R")
# or run the functions below directly.

# Safe behavior: does not push, tag, or modify git history.
# Requires: devtools, usethis, pkgdown (optional)

library(devtools)
library(usethis)

# Document package (Rd files, NAMESPACE)
document_package <- function() {
  message("==> Documenting package (roxygen -> Rd / NAMESPACE)")
  devtools::document()
  message("==> Documented")
}

# Build source tarball; returns path to built file
build_package <- function(path = "./", vignettes = TRUE) {
  message("==> Building source package tarball")
  pkg <- devtools::build(vignettes = vignettes, path = path)
  message("==> Built package: ", pkg)
  invisible(pkg)
}

# Optional: build pkgdown site for docs (local), if pkgdown configured
build_pkgdown_site <- function() {
  if (!requireNamespace("pkgdown", quietly = TRUE)) {
    message("pkgdown not installed. Install with install.packages('pkgdown') to build site.")
    return(invisible(NULL))
  }
  message("==> Building pkgdown site")
  pkgdown::build_site()
}

# Convenience wrapper: document then build
document_and_build <- function(path = "./", vignettes = TRUE) {
  document_package()
  build_package(path = path, vignettes = vignettes)
}

message("Loaded dev/document_build.R: use document_package(), build_package(), build_pkgdown_site(), document_and_build()")
message("Important: Run `git status` before committing. Do NOT commit large raw data or the 'shape/' directory. Use .gitignore to exclude them.")
