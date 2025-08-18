# release_helper.R
# Purpose: small helper functions to prepare a release locally.
# This script automates safe, local steps and prints the manual steps that
# usually require a git push or GitHub Release creation.
# Usage: source('dev/release_helper.R') and call functions.

library(usethis)
library(devtools)

# 1) Bump version using usethis::use_version(). This edits DESCRIPTION.
bump_version <- function(type = c("patch", "minor", "major")) {
  type <- match.arg(type)
  message(sprintf("==> Bumping package version (%s)", type))
  usethis::use_version(type)
  message("==> Remember to commit the DESCRIPTION change and update NEWS.md before tagging.")
}

# 2) Append a small release note to NEWS.md (creates file if missing)
append_news <- function(title, body = "", file = "NEWS.md") {
  if (!file.exists(file)) {
    message("NEWS.md not found; creating new file")
    writeLines(c("# NEWS", ""), con = file)
  }
  entry <- c("", paste0("## ", title), "", body, "")
  cat(paste0(entry, collapse = "\n"), file = file, append = TRUE)
  message("==> Appended release notes to ", file)
}

# 3) Create a lightweight draft release note and show the git commands to run
prepare_release <- function(tag = NULL, message_text = NULL) {
  message("==> Release preparation summary:")
  message(" - Ensure DESCRIPTION version and NEWS.md are committed")
  if (is.null(tag)) {
    message(" - Suggested next command: git tag -a vX.Y.Z -m \"Release vX.Y.Z\"")
  } else {
    message(sprintf(" - Suggested next command: git tag -a %s -m \"%s\"", tag, message_text %||% tag))
  }
  message(" - Push commits and tags: git push && git push --tags")
  message(" - Create GitHub Release from tag or use gh CLI: gh release create <tag> path/to/pkg.tar.gz --notes-file NEWS.md")
}

# Provide a safe release flow wrapper (does not run git or GH actions):
release_flow <- function(version_type = c("patch", "minor", "major"), news_title = NULL, news_body = "") {
  version_type <- match.arg(version_type)
  bump_version(version_type)
  if (!is.null(news_title)) append_news(news_title, news_body)
  message("Run the following commands to finish the release locally:")
  message("  git add DESCRIPTION NEWS.md")
  message("  git commit -m 'Prepare release'")
  message("  git tag -a vX.Y.Z -m 'Release vX.Y.Z'  # replace vX.Y.Z with actual version")
  message("  git push && git push --tags")
  message("Then create a GitHub Release (or use gh CLI) and attach built tarball if desired.")
}

message("Loaded dev/release_helper.R: use bump_version(), append_news(), prepare_release(), release_flow()")
message("Before releasing: ensure large data (shape/, data-raw/) are not committed. Run `git status` and confirm .gitignore includes these paths.")
