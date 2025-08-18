# Changelog

All notable changes to this project will be documented in this file.

Format conventions

- Use an "Unreleased" section for ongoing work.
- For each release add a top-level heading `## vX.Y.Z - YYYY-MM-DD` and short bullets grouped by category (Added, Changed, Fixed, Removed, Security).
- Keep bullets terse and link to issues/PRs when helpful: `(#123)`.

## Unreleased

### Added

-

### Changed

-

### Fixed

-

### Removed

-

### Security

-

---

## v0.0.0.9000 - (development snapshot)

- Initial development snapshot (internal build).

---

How to use

- Edit the `Unreleased` section while developing.
- When preparing a release:
  1. Move the `Unreleased` bullets into a new heading `## vX.Y.Z - YYYY-MM-DD` (replace `X.Y.Z` and date).
  2. Commit the change and tag the commit (e.g. `v1.2.0`).

Quick R helpers

- You can use `usethis::use_version("patch")` to bump `DESCRIPTION` and `usethis::use_github_release()` to create a GitHub release from a tag.

Notes

- Prefer `NEWS.md` (project root). CI or `CHANGELOG.md` conventions vary; this file is intentionally simple and human editable.
