# Changelog

All notable changes to Partial Association Explorer will be documented here.
The project follows the principles of [Keep a Changelog](https://keepachangelog.com/)
and intends to use semantic versioning for tagged releases.

## Unreleased

### Added

- A JOSS-formatted manuscript in `paper/`.
- Automated tests for the statistical core and application startup.
- Continuous integration for R release and development versions.
- Project dependency metadata in `DESCRIPTION`.
- Citation, support, governance, and data-provenance documentation.
- An extended-article area that is clearly separated from the JOSS submission.

### Changed

- Moved pure statistical and display helpers from `app.R` to
  `R/core_associations.R` so they can be tested without launching Shiny.
- Renamed the Shiny entry point from `app.r` to the conventional `app.R`.
- Corrected the variable-description download so it is a genuine CSV file.
- Referred to the example data as ESS Round 11 rather than ESS 2011.

## 0.1.0 - Planned

The first tagged release will be prepared after all authors approve the paper,
the automated checks pass on GitHub, and the release archive is ready for DOI
deposit.
