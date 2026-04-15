# Contributing to Partial Association Explorer

Thank you for your interest in contributing. This document describes how to report bugs, suggest improvements, and submit code changes.

## Code of Conduct

This project follows the [Contributor Covenant Code of Conduct](CODE_OF_CONDUCT.md). By participating you agree to abide by its terms.

## How to contribute

### Reporting bugs

Open a [GitHub issue](https://github.com/Thadhaeg/Partial-association-explorer/issues) and include:

- A minimal reproducible example (dataset or steps to trigger the problem).
- The R version and package versions (`sessionInfo()` output).
- What you expected to happen and what actually happened.
- Any error messages or screenshots.

### Suggesting enhancements

Open a GitHub issue with the `enhancement` label. Describe the use case and why the current behaviour is insufficient.

### Submitting a pull request

1. Fork the repository and create a branch from `main`:
   ```
   git checkout -b fix/short-description
   ```
2. Make your changes in `app.r` (or the relevant file).
3. Test the app manually with a representative dataset before opening the PR.
4. Open a pull request against `main`. Describe *what* changed and *why*.

## Development notes

- The app is a single-file Shiny application. The main file is `app.r`.
- Statistical helper functions (`partial_residuals`, `calculate_partial_eta_squared_with_F`, `compute_unconditional`, `compute_conditional`, etc.) are currently defined inside `server()`. When adding new helpers, follow the same convention until a refactor separates them.
- Keep UI labels consistent with the paper notation (`V_L`, `η²`, `r`).

## License

By contributing you agree that your contributions will be licensed under the [MIT License](LICENSE).
