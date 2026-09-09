# Contributing to Partial Association Explorer

Thank you for your interest in improving Partial Association Explorer. Contributions are welcome, especially bug reports, reproducible examples, documentation improvements, interface refinements, and carefully tested statistical extensions.

## Code of Conduct

This project follows the [Code of Conduct](CODE_OF_CONDUCT.md). By participating, you agree to follow it.

## Reporting bugs

Please open a [GitHub issue](https://github.com/Thadhaeg/Partial-association-explorer/issues) and include:

- A short description of the problem.
- The dataset or minimal steps needed to reproduce it.
- The selected variables and controls, if the bug appears after running an analysis.
- The threshold values used in the network, if relevant.
- The R version and package versions, ideally from `sessionInfo()`.
- Any error message, warning, or screenshot.

If the issue concerns categorical-categorical pair plots, please also mention the number of rows, columns, and whether the optimal submatrix fallback message appeared.

## Suggesting improvements

Feature requests are welcome. Please describe:

- The research or teaching use case.
- What the app currently makes difficult.
- Whether the change affects the statistical method, the interface, or only documentation.
- Whether backward compatibility with existing workflows matters.

For statistical changes, please include references or a short methodological justification when possible.

## Submitting pull requests

1. Fork the repository and create a branch from `main`.
2. Keep changes focused: one bug fix or feature per pull request is easiest to review.
3. Test the app manually before opening the pull request.
4. Describe what changed, why it changed, and how it was tested.
5. If the user interface changes, include screenshots when possible.

Example branch names:

```bash
git checkout -b fix/catcat-submatrix-message
git checkout -b feature/export-controls-descriptions
```

## Development notes

- The main application file is [`app.r`](app.r).
- The app currently follows a single-file Shiny structure, with most statistical helper functions defined inside `server()`.
- Numerical variables are detected with `is.numeric()`; non-numeric variables are treated as categorical.
- The network filters numerical-numerical and numerical-categorical pairs with `R^2 / eta^2`, categorical-categorical pairs with `V_L`, and all pairs with p-values.
- Categorical-categorical pair plots display observed counts and color cells by Pearson residuals.
- Large categorical-categorical tables are reduced to at most 7 by 7 cells when the full table has more than 49 cells.
- If `lpSolve` is unavailable or the submatrix optimization does not return a solution, the app uses a deterministic fallback and reports it in the interface.

## Manual test checklist

Before submitting a pull request, please test at least:

- Loading a `.csv` dataset.
- Loading an Excel dataset, if the change touches data import.
- Running the network without controls.
- Running the network with at least one control variable.
- Switching between conditional and unconditional network views.
- Exporting the association table as CSV.
- Opening numerical-numerical, numerical-categorical, and categorical-categorical pair plots.
- Showing the unconditional comparison below a conditional pair plot.
- Opening a large categorical-categorical pair plot that triggers the 7 by 7 submatrix display.

The included Belgian ESS 2011 dataset in [`data/ESS11_BE_data.csv`](data/ESS11_BE_data.csv) can be used for quick manual checks.

## Documentation

Please update the README or manuscript files when a change affects:

- The definition of an association measure.
- The reported p-value or statistical test.
- Network filtering behavior.
- Pair plot interpretation.
- Data import or variable type detection.

Do not commit local LaTeX build artifacts such as `.aux`, `.log`, `.out`, or `.spl` files.

## License

By contributing, you agree that your contributions will be licensed under the [MIT License](LICENSE).
