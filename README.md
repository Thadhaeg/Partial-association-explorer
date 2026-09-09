# Partial Association Explorer

**Partial Association Explorer** is an open-source **R Shiny** application for exploring dependence structures in datasets that contain both numerical and categorical variables.

The app computes unconditional and conditional pairwise associations, performs statistical tests, displays an interactive association network, and provides local pair plots adapted to each variable type. It is designed for exploratory work in social science, economics, public health, and other fields where researchers often need to distinguish direct-looking associations from patterns driven by confounding variables.

Partial Association Explorer builds on the spirit of AssociationExplorer by adding conditional association analysis, significance tests, likelihood-based measures for categorical pairs, comparison views between unconditional and conditional networks, enhanced pair plots, and CSV export of association results.

---

## What the app computes

For each selected pair of variables, the app chooses the measure and test according to the R type of the variables: numeric columns are treated as numerical variables, while all other columns are treated as categorical variables.

| Pair type | Unconditional measure | Conditional measure | Test | Pair plot |
|---|---|---|---|---|
| Numerical vs. numerical | `R^2` from Pearson correlation | `R^2` from partial correlation | t-test | Scatter plot or added-variable residual plot |
| Numerical vs. categorical | `eta^2` from ANOVA | Partial `eta^2` from ANCOVA | F-test | Group means or residualized group means |
| Categorical vs. categorical | `V_L` | `V_L|Z` | Likelihood-ratio chi-square test | Contingency table with observed counts and Pearson residual colors |

For numerical-numerical pairs, the signed Pearson or partial correlation is kept for interpretation, but the network filtering strength is `R^2`. For numerical-categorical pairs, the app reports and filters on `eta^2` or partial `eta^2`, not on `sqrt(eta^2)`.

---

## Main features

### Data import and variable management

- Import `.csv`, `.xlsx`, or `.xls` datasets.
- Optionally upload a two-column variable-description file with columns `Variable` and `Description`.
- Remove variables with no usable variation before analysis.
- Detect numerical variables from their R type with `is.numeric()`; factors, character columns, ordered factors, and other non-numeric columns are treated as categorical.
- Select observed variables and optional control variables.
- Clear the selected observed-variable list with one button.

### Association analysis

- Compute all pairwise associations among the selected observed variables.
- Compute unconditional associations when no controls are selected.
- Compute conditional or partial associations when control variables are selected.
- Apply the same selected controls consistently across numerical-numerical, numerical-categorical, and categorical-categorical pairs.
- Cache pair results during a session so repeated views do not recompute unchanged pair problems unnecessarily.

### Association network

- Display an interactive `visNetwork` graph where nodes are variables and edges are retained associations.
- Filter edges with three sliders: `R^2 / eta^2` for numerical-numerical and numerical-categorical pairs, `V_L` for categorical-categorical pairs, and p-value for all pairs.
- Prune isolated nodes automatically after filtering.
- Hover over edges to inspect the association measure, value, and p-value.
- Hover over nodes to inspect variable descriptions when a description file is provided.
- Export the association table as CSV, including variable names, descriptions, measures, p-values, and selected controls.

When controls are selected, the network can be switched between the conditional and unconditional views. In the selected view, edges that newly appear relative to the other view are shown in green, while edges that disappear relative to the other view are shown in a muted dashed style.

### Pair plots

The pair plot tab gives a local view of each retained association.

- Numerical-numerical pairs show a scatter plot without controls or an added-variable plot with controls.
- Numerical-categorical pairs show group means without controls or residualized group means with controls.
- Categorical-categorical pairs show a contingency table where cell values are observed counts `O_ij` and cell colors are Pearson residuals `R_ij`.
- Association values and p-values are displayed directly on the plot.
- Variable descriptions and selected controls are displayed when available.
- When controls are selected, the unconditional pair plot can be shown below the conditional plot for direct comparison.
- Associations that disappear after conditioning remain available in the pair plot list with a faded style.
- Associations that appear only after conditioning are highlighted with a light green style.

For large categorical-categorical tables, the app displays a reduced but informative submatrix. If the table has more than 49 cells, it selects at most 7 rows and 7 columns by maximizing the sum of squared Pearson residual scores,

```text
S_ij = R_ij^2.
```

The app uses `lpSolve` for this submatrix selection when available and falls back to a deterministic heuristic when the optimization problem cannot be solved. The interface reports when the fallback is used.

---

## Installation

Partial Association Explorer requires R. R version 4.1 or later is recommended.

Install the required packages:

```r
install.packages(c(
  "shiny", "bslib", "dplyr", "ggplot2", "scales",
  "reactable", "tidygraph", "visNetwork", "readxl",
  "janitor", "shinyjs", "tibble", "shinycssloaders",
  "nnet", "lpSolve"
))
```

Clone the repository and launch the app from the repository root:

```r
shiny::runApp("app.r")
```

You can also run the app directly from GitHub:

```r
shiny::runGitHub("Partial-association-explorer", "Thadhaeg")
```

---

## Basic workflow

1. Upload a CSV or Excel dataset.
2. Optionally upload a variable-description file with columns `Variable` and `Description`.
3. Select the variables to explore.
4. Optionally select control variables.
5. Click **Visualize all associations**.
6. Filter the network by association strength and p-value.
7. Inspect retained, appearing, and disappearing associations through pair plots.
8. Export the association results if needed.

---

## Example data and paper

The repository includes a Belgian subset of the European Social Survey 2011 in [`data/ESS11_BE_data.csv`](data/ESS11_BE_data.csv). The paper draft and screenshots used to document the software are available in [`paper/`](paper/).

---

## Contributing

Contributions are welcome. Please read [`CONTRIBUTING.md`](CONTRIBUTING.md) before opening an issue or pull request.

This project follows the [Partial Association Explorer Code of Conduct](CODE_OF_CONDUCT.md).

---

## Citation

If you use Partial Association Explorer in academic work, please cite the accompanying paper once citation details are finalized. Until then, the current manuscript is available in [`paper/partial_association_explorer_article.pdf`](paper/partial_association_explorer_article.pdf).

---

## License

The source code is distributed under the [`MIT License`](LICENSE). Data files, screenshots, and manuscript material may be subject to their own citation or reuse requirements.
