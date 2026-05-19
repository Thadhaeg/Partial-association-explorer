# AssociationProfiler

**AssociationProfiler** is an open-source interactive **R Shiny** application for profiling unconditional and conditional associations in multivariate datasets containing numerical and categorical variables.

For every selected variable pair, the app computes a global association measure, a significance test, and local pairwise diagnostics adapted to the variable types. Users can filter an interactive association network by association strength and statistical significance, then inspect retained edges through harmonized pair plots.

| Variable types | Unconditional measure | Conditional measure |
|---|---|---|
| Continuous vs. Continuous | Pearson's *r* | Partial *r* (added-variable regression) |
| Continuous vs. Categorical | η² (ANOVA) | Partial η² (ANCOVA) |
| Categorical vs. Categorical | *V*_L (Linfoot/Cox–Snell LR) | *V*_L\|Z (conditional LR) |

---

## Statement of need

Many exploratory analyses start by asking which variables in a dataset are associated. This question is difficult when the dataset combines numerical variables, nominal or ordinal factors, and possible confounders. Classical correlation matrices cover only numerical variables; contingency-table measures do not extend naturally to mixed variable types; and marginal pairwise summaries can be misleading when two variables are both related to a background variable.

AssociationProfiler addresses this as a software problem. It provides a unified workflow for computing and visualizing both global and local associations between all selected variable pairs, with support for optional control variables applied simultaneously across all pair types.

---

## Features

### Upload and manage data

- Import `.csv`, `.xlsx`, or `.xls` files
- Automatically detects variable types (numerical vs. categorical)
- Cleans variable names via `{janitor}`
- Optionally upload a two-column variable-description file (`Variable`, `Description`) to annotate nodes in the network and the variables table

### Association analysis

- Computes unconditional **and** conditional association measures depending on variable types and whether control variables are selected
- **Categorical–categorical:** *V*_L uses a Linfoot/Cox–Snell transformation of the likelihood-ratio deviance from a structured multinomial model; *V*_L\|Z extends this to the conditional case with numerical or categorical controls as regression covariates
- **Numerical–categorical:** η² (ANOVA) without controls; partial η² (ANCOVA extra-sum-of-squares) with controls; both reported as sqrt(η²) for comparability with the other measures
- **Numerical–numerical:** Pearson's *r* without controls; partial *r* via added-variable regression with controls
- Displays effect-size **strength indicators** and test statistics (χ², *F*, *t*) with *p*-values

### Association network

- Builds an interactive network graph (via `visNetwork`) where nodes are variables and edge widths reflect association strength
- Adjustable sliders filter edges by:
  - *R*² / η² range for numerical–numerical and numerical–categorical pairs
  - *V*_L range for categorical–categorical pairs
  - *p*-value range for all pairs
- Isolated nodes (no edges above threshold) are pruned automatically
- Hover over edges to see the association measure and value; hover over nodes to see variable descriptions

### Pair plots

Bivariate visualisations for all retained edges, adapted to variable type and control selection:

- **Numerical–numerical:** scatter plot with linear trend (no controls) or added-variable plot of residuals (with controls); axes can be reversed
- **Numerical–categorical:** group mean plot of the raw outcome (no controls) or of the residualized outcome after removing control effects (with controls)
- **Categorical–categorical:** contingency table coloured by Pearson residuals *R*_ij = (O_ij − E⁰_ij) / √E⁰_ij, with cell values showing observed-minus-expected deviations *D*_ij; interaction parameter table (γ_ij) shown when available

### Optimal submatrix display

Large contingency tables are truncated to an informative submatrix: the app selects rows and columns that maximise the sum of cell-level likelihood-ratio contributions C_ij = 2 O_ij log(O_ij / E⁰_ij), using `lpSolve` when available and a heuristic fallback otherwise.

---

## Installation

AssociationProfiler requires R (≥ 4.1 recommended). Install the required packages:

```r
install.packages(c(
  "shiny", "bslib", "dplyr", "ggplot2", "scales",
  "reactable", "tidygraph", "visNetwork", "readxl",
  "janitor", "shinyjs", "tibble", "shinycssloaders",
  "nnet", "lpSolve"
))
```

Clone the repository and launch the app:

```r
# From the repository root
shiny::runApp("app.r")
```

Or run directly from GitHub:

```r
shiny::runGitHub("Partial-association-explorer", "Thadhaeg")
```

---

## Usage

1. **Data tab** — Upload a CSV or Excel dataset. Optionally upload a variable-description file (two columns: `Variable`, `Description`). Click **Process data**.
2. **Variables tab** — Select variables to include in the analysis and (optionally) control variables to adjust for. Control variables are used for all association computations but do not appear as network nodes. Click **Visualize all associations**.
3. **Correlation Network tab** — Adjust the three threshold sliders to filter edges by association strength and *p*-value. The network summary reports the number of visible nodes and edges, broken down by pair type.
4. **Pairs Plots tab** — Click **See pairs plots** to inspect bivariate visualisations for all retained edges.

---

## Example dataset

The repository includes a Belgian subset of the European Social Survey (Round 11) in `data/ESS11_BE_data.csv`, which can be used to explore associations between socio-demographic variables with and without control variables.

---

## Contributing

Contributions are welcome. Please read [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines on reporting bugs, suggesting enhancements, and submitting pull requests.

This project follows the [Contributor Covenant Code of Conduct](CODE_OF_CONDUCT.md).

---

## Citation

If you use AssociationProfiler in your work, please cite the accompanying paper (citation details to be updated upon publication).

---

## License

This project is licensed under the [MIT License](LICENSE).
