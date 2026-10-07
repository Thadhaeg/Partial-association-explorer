# Getting started

## Requirements

- R 4.1 or later.
- The packages listed in the repository `DESCRIPTION` file.

Install the runtime dependencies from the repository root:

```r
install.packages(c(
  "shiny", "bslib", "dplyr", "ggplot2", "scales", "reactable",
  "visNetwork", "readxl", "janitor", "shinyjs", "tibble",
  "shinycssloaders", "lpSolve"
))
```

Alternatively, restore the recorded dependency versions:

```r
install.packages("renv")
renv::restore()
```

Launch the application:

```r
shiny::runApp(".")
```

## Minimal workflow

1. Open the **Data** tab and upload a CSV, XLS, or XLSX dataset.
2. Optionally upload a description file containing exactly the columns
   `Variable` and `Description`.
3. Select at least two analysis variables.
4. Optionally select control variables; controls are used for adjustment but
   are not drawn as network nodes.
5. Compute the network and adjust the association-strength and p-value filters.
6. Open the pair-plot view to inspect the observations or residual structure
   behind each retained edge.
7. Export the association table when a machine-readable record is needed.

The files in `data/` can be used to reproduce the manuscript example. Their
separate provenance and license are documented in `data/README.md`.

## Interpretation

The application is exploratory. Conditional associations depend on the chosen
controls and the working statistical models; they should not be interpreted as
causal effects without an appropriate research design.
