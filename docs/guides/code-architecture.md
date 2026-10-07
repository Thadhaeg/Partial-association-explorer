# Code architecture

Partial Association Explorer is intentionally kept as a small Shiny project
rather than an R package. The implementation is divided between the application
orchestration in `app.R` and UI-independent computations in
`R/core_associations.R`.

## `app.R`

`app.R` is organized in the same order as the application workflow:

1. package dependencies and the shared-helper import;
2. the complete user-interface declaration;
3. reactive state and marginal/conditional view selection;
4. data and description-file import;
5. variable selection and navigation;
6. association computation and thresholded pair selection;
7. network rendering and CSV export; and
8. numerical-numerical, categorical-categorical, and
   numerical-categorical pair diagnostics.

The UI declaration remains in one block so every input and output identifier can
be checked directly against the server implementation. Statistical calculations
should not be added directly to the reactive graph when they can be expressed as
a deterministic helper in `R/core_associations.R`.

## `R/core_associations.R`

The core file has no Shiny reactive state. Its sections provide:

- shared display and metadata formatting;
- correlation, partial-correlation, ANOVA, and ANCOVA helpers;
- preparation of categorical pair problems and large-table display selection;
- the structured multinomial null and alternative models;
- result, filtering, export, and named-matrix utilities;
- marginal and conditional categorical-pair analyses; and
- pair caching and full association-matrix assembly.

Important internal conventions are documented at the beginning of the file.
In particular, numerical-numerical and numerical-categorical entries are stored
as `abs(r)` and `sqrt(eta^2)` so the common display and filtering layer can
square them. Categorical-categorical entries are stored directly as `V_L` or
`V_L|Z` and must not be squared.

## Data flow

```text
uploaded dataset
    -> remove non-varying columns
    -> select observed variables and optional controls
    -> calculate pairwise association/type/p-value matrices
    -> apply effect-size and p-value thresholds
    -> render the network, pair diagnostics, or CSV export
```

Categorical-categorical fits are cached by the unordered variable pair and the
selected control set because these models are the most expensive part of the
workflow. Threshold changes operate on already computed matrices and therefore
do not refit those models.

## Making changes safely

- Preserve the internal association-scale conventions described above.
- Apply identical complete-case and control handling to displayed diagnostics
  and their corresponding global association estimates.
- Keep selected controls out of the displayed node set; they are used only for
  adjustment.
- Add or update tests in `tests/test_core_associations.R` for statistical
  changes and keep `tests/test_app_smoke.R` passing.
- Run `Rscript tests/run_tests.R` from the repository root before opening a
  pull request.
