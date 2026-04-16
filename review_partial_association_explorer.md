# Code and Paper Review: Partial Association Explorer
**Prepared for:** Thaddée  
**Date:** 15 April 2026  
**Files reviewed:** `app.r`, `paper/draft/Association_explorer_methods.tex`  
**SoftwareX guidelines consulted:** yes (via public documentation, November 2023 template)

---

## Table of Contents

1. [Code Review](#1-code-review)
2. [Paper Review](#2-paper-review)
3. [Cross-Cutting Issues](#3-cross-cutting-issues)
4. [Priority Action List](#4-priority-action-list)

---

## 1. Code Review

### 1.1 Correctness

**Partial correlation (numeric-numeric, conditional)**

✓ The implementation in `partial_residuals()` regresses each variable on the controls independently and then correlates the two residual series (`app.r:1467-1482`, `2585-2587`). This is the correct added-variable / Frisch-Waugh approach as described in Section 2 of the paper. The t-statistic in `p_value_partial_cor()` uses `df = n_eff - k_controls - 2`, matching the paper's formula (`app.r:1703`). No error here, but there is a subtle counting issue: `k_controls` is set to `ncol(control_data)` before any variation-filtering (`app.r:2594`), so if some controls are constant and get dropped inside `partial_residuals()` (`app.r:1473-1474`), the degrees of freedom used for the p-value will be overestimated (too many df removed), making the test conservative. Fix: pass the number of controls *actually used* (post-filtering) back alongside the residuals.

**Partial eta-squared (numeric-categorical, conditional)**

✓ `calculate_partial_eta_squared_with_F()` computes `SS(X|Z) / (SS(X|Z) + RSS_full)`, which matches the paper's formula exactly. The fallback when `inherits(fit_res, "try-error")` returns `eta = 0` and `p_value = NA`, which is safe and visible to the user. Good.

✓ However, when `has_variation[cat_name]` is FALSE (only one group remains after filtering), the function returns `eta = 0`, `F = NA`, `p_value = NA` with no user notification (`app.r:1580-1589`). Because the association matrix stores `0` for this pair, it will be silently excluded from the network without any warning. Consider adding a `showNotification()` or a dedicated message in the UI.

**Categorical-categorical (VL / VL|Z)**

The approach uses a custom structured multinomial logit fitted via `optim()` (BFGS). This is mathematically sound and matches the paper. However:

- ✓ `optim()` with `method = "BFGS"` has no guarantee of finding the global optimum, especially for larger contingency tables. There is no convergence check: `fit$convergence` is never inspected anywhere in the file. A failed or non-converged fit silently propagates to `ll1`, potentially producing a negative `G2` (which would yield `VL = NA` and `p_value = NA` after `compute_lr_stats()` guards against negative `G2`, that guard is good). Add an explicit check on `fit$convergence != 0` and surface a warning.

- ✓ `safe_g2_cell()` is referenced inside the fallback path in `compute_unconditional()` (`app.r:2299`) but is **never defined** anywhere in the file. If `fit0` fails for any contingency table, the fallback will throw a `could not find function "safe_g2_cell"` error. This is a definite bug.

- ✓ In `compute_conditional()`, the result list renames `VL` to `VL/Z` at the very end (`app.r:2522`). Downstream code accesses `cor_result[["VL/Z"]]` (`app.r:2632`) in the pairs plot and `vl_value <- assoc_res[["VL/Z"]]` (`app.r:1078`) in the correlation matrix. This is consistent but brittle; a slash in a list name is legal in R but unusual and easy to break. Rename it to something like `VL_Z` throughout.

> NUANCE: The slash in a list name is not a functional R error — `[["VL/Z"]]` syntax works correctly. The concern is purely one of style robustness (e.g., `$` accessor does not work with names containing `/` or `|`).

**Network threshold application**

In `calculate_correlations()`, the threshold is applied at computation time: values below threshold are stored as 0. However the threshold slider is also used later in `filtered_data_for_pairs()` and `significant_pairs()` to re-filter. This double application is fine because the second filter uses `abs(mat) > 0`, which is consistent. But it means changing the threshold requires recomputing the entire correlation matrix (due to `force(input$threshold_num)` in `cor_matrix_reactive()`), which can be slow on large datasets. Consider separating computation from threshold application.

### 1.2 Consistency with the Paper

| Feature | Paper | Code | Status |
|---|---|---|---|
| Partial r: residual-on-residual | Section 2 | `partial_residuals()` (line 1467) | ✓ Consistent |
| Partial r: df = n-q-2 | Section 2 | `p_value_partial_cor()` (line 1703) | ✓ Consistent (see df bug above) |
| Partial eta-squared formula | Section 3 | `calculate_partial_eta_squared_with_F()` (line 1512) | ✓ Consistent |
| VL formula: sqrt(1 - exp(-G2/n)) | Section 4 | `compute_lr_stats()` | ✓ Consistent |
| H0 for cat-cat: additive logit (no gamma) | Section 4 | `fit_structured_mnl(..., include_gamma=FALSE)` | ✓ Consistent |
| H1 for cat-cat: logit + gamma | Section 4 | `fit_structured_mnl(..., include_gamma=TRUE)` | ✓ Consistent |
| Pair plot (num-cat, conditional): residualized means | Section 3 | mixed-case `renderPlot` block | ✓ Consistent |
| Pair plot (cat-cat): D, R, gamma tables | Section 4 | `renderUI` cat-cat block | ✓ Consistent |
| Added-variable plot axes label | Section 2 | "Residuals of X \| controls" (line 872) | ✓ Minor: paper says "adjusted for Z" (tex:239); wording differs |

✓ The association measure displayed in the network tooltip uses the raw matrix value (which can be `r` or `sqrt(eta2)` depending on case), while the network summary block labels edges as "r" for numeric-numeric. This is fine but the threshold slider is labeled "R²" in the UI (`app.r:108`, full label: `"Threshold for Quantitative-Quantitative and Quantitative-Categorical Associations (R²)"`). The slider filters on `r^2 >= threshold_num` for numeric-numeric (`app.r:2590`) but on `eta_sq >= threshold_num` for mixed cases (`app.r:2673`). The label "R²" is therefore misleading for mixed pairs, where the quantity being thresholded is eta-squared, not R-squared. The slider label should read something like "R² / η² threshold".

### 1.3 Code Quality and Readability

**Structural issues**

- ✓ All server logic, including substantial statistical helper functions (`fit_structured_mnl`, `unpack_theta`, `compute_eta`, `softmax_rows`, `compute_lr_stats`, etc.), is defined *inside* the `server()` function. This makes the file difficult to navigate (it is over 1400 lines long) and prevents unit testing. Move pure statistical functions to a separate `R/` directory or at minimum to the top-level scope outside `server()`.

- ✓ The main pairs-plot rendering block (`output$pairs_plot <- renderUI`) is very long (approx. 350 lines) with deeply nested `if/else` trees. Refactoring each variable-type case into its own named function (e.g., `render_numnum_plot()`, `render_catcat_plot()`, `render_mixed_plot()`) would substantially improve readability.

- ~~`data_env <- new.env()` followed by `data_env$full_data <- reactiveVal(NULL)` is an unusual pattern. A reactive value stored inside a plain environment will not properly trigger Shiny's reactivity system when accessed outside a reactive context. Use a standard `reactiveVal()` at the top-level of the server instead, e.g., `full_data <- reactiveVal(NULL)`.~~

> CORRECTION: This claim is **refuted**. `reactiveVal(NULL)` creates a reactive function object; storing it in `data_env$full_data` (`app.r:256`) does not break reactivity. When `data_env$full_data()` is called inside a reactive context such as `renderPlot` or `reactive` (lines 775, 1052, 1264, 2547), Shiny correctly registers the dependency and the invalidation chain works as intended. The pattern is unconventional but fully functional. The style recommendation to use a top-level `reactiveVal()` is still valid advice for clarity, but there is no actual bug here.

**Minor quality issues**

- ✓ `cat("Button", button_id, "clicked!...\n")` (`app.r:729-735`) and `cat("Partial correlation failed:", ...)` (`app.r:892`) debug print statements are left in production code. Remove them or replace with `message()`.

- ~~✓ The function `find_optimal_submatrix()` and `find_optimal_submatrix_heuristic()` are defined inside the server but are only called from `build_contingency_table()`, which is itself never called anywhere in the app.~~ *(Done: all three functions moved to `archive.r`.)*

- ~~✓ `library(VGAM)` is loaded at the top of the file but no VGAM function is called anywhere in the code.~~ *(Done: removed from `app.r`.)*

- ~~✓ `library(grid)` and `library(gridExtra)` are loaded but no calls to `grid::` or `gridExtra::` functions appear anywhere in the file.~~ *(Done: both removed from `app.r`.)*

- ✓ The `make_Z_design()` helper (`app.r:1722`) constructs a model matrix with `stats::model.matrix(~., data = Zdf)` and then removes the intercept column. This works but will silently fail if `Zdf` contains a factor with only one level (dropped by model.matrix), potentially shifting column indices. Add a check or use `model.matrix(~. -1, data = Zdf)` directly (being aware of the reference-category change this implies).

### 1.4 Potential Bugs and Edge Cases

1. ✓ **`safe_g2_cell` undefined** (critical): see Section 1.1 above. Called at `app.r:2299`, never defined anywhere in the file.

2. ✓ **Single-level factor controls**: if a control variable is a factor with only one level present after filtering complete cases, `model.matrix()` will drop it silently. Downstream, `ncol(Zmm)` will differ from `length(input$control_vars)`, and the `df` in the p-value calculation will be wrong. The variation check (`has_variation`) in `calculate_partial_eta_squared_with_F()` handles this for that function, but not consistently across all functions that build control matrices.

3. ~~✓ **`build_contingency_table()` never called**~~ *(Done: moved to `archive.r` along with `find_optimal_submatrix()` and `find_optimal_submatrix_heuristic()`.)*

4. ✓ **Large datasets and `optim()`**: for large contingency tables (say 10x10 = 100 cells, 81 free gamma parameters), the BFGS optimizer will be slow, and the app will appear to hang. No progress indicator or timeout is provided for the correlation matrix calculation. Consider capping the number of categories per variable at which the full model is attempted, or using `nnet::multinom()` with a Kronecker-product parametrization as a faster alternative.

5. ✓ **`reversed_axes` observer leak** (partially confirmed — see nuance): a new `observeEvent` for each "Reverse axes" button is registered inside `renderUI`, protected by `if (is.null(reversed_axes[[paste0("obs_", button_id)]]))` (`app.r:719`). This check works for button creation, but if the user changes the selected variables, new pairs are generated, new buttons are created, but the old observers remain registered in memory. Over a long session with many variable-set changes, this accumulates. Use `observe()` with `session$onSessionEnded` cleanup, or better, use a module architecture.

> NUANCE: The guard at line 719 *does* prevent duplicate observer creation for the same index. However, because button IDs are purely index-based (`paste0("reverse_", idx)`), when the user selects a different set of variables the existing observers (tied to old variable pairs) will still fire when buttons with the same numeric index are clicked. The axis-reversal state for old pairs (`reversed_axes$plot_1`, etc.) also persists and bleeds into new pairs at the same index. The real issue is therefore **stale state and stale observer bindings**, not purely accumulation.

6. ✓ **`compute_conditional` fallback**: when `fit0` fails (`app.r:2448-2451`), the code falls back to `compute_unconditional()` silently. The returned list will have `VL` named `VL` (not `VL/Z`), but the downstream renaming (`names(out)[names(out) == "VL"] <- "VL/Z"`) only applies to the conditional success path (`app.r:2522`). So on fallback, the association matrix code will call `cor_result[["VL/Z"]]` (`app.r:2632`) and get `NULL`, storing `0`. This will make the pair silently disappear from the network rather than falling back to the unconditional value. Fix: ensure the fallback also renames `VL` to `VL/Z` in the conditional function.

> NUANCE: There are **three** un-renamed fallback paths in `compute_conditional()`, not just one: (a) empty `Zdf` (`app.r:2402-2403`), (b) `fit0` failure (`app.r:2448-2451`), and (c) `fit1` failure (`app.r:2480-2497`, returns `make_catcat_result(VL = NA_real_, ...)` also not renamed). All three silently return `NULL` for `cor_result[["VL/Z"]]`.

### 1.5 Suggestions for Improvement

- ✓ Add a `README` section on installation and dependency management (e.g., an `renv.lock` file). The current `README.md` describes features but gives no installation instructions.

- Add at least a minimal `testthat` test file for the pure statistical helper functions (partial correlation, eta-squared, VL), separated from the Shiny UI.

- ~~✓ The file is named `partial_association_explorer_7.r`. The version number in the filename is unusual and will create citation and reproducibility issues. Use a proper versioning system (git tags + `DESCRIPTION` file or a version constant inside the app) and name the file `app.R`.~~ *(Done: file renamed to `app.r` and version string removed from footer.)*

- Consider replacing the monolithic optim-based cat-cat fitting with `nnet::multinom()`, which is already a declared dependency and handles larger tables more robustly. The structured parametrization can be encoded via a custom predictor matrix or by direct likelihood comparison between two `multinom()` calls with appropriate formula specifications.

### 1.6 Additional Findings

The following issues were not mentioned in the original review but were found during verification.

- **`threshold_cat` slider mislabeled as "Cramer's V"**: The second threshold slider (`app.r:115-121`) is labeled `"Threshold for Categorical-Categorical Associations (Cramer's V)"`. However, the code computes `VL` (the likelihood-ratio-based statistic defined in the paper), not Cramer's V. The `README.md` also calls it "Partial Cramer's V" (`README.md:6`), while the paper exclusively uses the notation `V_L`. This inconsistency between the UI label, the README, and the paper should be resolved: decide on one name (`V_L` or Cramér's V) and use it consistently.

- **`compute_conditional()` empty-Zdf fallback also returns without renaming**: When called with `Zdf = NULL` or `ncol(Zdf) == 0`, `compute_conditional()` immediately returns `compute_unconditional()` at `app.r:2402-2403`, before the renaming at line 2522. In practice this path is guarded by the caller (`has_controls && !is.null(control_data)`), so it should not be reached, but the function's internal contract is still inconsistent.

- ~~**Version mismatch between filename and footer**: The file is named `partial_association_explorer_7.r` (suggesting version 7), but the app footer displays `"v3.5.5."`.~~ *(Done: file renamed to `app.r` and version string removed from footer.)*

---

## 2. Paper Review

### 2.1 Compliance with SoftwareX Submission Guidelines

The current draft is a pure methods paper (a LaTeX article class document with sections on statistical theory). It does **not** comply with SoftwareX requirements in several important ways.

**Critical non-compliance issues**

- **Wrong template**: SoftwareX will only consider software articles submitted using the journal's own template, and the main body text should be a maximum of 3000 words (excluding metadata, tables, figures, references), with a maximum of six figures. The current draft uses a generic `article` class and has no metadata tables, no structured sections matching the template, and substantially exceeds the expected length for the body of a SoftwareX article if the statistical derivations are included verbatim.

- **Missing required sections**: The template requires: Motivation and significance (why the software matters), Software description (architecture, functionalities), Illustrative examples (demonstrations of major functions), and Impact (potential for scientific reuse). None of these sections exist in the current draft. The draft contains only statistical derivations; there is no description of the software architecture, no examples, and no discussion of impact or related software.

- **Missing metadata tables**: SoftwareX requires a structured metadata table (Table 1) with fields such as current code version, permanent link to code, legal software license, computing platform, programming language, and dependencies. This is entirely absent from the draft.

- ✓ **No license declared in the paper**: A MIT `LICENSE` file has been added to the repository root. The repository requirement is now satisfied. However, the paper itself still does not mention the license. The SoftwareX metadata table must explicitly state the license name and SPDX identifier (e.g., `MIT`). Add this to the metadata table when writing the SoftwareX manuscript.

- ✓ **GitHub requirement**: Code must be stored in an open-access GitHub repository; avoid using GitLab or other hosting platforms. The repository is on GitHub (`https://github.com/Thadhaeg/Partial-association-explorer`) and the footer link has been updated to point to it. **Remaining action**: record the URL in the SoftwareX metadata table.

**Formatting issues**

- ✓ The bibliography uses an `\begin{itemize}` list of references in author-book format (`Association_explorer_methods.tex:693-698`). SoftwareX uses numbered citations (`elsarticle-num` style). The references also mix styles; Agresti 1990 and 2013 are listed as books but McFadden 1974 is a book chapter. This needs to be formatted consistently with a `.bib` file and the correct citation style.

- The title "Association Measures, Test Statistics and Pair Plots for the Association Explorer Application" is appropriate as a working title for a methods appendix but would be weak as a SoftwareX title. A title like "AssociationExplorer: A Shiny App for Partial Association Analysis with Visualization" would be more suitable for the journal.

**Actionable recommendation**: The current LaTeX file is best repurposed as a technical appendix or supplementary material. A separate SoftwareX-compliant manuscript should be written from scratch using the official template, with the statistical content condensed into a "Software Description" subsection. The full derivations can be referenced as supplementary material or posted as a preprint.

### 2.2 Clarity and Completeness of the Statistical Descriptions

**Strengths**

- The three cases (numeric-numeric, numeric-categorical, categorical-categorical) are clearly separated and the notation is consistent within each section.
- The distinction between unconditional and conditional association is well motivated and the derivations are mostly self-contained.
- The VL measure and its derivation from G2 are clearly explained.

**Issues**

- ✓ **Section 1 (Introduction)**: The introduction lists six cases (`Association_explorer_methods.tex:22-29`) but the paper only covers three "type pairs" in three body sections (Case A: numeric-numeric, Case B: numeric-categorical, Case C: categorical-categorical). Numbering them as six distinct cases in the introduction but then organizing the paper around three sections creates a mismatch; a reader expects six distinct sections.

- **VL range**: The text states `VL` takes values in `[0,1)`, but the formula `sqrt(1 - exp(-G2/n))` can equal 1 only when `G2 -> infinity`. The notation `[0,1)` is technically correct but the practical upper bound for real datasets is much less than 1. Noting a typical interpretation scale (e.g., following Cohen-like conventions adapted for VL) would be helpful for users.

- **The pair plots section (Sections 2.3, 3.3, 4.2)** describe what is plotted but give no description of how to read or interpret the plots in an applied context. Since this is ultimately a software paper, a worked example or figure would be far more useful here than the current purely formal descriptions.

- **Case B pair plot, conditional case**: Steps 1 and 2 describe regressing Y on controls and computing group means of residuals. This is correct and matches the code. However, the paper does not note that the residualized means sum to approximately zero by construction (because OLS residuals have zero mean). Mentioning this helps users interpret plots where all bars are near zero.

- **Case C (cat-cat) section**: The paper describes the identifiability constraints (corner constraints on gamma) but does not mention that this means the reference category combination is always (first level of X, first level of Y). This is implicit but important for users interpreting the gamma table in the UI.

- **No discussion of fallback behavior**: The paper does not mention what happens when the optimization fails (no convergence, sparse tables, etc.). The code has fallback paths; the paper should acknowledge their existence.

- **Degrees of freedom for VL test**: the text states df = (I-1)(J-1) for the LR test, which is correct for the unconditional case. For the conditional case the same df is used, which is also correct (the gamma interaction block has (I-1)(J-1) free parameters regardless of Z). This could be stated more explicitly.

- ✓ **Missing reference**: The VL measure `sqrt(1 - exp(-G2/n))` derives from the transformation described in Cox and Snell (1989) and is related to Nagelkerke's R2. The paper lists Cox and Snell (1989) in the references (`Association_explorer_methods.tex:696`) but there is no `\cite` command in the body text at or near the VL definition (`Association_explorer_methods.tex:434`). Add an in-text citation at the definition of VL.

### 2.3 Accuracy

The statistical derivations are correct and match the implemented code with one exception:

- ✓ **Section 2.1, unconditional case**: The paper defines `R2_XY = r_XY^2` (`Association_explorer_methods.tex:95`) and says it is used as the global symmetric association measure. The code stores `abs(r)` in the correlation matrix (`app.r:2590`: `cor_val <- ifelse(r^2 >= threshold_num, abs(r), 0)`) rather than `r^2`. The threshold slider is labeled R² but compared as `r^2 >= threshold_num`. So the value plotted in the network tooltip is `|r|`, not `r^2`. The paper should clarify which quantity is displayed where, or the code should be made consistent (store `r^2` and compare `r^2 >= threshold`).

---

## 3. Cross-Cutting Issues

### 3.1 Measure Names and Display

✓ The paper consistently uses `R^2_{XY}` and `R^2_{XY.Z}` as the reported measures for numeric-numeric association. The code stores and displays `|r|` (absolute Pearson or partial correlation) in the matrix and network, not `r^2` (`app.r:2590`: `cor_val <- ifelse(r^2 >= threshold_num, abs(r), 0)`). The slider labeled "R²" filters on `r^2 >= threshold` but the edge tooltip shows `r = 0.72` (for example), not `R2 = 0.52`. This inconsistency will confuse users reading the paper alongside the app. **Decision needed**: pick one representation (`r` or `R^2`) and use it throughout both the paper and the UI.

### 3.2 Scope of Features Described vs. Implemented

The paper describes only the three statistical cases and their pair plots. It does not describe:

- The network visualization and its construction (edge width, edge length, color, p-value filtering).
- The variable description upload feature.
- The "Reverse axes" button on scatter plots.
- The threshold sliders and p-value filter.
- The significance level filtering that gates which edges are drawn.

For a SoftwareX article, the software description section must cover the app's main features, not just the statistical background. The current draft would need to be substantially extended (or, given the word limit, the statistical content would need to be condensed significantly to make room).

### 3.3 Notation Mismatches

| Paper notation | Code equivalent | Issue |
|---|---|---|
| `r_{XY.Z}` | `partial_cor` (variable name) | Consistent, no issue |
| `eta^2_{X|Z}` | `partial_eta_sq` | Consistent |
| `V_L` (unconditional) | `VL` (list element) | ✓ Consistent |
| `V_{L\|Z}` (conditional) | `VL/Z` (list name, line 2522) | ✓ The slash in a list name is non-standard; use `VL_Z` |
| `G^2` | never stored as a named quantity | ✓ G2 is computed but not returned from `compute_lr_stats` separately; only VL and p_value are returned. If future features need G2 directly (e.g., for display), it will need to be re-derived. |

### 3.4 Methods Described in the Paper but Not Implemented

- ✓ The paper describes interaction parameters `gamma_ij` as being displayed in the pair plots alongside D and R. The code does include a gamma table in the UI (`app.r:1154-1191`). **However**, the gamma matrix returned by `fit1$params$gamma` is a full I×J matrix including the reference row/column (which are always 0 by constraint, `app.r:1997-2003`). The paper's notation indexes gamma only for non-reference cells. The displayed table converts the full matrix directly (`app.r:1155`) and therefore shows a row and column of zeros corresponding to the reference category, which could confuse users. Add a note in the UI or trim the reference row/column from the displayed gamma table.

- ✓ Section 2.3 of the paper describes the added-variable plot as "also displaying the corresponding slope and partial correlation." The code does include both in the subtitle (`partial correlation = ... | Slope = ...`), which is consistent. Good.

---

## 4. Priority Action List

The following items are ordered by urgency for the meeting.

### Critical (must fix before any public release or submission)

1. **Define `safe_g2_cell()`** or remove the fallback branch that calls it (`app.r:2299`). Currently causes a fatal error whenever `fit0` fails in `compute_unconditional()`. *(Confirmed: function is called but never defined.)*

2. ~~**Add an open-source license** to the repository (`LICENSE` file).~~ *(Done: MIT License added to the repository root.)* **Remaining action**: declare the license (MIT, SPDX: `MIT`) in the SoftwareX metadata table and in the paper.

3. **Fix `compute_conditional()` fallback**: ensure the returned list uses `VL/Z` (or better, `VL_Z`) consistently, including in **all three** fallback paths (`app.r:2402-2403`, `2448-2451`, `2480-2497`), so the association matrix never receives a silent `NULL`. *(Confirmed: three un-renamed paths found, not just one.)*

4. **Check `optim()` convergence**: inspect `fit$convergence` after each call to `fit_structured_mnl()` and surface a warning when it is non-zero. *(Confirmed: no convergence check anywhere in the file.)*

### High priority (needed for a clean submission)

5. **Redesign the paper using the SoftwareX template**: the current draft needs to be restructured entirely. Write the SoftwareX article using the official template with required sections (Motivation, Software description, Illustrative examples, Impact). Move the detailed derivations to supplementary material.

6. **Clarify `|r|` vs `R^2` display inconsistency** throughout the paper, the UI labels, and the code. *(Confirmed: `abs(r)` stored at line 2590, paper defines `R²` as the measure.)*

7. **Rename `VL/Z`** to `VL_Z` everywhere for robustness. *(Confirmed: slash in list name works but prevents use of `$` accessor.)*

8. ~~**Remove dead code**: `build_contingency_table()`, `find_optimal_submatrix()`, `find_optimal_submatrix_heuristic()`, `library(VGAM)`, `library(grid)`, `library(gridExtra)`.~~ *(Done: three functions moved to `archive.r`; three unused `library()` calls removed from `app.r`.)*

9. **Fix the `threshold_cat` slider label**: change `"Cramer's V"` (`app.r:116`) to `V_L` to match the actual statistic computed, and align with README and paper. *(New finding from Section 1.6.)*

### Medium priority (code quality and robustness)

10. **Move statistical helpers outside `server()`** to enable unit testing and improve readability.

11. ~~**Fix the `data_env$full_data` reactivity pattern**: replace with a standard `reactiveVal()` at server scope.~~ *(Removed: this pattern is functional — see Section 1.3 correction. Refactoring is still a style improvement but not a bug fix.)*

12. **Fix the df overcounting** in `p_value_partial_cor()` when some controls have been dropped during variation filtering (`app.r:2594` vs `partial_residuals()` line 1473).

13. **Add convergence/progress feedback** for large cat-cat computations (spinner or warning about computation time for large tables).

14. ~~**Rename the file** from `partial_association_explorer_7.r` to `app.R` and reconcile the version identifier with the footer string.~~ *(Done: file renamed to `app.r` and version string removed from footer.)*

### Lower priority (polish)

15. Remove debug `cat()` calls from production code (lines 729-735, 892).
16. Add a worked example to the paper/vignette.
17. Add in-text citation to Cox and Snell (1989) at the definition of VL (`Association_explorer_methods.tex:434`).
18. Trim reference rows/columns from the displayed gamma table in the cat-cat pair plots (`app.r:1155`).
19. Clarify the slider label "R²" to read "R² / η² threshold" for mixed pairs (`app.r:108`).