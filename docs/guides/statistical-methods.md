# Statistical methods

Partial Association Explorer assigns a global measure, a hypothesis test, and
a local diagnostic to each pair according to the R storage classes of the two
variables.

| Pair | Marginal analysis | Conditional analysis | Local view |
|---|---|---|---|
| Numerical--numerical | Pearson correlation and t-test | Correlation of residuals after adjustment and partial-correlation t-test | Scatter or added-variable plot |
| Numerical--categorical | Eta squared and ANOVA F-test | Partial eta squared and extra-sum-of-squares F-test | Raw or residualized group means |
| Categorical--categorical | Likelihood-ratio coefficient and chi-squared test | Nested structured multinomial models conditional on controls | Observed counts colored by Pearson residuals |

## Numerical pairs

For two numerical variables, the network uses squared Pearson correlation as
its strength. With controls, each variable is regressed on the active controls
and the residuals are correlated. Controls without variation are removed before
the model is fitted.

## Numerical--categorical pairs

The marginal effect size is eta squared from a one-way ANOVA decomposition.
With controls, a reduced model containing the controls is compared with a full
model containing the controls and categorical predictor. The additional sum of
squares is converted to partial eta squared and tested with an F statistic.

## Categorical pairs

The categorical coefficient is

$$V_L = \sqrt{1-\exp(-G^2/n)},$$

where $G^2$ is the likelihood-ratio statistic comparing a null model without
the row-by-column interaction to an alternative containing that interaction.
For conditional analysis, both models allow row and column categories to depend
on the selected controls. The interaction block remains the difference between
the nested models.

The local display prints observed cell counts and colors cells using Pearson
residuals under the fitted null model. When a table exceeds 49 cells, the app
selects at most seven rows and seven columns to retain as much squared-residual
score as possible. It uses integer programming when `lpSolve` is available and
a deterministic heuristic otherwise.

## Limitations

- R storage class determines variable type; ordinal factors are treated as
  categorical unless the user recodes them.
- The three strength measures are bounded but are not directly equivalent.
- Sparse tables, rare categories, and unstable multinomial fits can affect
  categorical results.
- Statistical significance is not evidence of causality or practical
  importance.

The extended manuscript in `docs/extended-article/` contains full derivations,
implementation detail, and worked examples.
