---
title: 'AssociationProfiler: Interactive Profiling of Global and Local Conditional Associations in Mixed-Type Data'
tags:
  - R
  - Shiny
  - exploratory data analysis
  - association analysis
  - conditional association
  - mixed-type data
  - correlation network
authors:
  - name: Thaddée D'haegeleer
    corresponding: true
    affiliation: 1
  - name: Cédric Heuchenne
    affiliation: "1, 2"
  - name: Antoine Soetewey
    orcid: 0000-0001-8159-0804
    affiliation: "1, 2"
affiliations:
  - name: The Center for Applied Public Economics (CAPE), UCLouvain Saint-Louis Bruxelles, Boulevard du Jardin Botanique 43, 1000 Brussels, Belgium
    index: 1
  - name: HEC Liège, ULiège, Rue Louvrex 14, 4000 Liège, Belgium
    index: 2
date: 19 May 2026
bibliography: paper.bib
---

# Summary

AssociationProfiler is an open-source R Shiny application [@shiny] for profiling unconditional
and conditional associations in multivariate datasets that combine numerical and categorical
variables. For every selected variable pair the application computes a global association
measure, a significance test, and local pairwise diagnostics adapted to the variable-type
combination. Users filter an interactive network graph with sliders for association strength
and statistical significance, then inspect retained edges through harmonised pair plots.
The application handles all three pair types (numerical-numerical, numerical-categorical,
and categorical-categorical) and applies the same workflow with or without user-specified
control variables, enabling direct comparison of marginal and adjusted associations.
Source code and an example dataset are available at
<https://github.com/Thadhaeg/Partial-association-explorer>.

# Statement of need

Exploratory analysis of multivariate data is challenging when a dataset combines continuous
measurements with nominal or ordinal factors and when plausible confounders are present.
Classical correlation matrices apply only to numerical variables; contingency-table measures do
not extend naturally to mixed pair types; and marginal pairwise summaries can be misleading when
both variables are related to a common background factor such as age, education, or region.

No existing R tool provides a unified interface for all three pair types (numerical-numerical,
numerical-categorical, and categorical-categorical) together with a consistent control-variable
workflow applied simultaneously across pair types. AssociationProfiler addresses this gap. It is
designed for two primary audiences: researchers who need a fast screening tool before formal
modelling, to identify which associations persist after adjustment for confounders; and
non-specialist users (journalists, students, and data communicators) who require accessible
visualisations without writing regression, ANOVA, or multinomial likelihood code.

# State of the field

For numerical data, R packages such as corrplot [@corrplot2024], ggcorrplot [@ggcorrplot2023], and GGally [@GGally2025] provide correlation
matrices and pairwise scatter-plot grids. For categorical pairs, classical association
measures such as Pearson's chi-squared [@pearson1900], Cramér's $V$ [@cramer1946],
information-theoretic coefficients [@linfoot1957; @hamdan1971], and log-linear models
[@agresti2013] are well established. The ggstatsplot package [@patil2021ggstatsplot] overlays statistical summaries
on individual bivariate plots but does not offer a network view or a unified conditional-association
workflow. AssociationExplorer [@soetewey2026] provided an accessible Shiny interface for marginal
mixed-type associations but does not support control-variable adjustment. AssociationProfiler
complements these tools by offering a single conditional and unconditional association-profiling
framework covering all three pair types through one control-variable selection step.

# Software design

AssociationProfiler is a standalone Shiny application [@shiny] built in R [@r] with a five-panel
interface: data upload, variable selection with optional control variables, association network
(via visNetwork [@visnetwork]), pair plots, and help. Variables are auto-detected as numerical
or categorical during preprocessing. Control variables appear in all association computations
but are excluded from network nodes.

**Numerical-numerical pairs.** Without controls, Pearson's $r$ and $R^2$ are reported.
With controls, both variables are residualised on the controls by ordinary least squares and the
partial correlation of the residuals is used. Pair plots show raw scatter (no controls) or
added-variable plots (with controls), both with a fitted linear trend.

**Numerical-categorical pairs.** Without controls, the ANOVA effect size
$\eta^2 = \mathrm{SSB}/\mathrm{SST}$ is computed [@cohen1973]. With controls, a partial
$\eta^2_{X|Z}$ is derived from an ANCOVA extra-sum-of-squares comparison between a reduced
model $(Y \sim Z)$ and a full model $(Y \sim X + Z)$, with an $F$-test providing the $p$-value.
Pair plots display group means of the raw or residualised outcome, keeping the same visual
grammar across conditional and unconditional cases.

**Categorical-categorical pairs.** The global association coefficient is
$$V_L = \sqrt{1 - \exp(-G^2/n)},$$
where $G^2 = 2(\ell_1 - \ell_0)$ is the likelihood-ratio deviance between a null independence
model and an interaction alternative, and $n$ is the sample size. Without controls, $V_L$ equals
the sample analogue of Linfoot's informational coefficient of correlation [@linfoot1957] and the
square root of a Cox-Snell $R^2$ [@coxsnell1989]. With controls, a structured multinomial model
regresses the joint outcome $(X, Y)$ on the controls as regression covariates, yielding the
conditional coefficient
$$V_{L|Z} = \sqrt{1 - \exp(-G^2_{|Z}/n)}.$$
This formulation avoids discretising continuous controls into multiway contingency tables while
preserving the nested likelihood-ratio comparison between the null and alternative models.

**Local diagnostics and large-table display.** Categorical pair plots show
observed-minus-expected cell deviations $D_{ij} = O_{ij} - E^{(0)}_{ij}$ coloured by
Pearson residuals $R_{ij} = (O_{ij} - E^{(0)}_{ij})/\sqrt{E^{(0)}_{ij}}$. For large
contingency tables, an informative submatrix is selected by maximising the sum of cell-level
likelihood-ratio contributions $C_{ij} = 2 O_{ij} \log(O_{ij}/E^{(0)}_{ij})$ via integer
programming [@lpsolve], with a greedy heuristic fallback.

# Research impact statement

AssociationProfiler enables exploratory questions that standard pairwise tools cannot answer:
whether an observed association persists after adjusting for selected confounders, and which
categorical cells drive a global signal. The conditional workflow applies uniformly across all
pair types without requiring users to write model-specific code, making cautious data exploration
accessible to non-specialists. The application can serve as a teaching and diagnostic tool for
partial correlation, ANCOVA, likelihood-ratio testing, pseudo-$R^2$ measures, and conditional
independence. A Belgian ESS Round 11 subset is bundled in the repository for immediate use. The full ESS dataset, codebook, and documentation are freely available at https://ess.sikt.no/en/ [@ess2024data; @ess2024doc].

# AI usage disclosure

The authors used ChatGPT to obtain suggestions for English phrasing and to improve clarity and readability during the writing process. All content was critically reviewed and finalized by the authors.

# Acknowledgements

The three authors thank UCLouvain for funding this research project. The last author gratefully acknowledges the Walloon Region and the *Service public de Wallonie (SPW) Recherche* in Belgium for funding the ODALON research project (Win2Wal 2023 - N°2310019 - ODALON3). The authors additionally thank the researchers from UCLouvain Saint-Louis Brussels involved in the Beamm research project for their feedback on previous versions of the software.

# References
