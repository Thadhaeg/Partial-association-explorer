library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(scales)
library(reactable)
library(tidygraph)
library(visNetwork)
library(readxl)
library(janitor)
library(shinyjs)
library(tibble)
library(shinycssloaders)
library(nnet)

if (file.exists("archive.r")) {
  sys.source("archive.r", envir = environment())
}


ui <- tagList(
  tags$head(
    tags$title("Partial Association Explorer"),
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tags$style(HTML("
      .nav-link:has(.faded-pair-tab),
      .bslib-nav-link:has(.faded-pair-tab),
      button:has(.faded-pair-tab) {
        opacity: 0.6 !important;
      }
      .faded-pair-tab {
        color: #6c757d;
      }
      .nav-link:has(.conditional-only-pair-tab),
      .bslib-nav-link:has(.conditional-only-pair-tab),
      button:has(.conditional-only-pair-tab) {
        opacity: 1 !important;
      }
      .conditional-only-pair-tab {
        color: #74c69d;
        font-weight: 600;
      }
      .pair-note-muted {
        margin-bottom: 12px;
        padding: 10px 12px;
        border-left: 4px solid #adb5bd;
        background: #f8f9fa;
        color: #5c6770;
      }
      .pair-note-positive {
        margin-bottom: 12px;
        padding: 10px 12px;
        border-left: 4px solid #74c69d;
        background: #f1fbf5;
        color: #3f6f55;
      }
    "))
  ),
  fluidPage(
    shinyjs::useShinyjs(),
    class = "app-container",
    theme = bs_theme(
      version = 5,
      bootswatch = "flatly",
      primary = "#0072B2",
      base_font = font_google("Roboto"),
      heading_font = font_google("Roboto Slab"),
      code_font = font_google("Fira Code")
    ),
    tags$div(class = "centered-padding-top"),
    titlePanel(div("Partial Association Explorer", class = "app-title")),
    br(),
    tabsetPanel(
      id = "main_tabs",
      type = "tabs",
      tabPanel(
        title = tags$strong("📁 Data"),
        value = "upload_tab",
        br(),
        br(),
        fileInput(
          "data_file",
          "Upload your dataset (CSV or Excel)",
          accept = c(
            "text/csv",
            "text/comma-separated-values",
            "text/plain",
            ".csv",
            ".xlsx",
            ".xls"
          )
        ),
        fileInput(
          "desc_file",
          "(Optional) Upload variable descriptions (CSV or Excel)",
          accept = c(
            "text/csv",
            "text/comma-separated-values",
            "text/plain",
            ".csv",
            ".xlsx",
            ".xls"
          )
        ),
        tags$p(
          style = "font-size:0.85em; color: #666666;",
          "The descriptions file must contain exactly two columns named 'Variable' and 'Description'."
        ),
        br(),
        actionButton("process_data", "Process data", class = "btn btn-primary")
      ),
      tabPanel(
        title = tags$strong("🔍 Variables"),
        value = "variables_tab",
        br(),
        br(),
        uiOutput("variable_checkboxes_ui"),
        div(
          style = "margin-top: 10px;",
          actionButton(
            "clear_selected_vars",
            "Empty variable list",
            class = "btn btn-outline-secondary"
          )
        ),
        br(),

        # Control variables section
        tags$h4("Control Variables (Optional)"),
        tags$p(
          style = "font-size:0.85em; color: #666666;",
          "Select variables to control for (adjust for their effects when calculating associations).",
          "Control variables will not appear in visualizations."
        ),
        uiOutput("control_vars_ui"),
        br(),

        uiOutput("go_to_network_ui"),
        br(),
        br(),
        br(),
        uiOutput("selected_vars_table_ui")
      ),
      tabPanel(
        title = tags$strong("🔗 Correlation Network"),
        value = "network_tab",
        sidebarLayout(
          sidebarPanel(
            sliderInput(
              "threshold_num",
              "Range for Quantitative-Quantitative and Quantitative-Categorical Associations (R² / η²)",
              min = 0,
              max = 1,
              value = c(0.5, 1),
              step = 0.05
            ),
            sliderInput(
              "threshold_cat",
              "Range for Categorical-Categorical Associations (V_L)",
              min = 0,
              max = 1,
              value = c(0.5, 1),
              step = 0.05
            ),

            sliderInput(
              "threshold_p",
              "Range for p-values",
              min = 0,
              max = 0.2,
              value = c(0, 0.05),
              step = 0.005
            ),

            tags$i(tags$span(
              style = "color: #666666",
              "Only associations within the selected ranges will be displayed in the plot."
            )),
            br(),
            br(),
            uiOutput("network_mode_toggle_ui"),
            br(),
            downloadButton(
              "download_associations_csv",
              "Export associations (CSV)",
              class = "btn btn-outline-primary"
            ),
            br(),
            br(),
            fluidRow(
              column(
                12,
                align = "center",
                actionButton(
                  "go_to_pairs",
                  "See pairs plots",
                  class = "btn btn-primary"
                )
              )
            )
          ),
          mainPanel(
            class = "panel-white",
            uiOutput("network_info"),
            withSpinner(
              visNetworkOutput("network_vis", height = "600px", width = "100%"),
              type = 6,
              color = "#0072B2"
            )
          )
        )
      ),
      tabPanel(
        title = tags$strong("📊 Pairs Plots"),
        value = "pairs_tab",
        fluidPage(
          class = "panel-white",
          uiOutput("pairs_mode_toggle_ui"),
          uiOutput("pairs_context_ui"),
          withSpinner(uiOutput("pairs_plot"), type = 6, color = "#0072B2")
        )
      ),
      tabPanel(
        title = tags$strong("❓ Help"),
        value = "help_tab",
        div(
          class = "help-container",
          br(),
          br(),
          h3("How to use the Partial Association Explorer app?"),
          br(),
          tags$ul(
            tags$li(
              "Upload your dataset (CSV or Excel) in the 'Data' tab. Optionally, upload a file with variable descriptions. This file must contain 2 columns called 'Variable' and 'Description'."
            ),
            tags$li(
              "In the 'Variables' tab, select the variables you want to explore. If you upload a file containing variables' descriptions, a summary table below shows the selected variables along with their descriptions."
            ),
            tags$li(
              "Click 'Visualize all associations' to access the correlation network."
            ),
            tags$li(
              "Adjust the thresholds to filter associations by strength. Only variables that have strong associations (as defined by the thresholds) will appear in the network and pairs plots."
            ),
            tags$li(
              "In the correlation network plot, thicker and shorter edges indicate stronger associations."
            ),
            tags$li(
              "Click 'See pairs plots' to display bivariate visualizations for retained associations."
            )
          )
        )
      )
    ),
    br(),
    tags$hr(),
    tags$footer(
      class = "app-footer",
      "See the ",
      tags$a(
        href = "https://github.com/Thadhaeg/Partial-association-explorer",
        "code",
        target = "_blank"
      )
    ),
  )
)

# =============================================================================
# Statistical helper functions (pure; no Shiny reactive dependencies)
# These are defined at the top level so they can be unit-tested independently
# of the Shiny server.
# =============================================================================

# Helper to build nice reactable tables
make_table <- function(df, columns_defs, column_groups = NULL) {
  reactable(
    df,
    columns = columns_defs,
    columnGroups = column_groups,
    bordered = TRUE,
    striped = TRUE,
    highlight = TRUE,
    defaultPageSize = 25,
    showPageSizeOptions = TRUE,
    pageSizeOptions = c(25, 50),
    theme = reactableTheme(
      headerStyle = list(fontWeight = "bold")
    )
  )
}

resolve_variable_description <- function(var_name, descriptions_df = NULL) {
  if (
    is.null(descriptions_df) ||
      !all(c("variable", "description") %in% names(descriptions_df))
  ) {
    return(var_name)
  }

  idx <- match(var_name, descriptions_df$variable)
  if (is.na(idx)) {
    return(var_name)
  }

  desc <- descriptions_df$description[[idx]]
  if (length(desc) == 0 || is.na(desc) || !nzchar(trimws(as.character(desc)))) {
    return(var_name)
  }

  as.character(desc)
}

format_plot_stat <- function(x, digits = 3) {
  if (is.null(x) || length(x) == 0 || !is.finite(x[[1]])) {
    return("NA")
  }

  formatC(as.numeric(x[[1]]), digits = digits, format = "f")
}

format_plot_p_value <- function(p_value) {
  if (is.null(p_value) || length(p_value) == 0 || is.na(p_value[[1]])) {
    return("NA")
  }

  if (p_value[[1]] < 0.001) {
    return("< 0.001")
  }

  formatC(signif(as.numeric(p_value[[1]]), 3), digits = 3, format = "fg", flag = "#")
}

display_association_value <- function(value, cor_type) {
  if (
    is.null(value) ||
      length(value) == 0 ||
      is.na(value[[1]]) ||
      is.null(cor_type) ||
      length(cor_type) == 0 ||
      is.na(cor_type[[1]]) ||
      !nzchar(trimws(as.character(cor_type[[1]])))
  ) {
    return(NA_real_)
  }

  if (cor_type %in% c("Pearson's r", "Partial r", "Eta²", "Partial Eta²")) {
    return(as.numeric(value[[1]])^2)
  }

  as.numeric(value[[1]])
}

display_measure_label <- function(cor_type) {
  if (
    is.null(cor_type) ||
      length(cor_type) == 0 ||
      is.na(cor_type[[1]]) ||
      !nzchar(trimws(as.character(cor_type[[1]])))
  ) {
    return(NA_character_)
  }

  dplyr::case_when(
    cor_type[[1]] == "Pearson's r" ~ "R²",
    cor_type[[1]] == "Partial r" ~ "Partial R²",
    cor_type[[1]] == "Eta²" ~ "Eta²",
    cor_type[[1]] == "Partial Eta²" ~ "Partial Eta²",
    TRUE ~ as.character(cor_type[[1]])
  )
}

collapse_named_descriptions <- function(var_names, descriptions_df = NULL) {
  if (is.null(var_names) || length(var_names) == 0) {
    return(NA_character_)
  }

  paste(
    vapply(
      var_names,
      function(x) {
        paste0(x, " = ", resolve_variable_description(x, descriptions_df))
      },
      character(1)
    ),
    collapse = "; "
  )
}

format_controls_context_text <- function(
  selected_controls,
  descriptions_df = NULL,
  apply_controls = FALSE
) {
  if (is.null(selected_controls) || length(selected_controls) == 0) {
    return("No controls selected.")
  }

  controls_text <- collapse_named_descriptions(selected_controls, descriptions_df)

  if (isTRUE(apply_controls)) {
    paste0("Controls applied: ", controls_text)
  } else {
    paste0("Selected controls (not applied in this view): ", controls_text)
  }
}

# NEW : Helper: compute residuals of y after regressing on controls
partial_residuals <- function(y, controls_df) {
  if (is.null(controls_df) || ncol(controls_df) == 0) {
    return(y)
  }

  # Drop controls with no variation
  keep <- sapply(controls_df, function(z) length(unique(z[!is.na(z)])) > 1)
  controls_clean <- controls_df[, keep, drop = FALSE]

  if (ncol(controls_clean) == 0) {
    return(y)
  }

  dfm <- data.frame(y = y, controls_clean)
  residuals(lm(y ~ ., data = dfm))
}

# Returns the number of controls that actually have variation (same filter as
# partial_residuals uses internally), so p_value_partial_cor() gets the correct df.
count_active_controls <- function(controls_df) {
  if (is.null(controls_df) || ncol(controls_df) == 0) return(0L)
  sum(sapply(controls_df, function(z) length(unique(z[!is.na(z)])) > 1))
}

# NEW: Residualize a numeric outcome on controls (Y ~ Z) and return residuals
residualize_on_controls <- function(y, controls_df) {
  if (is.null(controls_df) || ncol(controls_df) == 0) {
    return(y)
  }

  # Keep controls with variation only (avoid singular fits)
  keep <- sapply(controls_df, function(z) length(unique(z[!is.na(z)])) > 1)
  controls_clean <- controls_df[, keep, drop = FALSE]
  if (ncol(controls_clean) == 0) {
    return(y)
  }

  dfm <- data.frame(y = y, controls_clean)
  dfm <- stats::na.omit(dfm)
  if (nrow(dfm) == 0) {
    return(numeric(0))
  }

  fit <- try(stats::lm(y ~ ., data = dfm), silent = TRUE)
  if (inherits(fit, "try-error")) {
    return(numeric(0))
  }

  residuals(fit)
}

# NEW: Partial eta-squared with F-test and p-value
calculate_partial_eta_squared_with_F <- function(
  num_var,
  cat_var,
  control_data = NULL
) {
  # Build initial data frame robustly
  if (is.null(control_data) || nrow(control_data) == 0) {
    df_temp <- data.frame(
      num_var = num_var,
      cat_var = as.factor(cat_var)
    )
  } else {
    if (
      length(num_var) != nrow(control_data) ||
        length(cat_var) != nrow(control_data)
    ) {
      return(list(
        eta = 0,
        eta_sq = 0,
        F = NA_real_,
        df1 = 0,
        df2 = 0,
        p_value = NA_real_,
        type = "sqrt(Partial Eta²)"
      ))
    }
    df_temp <- data.frame(
      num_var = num_var,
      cat_var = as.factor(cat_var),
      control_data
    )
  }

  df_temp <- stats::na.omit(df_temp)

  # Need some data
  if (nrow(df_temp) == 0) {
    return(list(
      eta = 0,
      eta_sq = 0,
      F = NA_real_,
      df1 = 0,
      df2 = 0,
      p_value = NA_real_,
      type = "sqrt(Partial Eta²)"
    ))
  }

  # Names for convenience
  all_names <- names(df_temp)
  response_name <- "num_var"
  cat_name <- "cat_var"
  control_names <- setdiff(all_names, c(response_name, cat_name))

  # Check variation for all non-response variables (cat_var + controls)
  vars_nonresp <- c(cat_name, control_names)

  has_variation <- sapply(df_temp[, vars_nonresp, drop = FALSE], function(z) {
    # For factors: require at least 2 used levels and 2 unique values
    if (is.factor(z)) {
      used_levels <- unique(z[!is.na(z)])
      length(used_levels) > 1 && length(unique(z[!is.na(z)])) > 1
    } else {
      length(unique(z[!is.na(z)])) > 1
    }
  })

  # If categorical predictor has no variation → nothing to test
  if (!isTRUE(has_variation[cat_name])) {
    return(list(
      eta = 0,
      eta_sq = 0,
      F = NA_real_,
      df1 = 0,
      df2 = 0,
      p_value = NA_real_,
      type = "sqrt(Partial Eta²)"
    ))
  }

  # Keep only controls that have variation
  controls_kept <- control_names[has_variation[control_names]]

  # Rebuild df_temp with: num_var, cat_var, and only "good" controls
  df_temp <- df_temp[,
    c(response_name, cat_name, controls_kept),
    drop = FALSE
  ]

  # If num_var has no variance → nothing to explain
  if (var(df_temp[[response_name]]) == 0) {
    return(list(
      eta = 0,
      eta_sq = 0,
      F = NA_real_,
      df1 = 0,
      df2 = 0,
      p_value = NA_real_,
      type = "sqrt(Partial Eta²)"
    ))
  }

  # Fit models safely
  fit_res <- try(
    {
      # Full model: num_var ~ cat_var + controls
      model_full <- lm(num_var ~ ., data = df_temp)

      # Reduced model:
      #   if we have controls: num_var ~ controls
      #   if no controls: num_var ~ 1 (intercept only)
      if (length(controls_kept) > 0) {
        df_reduced <- df_temp[, c(response_name, controls_kept), drop = FALSE]
        model_reduced <- lm(num_var ~ ., data = df_reduced)
      } else {
        df_reduced <- df_temp[, response_name, drop = FALSE]
        model_reduced <- lm(num_var ~ 1, data = df_reduced)
      }

      list(
        full = model_full,
        reduced = model_reduced
      )
    },
    silent = TRUE
  )

  if (inherits(fit_res, "try-error")) {
    # If anything weird happens in lm, fail gracefully
    return(list(
      eta = 0,
      eta_sq = 0,
      F = NA_real_,
      df1 = 0,
      df2 = 0,
      p_value = NA_real_,
      type = "sqrt(Partial Eta²)"
    ))
  }

  model_full <- fit_res$full
  model_reduced <- fit_res$reduced

  ss_res_full <- sum(residuals(model_full)^2)
  ss_res_reduced <- sum(residuals(model_reduced)^2)
  ss_effect <- ss_res_reduced - ss_res_full

  # df for the categorical factor (m - 1)
  m <- nlevels(df_temp[[cat_name]])
  q <- m - 1 # numerator df
  df2 <- df.residual(model_full) # denominator df

  if (ss_effect <= 0 || ss_res_full <= 0 || q <= 0 || df2 <= 0) {
    return(list(
      eta = 0,
      eta_sq = 0,
      F = NA_real_,
      df1 = q,
      df2 = df2,
      p_value = NA_real_,
      type = "sqrt(Partial Eta²)"
    ))
  }

  partial_eta_sq <- ss_effect / (ss_effect + ss_res_full)
  F_stat <- (ss_effect / q) / (ss_res_full / df2)
  p_val <- 1 - pf(F_stat, q, df2)

  list(
    eta = sqrt(partial_eta_sq),
    eta_sq = partial_eta_sq,
    F = F_stat,
    df1 = q,
    df2 = df2,
    p_value = p_val,
    type = "sqrt(Partial Eta²)"
  )
}

# NEW: p-value for (partial) correlation given controls
p_value_partial_cor <- function(r, n_eff, k_controls) {
  # r: partial correlation
  # n_eff: number of complete cases used
  # k_controls: number of control variables
  if (is.na(r)) {
    return(NA_real_)
  }
  if (abs(r) >= 1) {
    return(0)
  } # perfect correlation

  df <- n_eff - k_controls - 2
  if (df <= 0) {
    return(NA_real_)
  }

  t_stat <- r * sqrt(df / (1 - r^2))
  F_stat <- t_stat^2
  p_val <- 1 - pf(F_stat, 1, df)
  p_val
}

# ============================================================
# Case C (cat-cat)
# ============================================================

# ----------------------------
# Helpers for cat-cat computations
# ----------------------------

make_Z_design <- function(Zdf) {
  Zmm <- stats::model.matrix(~., data = Zdf) # includes intercept
  Zmm <- Zmm[, colnames(Zmm) != "(Intercept)", drop = FALSE]
  as.data.frame(Zmm)
}

get_W_levels <- function(obj) {
  # same nesting logic for W
  if (is.list(obj) && !is.null(obj$W)) {
    return(obj$W)
  }
  if (
    is.list(obj) &&
      !is.null(obj$fit) &&
      is.list(obj$fit) &&
      !is.null(obj$fit$W)
  ) {
    return(obj$fit$W)
  }
  stop("Cannot find W inside fit object.")
}

build_constraints_xy <- function(
  x_levels,
  y_levels,
  ref_x = x_levels[1],
  ref_y = y_levels[1],
  outcome_levels = NULL,
  sep = "___AE___",
  include_gamma = TRUE
) {
  # ---- Build full IJ outcome levels robustly ----
  I <- length(x_levels)
  J <- length(y_levels)

  base_lab <- paste(ref_x, ref_y, sep = sep)

  if (is.null(outcome_levels)) {
    grid <- expand.grid(
      x = as.character(x_levels),
      y = as.character(y_levels),
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    )
    outcome_levels <- paste(grid$x, grid$y, sep = sep)
  } else {
    outcome_levels <- as.character(outcome_levels)
  }

  if (!(base_lab %in% outcome_levels)) {
    stop("Baseline cell not found among outcome levels: ", base_lab)
  }

  # baseline first
  outcome_levels <- c(base_lab, setdiff(outcome_levels, base_lab))

  # ---- Parse outcome levels into (x,y) ----
  parts <- strsplit(outcome_levels, split = sep, fixed = TRUE)
  lens <- lengths(parts)
  if (any(lens != 2)) {
    bad <- outcome_levels[which(lens != 2)]
    stop(
      "Cannot parse W levels into (X,Y) using sep='",
      sep,
      "'. ",
      "Example bad levels: ",
      paste(utils::head(bad, 5), collapse = ", ")
    )
  }

  x_of_k <- vapply(parts, `[[`, character(1), 1)
  y_of_k <- vapply(parts, `[[`, character(1), 2)

  # ---- logits correspond to rows 1..(K-1) excluding baseline ----
  x_logits <- x_of_k[-1]
  y_logits <- y_of_k[-1]

  # K is the number of outcome categories we actually model
  K <- length(outcome_levels)

  if (include_gamma && K != I * J) {
    stop(
      "Cannot use full alpha/beta/gamma corner parametrization unless K == I*J (full grid)."
    )
  }

  if (length(x_logits) != (K - 1) || length(y_logits) != (K - 1)) {
    stop(
      "Internal error: logits length mismatch.\n",
      "K=",
      K,
      " so K-1=",
      (K - 1),
      "\n",
      "length(x_logits)=",
      length(x_logits),
      " length(y_logits)=",
      length(y_logits),
      "\n"
    )
  }

  # extra safety: stop early if anything is NA
  if (anyNA(x_logits) || anyNA(y_logits)) {
    stop(
      "Parsed x_logits/y_logits contain NA. First few outcomes:\n",
      paste(utils::head(outcome_levels, 10), collapse = "\n")
    )
  }

  # ---- Intercept block: alpha + beta (+ gamma if include_gamma) ----
  p_alpha <- I - 1
  p_beta <- J - 1
  p_gamma <- if (include_gamma) (I - 1) * (J - 1) else 0L
  p0 <- p_alpha + p_beta + p_gamma

  # Under H1 (include_gamma=TRUE), p0 must equal K-1.
  # Under H0 (include_gamma=FALSE), p0 < K-1 is expected and OK.
  if (include_gamma && p0 != (K - 1)) {
    stop("Mismatch: with gamma, p0 must equal K-1. Check levels.")
  }

  C0 <- matrix(0, nrow = K - 1, ncol = p0)

  cn <- c(
    paste0("alpha[", setdiff(x_levels, ref_x), "]"),
    paste0("beta[", setdiff(y_levels, ref_y), "]")
  )
  if (include_gamma) {
    cn <- c(
      cn,
      as.vector(outer(
        setdiff(x_levels, ref_x),
        setdiff(y_levels, ref_y),
        FUN = function(a, b) paste0("gamma[", a, ",", b, "]")
      ))
    )
  }
  colnames(C0) <- cn

  alpha_cols <- setNames(seq_len(p_alpha), setdiff(x_levels, ref_x))
  beta_cols <- setNames(p_alpha + seq_len(p_beta), setdiff(y_levels, ref_y))

  gamma_index <- NULL
  if (include_gamma) {
    gamma_names <- colnames(C0)[(p_alpha + p_beta + 1):p0]
    gamma_index <- setNames((p_alpha + p_beta + 1):p0, gamma_names)
  }

  for (r in seq_along(x_logits)) {
    xi <- x_logits[r]
    yj <- y_logits[r]

    if (is.na(xi) || is.na(yj)) {
      stop("NA xi/yj at r=", r, ". This should never happen.")
    }

    if (xi != ref_x) {
      C0[r, alpha_cols[[xi]]] <- 1
    }
    if (yj != ref_y) {
      C0[r, beta_cols[[yj]]] <- 1
    }
    if (include_gamma && xi != ref_x && yj != ref_y) {
      gnm <- paste0("gamma[", xi, ",", yj, "]")
      C0[r, gamma_index[[gnm]]] <- 1
    }
  }

  # ---- Z slope block: lambda_i,k + kappa_j,k ----
  pz <- (I - 1) + (J - 1)
  Cz <- matrix(0, nrow = K - 1, ncol = pz)
  colnames(Cz) <- c(
    paste0("lambda[", setdiff(x_levels, ref_x), "]"),
    paste0("kappa[", setdiff(y_levels, ref_y), "]")
  )

  lambda_cols <- setNames(seq_len(I - 1), setdiff(x_levels, ref_x))
  kappa_cols <- setNames((I - 1) + seq_len(J - 1), setdiff(y_levels, ref_y))

  for (r in seq_along(x_logits)) {
    xi <- x_logits[r]
    yj <- y_logits[r]
    if (xi != ref_x) {
      Cz[r, lambda_cols[[xi]]] <- 1
    }
    if (yj != ref_y) Cz[r, kappa_cols[[yj]]] <- 1
  }

  list(
    outcome_levels = outcome_levels,
    base_lab = base_lab,
    C0 = C0,
    Cz = Cz,
    ref_x = ref_x,
    ref_y = ref_y,
    sep = sep,
    x_levels = x_levels,
    y_levels = y_levels,
    include_gamma = include_gamma
  )
}

# ----------------------------
# Manual structured multinomial logit via optim()
# ----------------------------

softmax_rows <- function(eta) {
  # eta: n x K matrix
  m <- apply(eta, 1, max)
  ex <- exp(eta - m)
  ex / rowSums(ex)
}

# Build mapping from each joint category w = (x_i, y_j) to indices i, j
parse_W_levels <- function(W_levels, sep, x_levels, y_levels) {
  parts <- strsplit(W_levels, split = sep, fixed = TRUE)
  wx <- vapply(parts, `[[`, "", 1)
  wy <- vapply(parts, `[[`, "", 2)

  # validate
  if (any(!wx %in% x_levels) || any(!wy %in% y_levels)) {
    stop(
      "Some W levels cannot be mapped back to x_levels/y_levels. Check sep and factor labels."
    )
  }

  i_idx <- match(wx, x_levels)
  j_idx <- match(wy, y_levels)
  list(wx = wx, wy = wy, i = i_idx, j = j_idx)
}

if (!exists("find_optimal_submatrix_heuristic", mode = "function")) {
  find_optimal_submatrix_heuristic <- function(
      contribution_matrix,
      n = 5,
      reason = NULL
  ) {
    N <- nrow(contribution_matrix)
    M <- ncol(contribution_matrix)
    target_rows <- min(N, n)
    target_cols <- min(M, n)

    contribution_df <- data.frame(
      row = rep(seq_len(N), each = M),
      col = rep(seq_len(M), times = N),
      value = as.vector(contribution_matrix),
      stringsAsFactors = FALSE
    )

    contribution_df <- contribution_df[contribution_df$value > 0, , drop = FALSE]
    contribution_df <- contribution_df[order(-contribution_df$value), , drop = FALSE]

    if (nrow(contribution_df) == 0) {
      return(list(
        rows = seq_len(target_rows),
        cols = seq_len(target_cols),
        objective = 0,
        method = "heuristic",
        fallback_reason = reason
      ))
    }

    top_n <- min(target_rows * target_cols, nrow(contribution_df))
    top_rows <- unique(contribution_df$row[seq_len(top_n)])
    top_cols <- unique(contribution_df$col[seq_len(top_n)])

    if (length(top_rows) > target_rows) {
      top_rows <- top_rows[seq_len(target_rows)]
    }
    if (length(top_cols) > target_cols) {
      top_cols <- top_cols[seq_len(target_cols)]
    }

    list(
      rows = top_rows,
      cols = top_cols,
      objective = sum(contribution_matrix[top_rows, top_cols, drop = FALSE]),
      method = "heuristic",
      fallback_reason = reason
    )
  }
}

if (!exists("find_optimal_submatrix", mode = "function")) {
  find_optimal_submatrix <- function(contribution_matrix, n = 5) {
    N <- nrow(contribution_matrix)
    M <- ncol(contribution_matrix)
    target_rows <- min(N, n)
    target_cols <- min(M, n)

    if (N <= n && M <= n) {
      return(list(
        rows = seq_len(N),
        cols = seq_len(M),
        objective = sum(contribution_matrix),
        method = "full",
        fallback_reason = NULL
      ))
    }

    if (!require(lpSolve, quietly = TRUE)) {
      return(find_optimal_submatrix_heuristic(
        contribution_matrix,
        n,
        reason = "the lpSolve package is not available, so the exact binary optimization could not be run"
      ))
    }

    total_vars <- N + M + N * M
    objective <- c(
      rep(0, N + M),
      as.vector(t(contribution_matrix))
    )

    n_constraints <- 2 + 3 * N * M
    constraint_matrix <- matrix(0, nrow = n_constraints, ncol = total_vars)
    constraint_dir <- character(n_constraints)
    constraint_rhs <- numeric(n_constraints)

    constraint_index <- 1L
    constraint_matrix[constraint_index, seq_len(N)] <- 1
    constraint_dir[constraint_index] <- "=="
    constraint_rhs[constraint_index] <- target_rows
    constraint_index <- constraint_index + 1L

    constraint_matrix[constraint_index, N + seq_len(M)] <- 1
    constraint_dir[constraint_index] <- "=="
    constraint_rhs[constraint_index] <- target_cols
    constraint_index <- constraint_index + 1L

    for (i in seq_len(N)) {
      for (j in seq_len(M)) {
        z_index <- N + M + (i - 1L) * M + j

        constraint_matrix[constraint_index, i] <- -1
        constraint_matrix[constraint_index, z_index] <- 1
        constraint_dir[constraint_index] <- "<="
        constraint_rhs[constraint_index] <- 0
        constraint_index <- constraint_index + 1L

        constraint_matrix[constraint_index, N + j] <- -1
        constraint_matrix[constraint_index, z_index] <- 1
        constraint_dir[constraint_index] <- "<="
        constraint_rhs[constraint_index] <- 0
        constraint_index <- constraint_index + 1L

        constraint_matrix[constraint_index, i] <- -1
        constraint_matrix[constraint_index, N + j] <- -1
        constraint_matrix[constraint_index, z_index] <- 1
        constraint_dir[constraint_index] <- ">="
        constraint_rhs[constraint_index] <- -1
        constraint_index <- constraint_index + 1L
      }
    }

    solution <- lpSolve::lp(
      direction = "max",
      objective.in = objective,
      const.mat = constraint_matrix,
      const.dir = constraint_dir,
      const.rhs = constraint_rhs,
      all.bin = TRUE,
      compute.sens = FALSE
    )

    if (solution$status != 0) {
      return(find_optimal_submatrix_heuristic(
        contribution_matrix,
        n,
        reason = paste0(
          "the exact binary optimization returned solver status ",
          solution$status
        )
      ))
    }

    u_values <- solution$solution[seq_len(N)]
    v_values <- solution$solution[N + seq_len(M)]

    list(
      rows = which(round(u_values) == 1),
      cols = which(round(v_values) == 1),
      objective = solution$objval,
      method = "optimal",
      fallback_reason = NULL
    )
  }
}

group_catcat_observations <- function(Zmm, y_idx_local, K) {
  n_obs <- length(y_idx_local)

  if (is.null(Zmm) || ncol(Zmm) == 0) {
    counts <- matrix(
      as.numeric(tabulate(y_idx_local, nbins = K)),
      nrow = 1,
      ncol = K
    )

    return(list(
      counts = counts,
      weights = rowSums(counts),
      Z_group = NULL,
      group_id = rep.int(1L, n_obs)
    ))
  }

  Zdf <- as.data.frame(Zmm, check.names = FALSE)
  group_fac <- interaction(Zdf, drop = TRUE, lex.order = TRUE, sep = "\r")
  y_fac <- factor(y_idx_local, levels = seq_len(K))

  counts <- as.matrix(xtabs(~ group_fac + y_fac))
  first_idx <- match(levels(group_fac), group_fac)
  Z_group <- Zmm[first_idx, , drop = FALSE]

  list(
    counts = counts,
    weights = rowSums(counts),
    Z_group = Z_group,
    group_id = as.integer(group_fac)
  )
}

prepare_catcat_problem <- function(
  x_vec,
  y_vec,
  Zdf = NULL,
  sep = "___AE___"
) {
  x_fac <- droplevels(factor(x_vec))
  y_fac <- droplevels(factor(y_vec))

  ok <- if (is.null(Zdf) || ncol(Zdf) == 0) {
    stats::complete.cases(x_fac, y_fac)
  } else {
    stats::complete.cases(x_fac, y_fac, Zdf)
  }

  x_fac <- droplevels(x_fac[ok])
  y_fac <- droplevels(y_fac[ok])
  if (!is.null(Zdf) && ncol(Zdf) > 0) {
    Zdf <- Zdf[ok, , drop = FALSE]
  }

  if (length(x_fac) == 0) {
    return(list(
      empty = TRUE,
      x_fac = x_fac,
      y_fac = y_fac,
      Zdf = Zdf
    ))
  }

  x_levels <- levels(x_fac)
  y_levels <- levels(y_fac)
  I <- length(x_levels)
  J <- length(y_levels)

  grid <- expand.grid(
    x = as.character(x_levels),
    y = as.character(y_levels),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  W_levels <- paste(grid$x, grid$y, sep = sep)
  K <- length(W_levels)

  W_obs_labels <- paste(as.character(x_fac), as.character(y_fac), sep = sep)
  y_idx_local <- match(W_obs_labels, W_levels)
  if (anyNA(y_idx_local)) {
    stop("Some observed (X,Y) pairs could not be matched to full W_levels.")
  }

  Zmm <- NULL
  if (!is.null(Zdf) && ncol(Zdf) > 0) {
    Zmm <- as.matrix(make_Z_design(as.data.frame(Zdf)))
  }
  q <- if (is.null(Zmm)) 0L else ncol(Zmm)

  grouped <- group_catcat_observations(Zmm, y_idx_local, K)
  mapW <- parse_W_levels(W_levels, sep, x_levels, y_levels)
  O <- as.matrix(table(x_fac, y_fac))

  x_indicator <- if (I > 1) {
    outer(mapW$i, seq.int(2L, I), `==`) * 1
  } else {
    matrix(0, nrow = K, ncol = 0)
  }

  y_indicator <- if (J > 1) {
    outer(mapW$j, seq.int(2L, J), `==`) * 1
  } else {
    matrix(0, nrow = K, ncol = 0)
  }

  list(
    empty = FALSE,
    x_fac = x_fac,
    y_fac = y_fac,
    Zdf = Zdf,
    x_levels = x_levels,
    y_levels = y_levels,
    I = I,
    J = J,
    K = K,
    q = q,
    n_obs = length(y_idx_local),
    O = O,
    W_levels = W_levels,
    W_obs_labels = W_obs_labels,
    y_idx = y_idx_local,
    mapW = mapW,
    Z_group = grouped$Z_group,
    counts = grouped$counts,
    weights = grouped$weights,
    x_indicator = x_indicator,
    y_indicator = y_indicator
  )
}

# Pack/unpack theta for H1 and H0
# Baseline constraints: alpha[ref_x]=0, beta[ref_y]=0, gamma[ref_x,*]=0, gamma[*,ref_y]=0
make_param_index <- function(I, J, q, include_gamma = TRUE) {
  # free alpha: I-1, free beta: J-1
  # free gamma: (I-1)(J-1) if include_gamma else 0
  # free lambda: (I-1)*q, free kappa: (J-1)*q
  p_alpha <- I - 1
  p_beta <- J - 1
  p_gamma <- if (include_gamma) (I - 1) * (J - 1) else 0L
  p_lambda <- (I - 1) * q
  p_kappa <- (J - 1) * q

  list(
    p_alpha = p_alpha,
    p_beta = p_beta,
    p_gamma = p_gamma,
    p_lambda = p_lambda,
    p_kappa = p_kappa,
    p_total = p_alpha + p_beta + p_gamma + p_lambda + p_kappa
  )
}

unpack_theta <- function(theta, I, J, q, include_gamma = TRUE) {
  idx <- make_param_index(I, J, q, include_gamma)
  stopifnot(length(theta) == idx$p_total)

  pos <- 1
  take <- function(k) {
    if (k <= 0) {
      return(numeric(0))
    }
    out <- theta[pos:(pos + k - 1)]
    pos <<- pos + k
    out
  }

  alpha_free <- take(idx$p_alpha) # length I-1
  beta_free <- take(idx$p_beta) # length J-1
  gamma_free <- if (include_gamma) take(idx$p_gamma) else numeric(0)
  lambda_free <- take(idx$p_lambda) # length (I-1)*q
  kappa_free <- take(idx$p_kappa) # length (J-1)*q

  # Expand into full arrays with baseline = 0
  alpha <- c(0, alpha_free) # length I  (assumes ref_x is first level)
  beta <- c(0, beta_free) # length J  (assumes ref_y is first level)

  gamma <- matrix(0, nrow = I, ncol = J)
  if (include_gamma) {
    # fill only rows 2..I and cols 2..J (corner constraints)
    gamma[2:I, 2:J] <- matrix(
      gamma_free,
      nrow = I - 1,
      ncol = J - 1,
      byrow = FALSE
    )
  }

  lambda <- matrix(0, nrow = I, ncol = q)
  kappa <- matrix(0, nrow = J, ncol = q)
  if (q > 0) {
    lambda[2:I, ] <- matrix(
      lambda_free,
      nrow = I - 1,
      ncol = q,
      byrow = FALSE
    )
    kappa[2:J, ] <- matrix(kappa_free, nrow = J - 1, ncol = q, byrow = FALSE)
  }

  list(
    alpha = alpha,
    beta = beta,
    gamma = gamma,
    lambda = lambda,
    kappa = kappa
  )
}

expand_theta_with_gamma <- function(theta, I, J, q) {
  idx0 <- make_param_index(I, J, q, include_gamma = FALSE)
  idx1 <- make_param_index(I, J, q, include_gamma = TRUE)

  stopifnot(length(theta) == idx0$p_total)

  take_block <- function(values, pos, k) {
    if (k <= 0) {
      return(list(values = numeric(0), pos = pos))
    }

    list(
      values = values[pos:(pos + k - 1L)],
      pos = pos + k
    )
  }

  pos <- 1L
  block <- take_block(theta, pos, idx0$p_alpha)
  alpha_free <- block$values
  pos <- block$pos

  block <- take_block(theta, pos, idx0$p_beta)
  beta_free <- block$values
  pos <- block$pos

  block <- take_block(theta, pos, idx0$p_lambda)
  lambda_free <- block$values
  pos <- block$pos

  block <- take_block(theta, pos, idx0$p_kappa)
  kappa_free <- block$values

  c(
    alpha_free,
    beta_free,
    rep(0, idx1$p_gamma),
    lambda_free,
    kappa_free
  )
}

# Compute eta (n x K) for all joint outcomes in W_levels
compute_eta <- function(pars, mapW, Zmm = NULL, n_rows = NULL) {
  K <- length(mapW$i)
  q <- if (is.null(Zmm)) 0L else ncol(Zmm)
  if (is.null(n_rows)) {
    n_rows <- if (q > 0) nrow(Zmm) else 1L
  }

  # intercept part per category k
  base_cat <- pars$alpha[mapW$i] +
    pars$beta[mapW$j] +
    pars$gamma[cbind(mapW$i, mapW$j)]
  eta <- matrix(base_cat, nrow = n_rows, ncol = K, byrow = TRUE)

  # Z slopes
  if (q > 0) {
    slope_mat <- pars$lambda[mapW$i, , drop = FALSE] +
      pars$kappa[mapW$j, , drop = FALSE]
    eta <- eta + Zmm %*% t(slope_mat)
  }

  eta
}

evaluate_structured_mnl <- function(theta, prep, include_gamma = TRUE) {
  pars <- unpack_theta(theta, prep$I, prep$J, prep$q, include_gamma)
  eta <- compute_eta(
    pars,
    prep$mapW,
    Zmm = prep$Z_group,
    n_rows = nrow(prep$counts)
  )
  pi_hat <- softmax_rows(eta)

  if (any(!is.finite(pi_hat)) || any(pi_hat <= 0)) {
    return(list(
      nll = 1e12,
      grad = rep(0, length(theta)),
      pi_hat = pi_hat,
      params = pars,
      logLik = -1e12,
      expected_counts = matrix(
        0,
        nrow = prep$I,
        ncol = prep$J,
        dimnames = list(prep$x_levels, prep$y_levels)
      )
    ))
  }

  log_pi <- log(pi_hat)
  nll <- -sum(prep$counts * log_pi)

  resid <- prep$counts - pi_hat * prep$weights
  resid_by_cell <- matrix(
    colSums(resid),
    nrow = prep$I,
    ncol = prep$J,
    byrow = FALSE
  )

  alpha_grad <- if (prep$I > 1) rowSums(resid_by_cell)[-1] else numeric(0)
  beta_grad <- if (prep$J > 1) colSums(resid_by_cell)[-1] else numeric(0)
  gamma_grad <- if (include_gamma && prep$I > 1 && prep$J > 1) {
    as.vector(resid_by_cell[-1, -1, drop = FALSE])
  } else {
    numeric(0)
  }

  if (prep$q > 0 && prep$I > 1) {
    resid_x <- resid %*% prep$x_indicator
    lambda_grad <- as.vector(t(crossprod(prep$Z_group, resid_x)))
  } else {
    lambda_grad <- numeric(0)
  }

  if (prep$q > 0 && prep$J > 1) {
    resid_y <- resid %*% prep$y_indicator
    kappa_grad <- as.vector(t(crossprod(prep$Z_group, resid_y)))
  } else {
    kappa_grad <- numeric(0)
  }

  grad_ll <- c(
    alpha_grad,
    beta_grad,
    gamma_grad,
    lambda_grad,
    kappa_grad
  )

  fitted_counts <- pi_hat * prep$weights
  expected_counts <- matrix(
    colSums(fitted_counts),
    nrow = prep$I,
    ncol = prep$J,
    byrow = FALSE,
    dimnames = list(prep$x_levels, prep$y_levels)
  )

  list(
    nll = nll,
    grad = -grad_ll,
    pi_hat = pi_hat,
    params = pars,
    logLik = -nll,
    expected_counts = expected_counts
  )
}

fit_structured_mnl_prepared <- function(
  prep,
  include_gamma = TRUE,
  start = NULL
) {
  idx <- make_param_index(prep$I, prep$J, prep$q, include_gamma)
  theta0 <- if (is.null(start)) rep(0, idx$p_total) else start

  if (length(theta0) != idx$p_total) {
    stop("Starting value has the wrong length for this model.")
  }

  last_theta <- NULL
  last_eval <- NULL

  evaluate_cached <- function(theta) {
    if (!is.null(last_theta) && isTRUE(all(theta == last_theta))) {
      return(last_eval)
    }

    eval_res <- evaluate_structured_mnl(theta, prep, include_gamma)
    last_theta <<- theta
    last_eval <<- eval_res
    eval_res
  }

  fit <- stats::optim(
    par = theta0,
    fn = function(theta) evaluate_cached(theta)$nll,
    gr = function(theta) evaluate_cached(theta)$grad,
    method = "BFGS",
    control = list(maxit = 1000, reltol = 1e-8)
  )

  if (fit$convergence != 0) {
    warning(
      "optim() did not converge (code ", fit$convergence, ") for a ",
      prep$I, "x", prep$J, " table. VL result may be unreliable."
    )
  }

  fit_eval <- evaluate_cached(fit$par)

  list(
    fit = fit,
    pi_hat = fit_eval$pi_hat,
    W_levels = prep$W_levels,
    x_levels = prep$x_levels,
    y_levels = prep$y_levels,
    params = fit_eval$params,
    logLik = fit_eval$logLik,
    expected_counts = fit_eval$expected_counts
  )
}

# Fit structured multinomial with optim; returns fitted pi and params
fit_structured_mnl <- function(
  x_fac,
  y_fac,
  Zdf = NULL,
  sep = "___AE___",
  include_gamma = TRUE,
  start = NULL
) {
  prep <- prepare_catcat_problem(x_fac, y_fac, Zdf = Zdf, sep = sep)
  if (isTRUE(prep$empty)) {
    stop("No complete cases available for the structured cat-cat fit.")
  }

  fit_structured_mnl_prepared(
    prep,
    include_gamma = include_gamma,
    start = start
  )
}

safe_pearson_cell <- function(O, E) {
  if (is.na(O) || is.na(E) || E <= 0) {
    return(NA_real_)
  }
  (O - E) / sqrt(E)
}

safe_g2_cell <- function(O, E) {
  if (is.na(O) || is.na(E) || E <= 0 || O <= 0) {
    return(0)
  }
  2 * O * log(O / E)
}

# ----------------------------
# Helpers for cat-cat outputs
# ----------------------------

make_catcat_result <- function(
  VL = NA_real_,
  p_value = NA_real_,
  O = NULL,
  E0 = NULL,
  D = NULL,
  R = NULL,
  gamma = NULL,
  alpha = NULL,
  beta = NULL,
  lambda = NULL,
  kappa = NULL
) {
  list(
    VL = VL,
    p_value = p_value,
    O = O,
    E0 = E0,
    D = D,
    R = R,
    gamma = gamma,
    alpha = alpha,
    beta = beta,
    lambda = lambda,
    kappa = kappa
  )
}

compute_local_tables <- function(O, E0) {
  D <- O - E0
  R <- (O - E0) / sqrt(E0)
  R[!is.finite(R) | E0 <= 0] <- NA_real_
  dimnames(R) <- dimnames(O)

  list(D = D, R = R)
}

compute_lr_stats <- function(ll0, ll1, df, n) {
  G2 <- 2 * (ll1 - ll0)
  p_value <- if (df > 0 && is.finite(G2) && G2 >= 0) {
    1 - stats::pchisq(G2, df = df)
  } else {
    NA_real_
  }
  VL <- if (n > 0 && is.finite(G2)) sqrt(1 - exp(-G2 / n)) else NA_real_

  list(G2 = G2, p_value = p_value, VL = VL)
}

normalize_threshold_range <- function(x, default_min = 0, default_max = 1) {
  if (is.null(x) || length(x) == 0) {
    return(c(default_min, default_max))
  }
  if (length(x) == 1) {
    return(c(default_min, x[[1]]))
  }

  rng <- as.numeric(x[1:2])
  c(min(rng, na.rm = TRUE), max(rng, na.rm = TRUE))
}

apply_p_value_threshold <- function(mat, p_mat, threshold_p) {
  if (is.null(p_mat)) {
    return(mat)
  }

  p_rng <- normalize_threshold_range(threshold_p, default_min = 0, default_max = 0.2)
  sig_mask <- !is.na(p_mat) & p_mat >= p_rng[1] & p_mat <= p_rng[2]
  mat[!sig_mask] <- 0
  mat
}

filter_association_result <- function(
  cor_result,
  threshold_num,
  threshold_cat,
  threshold_p,
  prune = FALSE
) {
  req_fields <- c("cor_matrix", "cor_type_matrix", "p_matrix")
  if (is.null(cor_result) || !all(req_fields %in% names(cor_result))) {
    return(NULL)
  }

  mat <- apply_association_thresholds(
    cor_result$cor_matrix,
    cor_result$cor_type_matrix,
    threshold_num,
    threshold_cat
  )
  mat <- apply_p_value_threshold(mat, cor_result$p_matrix, threshold_p)
  mat[is.na(mat)] <- 0

  if (isTRUE(prune)) {
    mat <- prune_isolated_nodes(mat)
  }

  mat
}

matrix_has_edges <- function(mat) {
  !is.null(mat) &&
    nrow(mat) > 1 &&
    ncol(mat) > 1 &&
    sum(mat[upper.tri(mat)] != 0, na.rm = TRUE) > 0
}

build_association_export_df <- function(
  cor_result,
  data,
  descriptions_df = NULL,
  control_vars_selected = NULL,
  controls_applied = FALSE,
  view_mode = NULL,
  threshold_num = NULL,
  threshold_cat = NULL,
  threshold_p = NULL
) {
  if (is.null(cor_result) || is.null(data) || ncol(data) < 2) {
    return(data.frame())
  }

  vars <- names(data)
  pair_index <- combn(vars, 2, simplify = FALSE)
  filtered_mat <- filter_association_result(
    cor_result,
    threshold_num,
    threshold_cat,
    threshold_p,
    prune = FALSE
  )

  controls_selected_text <- collapse_named_descriptions(
    control_vars_selected,
    descriptions_df
  )

  rows <- lapply(pair_index, function(pair) {
    v1 <- pair[1]
    v2 <- pair[2]

    measure_type <- cor_result$cor_type_matrix[v1, v2]
    measure_label <- display_measure_label(measure_type)
    measure_value <- display_association_value(
      cor_result$cor_matrix[v1, v2],
      measure_type
    )
    retained <- FALSE
    if (!is.null(filtered_mat) && v1 %in% rownames(filtered_mat) && v2 %in% colnames(filtered_mat)) {
      retained <- filtered_mat[v1, v2] != 0
    }

    data.frame(
      variable_1 = v1,
      variable_1_description = resolve_variable_description(v1, descriptions_df),
      variable_2 = v2,
      variable_2_description = resolve_variable_description(v2, descriptions_df),
      association_measure = measure_label,
      association_strength = measure_value,
      p_value = cor_result$p_matrix[v1, v2],
      association_context = if (!is.null(view_mode) && length(view_mode) > 0) {
        as.character(view_mode[[1]])
      } else if (isTRUE(controls_applied)) {
        "conditional"
      } else {
        "unconditional"
      },
      controls_applied = controls_applied,
      selected_controls = if (length(control_vars_selected) > 0) {
        paste(control_vars_selected, collapse = "; ")
      } else {
        NA_character_
      },
      selected_controls_descriptions = controls_selected_text,
      retained_under_current_filters = retained,
      stringsAsFactors = FALSE
    )
  })

  dplyr::bind_rows(rows)
}

prune_isolated_nodes <- function(mat) {
  if (is.null(mat) || nrow(mat) == 0 || ncol(mat) == 0) {
    return(mat)
  }

  pruned <- mat
  repeat {
    adjacency <- (abs(pruned) > 0)
    diag(adjacency) <- FALSE
    keep <- rowSums(adjacency) > 0

    if (all(keep) || !any(keep)) {
      break
    }

    pruned <- pruned[keep, keep, drop = FALSE]
    if (nrow(pruned) == 0 || ncol(pruned) == 0) {
      break
    }
  }

  pruned
}

safe_named_square_subset <- function(mat, target_names, fill = 0) {
  target_names <- unique(as.character(target_names))

  out <- matrix(
    fill,
    nrow = length(target_names),
    ncol = length(target_names),
    dimnames = list(target_names, target_names)
  )

  if (
    is.null(mat) ||
      length(target_names) == 0 ||
      is.null(rownames(mat)) ||
      is.null(colnames(mat))
  ) {
    return(out)
  }

  common_names <- intersect(target_names, intersect(rownames(mat), colnames(mat)))
  if (length(common_names) > 0) {
    out[common_names, common_names] <- mat[common_names, common_names, drop = FALSE]
  }

  out
}

align_named_square_matrices <- function(primary_mat, secondary_mat, fill = 0) {
  primary_names <- if (!is.null(primary_mat) && !is.null(rownames(primary_mat))) {
    rownames(primary_mat)
  } else {
    character(0)
  }
  secondary_names <- if (!is.null(secondary_mat) && !is.null(rownames(secondary_mat))) {
    rownames(secondary_mat)
  } else {
    character(0)
  }

  all_names <- union(primary_names, secondary_names)

  list(
    primary = safe_named_square_subset(primary_mat, all_names, fill = fill),
    secondary = safe_named_square_subset(secondary_mat, all_names, fill = fill)
  )
}

safe_named_matrix_value <- function(mat, row_name, col_name, default = NA_real_) {
  if (
    is.null(mat) ||
      is.null(rownames(mat)) ||
      is.null(colnames(mat)) ||
      !(row_name %in% rownames(mat)) ||
      !(col_name %in% colnames(mat))
  ) {
    return(default)
  }

  mat[row_name, col_name]
}

compute_catcat_display_scores <- function(O, E0) {
  scores <- matrix(0, nrow = nrow(O), ncol = ncol(O), dimnames = dimnames(O))
  mask <- is.finite(E0) & (E0 > 0)
  scores[mask] <- ((O[mask] - E0[mask])^2) / E0[mask]
  scores[!is.finite(scores) | scores < 0] <- 0
  scores
}

select_catcat_display_submatrix <- function(contribution_matrix, max_dim = 7L) {
  n_rows <- nrow(contribution_matrix)
  n_cols <- ncol(contribution_matrix)

  if ((n_rows * n_cols) <= (max_dim * max_dim)) {
    return(list(
      rows = seq_len(n_rows),
      cols = seq_len(n_cols),
      reduced = FALSE,
      method = "full",
      fallback_reason = NULL
    ))
  }

  if (exists("find_optimal_submatrix", mode = "function")) {
    selection <- tryCatch(
      find_optimal_submatrix(contribution_matrix, n = max_dim),
      error = function(e) {
        find_optimal_submatrix_heuristic(
          contribution_matrix,
          n = max_dim,
          reason = paste0(
            "the exact binary optimization raised an error: ",
            conditionMessage(e)
          )
        )
      }
    )
    if (!is.null(selection)) {
      selection$reduced <- TRUE
      if (is.null(selection$method)) {
        selection$method <- "optimal"
      }
      if (is.null(selection$fallback_reason)) {
        selection$fallback_reason <- NULL
      }
      return(selection)
    }
  }

  selection <- find_optimal_submatrix_heuristic(
    contribution_matrix,
    n = max_dim,
    reason = "the exact optimization routine was not available in the current session"
  )
  selection$reduced <- TRUE
  selection
}

compute_marginal_expected <- function(O) {
  n <- sum(O)
  outer(rowSums(O), colSums(O)) / n
}

# ----------------------------
# Unconditional cat-cat
# ----------------------------

compute_unconditional <- function(x_vec, y_vec) {
  prep <- prepare_catcat_problem(x_vec, y_vec, Zdf = NULL, sep = "___AE___")

  if (isTRUE(prep$empty)) {
    return(make_catcat_result(
      VL = 0,
      p_value = NA_real_,
      O = NULL,
      E0 = NULL,
      D = NULL,
      R = NULL,
      gamma = NULL,
      alpha = NULL,
      beta = NULL,
      lambda = NULL,
      kappa = NULL
    ))
  }

  O <- prep$O
  n <- prep$n_obs
  I <- prep$I
  J <- prep$J
  df_lr <- (I - 1) * (J - 1)

  # H0 fit
  fit0 <- try(
    fit_structured_mnl_prepared(prep, include_gamma = FALSE),
    silent = TRUE
  )

  # Fallback: marginal expected counts under independence
  if (inherits(fit0, "try-error")) {
    E0 <- compute_marginal_expected(O)
    loc <- compute_local_tables(O, E0)
    mask <- (O > 0) & (E0 > 0)
    G2 <- sum(2 * O[mask] * log(O[mask] / E0[mask]), na.rm = TRUE)
    p_value <- if (df_lr > 0 && G2 >= 0) {
      1 - stats::pchisq(G2, df = df_lr)
    } else {
      NA_real_
    }
    VL <- if (n > 0) sqrt(1 - exp(-G2 / n)) else 0

    return(make_catcat_result(
      VL = VL,
      p_value = p_value,
      O = O,
      E0 = E0,
      D = loc$D,
      R = loc$R,
      gamma = NULL,
      alpha = NULL,
      beta = NULL,
      lambda = NULL,
      kappa = NULL
    ))
  }

  ll0 <- fit0$logLik
  E0 <- fit0$expected_counts
  loc <- compute_local_tables(O, E0)

  # H1 fit
  fit1 <- try(
    fit_structured_mnl_prepared(
      prep,
      include_gamma = TRUE,
      start = expand_theta_with_gamma(fit0$fit$par, prep$I, prep$J, prep$q)
    ),
    silent = TRUE
  )

  if (inherits(fit1, "try-error")) {
    return(make_catcat_result(
      VL = NA_real_,
      p_value = NA_real_,
      O = O,
      E0 = E0,
      D = loc$D,
      R = loc$R,
      gamma = NULL,
      alpha = NULL,
      beta = NULL,
      lambda = NULL,
      kappa = NULL
    ))
  }

  ll1 <- fit1$logLik
  lr <- compute_lr_stats(ll0, ll1, df_lr, n)

  params <- fit1$params

  make_catcat_result(
    VL = lr$VL,
    p_value = lr$p_value,
    O = O,
    E0 = E0,
    D = loc$D,
    R = loc$R,
    gamma = params$gamma,
    alpha = params$alpha,
    beta = params$beta,
    lambda = params$lambda,
    kappa = params$kappa
  )
}

# ----------------------------
# Conditional cat-cat
# ----------------------------

compute_conditional <- function(x_vec, y_vec, Zdf) {
  if (is.null(Zdf) || ncol(Zdf) == 0) {
    res <- compute_unconditional(x_vec, y_vec)
    names(res)[names(res) == "VL"] <- "VL_Z"
    return(res)
  }

  prep <- prepare_catcat_problem(
    x_vec,
    y_vec,
    Zdf = as.data.frame(Zdf),
    sep = "___AE___"
  )

  if (isTRUE(prep$empty)) {
    out <- make_catcat_result(
      VL = 0,
      p_value = NA_real_,
      O = NULL,
      E0 = NULL,
      D = NULL,
      R = NULL,
      gamma = NULL,
      alpha = NULL,
      beta = NULL,
      lambda = NULL,
      kappa = NULL
    )
    names(out)[names(out) == "VL"] <- "VL_Z"
    return(out)
  }

  O <- prep$O
  n <- prep$n_obs
  I <- prep$I
  J <- prep$J
  df_lr <- (I - 1) * (J - 1)

  # H0 fit
  fit0 <- try(
    fit_structured_mnl_prepared(prep, include_gamma = FALSE),
    silent = TRUE
  )

  # Conditional fallback -> revert to unconditional measure
  if (inherits(fit0, "try-error")) {
    base_res <- compute_unconditional(prep$x_fac, prep$y_fac)
    names(base_res)[names(base_res) == "VL"] <- "VL_Z"
    return(base_res)
  }

  ll0 <- fit0$logLik
  E0 <- fit0$expected_counts
  loc <- compute_local_tables(O, E0)

  # H1 fit
  fit1 <- try(
    fit_structured_mnl_prepared(
      prep,
      include_gamma = TRUE,
      start = expand_theta_with_gamma(fit0$fit$par, prep$I, prep$J, prep$q)
    ),
    silent = TRUE
  )

  if (inherits(fit1, "try-error")) {
    out <- make_catcat_result(
      VL = NA_real_,
      p_value = NA_real_,
      O = O,
      E0 = E0,
      D = loc$D,
      R = loc$R,
      gamma = NULL,
      alpha = NULL,
      beta = NULL,
      lambda = NULL,
      kappa = NULL
    )
    names(out)[names(out) == "VL"] <- "VL_Z"
    return(out)
  }

  ll1 <- fit1$logLik
  lr <- compute_lr_stats(ll0, ll1, df_lr, n)

  params <- fit1$params

  out <- make_catcat_result(
    VL = lr$VL,
    p_value = lr$p_value,
    O = O,
    E0 = E0,
    D = loc$D,
    R = loc$R,
    gamma = params$gamma,
    alpha = params$alpha,
    beta = params$beta,
    lambda = params$lambda,
    kappa = params$kappa
  )

  names(out)[names(out) == "VL"] <- "VL_Z"
  out
}

make_pair_cache_key <- function(v1, v2, control_vars = NULL) {
  pair_part <- paste(sort(c(v1, v2)), collapse = "||")
  control_part <- if (is.null(control_vars) || length(control_vars) == 0) {
    ""
  } else {
    paste(sort(control_vars), collapse = "||")
  }

  paste(pair_part, control_part, sep = "__controls__")
}

get_cached_pair_result <- function(cache_env, key) {
  if (is.null(cache_env) || !exists(key, envir = cache_env, inherits = FALSE)) {
    return(NULL)
  }

  get(key, envir = cache_env, inherits = FALSE)
}

set_cached_pair_result <- function(cache_env, key, value) {
  if (!is.null(cache_env)) {
    assign(key, value, envir = cache_env)
  }

  value
}

# Apply UI thresholds after the expensive association matrix has already been computed.
apply_association_thresholds <- function(
  cor_matrix,
  cor_type_matrix,
  threshold_num,
  threshold_cat
) {
  cor_filtered <- cor_matrix
  num_rng <- normalize_threshold_range(threshold_num, default_min = 0, default_max = 1)
  cat_rng <- normalize_threshold_range(threshold_cat, default_min = 0, default_max = 1)

  cat_mask <- cor_type_matrix %in% c("VL", "VL|Z")
  other_mask <- cor_type_matrix != "" & !cat_mask

  cor_filtered[cat_mask & (cor_filtered < cat_rng[1] | cor_filtered > cat_rng[2])] <- 0
  cor_sq <- cor_filtered^2
  cor_filtered[other_mask & (cor_sq < num_rng[1] | cor_sq > num_rng[2])] <- 0

  diag(cor_filtered) <- 1
  cor_filtered
}

# UPDATED : Function to calculate correlations and partial correlations
# full_data: the complete dataset including control columns (passed explicitly
#            so this function has no Shiny reactive dependencies).
calculate_correlations <- function(
  data,
  threshold_num = NULL,
  threshold_cat = NULL,
  control_vars = NULL,
  full_data = NULL,
  pair_cache = NULL
) {
  vars <- names(data)
  n <- length(vars)

  cor_matrix <- matrix(0, n, n, dimnames = list(vars, vars))
  cor_type_matrix <- matrix("", n, n, dimnames = list(vars, vars))
  p_matrix <- matrix(NA_real_, n, n, dimnames = list(vars, vars))

  combs <- combn(vars, 2, simplify = FALSE)

  has_controls <- !is.null(control_vars) && length(control_vars) > 0

  for (pair in combs) {
    v1 <- pair[1]
    v2 <- pair[2]

    is_num1 <- is.numeric(data[[v1]])
    is_num2 <- is.numeric(data[[v2]])

    cor_val <- 0
    cor_type <- ""
    p_val <- NA_real_ # reset for this pair

    # --- Handle controls / complete cases ---
    if (has_controls && !is.null(full_data)) {
      complete_cases <- complete.cases(
        full_data[[v1]],
        full_data[[v2]],
        full_data[, control_vars, drop = FALSE]
      )
      x <- full_data[[v1]][complete_cases]
      y <- full_data[[v2]][complete_cases]
      control_data <- full_data[complete_cases, control_vars, drop = FALSE]
    } else {
      complete_cases <- complete.cases(data[[v1]], data[[v2]])
      x <- data[[v1]][complete_cases]
      y <- data[[v2]][complete_cases]
      control_data <- NULL
    }

    # ---------- Numeric vs numeric ----------
    if (is_num1 && is_num2) {
      if (length(x) > 0 && length(y) > 0) {
        if (has_controls && !is.null(control_data)) {
          # Partial correlation
          tryCatch(
            {
              resid_x <- partial_residuals(x, control_data)
              resid_y <- partial_residuals(y, control_data)
              r <- cor(resid_x, resid_y, use = "complete.obs")

              if (!is.na(r)) {
                cor_val <- abs(r)
                cor_type <- "Partial r"

                n_eff <- length(resid_x)
                k_controls <- count_active_controls(control_data)
                p_val <- p_value_partial_cor(r, n_eff, k_controls)
              }
            },
            error = function(e) {
              # Fallback to raw Pearson's r if partial fails
              r <- cor(x, y, use = "complete.obs")
              if (!is.na(r)) {
                cor_val <- abs(r)
                cor_type <- "Pearson's r"

                n_eff <- length(x)
                p_val <- p_value_partial_cor(r, n_eff, 0)
              }
            }
          )
        } else {
          # Regular Pearson correlation
          r <- cor(x, y, use = "complete.obs")
          if (!is.na(r)) {
            cor_val <- abs(r)
            cor_type <- "Pearson's r"

            n_eff <- length(x)
            p_val <- p_value_partial_cor(r, n_eff, 0)
          }
        }
      }

      # ---------- Categorical vs categorical (VL) ----------
    } else if (!is_num1 && !is_num2) {
      if (length(x) > 0 && length(y) > 0) {
        cache_key <- make_pair_cache_key(
          v1,
          v2,
          if (has_controls) control_vars else NULL
        )
        cor_result <- get_cached_pair_result(pair_cache, cache_key)

        if (is.null(cor_result)) {
          if (has_controls && !is.null(control_data)) {
            cor_result <- compute_conditional(
              x_vec = x,
              y_vec = y,
              Zdf = control_data
            )
          } else {
            cor_result <- compute_unconditional(
              x_vec = x,
              y_vec = y
            )
          }
          cor_result <- set_cached_pair_result(pair_cache, cache_key, cor_result)
        }

        if (has_controls && !is.null(control_data)) {
          vl_value <- cor_result[["VL_Z"]]
          cor_type <- "VL|Z"
        } else {
          vl_value <- cor_result$VL
          cor_type <- "VL"
        }

        cor_val <- ifelse(!is.na(vl_value), vl_value, 0)
        p_val <- cor_result$p_value
      }

      # ---------- Mixed case (numeric vs categorical) ----------
    } else {
      if (is_num1) {
        num_var <- x
        cat_var <- y
      } else {
        num_var <- y
        cat_var <- x
      }

      if (length(num_var) > 0 && length(cat_var) > 0) {
        res_eta <- calculate_partial_eta_squared_with_F(
          num_var = num_var,
          cat_var = cat_var,
          control_data = if (has_controls && !is.null(control_data)) {
            control_data
          } else {
            NULL
          }
        )

        if (!is.na(res_eta$eta)) {
          cor_val <- res_eta$eta
          cor_type <- if (has_controls && !is.null(control_data)) {
            "Partial Eta²"
          } else {
            "Eta²"
          }
          p_val <- res_eta$p_value
        }
      }
    }

    # --- Store results symmetrically ---
    cor_matrix[v1, v2] <- cor_matrix[v2, v1] <- cor_val
    cor_type_matrix[v1, v2] <- cor_type_matrix[v2, v1] <- cor_type

    if (!is.na(p_val)) {
      p_matrix[v1, v2] <- p_matrix[v2, v1] <- p_val
    }
  } # end for (pair in combs)

  diag(cor_matrix) <- 1
  cor_matrix[is.na(cor_matrix)] <- 0

  diag(p_matrix) <- NA_real_

  list(
    cor_matrix = cor_matrix,
    cor_type_matrix = cor_type_matrix,
    p_matrix = p_matrix
  )
}

# =============================================================================
# Shiny server
# =============================================================================

server <- function(input, output, session) {
  data <- reactiveVal(NULL)
  var_descriptions <- reactiveVal(NULL)

  # Counts visible nodes and edges
  network_summary <- reactive({
    req(network_data())

    nodes <- network_data()$nodes
    edges <- network_data()$edges

    list(
      n_nodes = if (!is.null(nodes)) nrow(nodes) else 0,
      n_edges = if (!is.null(edges)) nrow(edges) else 0
    )
  })

  # NEW: Track which plots have reversed axes
  reversed_axes <- reactiveValues()

  # NEW: Control variables logic
  # Store full dataset for control variable access
  data_env <- new.env()
  data_env$full_data <- reactiveVal(NULL)
  pair_cache <- new.env(parent = emptyenv())
  association_view_mode <- reactiveVal("unconditional")
  active_pair_plot_tab <- reactiveVal(NULL)
  show_unconditional_pair_plots <- reactiveVal(FALSE)

  flip_association_view <- function() {
    if (!has_controls()) {
      association_view_mode("unconditional")
      return(invisible(NULL))
    }

    association_view_mode(
      if (identical(association_view_mode(), "conditional")) {
        "unconditional"
      } else {
        "conditional"
      }
    )
  }

  # NEW: Control variables UI
  output$control_vars_ui <- renderUI({
    req(data())
    selectizeInput(
      inputId = "control_vars",
      label = "Select control variables:",
      choices = names(data()),
      selected = NULL,
      multiple = TRUE,
      width = "100%",
      options = list(
        maxItems = NULL,
        plugins = list("remove_button"),
        placeholder = "Choose control variables...",
        openOnFocus = TRUE
      )
    )
  })

  # NEW: Filtered variables for visualization (excludes controls)
  visualization_vars <- reactive({
    req(input$selected_vars)
    if (!is.null(input$control_vars) && length(input$control_vars) > 0) {
      setdiff(input$selected_vars, input$control_vars)
    } else {
      input$selected_vars
    }
  })

  # NEW: Check if we have controls
  has_controls <- reactive({
    !is.null(input$control_vars) && length(input$control_vars) > 0
  })

  observeEvent(input$control_vars, {
    association_view_mode(if (has_controls()) "conditional" else "unconditional")
    if (!has_controls()) {
      show_unconditional_pair_plots(FALSE)
    }
  }, ignoreInit = FALSE)

  observeEvent(input$process_data, {
    association_view_mode("unconditional")
    active_pair_plot_tab(NULL)
    show_unconditional_pair_plots(FALSE)
  }, ignoreInit = TRUE)

  current_view_uses_controls <- reactive({
    has_controls() && identical(association_view_mode(), "conditional")
  })

  current_view_controls <- reactive({
    if (current_view_uses_controls()) {
      input$control_vars
    } else {
      NULL
    }
  })

  current_view_label <- reactive({
    if (current_view_uses_controls()) {
      "conditional"
    } else {
      "unconditional"
    }
  })

  alternative_view_label <- reactive({
    if (!has_controls()) {
      NA_character_
    } else if (current_view_uses_controls()) {
      "unconditional"
    } else {
      "conditional"
    }
  })

  output$network_mode_toggle_ui <- renderUI({
    if (!has_controls()) {
      return(NULL)
    }

    button_label <- if (current_view_uses_controls()) {
      "Switch to unconditional comparison"
    } else {
      "Switch to conditional comparison"
    }

    tagList(
      actionButton(
        "toggle_network_view",
        button_label,
        class = "btn btn-secondary"
      ),
      tags$p(
        style = "margin-top:8px; font-size:0.85em; color:#666666;",
        paste0(
          "Current view: ",
          current_view_label(),
          ". Green edges are unique to the current view; gray dashed edges are only in the alternative view."
        )
      )
    )
  })

  output$pairs_mode_toggle_ui <- renderUI({
    if (!has_controls()) {
      return(NULL)
    }

    button_label <- if (isTRUE(show_unconditional_pair_plots())) {
      "Hide unconditional comparison"
    } else {
      "Show unconditional comparison below"
    }

    tagList(
      div(
        style = "margin: 12px 0;",
        actionButton(
          "toggle_pair_comparison",
          button_label,
          class = "btn btn-secondary"
        )
      )
    )
  })

  output$pairs_context_ui <- renderUI({
    div(
      style = "margin: 4px 0 14px 0; padding: 10px 12px; background: #f8f9fa; border-left: 4px solid #0072B2;",
      tags$div(
        style = "font-weight: 700; margin-bottom: 4px;",
        paste0(
          "Displayed pair-plot view: ",
          if (has_controls()) "Conditional" else "Unconditional"
        )
      ),
      tags$div(
        style = "font-size: 0.92em; color: #555;",
        format_controls_context_text(
          selected_controls = input$control_vars,
          descriptions_df = var_descriptions(),
          apply_controls = has_controls()
        )
      ),
      if (has_controls()) {
        tagList(
          tags$div(
            style = "font-size: 0.9em; color: #666; margin-top: 4px;",
            if (isTRUE(show_unconditional_pair_plots())) {
              "Unconditional comparison is displayed below each conditional pair plot."
            } else {
              "Use the button above to show or hide the unconditional comparison below each conditional pair plot."
            }
          ),
          tags$div(
            style = "font-size: 0.9em; color: #666; margin-top: 4px;",
            "Faded pair tabs correspond to associations retained without controls but no longer retained after conditioning."
          )
        )
      }
    )
  })

  observeEvent(input$toggle_network_view, {
    flip_association_view()
  }, ignoreInit = TRUE)

  observeEvent(input$toggle_pair_comparison, {
    show_unconditional_pair_plots(!isTRUE(show_unconditional_pair_plots()))
  }, ignoreInit = TRUE)

  observeEvent(input$bivariate_tabs, {
    active_pair_plot_tab(input$bivariate_tabs)
  }, ignoreInit = FALSE)

  observeEvent(input$process_data, {
    req(input$data_file)

    # Read the uploaded data
    data_path <- input$data_file$datapath
    if (grepl("\\.csv$", data_path, ignore.case = TRUE)) {
      data_df <- read.csv(data_path, stringsAsFactors = TRUE)
    } else if (grepl("\\.(xlsx|xls)$", data_path, ignore.case = TRUE)) {
      data_df <- read_excel(data_path) |>
        mutate(across(where(is.character), as.factor))
    } else {
      stop("Unsupported file format for data file.")
    }

    # Remove variables with all equal values (e.g., variance zero)
    original_names <- names(data_df)
    data_df <- data_df[,
      sapply(data_df, function(x) length(unique(x[!is.na(x)])) > 1),
      drop = FALSE
    ]

    # Store filtered data
    data(data_df)
    data_env$full_data(data_df) # NEW: Store the full dataset with all variables
    cache_keys <- ls(envir = pair_cache, all.names = TRUE)
    if (length(cache_keys) > 0) {
      rm(list = cache_keys, envir = pair_cache)
    }

    # Show a warning if variables were removed
    removed_vars <- setdiff(original_names, names(data_df))
    if (length(removed_vars) > 0) {
      showNotification(
        paste(
          "The following variables were removed because they contain only one unique value:",
          paste(removed_vars, collapse = ", ")
        ),
        type = "warning"
      )
    }

    # Initialize descriptions with variable names as default descriptions
    default_descriptions <- data.frame(
      variable = names(data_df),
      description = names(data_df),
      stringsAsFactors = FALSE
    )

    # Read the uploaded descriptions if a file is provided
    if (!is.null(input$desc_file)) {
      # 1) read the uploaded file
      desc_path <- input$desc_file$datapath
      if (grepl("\\.csv$", desc_path, ignore.case = TRUE)) {
        user_desc <- read.csv(
          desc_path,
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      } else {
        user_desc <- read_excel(desc_path)
      }

      # Trim whitespace from column names
      colnames(user_desc) <- trimws(colnames(user_desc))

      # Validate the description file
      validation_passed <- TRUE

      if (length(colnames(user_desc)) != 2) {
        showNotification(
          "The description file must contain exactly two columns named 'Variable' and 'Description'.",
          type = "error",
          duration = NULL
        )
        validation_passed <- FALSE
      } else if (!all(c("Variable", "Description") %in% colnames(user_desc))) {
        showNotification(
          paste(
            "The description file must contain exactly two columns named 'Variable' and 'Description'.",
            "Found columns:",
            paste(sQuote(colnames(user_desc)), collapse = ", ")
          ),
          type = "error",
          duration = NULL
        )
        validation_passed <- FALSE
      }

      if (validation_passed) {
        # If validation passes, continue with processing
        user_desc <- user_desc |>
          janitor::clean_names() |>
          select(variable, description)

        merged_desc <- default_descriptions |>
          left_join(user_desc, by = "variable") |>
          mutate(
            description = ifelse(
              is.na(description.y) | description.y == "",
              variable,
              description.y
            )
          ) |>
          select(variable, description)
        var_descriptions(merged_desc)
      } else {
        var_descriptions(default_descriptions)
      }
    } else {
      var_descriptions(default_descriptions)
    }

    # Redirect to the Variables tab after processing the data
    updateTabsetPanel(session, "main_tabs", selected = "variables_tab")
  })

  output$variable_checkboxes_ui <- renderUI({
    req(data())
    selectizeInput(
      inputId = "selected_vars",
      label = "Select variables to include:",
      choices = names(data()),
      selected = names(data()),
      multiple = TRUE,
      width = "100%", # ⬅️ This makes the input take full width of its container
      options = list(
        maxItems = NULL,
        plugins = list("remove_button"),
        placeholder = "Choose variables...",
        openOnFocus = TRUE
      )
    )
  })

  valid_selected_vars <- reactive({
    req(input$selected_vars)
    input$selected_vars
  })

  observeEvent(input$clear_selected_vars, {
    updateSelectizeInput(
      session,
      inputId = "selected_vars",
      selected = character(0)
    )
  }, ignoreInit = TRUE)

  output$go_to_network_ui <- renderUI({
    req(input$selected_vars)
    actionButton(
      "go_to_network",
      "Visualize all associations",
      class = "btn btn-primary"
    )
  })

  output$selected_vars_table_ui <- renderUI({
    req(input$selected_vars)
    # hide the table unless the user has uploaded a custom descriptions file
    req(input$desc_file)
    reactableOutput("selected_vars_table")
  })

  output$selected_vars_table <- renderReactable({
    req(var_descriptions())
    req(valid_selected_vars())

    df <- tibble(variable = valid_selected_vars()) |>
      left_join(var_descriptions(), by = "variable")

    cols <- list(
      variable = colDef(name = "Variable", minWidth = 150),
      description = colDef(name = "Description", html = TRUE, minWidth = 400)
    )

    make_table(df, cols)
  })

  observeEvent(input$go_to_network, {
    updateTabsetPanel(session, inputId = "main_tabs", selected = "network_tab")
  })

  observeEvent(input$go_to_pairs, {
    updateTabsetPanel(session, inputId = "main_tabs", selected = "pairs_tab")
  })

  selected_data_reactive <- reactive({
    req(data())
    selected_vars <- visualization_vars()
    data()[, selected_vars, drop = FALSE]
  })

  cor_matrix_unconditional_reactive <- reactive({
    calculate_correlations(
      data = selected_data_reactive(),
      control_vars = NULL,
      full_data = NULL,
      pair_cache = pair_cache
    )
  })

  cor_matrix_conditional_reactive <- reactive({
    if (!has_controls()) {
      return(cor_matrix_unconditional_reactive())
    }

    calculate_correlations(
      data = selected_data_reactive(),
      control_vars = input$control_vars,
      full_data = data_env$full_data(),
      pair_cache = pair_cache
    )
  })

  cor_matrix_reactive <- reactive({
    if (current_view_uses_controls()) {
      cor_matrix_conditional_reactive()
    } else {
      cor_matrix_unconditional_reactive()
    }
  })

  comparison_cor_matrix_reactive <- reactive({
    if (!has_controls()) {
      return(NULL)
    }

    if (current_view_uses_controls()) {
      cor_matrix_unconditional_reactive()
    } else {
      cor_matrix_conditional_reactive()
    }
  })

  cor_matrix_vals <- reactive({
    cor_matrix_reactive()
  })

  current_filtered_matrix_raw <- reactive({
    filter_association_result(
      cor_matrix_reactive(),
      input$threshold_num,
      input$threshold_cat,
      input$threshold_p,
      prune = FALSE
    )
  })

  current_filtered_matrix <- reactive({
    mat <- current_filtered_matrix_raw()
    if (is.null(mat)) {
      return(NULL)
    }
    prune_isolated_nodes(mat)
  })

  comparison_filtered_matrix_raw <- reactive({
    if (!has_controls()) {
      return(NULL)
    }

    filter_association_result(
      comparison_cor_matrix_reactive(),
      input$threshold_num,
      input$threshold_cat,
      input$threshold_p,
      prune = FALSE
    )
  })

  pair_plots_primary_cor_result <- reactive({
    if (has_controls()) {
      cor_matrix_conditional_reactive()
    } else {
      cor_matrix_unconditional_reactive()
    }
  })

  pair_plots_primary_filtered_matrix_raw <- reactive({
    filter_association_result(
      pair_plots_primary_cor_result(),
      input$threshold_num,
      input$threshold_cat,
      input$threshold_p,
      prune = FALSE
    )
  })

  pair_plots_primary_filtered_matrix <- reactive({
    mat <- pair_plots_primary_filtered_matrix_raw()
    if (is.null(mat)) {
      return(NULL)
    }
    prune_isolated_nodes(mat)
  })

  pair_plots_unconditional_filtered_matrix_raw <- reactive({
    if (!has_controls()) {
      return(NULL)
    }

    filter_association_result(
      cor_matrix_unconditional_reactive(),
      input$threshold_num,
      input$threshold_cat,
      input$threshold_p,
      prune = FALSE
    )
  })

  significant_pairs <- reactive({
    req(input$threshold_num)
    req(input$threshold_cat)

    if (!has_controls()) {
      filtered_matrix <- pair_plots_primary_filtered_matrix()

      if (is.null(filtered_matrix) || ncol(filtered_matrix) == 0) {
        return(NULL)
      }

      pairs <- which(
        filtered_matrix != 0 & upper.tri(filtered_matrix),
        arr.ind = TRUE
      )

      if (nrow(pairs) == 0) {
        return(NULL)
      }

      return(data.frame(
        var1 = rownames(filtered_matrix)[pairs[, 1]],
        var2 = colnames(filtered_matrix)[pairs[, 2]],
        retained_conditional = FALSE,
        retained_unconditional = TRUE,
        faded = FALSE,
        conditional_only = FALSE,
        stringsAsFactors = FALSE
      ))
    }

    cond_mat <- pair_plots_primary_filtered_matrix_raw()
    uncond_mat <- pair_plots_unconditional_filtered_matrix_raw()

    if (is.null(cond_mat) && is.null(uncond_mat)) {
      return(NULL)
    }

    if (is.null(cond_mat)) {
      cond_mat <- uncond_mat * 0
    }
    if (is.null(uncond_mat)) {
      uncond_mat <- cond_mat * 0
    }

    aligned_pair_mats <- align_named_square_matrices(
      cond_mat,
      uncond_mat,
      fill = 0
    )
    cond_mat <- aligned_pair_mats$primary
    uncond_mat <- aligned_pair_mats$secondary

    if (nrow(cond_mat) == 0 || ncol(cond_mat) == 0) {
      return(NULL)
    }

    union_pairs <- which(
      ((cond_mat != 0) | (uncond_mat != 0)) & upper.tri(cond_mat),
      arr.ind = TRUE
    )

    if (nrow(union_pairs) == 0) {
      return(NULL)
    }

    out <- data.frame(
      var1 = rownames(cond_mat)[union_pairs[, 1]],
      var2 = colnames(cond_mat)[union_pairs[, 2]],
      retained_conditional = cond_mat[union_pairs] != 0,
      retained_unconditional = uncond_mat[union_pairs] != 0,
      stringsAsFactors = FALSE
    )

    out$faded <- !out$retained_conditional & out$retained_unconditional
    out$conditional_only <- out$retained_conditional & !out$retained_unconditional
    out[order(out$faded, out$var1, out$var2), , drop = FALSE]
  })

  filtered_data_for_pairs <- reactive({
    pairs <- significant_pairs()

    if (is.null(pairs) || nrow(pairs) == 0) {
      return(NULL)
    }

    vars_to_keep <- unique(c(pairs$var1, pairs$var2))
    data()[, vars_to_keep, drop = FALSE]
  })

  output$network_info <- renderUI({
    cor_result <- cor_matrix_reactive()
    req(cor_result)

    active_mat <- current_filtered_matrix_raw()
    compare_mat <- comparison_filtered_matrix_raw()

    if (is.null(active_mat)) {
      return(NULL)
    }

    active_mat[is.na(active_mat)] <- 0
    if (is.null(compare_mat)) {
      compare_mat <- active_mat * 0
    } else {
      compare_mat[is.na(compare_mat)] <- 0
    }

    aligned_info_mats <- align_named_square_matrices(
      active_mat,
      compare_mat,
      fill = 0
    )
    active_mat <- aligned_info_mats$primary
    compare_mat <- aligned_info_mats$secondary

    union_presence <- (active_mat != 0) | (compare_mat != 0)
    diag(union_presence) <- FALSE
    keep <- rowSums(union_presence) > 0
    n_nodes <- sum(keep)

    active_pruned <- current_filtered_matrix()
    n_active_edges <- if (matrix_has_edges(active_pruned)) {
      sum(active_pruned[upper.tri(active_pruned)] != 0, na.rm = TRUE)
    } else {
      0
    }

    active_type_mat <- cor_result$cor_type_matrix
    active_edgelist <- if (matrix_has_edges(active_pruned)) {
      which(active_pruned != 0 & upper.tri(active_pruned), arr.ind = TRUE)
    } else {
      matrix(integer(0), ncol = 2)
    }
    edge_types <- if (nrow(active_edgelist) > 0) {
      active_type_mat[active_edgelist]
    } else {
      character(0)
    }

    n_catcat <- sum(edge_types %in% c("VL", "VL|Z"), na.rm = TRUE)
    n_numnum <- sum(edge_types %in% c("Pearson's r", "Partial r"), na.rm = TRUE)
    n_mixed <- n_active_edges - n_catcat - n_numnum

    view_context_block <- div(
      style = "margin-bottom: 10px; padding: 10px 12px; background: #f8f9fa; border-left: 4px solid #0072B2;",
      tags$div(
        style = "font-weight: 700; margin-bottom: 4px;",
        paste0(
          "Displayed network view: ",
          if (current_view_uses_controls()) "Conditional" else "Unconditional"
        )
      ),
      tags$div(
        style = "font-size: 0.92em; color: #555;",
        format_controls_context_text(
          selected_controls = input$control_vars,
          descriptions_df = var_descriptions(),
          apply_controls = current_view_uses_controls()
        )
      )
    )

    if (!has_controls()) {
      return(tagList(
        view_context_block,
        div(
          style = "margin-bottom: 8px; font-size: 13px;",
          strong("Network summary: "),
          paste0(n_nodes, " variables, ", n_active_edges, " associations"),
          br(),
          span(
            style = "opacity: 0.85;",
            paste0(
              "Breakdown: ",
              n_catcat,
              " cat–cat (VL/VL|Z), ",
              n_numnum,
              " num–num (R²), ",
              n_mixed,
              " mixed (η²)"
            )
          )
        )
      ))
    }

    compare_result <- comparison_cor_matrix_reactive()
    keep_names <- names(keep)[keep]
    active_keep <- active_mat[keep_names, keep_names, drop = FALSE]
    compare_keep <- compare_mat[keep_names, keep_names, drop = FALSE]
    union_edgelist <- which(
      ((active_keep != 0) | (compare_keep != 0)) & upper.tri(active_keep),
      arr.ind = TRUE
    )

    active_present <- active_keep[union_edgelist] != 0
    compare_present <- compare_keep[union_edgelist] != 0
    n_shared <- sum(active_present & compare_present, na.rm = TRUE)
    n_current_only <- sum(active_present & !compare_present, na.rm = TRUE)
    n_alternative_only <- sum(!active_present & compare_present, na.rm = TRUE)

    tagList(
      view_context_block,
      div(
        style = "margin-bottom: 8px; font-size: 13px;",
        strong("Network summary: "),
        paste0(n_nodes, " variables displayed"),
        br(),
        span(
          style = "opacity: 0.9;",
          paste0(
            "Current ",
            current_view_label(),
            " view: ",
            n_active_edges,
            " retained associations."
          )
        ),
        br(),
        span(
          style = "opacity: 0.85;",
          paste0(
            "Shared with the ",
            alternative_view_label(),
            " view: ",
            n_shared,
            " | Only current: ",
            n_current_only,
            " | Only ",
            alternative_view_label(),
            ": ",
            n_alternative_only
          )
        ),
        br(),
        span(
          style = "opacity: 0.85;",
          paste0(
            "Breakdown in current view: ",
            n_catcat,
            " cat–cat (VL/VL|Z), ",
            n_numnum,
            " num–num (R²), ",
            n_mixed,
            " mixed (η²)"
          )
        )
      )
    )
  })

  output$network_vis <- renderVisNetwork({
    active_result <- cor_matrix_reactive()
    active_filtered <- current_filtered_matrix_raw()

    req(active_result)
    req(active_filtered)

    active_filtered[is.na(active_filtered)] <- 0

    compare_result <- comparison_cor_matrix_reactive()
    compare_filtered <- comparison_filtered_matrix_raw()
    if (is.null(compare_filtered)) {
      compare_filtered <- active_filtered * 0
    } else {
      compare_filtered[is.na(compare_filtered)] <- 0
    }

    aligned_network_mats <- align_named_square_matrices(
      active_filtered,
      compare_filtered,
      fill = 0
    )
    active_filtered <- aligned_network_mats$primary
    compare_filtered <- aligned_network_mats$secondary

    union_presence <- (active_filtered != 0) | (compare_filtered != 0)
    diag(union_presence) <- FALSE
    keep <- rowSums(union_presence) > 0

    validate(
      need(
        any(keep),
        "No associations above the thresholds and significance level. Please adjust the thresholds or select different variables."
      )
    )

    keep_names <- names(keep)[keep]
    active_mat <- safe_named_square_subset(active_filtered, keep_names, fill = 0)
    compare_mat <- safe_named_square_subset(compare_filtered, keep_names, fill = 0)
    active_type_mat <- safe_named_square_subset(active_result$cor_type_matrix, keep_names, fill = "")
    active_p_mat <- safe_named_square_subset(active_result$p_matrix, keep_names, fill = NA_real_)

    compare_type_mat <- if (is.null(compare_result)) {
      matrix("", nrow = nrow(active_mat), ncol = ncol(active_mat), dimnames = dimnames(active_mat))
    } else {
      safe_named_square_subset(compare_result$cor_type_matrix, keep_names, fill = "")
    }
    compare_p_mat <- if (is.null(compare_result)) {
      matrix(NA_real_, nrow = nrow(active_mat), ncol = ncol(active_mat), dimnames = dimnames(active_mat))
    } else {
      safe_named_square_subset(compare_result$p_matrix, keep_names, fill = NA_real_)
    }

    nodes <- data.frame(id = colnames(active_mat), stringsAsFactors = FALSE) |>
      left_join(var_descriptions(), by = c("id" = "variable")) |>
      mutate(
        label = id,
        title = description,
        size = 15
      ) |>
      select(id, label, title, size)

    edgelist <- which(
      ((active_mat != 0) | (compare_mat != 0)) & upper.tri(active_mat),
      arr.ind = TRUE
    )

    edges <- data.frame(
      from = rownames(active_mat)[edgelist[, 1]],
      to = colnames(active_mat)[edgelist[, 2]],
      stringsAsFactors = FALSE
    )

    active_present <- active_mat[edgelist] != 0
    compare_present <- compare_mat[edgelist] != 0

    active_strengths <- abs(active_mat[edgelist])
    compare_strengths <- abs(compare_mat[edgelist])
    strengths <- ifelse(active_present, active_strengths, compare_strengths)

    if (length(strengths) <= 1 || max(strengths) == min(strengths)) {
      edges$width <- 3
    } else {
      edges$width <- 1 +
        4 * (strengths - min(strengths)) / (max(strengths) - min(strengths))
    }

    if (!has_controls()) {
      edges$color <- "#4C78A8"
      edges$dashes <- FALSE
    } else {
      edges$color <- ifelse(
        active_present & compare_present,
        "#4C78A8",
        ifelse(active_present, "#2CA25F", "#B0B0B0")
      )
      edges$dashes <- !active_present & compare_present
    }

    active_types <- active_type_mat[edgelist]
    compare_types <- compare_type_mat[edgelist]
    active_values <- mapply(
      function(from, to) safe_named_matrix_value(active_result$cor_matrix, from, to, default = NA_real_),
      edges$from,
      edges$to,
      SIMPLIFY = TRUE
    )
    active_p_values <- mapply(
      function(from, to) safe_named_matrix_value(active_result$p_matrix, from, to, default = NA_real_),
      edges$from,
      edges$to,
      SIMPLIFY = TRUE
    )
    compare_values <- if (is.null(compare_result)) {
      rep(NA_real_, nrow(edges))
    } else {
      mapply(
        function(from, to) safe_named_matrix_value(compare_result$cor_matrix, from, to, default = NA_real_),
        edges$from,
        edges$to,
        SIMPLIFY = TRUE
      )
    }
    compare_p_values <- if (is.null(compare_result)) {
      rep(NA_real_, nrow(edges))
    } else {
      mapply(
        function(from, to) safe_named_matrix_value(compare_result$p_matrix, from, to, default = NA_real_),
        edges$from,
        edges$to,
        SIMPLIFY = TRUE
      )
    }
    active_display <- mapply(
      display_association_value,
      active_values,
      active_types,
      SIMPLIFY = TRUE
    )
    compare_display <- mapply(
      display_association_value,
      compare_values,
      compare_types,
      SIMPLIFY = TRUE
    )

    active_measure_labels <- vapply(active_types, display_measure_label, character(1))
    compare_measure_labels <- vapply(compare_types, display_measure_label, character(1))

    current_state_labels <- ifelse(
      active_present & compare_present,
      "Retained in both views",
      ifelse(
        active_present,
        paste0("Only in the ", current_view_label(), " view"),
        paste0("Only in the ", alternative_view_label(), " view")
      )
    )

    active_titles <- paste0(
      current_view_label(),
      ": ",
      ifelse(
        is.na(active_measure_labels),
        "not available",
        paste0(
          active_measure_labels,
          " = ",
          formatC(active_display, digits = 3, format = "f"),
          " | p = ",
          vapply(active_p_values, format_plot_p_value, character(1))
        )
      )
    )
    compare_titles <- if (is.null(compare_result)) {
      rep("", nrow(edges))
    } else {
      paste0(
        alternative_view_label(),
        ": ",
        ifelse(
          is.na(compare_measure_labels),
          "not available",
          paste0(
            compare_measure_labels,
            " = ",
            formatC(compare_display, digits = 3, format = "f"),
            " | p = ",
            vapply(compare_p_values, format_plot_p_value, character(1))
          )
        )
      )
    }

    edges$title <- paste0(
      "<b>",
      edges$from,
      " - ",
      edges$to,
      "</b><br>",
      "Status: ",
      current_state_labels,
      "<br>",
      active_titles,
      if (!is.null(compare_result)) paste0("<br>", compare_titles) else ""
    )

    min_len <- 100
    max_len <- 500
    edges$length <- (1 - strengths) * (max_len - min_len) + min_len

    visNetwork(nodes, edges, width = "100%", height = "900px") |>
      visNodes(
        color = list(
          background = "lightgray",
          border = "lightgray",
          highlight = list(border = "darkgray", background = "darkgray")
        )
      ) |>
      visEdges(smooth = FALSE) |>
      visPhysics(
        enabled = TRUE,
        stabilization = TRUE,
        solver = "forceAtlas2Based"
      ) |>
      visOptions(
        highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
        nodesIdSelection = FALSE,
        manipulation = FALSE
      ) |>
      visInteraction(
        zoomView = TRUE,
        dragView = FALSE,
        navigationButtons = FALSE
      ) |>
      visLayout(randomSeed = 123)
  })

  output$download_associations_csv <- downloadHandler(
    filename = function() {
      paste0(
        "associations_",
        current_view_label(),
        "_",
        format(Sys.Date(), "%Y%m%d"),
        ".csv"
      )
    },
    content = function(file) {
      export_df <- build_association_export_df(
        cor_result = cor_matrix_reactive(),
        data = selected_data_reactive(),
        descriptions_df = var_descriptions(),
        control_vars_selected = input$control_vars,
        controls_applied = current_view_uses_controls(),
        view_mode = current_view_label(),
        threshold_num = input$threshold_num,
        threshold_cat = input$threshold_cat,
        threshold_p = input$threshold_p
      )

      utils::write.csv(export_df, file, row.names = FALSE, na = "")
    }
  )

  output$pairs_plot <- renderUI({
    req(input$main_tabs == "pairs_tab")
    pairs <- significant_pairs()
    if (is.null(pairs) || nrow(pairs) == 0) {
      return(tags$p(
        "No variable pairs exceed the threshold to display bivariate plots. Please adjust the thresholds or select different variables.",
        style = "color: gray;"
      ))
    }
    df <- filtered_data_for_pairs()
    tab_ids <- paste0(pairs$var1, "__PAIR__", pairs$var2)

    # Create observers for reverse buttons OUTSIDE the renderUI
    isolate({
      for (i in seq_len(nrow(pairs))) {
        local({
          idx <- i
          plot_id <- paste0("plot_", idx)
          button_id <- paste0("reverse_", idx)

          # Only create observer if it doesn't exist
          if (is.null(reversed_axes[[paste0("obs_", button_id)]])) {
            observeEvent(
              input[[button_id]],
              {
                current_state <- reversed_axes[[plot_id]]
                reversed_axes[[plot_id]] <- if (is.null(current_state)) {
                  TRUE
                } else {
                  !current_state
                }
              },
              ignoreInit = TRUE
            )
            reversed_axes[[paste0("obs_", button_id)]] <- TRUE
          }
        })
      }
    })

    tabs <- lapply(seq_len(nrow(pairs)), function(i) {
      v1 <- pairs$var1[i]
      v2 <- pairs$var2[i]
      tab_id <- tab_ids[[i]]

      # Get the descriptions
      desc_lookup <- var_descriptions()
      desc1 <- resolve_variable_description(v1, desc_lookup)
      desc2 <- resolve_variable_description(v2, desc_lookup)

      plotname <- paste0("plot_", i)
      comparison_plotname <- paste0("plot_unconditional_", i)
      is_num1 <- is.numeric(df[[v1]])
      is_num2 <- is.numeric(df[[v2]])
      is_faded_pair <- isTRUE(pairs$faded[[i]])
      is_conditional_only_pair <- isTRUE(pairs$conditional_only[[i]])
      tab_title <- if (is_conditional_only_pair) {
        tags$span(class = "conditional-only-pair-tab", paste0(v1, " vs ", v2))
      } else if (is_faded_pair) {
        tags$span(class = "faded-pair-tab", paste0(v1, " vs ", v2))
      } else {
        paste0(v1, " vs ", v2)
      }
      conditional_note_ui <- if (has_controls() && is_conditional_only_pair) {
        tags$div(
          class = "pair-note-positive",
          "This association is retained in the conditional view but is not retained in the unconditional view under the current thresholds."
        )
      } else if (has_controls() && is_faded_pair) {
        tags$div(
          class = "pair-note-muted",
          "This association is retained in the unconditional view but is no longer retained after conditioning under the current thresholds."
        )
      } else {
        NULL
      }

      # Create a clean subset without NAs for these variables
      plot_data <- df %>%
        filter(!is.na(.data[[v1]]), !is.na(.data[[v2]]))

      # Numeric vs numeric case
      if (is_num1 && is_num2) {
        controls_exist <- has_controls()
        view_control_vars <- if (controls_exist) input$control_vars else NULL

        if (controls_exist) {
          # NEW: with controls : Added-variable plot (partial regression plot)
          output[[plotname]] <- renderPlot({
            # Force reactivity to reversed_axes changes
            force(reversed_axes[[plotname]])

            # NEW : get data from full dataset to access control variables
            full_df <- data_env$full_data()
            if (is.null(full_df)) {
              full_df <- data() # Fallback to current data if full data not available
            }

            # Create complete dataset with the pair variables AND control variables
            all_vars <- c(v1, v2, view_control_vars)

            # Check if all required columns exist in full data
            missing_cols <- setdiff(all_vars, names(full_df))
            if (length(missing_cols) > 0) {
              plot.new()
              text(
                0.5,
                0.5,
                paste(
                  "Missing columns in full data:",
                  paste(missing_cols, collapse = ", ")
                ),
                cex = 1.2,
                adj = 0.5
              )
              return()
            }

            # Get complete cases from FULL dataset
            complete_cases <- complete.cases(full_df[, all_vars])
            plot_data_full <- full_df[complete_cases, all_vars]

            if (nrow(plot_data_full) == 0) {
              plot.new()
              text(
                0.5,
                0.5,
                "No complete data available after controlling for variables",
                cex = 1.2,
                adj = 0.5
              )
              return()
            }

            tryCatch(
              {
                # Calculate residuals after controlling for other variables
                control_data <- plot_data_full[,
                  view_control_vars,
                  drop = FALSE
                ]

                resid_x <- partial_residuals(plot_data_full[[v1]], control_data)
                resid_y <- partial_residuals(plot_data_full[[v2]], control_data)

                # Calculate partial correlation
                partial_cor <- cor(resid_x, resid_y, use = "complete.obs")
                partial_r2_text <- format_plot_stat(partial_cor^2)
                n_eff <- length(resid_x)
                k_controls <- count_active_controls(control_data)
                p_val_text <- format_plot_p_value(
                  p_value_partial_cor(partial_cor, n_eff, k_controls)
                )

                # Check if axes should be reversed
                is_reversed <- if (is.null(reversed_axes[[plotname]])) {
                  FALSE
                } else {
                  reversed_axes[[plotname]]
                }

                # Determine which residuals to use for X and Y
                x_resid <- if (is_reversed) resid_y else resid_x
                y_resid <- if (is_reversed) resid_x else resid_y
                x_desc <- if (is_reversed) desc2 else desc1
                y_desc <- if (is_reversed) desc1 else desc2

                # Calculate regression slope for the residuals (respect reversed axes)
                if (is_reversed) {
                  lm_resid <- lm(resid_x ~ resid_y)
                } else {
                  lm_resid <- lm(resid_y ~ resid_x)
                }
                slope <- coef(lm_resid)[2]
                slope_text <- ifelse(is.na(slope), "NA", round(slope, 3))

                # Create added-variable plot with slope
                ggplot(
                  data.frame(x = x_resid, y = y_resid),
                  aes(x = x, y = y)
                ) +
                  geom_point(alpha = 0.6, color = "steelblue", size = 2) +
                  geom_smooth(
                    method = "lm",
                    se = TRUE,
                    color = "darkred",
                    linewidth = 1,
                    fill = "pink",
                    alpha = 0.2
                  ) +
                  labs(
                    x = paste0("Residuals of ", x_desc, " | controls"),
                    y = paste0("Residuals of ", y_desc, " | controls"),
                    title = "Added-Variable Plot (Partial Regression)",
                    subtitle = paste0(
                      "Partial R² = ",
                      partial_r2_text,
                      " | p-value = ",
                      p_val_text,
                      " | Slope = ",
                      slope_text,
                      "\nControls: ",
                      paste(view_control_vars, collapse = ", ")
                    )
                  ) +
                  theme_minimal(base_size = 14) +
                  theme(
                    plot.title = element_text(face = "bold"),
                    plot.subtitle = element_text(color = "gray40", size = 10),
                    plot.title.position = "plot"
                  )
              },
              error = function(e) {
                # Fallback to regular scatter plot if partial correlation fails
                current_cor <- if (nrow(plot_data) > 0) {
                  cor(plot_data[[v1]], plot_data[[v2]], use = "complete.obs")
                } else {
                  NA
                }
                r2_text <- format_plot_stat(current_cor^2)
                p_val_text <- format_plot_p_value(
                  p_value_partial_cor(current_cor, nrow(plot_data), 0)
                )

                ggplot(plot_data, aes(x = .data[[v1]], y = .data[[v2]])) +
                  geom_jitter(
                    alpha = 0.6,
                    color = "steelblue",
                    width = 0.5,
                    height = 0.5
                  ) +
                  geom_smooth(
                    method = "lm",
                    se = FALSE,
                    color = "darkred",
                    linewidth = 1
                  ) +
                  labs(
                    x = desc1,
                    y = desc2,
                    title = "Regular Scatter Plot (Partial Correlation Failed)",
                    subtitle = paste0(
                      "R² = ",
                      r2_text,
                      " | p-value = ",
                      p_val_text,
                      " | Error: ",
                      e$message
                    )
                  ) +
                  scale_x_continuous(
                    labels = label_number(big.mark = ",", decimal.mark = ".")
                  ) +
                  scale_y_continuous(
                    labels = label_number(big.mark = ",", decimal.mark = ".")
                  ) +
                  theme_minimal(base_size = 14) +
                  theme(
                    plot.title = element_text(face = "bold"),
                    plot.subtitle = element_text(color = "gray40", size = 10),
                    plot.title.position = "plot"
                  )
              }
            )
          })

          output[[comparison_plotname]] <- renderPlot({
            force(reversed_axes[[plotname]])

            if (nrow(plot_data) > 0) {
              is_reversed <- if (is.null(reversed_axes[[plotname]])) {
                FALSE
              } else {
                reversed_axes[[plotname]]
              }

              x_var <- if (is_reversed) v2 else v1
              y_var <- if (is_reversed) v1 else v2
              x_desc <- if (is_reversed) desc2 else desc1
              y_desc <- if (is_reversed) desc1 else desc2

              current_cor <- cor(
                plot_data[[v1]],
                plot_data[[v2]],
                use = "complete.obs"
              )
              r2_text <- format_plot_stat(current_cor^2)
              p_val_text <- format_plot_p_value(
                p_value_partial_cor(current_cor, nrow(plot_data), 0)
              )

              if (is_reversed) {
                lm_regular <- lm(plot_data[[v1]] ~ plot_data[[v2]])
              } else {
                lm_regular <- lm(plot_data[[v2]] ~ plot_data[[v1]])
              }
              slope_regular <- coef(lm_regular)[2]
              slope_text_regular <- ifelse(
                is.na(slope_regular),
                "NA",
                round(slope_regular, 3)
              )

              ggplot(plot_data, aes(x = .data[[x_var]], y = .data[[y_var]])) +
                geom_jitter(
                  alpha = 0.6,
                  color = "steelblue",
                  width = 0.5,
                  height = 0.5
                ) +
                geom_smooth(
                  method = "lm",
                  se = FALSE,
                  color = "darkred",
                  linewidth = 1
                ) +
                labs(
                  x = x_desc,
                  y = y_desc,
                  title = "Unconditional Scatter Plot",
                  subtitle = paste0(
                    "R² = ",
                    r2_text,
                    " | p-value = ",
                    p_val_text,
                    " | Slope = ",
                    slope_text_regular
                  )
                ) +
                scale_x_continuous(
                  labels = label_number(big.mark = ",", decimal.mark = ".")
                ) +
                scale_y_continuous(
                  labels = label_number(big.mark = ",", decimal.mark = ".")
                ) +
                theme_minimal(base_size = 14) +
                theme(
                  plot.title = element_text(face = "bold"),
                  plot.subtitle = element_text(color = "gray40", size = 10),
                  plot.title.position = "plot"
                )
            } else {
              plot.new()
              text(0.5, 0.5, "No valid data available", cex = 1.5, adj = 0.5)
            }
          })
        } else {
          # WITHOUT CONTROLS: Regular scatter plot
          output[[plotname]] <- renderPlot({
            # Force reactivity to reversed_axes changes
            force(reversed_axes[[plotname]])

            if (nrow(plot_data) > 0) {
              # Check if axes should be reversed
              is_reversed <- if (is.null(reversed_axes[[plotname]])) {
                FALSE
              } else {
                reversed_axes[[plotname]]
              }

              # Determine which variable is X and which is Y
              x_var <- if (is_reversed) v2 else v1
              y_var <- if (is_reversed) v1 else v2
              x_desc <- if (is_reversed) desc2 else desc1
              y_desc <- if (is_reversed) desc1 else desc2

              # Calculate correlation
              current_cor <- cor(
                plot_data[[v1]],
                plot_data[[v2]],
                use = "complete.obs"
              )
              r2_text <- format_plot_stat(current_cor^2)
              p_val_text <- format_plot_p_value(
                p_value_partial_cor(current_cor, nrow(plot_data), 0)
              )

              # Calculate slope (respect reversed axes)
              if (is_reversed) {
                lm_regular <- lm(plot_data[[v1]] ~ plot_data[[v2]])
              } else {
                lm_regular <- lm(plot_data[[v2]] ~ plot_data[[v1]])
              }
              slope_regular <- coef(lm_regular)[2]
              slope_text_regular <- ifelse(
                is.na(slope_regular),
                "NA",
                round(slope_regular, 3)
              )

              ggplot(plot_data, aes(x = .data[[x_var]], y = .data[[y_var]])) +
                geom_jitter(
                  alpha = 0.6,
                  color = "steelblue",
                  width = 0.5,
                  height = 0.5
                ) +
                geom_smooth(
                  method = "lm",
                  se = FALSE,
                  color = "darkred",
                  linewidth = 1
                ) +
                labs(
                  x = x_desc,
                  y = y_desc,
                  title = "Scatter Plot",
                  subtitle = paste0(
                    "R² = ",
                    r2_text,
                    " | p-value = ",
                    p_val_text,
                    " | Slope = ",
                    slope_text_regular
                  )
                ) +
                scale_x_continuous(
                  labels = label_number(big.mark = ",", decimal.mark = ".")
                ) +
                scale_y_continuous(
                  labels = label_number(big.mark = ",", decimal.mark = ".")
                ) +
                theme_minimal(base_size = 14) +
                theme(
                  plot.title = element_text(face = "bold"),
                  plot.subtitle = element_text(color = "gray40", size = 10),
                  plot.title.position = "plot"
                )
            } else {
              plot.new()
              text(0.5, 0.5, "No valid data available", cex = 1.5, adj = 0.5)
            }
          })
        }

        # NEW : add a "reverse axes" button in the pair plots window
        nav_panel(
          title = tab_title,
          value = tab_id,
          div(
            style = "position: relative;",
            conditional_note_ui,
            plotOutput(plotname, height = "600px"),
            if (controls_exist && isTRUE(show_unconditional_pair_plots())) {
              tagList(
                tags$hr(),
                tags$div(
                  style = "font-weight:600; margin: 10px 0 6px 0;",
                  "Unconditional comparison"
                ),
                plotOutput(comparison_plotname, height = "600px")
              )
            },
            # Button to reverse axes
            div(
              style = "position: absolute; top: 10px; right: 10px;",
              actionButton(
                inputId = paste0("reverse_", i),
                label = "↺ Reverse axes",
                class = "btn-sm btn-outline-primary"
              )
            )
          )
        )
      } else if (!is_num1 && !is_num2) {
        # Categorical vs categorical case:

        output[[plotname]] <- renderUI({
          if (nrow(plot_data) == 0) {
            return(div(
              "No valid data available",
              style = "padding: 20px; text-align: center;"
            ))
          }

          controls_exist <- has_controls()
          view_control_vars <- if (controls_exist) input$control_vars else NULL

          full_df <- data_env$full_data()
          if (is.null(full_df)) {
            full_df <- data()
          }

          all_vars <- c(
            v1,
            v2,
            if (controls_exist) view_control_vars else NULL
          )
          df_full <- full_df[, all_vars, drop = FALSE]
          df_full <- df_full[complete.cases(df_full), , drop = FALSE]

          if (nrow(df_full) == 0) {
            return(div(
              "No complete data available",
              style = "padding: 20px; text-align: center;"
            ))
          }

          cache_key <- make_pair_cache_key(
            v1,
            v2,
            if (controls_exist) view_control_vars else NULL
          )
          assoc_res <- get_cached_pair_result(pair_cache, cache_key)

          if (controls_exist) {
            if (is.null(assoc_res)) {
              assoc_res <- compute_conditional(
                x_vec = df_full[[v1]],
                y_vec = df_full[[v2]],
                Zdf = df_full[, view_control_vars, drop = FALSE]
              )
              assoc_res <- set_cached_pair_result(pair_cache, cache_key, assoc_res)
            }
            vl_value <- assoc_res[["VL_Z"]]
            assoc_title <- "Conditional categorical association"
          } else {
            if (is.null(assoc_res)) {
              assoc_res <- compute_unconditional(
                x_vec = df_full[[v1]],
                y_vec = df_full[[v2]]
              )
              assoc_res <- set_cached_pair_result(pair_cache, cache_key, assoc_res)
            }
            vl_value <- assoc_res$VL
            assoc_title <- "Unconditional categorical association"
          }

          O <- assoc_res$O
          E0 <- assoc_res$E0
          D <- assoc_res$D
          R <- assoc_res$R

          validate(
            need(
              !is.null(D) && !is.null(R),
              "Could not compute local association table."
            )
          )

          display_score_matrix <- compute_catcat_display_scores(O, E0)
          submatrix_selection <- select_catcat_display_submatrix(
            display_score_matrix,
            max_dim = 7L
          )

          D_display <- D[submatrix_selection$rows, submatrix_selection$cols, drop = FALSE]
          O_display <- O[submatrix_selection$rows, submatrix_selection$cols, drop = FALSE]
          R_display <- R[submatrix_selection$rows, submatrix_selection$cols, drop = FALSE]
          score_display <- display_score_matrix[
            submatrix_selection$rows,
            submatrix_selection$cols,
            drop = FALSE
          ]

          display_info_ui <- NULL
          if (isTRUE(submatrix_selection$reduced)) {
            total_score <- sum(display_score_matrix, na.rm = TRUE)
            selected_score <- sum(score_display, na.rm = TRUE)
            coverage_pct <- if (total_score > 0) {
              100 * selected_score / total_score
            } else {
              NA_real_
            }
            fallback_reason_text <- submatrix_selection$fallback_reason
            if (is.null(fallback_reason_text) || is.na(fallback_reason_text)) {
              fallback_reason_text <- ""
            }
            selection_reason <- if (
              identical(submatrix_selection$method, "heuristic") &&
              nzchar(fallback_reason_text)
            ) {
              paste0(
                " Heuristic fallback used because ",
                fallback_reason_text,
                "."
              )
            } else if (identical(submatrix_selection$method, "optimal")) {
              " Exact binary optimization was used."
            } else {
              ""
            }

            display_info_ui <- tags$p(
              style = "font-size:0.85em; color:#666;",
              paste0(
                "Large table detected (",
                nrow(D),
                "x",
                ncol(D),
                " = ",
                nrow(D) * ncol(D),
                " cells). Showing the best ",
                nrow(D_display),
                "x",
                ncol(D_display),
                " submatrix selected from squared Pearson-residual scores",
                if (is.finite(coverage_pct)) {
                  paste0(
                    " (",
                    round(coverage_pct, 1),
                    "% of total score)."
                  )
                } else {
                  "."
                },
                selection_reason
              )
            )
          }

          # Build table for display: values = O, color = R
          display_df <- as.data.frame.matrix(O_display)
          display_df <- tibble::rownames_to_column(display_df, var = v1)

          # Range for residual coloring
          max_abs_r <- max(abs(R_display), na.rm = TRUE)
          if (!is.finite(max_abs_r) || max_abs_r == 0) {
            max_abs_r <- 1
          }

          column_defs <- lapply(seq_along(display_df), function(j) {
            colname <- names(display_df)[j]

            if (colname == v1) {
              colDef(
                name = paste0(desc1, " (row levels)"),
                minWidth = 160
              )
            } else {
              colDef(
                name = colname,
                align = "center",
                cell = function(value, index) {
                  row_name <- display_df[[v1]][index]
                  o_val <- O_display[row_name, colname]
                  r_val <- R_display[row_name, colname]

                  if (is.na(r_val) || is.na(o_val)) {
                    return(div(
                      style = "background-color:#f8f9fa; padding:4px; min-height:1.6em;",
                      ""
                    ))
                  }

                  # intensity from |R|
                  intensity <- min(1, abs(r_val) / max_abs_r)

                  # red if positive, blue if negative
                  bg_col <- if (r_val >= 0) {
                    rgb(1, 1 - intensity, 1 - intensity)
                  } else {
                    rgb(1 - intensity, 1 - intensity, 1)
                  }

                  div(
                    style = paste0(
                      "background-color:",
                      bg_col,
                      "; padding:4px; min-height:1.6em; font-weight:500;"
                    ),
                    format(as.integer(round(as.numeric(o_val))), big.mark = ",", trim = TRUE)
                  )
                }
              )
            }
          })
          names(column_defs) <- names(display_df)

          column_groups <- list(
            colGroup(
              name = desc2,
              columns = setdiff(names(display_df), v1)
            )
          )

          tagList(
            h4(assoc_title),
            tags$p(
              style = "font-size:0.9em; color:#666;",
              paste0(
                if (controls_exist) "VL|Z = " else "VL = ",
                format_plot_stat(vl_value),
                " | p-value = ",
                format_plot_p_value(assoc_res$p_value)
              )
            ),
            tags$p(
              style = "font-size:0.85em; color:#666;",
              paste0("Rows: ", desc1, " | Columns: ", desc2)
            ),
            tags$p(
              style = "font-size:0.85em; color:#666;",
              "Cell values show observed counts O; colors show Pearson residuals R: red = over-represented, blue = under-represented, darker = stronger."
            ),
            display_info_ui,
            make_table(display_df, column_defs, column_groups = column_groups)
          )
        })

        if (has_controls()) {
          output[[comparison_plotname]] <- renderUI({
            if (nrow(plot_data) == 0) {
              return(div(
                "No valid data available",
                style = "padding: 20px; text-align: center;"
              ))
            }

            full_df <- data_env$full_data()
            if (is.null(full_df)) {
              full_df <- data()
            }

            df_full <- full_df[, c(v1, v2), drop = FALSE]
            df_full <- df_full[complete.cases(df_full), , drop = FALSE]

            if (nrow(df_full) == 0) {
              return(div(
                "No complete data available",
                style = "padding: 20px; text-align: center;"
              ))
            }

            cache_key <- make_pair_cache_key(v1, v2, NULL)
            assoc_res <- get_cached_pair_result(pair_cache, cache_key)

            if (is.null(assoc_res)) {
              assoc_res <- compute_unconditional(
                x_vec = df_full[[v1]],
                y_vec = df_full[[v2]]
              )
              assoc_res <- set_cached_pair_result(pair_cache, cache_key, assoc_res)
            }

            O <- assoc_res$O
            E0 <- assoc_res$E0
            D <- assoc_res$D
            R <- assoc_res$R

            validate(
              need(
                !is.null(D) && !is.null(R),
                "Could not compute local association table."
              )
            )

            display_score_matrix <- compute_catcat_display_scores(O, E0)
            submatrix_selection <- select_catcat_display_submatrix(
              display_score_matrix,
              max_dim = 7L
            )

            D_display <- D[submatrix_selection$rows, submatrix_selection$cols, drop = FALSE]
            O_display <- O[submatrix_selection$rows, submatrix_selection$cols, drop = FALSE]
            R_display <- R[submatrix_selection$rows, submatrix_selection$cols, drop = FALSE]
            score_display <- display_score_matrix[
              submatrix_selection$rows,
              submatrix_selection$cols,
              drop = FALSE
            ]

            display_info_ui <- NULL
            if (isTRUE(submatrix_selection$reduced)) {
              total_score <- sum(display_score_matrix, na.rm = TRUE)
              selected_score <- sum(score_display, na.rm = TRUE)
              coverage_pct <- if (total_score > 0) {
                100 * selected_score / total_score
              } else {
                NA_real_
              }
              fallback_reason_text <- submatrix_selection$fallback_reason
              if (is.null(fallback_reason_text) || is.na(fallback_reason_text)) {
                fallback_reason_text <- ""
              }
              selection_reason <- if (
                identical(submatrix_selection$method, "heuristic") &&
                nzchar(fallback_reason_text)
              ) {
                paste0(
                  " Heuristic fallback used because ",
                  fallback_reason_text,
                  "."
                )
              } else if (identical(submatrix_selection$method, "optimal")) {
                " Exact binary optimization was used."
              } else {
                ""
              }

              display_info_ui <- tags$p(
                style = "font-size:0.85em; color:#666;",
                paste0(
                  "Large table detected (",
                  nrow(D),
                  "x",
                  ncol(D),
                  " = ",
                  nrow(D) * ncol(D),
                  " cells). Showing the best ",
                  nrow(D_display),
                  "x",
                  ncol(D_display),
                  " submatrix selected from squared Pearson-residual scores",
                  if (is.finite(coverage_pct)) {
                    paste0(
                      " (",
                      round(coverage_pct, 1),
                      "% of total score)."
                    )
                  } else {
                    "."
                  },
                  selection_reason
                )
              )
            }

            display_df <- as.data.frame.matrix(O_display)
            display_df <- tibble::rownames_to_column(display_df, var = v1)

            max_abs_r <- max(abs(R_display), na.rm = TRUE)
            if (!is.finite(max_abs_r) || max_abs_r == 0) {
              max_abs_r <- 1
            }

            column_defs <- lapply(seq_along(display_df), function(j) {
              colname <- names(display_df)[j]

              if (colname == v1) {
                colDef(
                  name = paste0(desc1, " (row levels)"),
                  minWidth = 160
                )
              } else {
                colDef(
                  name = colname,
                  align = "center",
                  cell = function(value, index) {
                    row_name <- display_df[[v1]][index]
                    o_val <- O_display[row_name, colname]
                    r_val <- R_display[row_name, colname]

                    if (is.na(r_val) || is.na(o_val)) {
                      return(div(
                        style = "background-color:#f8f9fa; padding:4px; min-height:1.6em;",
                        ""
                      ))
                    }

                    intensity <- min(1, abs(r_val) / max_abs_r)
                    bg_col <- if (r_val >= 0) {
                      rgb(1, 1 - intensity, 1 - intensity)
                    } else {
                      rgb(1 - intensity, 1 - intensity, 1)
                    }

                    div(
                      style = paste0(
                        "background-color:",
                        bg_col,
                        "; padding:4px; min-height:1.6em; font-weight:500;"
                      ),
                      format(as.integer(round(as.numeric(o_val))), big.mark = ",", trim = TRUE)
                    )
                  }
                )
              }
            })
            names(column_defs) <- names(display_df)

            column_groups <- list(
              colGroup(
                name = desc2,
                columns = setdiff(names(display_df), v1)
              )
            )

            tagList(
              h4("Unconditional categorical association"),
              tags$p(
                style = "font-size:0.9em; color:#666;",
                paste0(
                  "VL = ",
                  format_plot_stat(assoc_res$VL),
                  " | p-value = ",
                  format_plot_p_value(assoc_res$p_value)
                )
              ),
              tags$p(
                style = "font-size:0.85em; color:#666;",
                paste0("Rows: ", desc1, " | Columns: ", desc2)
              ),
              tags$p(
                style = "font-size:0.85em; color:#666;",
                "Cell values show observed counts O; colors show Pearson residuals R: red = over-represented, blue = under-represented, darker = stronger."
              ),
              display_info_ui,
              make_table(display_df, column_defs, column_groups = column_groups)
            )
          })
        }

        nav_panel(
          title = tab_title,
          value = tab_id,
          tagList(
            conditional_note_ui,
            uiOutput(plotname),
            if (has_controls() && isTRUE(show_unconditional_pair_plots())) {
              tagList(
                tags$hr(),
                tags$div(
                  style = "font-weight:600; margin: 10px 0 6px 0;",
                  "Unconditional comparison"
                ),
                uiOutput(comparison_plotname)
              )
            }
          )
        )
      } else {
        # Mixed case (numeric vs categorical)
        if (is_num1) {
          num_var <- v1
          cat_var <- v2
          desc_num <- desc1
          desc_cat <- desc2
        } else {
          num_var <- v2
          cat_var <- v1
          desc_num <- desc2
          desc_cat <- desc1
        }

        output[[plotname]] <- renderPlot({
          if (nrow(plot_data) == 0) {
            plot.new()
            text(0.5, 0.5, "No valid data available", cex = 1.5, adj = 0.5)
            return()
          }

          controls_exist <- has_controls()
          view_control_vars <- if (controls_exist) input$control_vars else NULL

          # If no controls: keep your original unconditional means plot
          if (!controls_exist) {
            df_sum <- plot_data |>
              group_by(.data[[cat_var]]) |>
              summarise(
                mean_val = mean(.data[[num_var]], na.rm = TRUE),
                .groups = "drop"
              ) |>
              arrange(mean_val) |>
              mutate(
                {{ cat_var }} := factor(
                  .data[[cat_var]],
                  levels = .data[[cat_var]]
                )
              )

            res_eta <- calculate_partial_eta_squared_with_F(
              num_var = plot_data[[num_var]],
              cat_var = plot_data[[cat_var]],
              control_data = NULL
            )
            assoc_text <- format_plot_stat(res_eta$eta_sq)
            p_val_text <- format_plot_p_value(res_eta$p_value)

            ggplot(df_sum, aes(x = .data[[cat_var]], y = mean_val)) +
              geom_col(fill = "steelblue", width = 0.6) +
              geom_text(
                aes(
                  label = format(
                    round(mean_val, 2),
                    big.mark = ",",
                    decimal.mark = "."
                  )
                ),
                hjust = 1.1,
                color = "white",
                size = 4
              ) +
              labs(
                x = desc_cat,
                y = paste0('Mean of "', desc_num, '"'),
                title = "Group Means Plot",
                subtitle = paste0(
                  "Eta² = ",
                  assoc_text,
                  " | p-value = ",
                  p_val_text
                )
              ) +
              scale_y_continuous(
                labels = label_number(big.mark = ",", decimal.mark = ".")
              ) +
              theme_minimal(base_size = 14) +
              theme(
                plot.title = element_text(face = "bold"),
                plot.subtitle = element_text(color = "gray40", size = 10),
                plot.title.position = "plot"
              ) +
              coord_flip()
          } else {
            #  ===== CONDITIONAL CASE: RESIDUALIZED MEANS =====
            # Use full data to access controls
            full_df <- data_env$full_data()
            if (is.null(full_df)) {
              full_df <- data()
            }

            all_vars <- c(num_var, cat_var, view_control_vars)

            # Check that all needed columns exist
            missing_cols <- setdiff(all_vars, names(full_df))
            if (length(missing_cols) > 0) {
              plot.new()
              text(
                0.5,
                0.5,
                paste(
                  "Missing columns in full data:",
                  paste(missing_cols, collapse = ", ")
                ),
                cex = 1.2,
                adj = 0.5
              )
              return()
            }

            # Build data frame with numeric, categorical and controls
            df_full <- data.frame(
              num_var = full_df[[num_var]],
              cat_var = as.factor(full_df[[cat_var]]),
              full_df[, view_control_vars, drop = FALSE]
            )

            df_full <- stats::na.omit(df_full)

            if (nrow(df_full) == 0) {
              plot.new()
              text(
                0.5,
                0.5,
                "No complete data available after including controls",
                cex = 1.2,
                adj = 0.5
              )
              return()
            }

            # Keep only controls with variation (same logic as in calculate_partial_eta_squared_with_F)
            all_names <- names(df_full)
            response_name <- "num_var"
            cat_name <- "cat_var"
            control_names <- setdiff(all_names, c(response_name, cat_name))

            vars_nonresp <- c(cat_name, control_names)
            has_variation <- sapply(
              df_full[, vars_nonresp, drop = FALSE],
              function(z) {
                if (is.factor(z)) {
                  used_levels <- unique(z[!is.na(z)])
                  length(used_levels) > 1 && length(unique(z[!is.na(z)])) > 1
                } else {
                  length(unique(z[!is.na(z)])) > 1
                }
              }
            )

            controls_kept <- control_names[has_variation[control_names]]

            # Rebuild df_full with only useful controls
            df_full <- df_full[,
              c(response_name, cat_name, controls_kept),
              drop = FALSE
            ]

            # If numeric has no variance, nothing to display
            if (var(df_full[[response_name]]) == 0) {
              plot.new()
              text(
                0.5,
                0.5,
                "No variance in numeric variable after filtering",
                cex = 1.2,
                adj = 0.5
              )
              return()
            }

            res_eta <- calculate_partial_eta_squared_with_F(
              num_var = df_full[[response_name]],
              cat_var = df_full[[cat_name]],
              control_data = if (length(controls_kept) > 0) {
                df_full[, controls_kept, drop = FALSE]
              } else {
                NULL
              }
            )
            assoc_text <- format_plot_stat(res_eta$eta_sq)
            p_val_text <- format_plot_p_value(res_eta$p_value)

            # 1) Residualize Y on controls only: num_var ~ controls_kept
            formula_ctrl <- if (length(controls_kept) > 0) {
              as.formula(paste(
                "num_var ~",
                paste(controls_kept, collapse = " + ")
              ))
            } else {
              as.formula("num_var ~ 1")
            }

            fit_ctrl <- try(lm(formula_ctrl, data = df_full), silent = TRUE)

            if (inherits(fit_ctrl, "try-error")) {
              # Fallback to unconditional means if residualization fails
              df_sum <- plot_data |>
                group_by(.data[[cat_var]]) |>
                summarise(
                  mean_val = mean(.data[[num_var]], na.rm = TRUE),
                  .groups = "drop"
                ) |>
                arrange(mean_val) |>
                mutate(
                  {{ cat_var }} := factor(
                    .data[[cat_var]],
                    levels = .data[[cat_var]]
                  )
                )

              ggplot(df_sum, aes(x = .data[[cat_var]], y = mean_val)) +
                geom_col(fill = "steelblue", width = 0.6) +
                geom_text(
                  aes(
                    label = format(
                      round(mean_val, 2),
                      big.mark = ",",
                      decimal.mark = "."
                    )
                  ),
                  hjust = 1.1,
                  color = "white",
                  size = 4
                ) +
                labs(
                  x = desc_cat,
                  y = paste0(
                    'Mean of "',
                    desc_num,
                    '" (unadjusted; residualization failed)'
                  ),
                  title = "Group Means Plot (Fallback)",
                  subtitle = paste0(
                    "Partial Eta² = ",
                    assoc_text,
                    " | p-value = ",
                    p_val_text,
                    " | Residualization failed"
                  )
                ) +
                scale_y_continuous(
                  labels = label_number(big.mark = ",", decimal.mark = ".")
                ) +
                theme_minimal(base_size = 14) +
                theme(
                  plot.title = element_text(face = "bold"),
                  plot.subtitle = element_text(color = "gray40", size = 10),
                  plot.title.position = "plot"
                ) +
                coord_flip()
            } else {
              # 2) Compute residuals and then group means of residuals
              df_full$y_resid <- residuals(fit_ctrl)

              df_res <- df_full |>
                group_by(cat_var) |>
                summarise(
                  resid_mean = mean(y_resid, na.rm = TRUE),
                  .groups = "drop"
                ) |>
                arrange(resid_mean) |>
                mutate(cat_var = factor(cat_var, levels = cat_var))

              ggplot(df_res, aes(x = cat_var, y = resid_mean)) +
                geom_col(fill = "steelblue", width = 0.6) +
                geom_text(
                  aes(
                    label = format(
                      round(resid_mean, 2),
                      big.mark = ",",
                      decimal.mark = "."
                    )
                  ),
                  hjust = 1.1,
                  color = "white",
                  size = 4
                ) +
                geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.6) +
                labs(
                  x = desc_cat,
                  y = paste0(
                    'Residualized mean of "',
                    desc_num,
                    '" (after removing controls)'
                  ),
                  title = "Residualized Group Means Plot",
                  subtitle = paste0(
                    "Partial Eta² = ",
                    assoc_text,
                    " | p-value = ",
                    p_val_text,
                    "\nResidualized on: ",
                    if (length(controls_kept) > 0) {
                      paste(controls_kept, collapse = ", ")
                    } else {
                      "none"
                    }
                  )
                ) +
                scale_y_continuous(
                  labels = label_number(big.mark = ",", decimal.mark = ".")
                ) +
                theme_minimal(base_size = 14) +
                theme(
                  plot.title = element_text(face = "bold"),
                  plot.subtitle = element_text(color = "gray40", size = 10),
                  plot.title.position = "plot"
                ) +
                coord_flip()
            }
          }
        })

        if (has_controls()) {
          output[[comparison_plotname]] <- renderPlot({
            if (nrow(plot_data) == 0) {
              plot.new()
              text(0.5, 0.5, "No valid data available", cex = 1.5, adj = 0.5)
              return()
            }

            df_sum <- plot_data |>
              group_by(.data[[cat_var]]) |>
              summarise(
                mean_val = mean(.data[[num_var]], na.rm = TRUE),
                .groups = "drop"
              ) |>
              arrange(mean_val) |>
              mutate(
                {{ cat_var }} := factor(
                  .data[[cat_var]],
                  levels = .data[[cat_var]]
                )
              )

            res_eta <- calculate_partial_eta_squared_with_F(
              num_var = plot_data[[num_var]],
              cat_var = plot_data[[cat_var]],
              control_data = NULL
            )
            assoc_text <- format_plot_stat(res_eta$eta_sq)
            p_val_text <- format_plot_p_value(res_eta$p_value)

            ggplot(df_sum, aes(x = .data[[cat_var]], y = mean_val)) +
              geom_col(fill = "steelblue", width = 0.6) +
              geom_text(
                aes(
                  label = format(
                    round(mean_val, 2),
                    big.mark = ",",
                    decimal.mark = "."
                  )
                ),
                hjust = 1.1,
                color = "white",
                size = 4
              ) +
              labs(
                x = desc_cat,
                y = paste0('Mean of "', desc_num, '"'),
                title = "Unconditional Group Means Plot",
                subtitle = paste0(
                  "Eta² = ",
                  assoc_text,
                  " | p-value = ",
                  p_val_text
                )
              ) +
              scale_y_continuous(
                labels = label_number(big.mark = ",", decimal.mark = ".")
              ) +
              theme_minimal(base_size = 14) +
              theme(
                plot.title = element_text(face = "bold"),
                plot.subtitle = element_text(color = "gray40", size = 10),
                plot.title.position = "plot"
              ) +
              coord_flip()
          })
        }

        nav_panel(
          title = tab_title,
          value = tab_id,
          tagList(
            conditional_note_ui,
            plotOutput(plotname, height = "600px"),
            if (has_controls() && isTRUE(show_unconditional_pair_plots())) {
              tagList(
                tags$hr(),
                tags$div(
                  style = "font-weight:600; margin: 10px 0 6px 0;",
                  "Unconditional comparison"
                ),
                plotOutput(comparison_plotname, height = "600px")
              )
            }
          )
        )
      }
    })

    selected_tab <- active_pair_plot_tab()
    if (is.null(selected_tab) || !(selected_tab %in% tab_ids)) {
      selected_tab <- tab_ids[[1]]
    }

    tagList(
      navset_card_tab(
        id = "bivariate_tabs",
        selected = selected_tab,
        !!!tabs
      )
    )
  })

}

shinyApp(ui, server)
