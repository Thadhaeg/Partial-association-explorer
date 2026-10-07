# =============================================================================
# Core association engine and shared presentation helpers
# =============================================================================
#
# This file deliberately contains no Shiny reactive state. It can therefore be
# sourced by the application and by the automated tests without launching a
# Shiny session.
#
# Data conventions used throughout the file:
# - numeric R columns are treated as numerical variables; all other columns are
#   treated as categorical variables;
# - non-categorical entries in `cor_matrix` store |r| or sqrt(eta^2), so the
#   shared filtering and export helpers square them before displaying R^2 or
#   eta^2;
# - categorical entries store V_L (or V_L|Z) directly and are not squared;
# - pairwise routines use complete cases for the two variables and any selected
#   controls;
# - matrices are symmetric and carry variable names on both dimensions.
#
# The file is organized as follows:
# 1. Shared display and metadata helpers
# 2. Numerical association helpers
# 3. Categorical model preparation and large-table selection
# 4. Structured multinomial model fitting
# 5. Result, filtering, and matrix utilities
# 6. Categorical pair analyses
# 7. Pair caching and full association-matrix assembly


# =============================================================================
# Shared display and metadata helpers
# =============================================================================

# Build a consistently styled reactable table for app outputs.
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

# Return a human-readable description, falling back to the variable name.
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

# Format a scalar statistic for plot subtitles and network tooltips.
format_plot_stat <- function(x, digits = 3) {
  if (is.null(x) || length(x) == 0 || !is.finite(x[[1]])) {
    return("NA")
  }

  formatC(as.numeric(x[[1]]), digits = digits, format = "f")
}

# Format a scalar p-value while retaining a clear threshold below 0.001.
format_plot_p_value <- function(p_value) {
  if (is.null(p_value) || length(p_value) == 0 || is.na(p_value[[1]])) {
    return("NA")
  }

  if (p_value[[1]] < 0.001) {
    return("< 0.001")
  }

  formatC(signif(as.numeric(p_value[[1]]), 3), digits = 3, format = "fg", flag = "#")
}

# Convert an internally stored association value to its displayed scale.
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

# Map internal association-type labels to labels shown to users.
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

# Collapse variable names and descriptions into one export/context string.
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

# Describe whether the selected controls are active in the current view.
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

# =============================================================================
# Numerical association helpers
# =============================================================================

# Residualize a numerical variable on controls that contain usable variation.
partial_residuals <- function(y, controls_df) {
  if (is.null(controls_df) || ncol(controls_df) == 0) {
    return(y)
  }

  # Constant controls provide no information and can make model fits singular.
  keep <- sapply(controls_df, function(z) length(unique(z[!is.na(z)])) > 1)
  controls_clean <- controls_df[, keep, drop = FALSE]

  if (ncol(controls_clean) == 0) {
    return(y)
  }

  dfm <- data.frame(y = y, controls_clean)
  residuals(lm(y ~ ., data = dfm))
}

# Count the controls retained by partial_residuals() for the test degrees of
# freedom.
count_active_controls <- function(controls_df) {
  if (is.null(controls_df) || ncol(controls_df) == 0) {
    return(0L)
  }
  sum(sapply(controls_df, function(z) length(unique(z[!is.na(z)])) > 1))
}

# Compute eta-squared (or partial eta-squared) and its extra-sum-of-squares
# F-test for a numerical-categorical pair.
calculate_partial_eta_squared_with_F <- function(
  num_var,
  cat_var,
  control_data = NULL
) {
  empty_result <- function(df1 = 0, df2 = 0) {
    list(
      eta = 0,
      eta_sq = 0,
      F = NA_real_,
      df1 = df1,
      df2 = df2,
      p_value = NA_real_,
      type = "sqrt(Partial Eta²)"
    )
  }

  # Build a common modeling frame for marginal and adjusted analyses.
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
      return(empty_result())
    }
    df_temp <- data.frame(
      num_var = num_var,
      cat_var = as.factor(cat_var),
      control_data
    )
  }

  df_temp <- stats::na.omit(df_temp)

  # No estimate is available without a complete observation.
  if (nrow(df_temp) == 0) {
    return(empty_result())
  }

  # Use stable internal names when constructing the nested formulas.
  all_names <- names(df_temp)
  response_name <- "num_var"
  cat_name <- "cat_var"
  control_names <- setdiff(all_names, c(response_name, cat_name))

  # Identify the categorical predictor and controls with usable variation.
  vars_nonresp <- c(cat_name, control_names)

  has_variation <- sapply(df_temp[, vars_nonresp, drop = FALSE], function(z) {
    if (is.factor(z)) {
      used_levels <- unique(z[!is.na(z)])
      length(used_levels) > 1 && length(unique(z[!is.na(z)])) > 1
    } else {
      length(unique(z[!is.na(z)])) > 1
    }
  })

  # A single-level categorical predictor has no group effect to test.
  if (!isTRUE(has_variation[cat_name])) {
    return(empty_result())
  }

  # Constant controls add no information and can make model fits singular.
  controls_kept <- control_names[has_variation[control_names]]

  # Retain the response, factor, and usable controls only.
  df_temp <- df_temp[,
    c(response_name, cat_name, controls_kept),
    drop = FALSE
  ]

  # A constant numerical outcome has no variation to explain.
  if (var(df_temp[[response_name]]) == 0) {
    return(empty_result())
  }

  # Compare the full factor-plus-controls model with the controls-only model.
  fit_res <- try(
    {
      model_full <- lm(num_var ~ ., data = df_temp)

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
    return(empty_result())
  }

  model_full <- fit_res$full
  model_reduced <- fit_res$reduced

  ss_res_full <- sum(residuals(model_full)^2)
  ss_res_reduced <- sum(residuals(model_reduced)^2)
  ss_effect <- ss_res_reduced - ss_res_full

  # The factor contributes m - 1 numerator degrees of freedom.
  m <- nlevels(df_temp[[cat_name]])
  q <- m - 1
  df2 <- df.residual(model_full)

  if (ss_effect <= 0 || ss_res_full <= 0 || q <= 0 || df2 <= 0) {
    return(empty_result(df1 = q, df2 = df2))
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

# Compute the two-sided p-value for a Pearson or partial correlation.
p_value_partial_cor <- function(r, n_eff, k_controls) {
  if (is.na(r)) {
    return(NA_real_)
  }
  if (abs(r) >= 1) {
    return(0)
  }

  df <- n_eff - k_controls - 2
  if (df <= 0) {
    return(NA_real_)
  }

  t_stat <- r * sqrt(df / (1 - r^2))
  F_stat <- t_stat^2
  p_val <- 1 - pf(F_stat, 1, df)
  p_val
}

# =============================================================================
# Categorical model preparation and large-table selection
# =============================================================================

# Encode numerical and categorical controls as a model matrix without an
# intercept; intercept-like category effects are handled explicitly later.
make_Z_design <- function(Zdf) {
  Zmm <- stats::model.matrix(~., data = Zdf)
  Zmm <- Zmm[, colnames(Zmm) != "(Intercept)", drop = FALSE]
  as.data.frame(Zmm)
}

# Apply a numerically stable row-wise softmax to an n-by-K predictor matrix.
softmax_rows <- function(eta) {
  m <- apply(eta, 1, max)
  ex <- exp(eta - m)
  ex / rowSums(ex)
}

# Map each joint outcome label W = (X_i, Y_j) back to its row and column.
parse_W_levels <- function(W_levels, sep, x_levels, y_levels) {
  parts <- strsplit(W_levels, split = sep, fixed = TRUE)
  wx <- vapply(parts, `[[`, "", 1)
  wy <- vapply(parts, `[[`, "", 2)

  if (any(!wx %in% x_levels) || any(!wy %in% y_levels)) {
    stop(
      "Some W levels cannot be mapped back to x_levels/y_levels. Check sep and factor labels."
    )
  }

  i_idx <- match(wx, x_levels)
  j_idx <- match(wy, y_levels)
  list(wx = wx, wy = wy, i = i_idx, j = j_idx)
}

# Keep these definitions conditional so a caller can inject an alternative
# large-table selector before sourcing this file.
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

    if (!requireNamespace("lpSolve", quietly = TRUE)) {
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

# Collapse identical control-design rows into grouped outcome counts. This is a
# computational optimization; it does not change the likelihood.
group_catcat_observations <- function(Zmm, y_idx_local, K) {
  if (is.null(Zmm) || ncol(Zmm) == 0) {
    counts <- matrix(
      as.numeric(tabulate(y_idx_local, nbins = K)),
      nrow = 1,
      ncol = K
    )

    return(list(
      counts = counts,
      weights = rowSums(counts),
      Z_group = NULL
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
    Z_group = Z_group
  )
}

# Prepare complete cases, the joint outcome grid, control groups, and indicator
# matrices shared by the categorical null and alternative fits.
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
      y_fac = y_fac
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
    x_levels = x_levels,
    y_levels = y_levels,
    I = I,
    J = J,
    K = K,
    q = q,
    n_obs = length(y_idx_local),
    O = O,
    mapW = mapW,
    Z_group = grouped$Z_group,
    counts = grouped$counts,
    weights = grouped$weights,
    x_indicator = x_indicator,
    y_indicator = y_indicator
  )
}

# =============================================================================
# Structured multinomial model fitting
# =============================================================================
# The first X level and first Y level are reference categories. Their alpha,
# beta, lambda, and kappa terms are fixed at zero; gamma is also zero throughout
# the first row and first column. The alternative adds the remaining gamma
# interaction block to the conditional-independence null model.

# Describe the contiguous parameter-vector blocks for either nested model.
make_param_index <- function(I, J, q, include_gamma = TRUE) {
  # Free blocks: alpha (I - 1), beta (J - 1), optional gamma
  # ((I - 1)(J - 1)), lambda ((I - 1)q), and kappa ((J - 1)q).
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

# Expand the free parameter vector into full corner-constrained arrays.
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

  # Expand into full arrays with reference-category coefficients fixed at zero.
  alpha <- c(0, alpha_free) # length I  (assumes ref_x is first level)
  beta <- c(0, beta_free) # length J  (assumes ref_y is first level)

  gamma <- matrix(0, nrow = I, ncol = J)
  if (include_gamma) {
    # Fill only the non-reference rows and columns.
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

# Insert a zero-valued interaction block when warm-starting the alternative
# model from the fitted null model.
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

# Compute the linear predictor for every grouped row and joint outcome.
compute_eta <- function(pars, mapW, Zmm = NULL, n_rows = NULL) {
  K <- length(mapW$i)
  q <- if (is.null(Zmm)) 0L else ncol(Zmm)
  if (is.null(n_rows)) {
    n_rows <- if (q > 0) nrow(Zmm) else 1L
  }

  # Add the category-specific intercept component.
  base_cat <- pars$alpha[mapW$i] +
    pars$beta[mapW$j] +
    pars$gamma[cbind(mapW$i, mapW$j)]
  eta <- matrix(base_cat, nrow = n_rows, ncol = K, byrow = TRUE)

  # Add the control-dependent component when controls are present.
  if (q > 0) {
    slope_mat <- pars$lambda[mapW$i, , drop = FALSE] +
      pars$kappa[mapW$j, , drop = FALSE]
    eta <- eta + Zmm %*% t(slope_mat)
  }

  eta
}

# Evaluate the negative log-likelihood, analytic gradient, fitted
# probabilities, and expected counts at one parameter vector.
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
    params = pars,
    logLik = -nll,
    expected_counts = expected_counts
  )
}

# Fit one prepared null or alternative model with BFGS. The small evaluation
# cache avoids recomputing the likelihood when optim() asks for the objective
# and gradient at the same parameter vector.
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
    params = fit_eval$params,
    logLik = fit_eval$logLik,
    expected_counts = fit_eval$expected_counts
  )
}

# =============================================================================
# Result, filtering, and matrix utilities
# =============================================================================

# Create a stable result shape for every categorical-categorical outcome,
# including failed or empty fits.
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

# Compute observed-minus-expected deviations and Pearson residuals.
compute_local_tables <- function(O, E0) {
  D <- O - E0
  R <- (O - E0) / sqrt(E0)
  R[!is.finite(R) | E0 <= 0] <- NA_real_
  dimnames(R) <- dimnames(O)

  list(D = D, R = R)
}

# Convert nested-model log-likelihoods to G^2, its p-value, and bounded V_L.
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

# Normalize scalar or two-ended UI thresholds to an ordered length-two range.
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

# Zero matrix entries whose p-values fall outside the selected range.
apply_p_value_threshold <- function(mat, p_mat, threshold_p) {
  if (is.null(p_mat)) {
    return(mat)
  }

  p_rng <- normalize_threshold_range(threshold_p, default_min = 0, default_max = 0.2)
  sig_mask <- !is.na(p_mat) & p_mat >= p_rng[1] & p_mat <= p_rng[2]
  mat[!sig_mask] <- 0
  mat
}

# Apply effect-size and p-value thresholds to one complete association result.
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

# Report whether a symmetric matrix has any non-zero off-diagonal entry.
matrix_has_edges <- function(mat) {
  !is.null(mat) &&
    nrow(mat) > 1 &&
    ncol(mat) > 1 &&
    sum(mat[upper.tri(mat)] != 0, na.rm = TRUE) > 0
}

# Convert a matrix result into one row per variable pair for CSV export.
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

# Iteratively remove variables with no retained association.
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

# Align a named square matrix to a requested variable set, filling missing cells.
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

# Align two network matrices to the union of their variable names.
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

# Retrieve a named matrix cell without raising subscript errors.
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

# Score categorical cells by squared Pearson residual for display selection.
compute_catcat_display_scores <- function(O, E0) {
  scores <- matrix(0, nrow = nrow(O), ncol = ncol(O), dimnames = dimnames(O))
  mask <- is.finite(E0) & (E0 > 0)
  scores[mask] <- ((O[mask] - E0[mask])^2) / E0[mask]
  scores[!is.finite(scores) | scores < 0] <- 0
  scores
}

# Select a readable high-information submatrix for large contingency tables.
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

# Compute independence-model expected counts directly from table margins.
compute_marginal_expected <- function(O) {
  n <- sum(O)
  outer(rowSums(O), colSums(O)) / n
}

# =============================================================================
# Categorical pair analyses
# =============================================================================

# Fit the marginal independence and association models for a categorical pair.
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

  # Fit the independence null model.
  fit0 <- try(
    fit_structured_mnl_prepared(prep, include_gamma = FALSE),
    silent = TRUE
  )

  # Fall back to closed-form independence expectations if optimization fails.
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

  # Fit the interaction alternative, warm-started from the null.
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

# Fit the conditional independence and association models for a categorical
# pair, given a shared control data frame.
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

  # Fit the conditional-independence null model.
  fit0 <- try(
    fit_structured_mnl_prepared(prep, include_gamma = FALSE),
    silent = TRUE
  )

  # Fall back to the marginal analysis if the conditional null cannot be fit.
  if (inherits(fit0, "try-error")) {
    base_res <- compute_unconditional(prep$x_fac, prep$y_fac)
    names(base_res)[names(base_res) == "VL"] <- "VL_Z"
    return(base_res)
  }

  ll0 <- fit0$logLik
  E0 <- fit0$expected_counts
  loc <- compute_local_tables(O, E0)

  # Fit the interaction alternative, warm-started from the null.
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

# =============================================================================
# Pair caching and full association-matrix assembly
# =============================================================================

# Build an order-invariant cache key from a pair and its controls.
make_pair_cache_key <- function(v1, v2, control_vars = NULL) {
  pair_part <- paste(sort(c(v1, v2)), collapse = "||")
  control_part <- if (is.null(control_vars) || length(control_vars) == 0) {
    ""
  } else {
    paste(sort(control_vars), collapse = "||")
  }

  paste(pair_part, control_part, sep = "__controls__")
}

# Retrieve a cached pair result without inheriting from parent environments.
get_cached_pair_result <- function(cache_env, key) {
  if (is.null(cache_env) || !exists(key, envir = cache_env, inherits = FALSE)) {
    return(NULL)
  }

  get(key, envir = cache_env, inherits = FALSE)
}

# Store a pair result and return it for convenient assignment chaining.
set_cached_pair_result <- function(cache_env, key, value) {
  if (!is.null(cache_env)) {
    assign(key, value, envir = cache_env)
  }

  value
}

# Apply effect-size thresholds after the expensive pair calculations. Values
# for numerical and mixed pairs are squared here; V_L values are used directly.
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

# Assemble symmetric association, type, and p-value matrices for every pair.
# `data` contains the displayed variables; `full_data` additionally supplies
# control columns while keeping this function independent of Shiny state.
calculate_correlations <- function(
  data,
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

    # Build the pairwise complete-case sample, including selected controls.
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

    # Numerical-numerical pair.
    if (is_num1 && is_num2) {
      if (length(x) > 0 && length(y) > 0) {
        if (has_controls && !is.null(control_data)) {
          # Residual correlation after applying the common controls.
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
              # Fall back to Pearson's r if residualization fails.
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
          # Marginal Pearson correlation.
          r <- cor(x, y, use = "complete.obs")
          if (!is.na(r)) {
            cor_val <- abs(r)
            cor_type <- "Pearson's r"

            n_eff <- length(x)
            p_val <- p_value_partial_cor(r, n_eff, 0)
          }
        }
      }

      # Categorical-categorical pair.
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

      # Numerical-categorical pair.
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

    # Store the pair result symmetrically.
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
