# archive.r
# This file contains functions removed from app.r that are not currently used
# but are preserved here for potential future use.
#
# Dependencies (not loaded by app.r):
#   library(grid)
#   library(gridExtra)
#   library(VGAM)
#
# Note: build_contingency_table() references the reactive var_descriptions()
# and make_table(); it can only be called from within a Shiny reactive context.

# library(grid)
# library(gridExtra)
# library(VGAM)

# MILP-based optimal submatrix selection
# Requires: lpSolve package
find_optimal_submatrix <- function(contribution_matrix, n = 5) {
  N <- nrow(contribution_matrix)
  M <- ncol(contribution_matrix)

  # If matrix is already small enough, return all rows/columns
  if (N <= n && M <= n) {
    return(list(
      rows = 1:N,
      cols = 1:M,
      objective = sum(contribution_matrix)
    ))
  }

  if (!require(lpSolve, quietly = TRUE)) {
    stop("Please install lpSolve package: install.packages('lpSolve')")
  }

  # Number of decision variables: u_i (N) + v_j (M) + z_ij (N*M)
  total_vars <- N + M + N * M

  # Objective function coefficients
  objective <- c(
    rep(0, N + M), # u_i and v_j coefficients
    as.vector(t(contribution_matrix)) # z_ij coefficients - NOTE: transposed!
  )

  # Build constraint matrix
  n_constraints <- 2 + 3 * N * M # 2 cardinality constraints + 3 constraints per z_ij
  constraint_matrix <- matrix(0, nrow = n_constraints, ncol = total_vars)
  constraint_dir <- character(n_constraints)
  constraint_rhs <- numeric(n_constraints)

  constraint_index <- 1

  # Constraint 1: Sum of u_i = n
  constraint_matrix[constraint_index, 1:N] <- 1
  constraint_dir[constraint_index] <- "=="
  constraint_rhs[constraint_index] <- n
  constraint_index <- constraint_index + 1

  # Constraint 2: Sum of v_j = n
  constraint_matrix[constraint_index, (N + 1):(N + M)] <- 1
  constraint_dir[constraint_index] <- "=="
  constraint_rhs[constraint_index] <- n
  constraint_index <- constraint_index + 1

  # Constraints for z_ij = u_i * v_j
  for (i in 1:N) {
    for (j in 1:M) {
      z_index <- N + M + (i - 1) * M + j # Index of z_ij variable

      # z_ij <= u_i
      constraint_matrix[constraint_index, i] <- -1
      constraint_matrix[constraint_index, z_index] <- 1
      constraint_dir[constraint_index] <- "<="
      constraint_rhs[constraint_index] <- 0
      constraint_index <- constraint_index + 1

      # z_ij <= v_j
      constraint_matrix[constraint_index, N + j] <- -1
      constraint_matrix[constraint_index, z_index] <- 1
      constraint_dir[constraint_index] <- "<="
      constraint_rhs[constraint_index] <- 0
      constraint_index <- constraint_index + 1

      # z_ij >= u_i + v_j - 1
      constraint_matrix[constraint_index, i] <- -1
      constraint_matrix[constraint_index, N + j] <- -1
      constraint_matrix[constraint_index, z_index] <- 1
      constraint_dir[constraint_index] <- ">="
      constraint_rhs[constraint_index] <- -1
      constraint_index <- constraint_index + 1
    }
  }

  # Solve the MILP
  solution <- lp(
    direction = "max",
    objective.in = objective,
    const.mat = constraint_matrix,
    const.dir = constraint_dir,
    const.rhs = constraint_rhs,
    all.bin = TRUE,
    compute.sens = FALSE
  )

  if (solution$status != 0) {
    warning("MILP solver failed with status: ", solution$status)
    # Fallback to heuristic
    return(find_optimal_submatrix_heuristic(contribution_matrix, n))
  }

  # Extract solution
  u_values <- solution$solution[1:N]
  v_values <- solution$solution[(N + 1):(N + M)]

  # Use proper rounding for binary variables
  selected_rows <- which(round(u_values) == 1)
  selected_cols <- which(round(v_values) == 1)

  # Verify the solution
  actual_objective <- sum(contribution_matrix[selected_rows, selected_cols])

  # Debug output
  cat("MILP reported objective:", solution$objval, "\n")
  cat("Actual submatrix objective:", actual_objective, "\n")
  cat(
    "Selected",
    length(selected_rows),
    "rows and",
    length(selected_cols),
    "columns\n"
  )

  return(list(
    rows = selected_rows,
    cols = selected_cols,
    objective = solution$objval,
    actual_objective = actual_objective
  ))
}

# Heuristic fallback for find_optimal_submatrix()
find_optimal_submatrix_heuristic <- function(contribution_matrix, n = 5) {
  N <- nrow(contribution_matrix)
  M <- ncol(contribution_matrix)

  # Flatten and get top individual cells
  contribution_df <- data.frame(
    row = rep(1:N, each = M),
    col = rep(1:M, times = N),
    value = as.vector(contribution_matrix),
    stringsAsFactors = FALSE
  )

  # Remove zeros and sort
  contribution_df <- contribution_df[contribution_df$value > 0, ]
  contribution_df <- contribution_df[order(-contribution_df$value), ]

  # Get top categories from each variable
  top_rows <- unique(contribution_df$row[1:min(n * n, nrow(contribution_df))])
  top_cols <- unique(contribution_df$col[1:min(n * n, nrow(contribution_df))])

  # Take top n from each
  if (length(top_rows) > n) {
    top_rows <- top_rows[1:n]
  }
  if (length(top_cols) > n) {
    top_cols <- top_cols[1:n]
  }

  objective_value <- sum(contribution_matrix[
    top_rows,
    top_cols,
    drop = FALSE
  ])

  return(list(
    rows = top_rows,
    cols = top_cols,
    objective = objective_value
  ))
}

# Build and render a contingency table UI element.
# For small tables (<=25 cells): standard contingency table with blue colour scale.
# For large tables: uses MILP submatrix selection via find_optimal_submatrix().
# NOTE: references var_descriptions() and make_table() — must be called from
# within a Shiny reactive context where those objects are in scope.
build_contingency_table <- function(df, v1, v2, n = 5) {
  desc1 <- var_descriptions()$description[var_descriptions()$variable == v1]
  desc2 <- var_descriptions()$description[var_descriptions()$variable == v2]

  # Ensure df has the correct columns
  if (!v1 %in% colnames(df) || !v2 %in% colnames(df)) {
    return(div(
      "Invalid variable names for contingency table",
      style = "color:red"
    ))
  }

  # Create a version of the data with NAs removed for the contingency table
  df_clean <- df[complete.cases(df[, c(v1, v2)]), ]

  if (nrow(df_clean) == 0) {
    return(div(
      "No valid data available",
      style = "padding: 20px; text-align: center;"
    ))
  }

  tbl <- table(df_clean[[v1]], df_clean[[v2]])

  if (length(tbl) == 0) {
    return(div(
      "No valid data available",
      style = "padding: 20px; text-align: center;"
    ))
  }

  # Calculate chi-squared test
  chi_test <- tryCatch(
    {
      chisq.test(tbl, simulate.p.value = TRUE)
    },
    error = function(e) NULL
  )

  if (is.null(chi_test)) {
    return(div(
      "Cannot compute chi-squared test for this table",
      style = "padding: 20px; text-align: center;"
    ))
  }

  # Get expected frequencies and residuals
  expected <- chi_test$expected
  residuals <- chi_test$residuals

  # Calculate contributions: (observed - expected)² / expected
  contributions <- (tbl - expected)^2 / expected
  contributions[is.na(contributions)] <- 0

  # Calculate percentage contributions
  total_chi2 <- chi_test$statistic
  contribution_pct <- (contributions / total_chi2) * 100

  # For small tables (<=25 cells), show the traditional table
  if (nrow(tbl) * ncol(tbl) <= 25) {
    tbl_with_margins <- addmargins(tbl)
    df_table <- as.data.frame.matrix(tbl_with_margins)
    df_table <- tibble::rownames_to_column(df_table, var = desc1)

    inner_vals <- tbl
    min_val <- min(inner_vals)
    max_val <- max(inner_vals)
    pal <- colorRampPalette(c("#e1f5fe", "#0288d1"))(100)

    column_defs <- lapply(seq_along(df_table), function(j) {
      colname <- names(df_table)[j]
      if (colname == desc1) {
        colDef(name = desc1, minWidth = 150)
      } else {
        colDef(
          name = colname,
          align = "center",
          cell = function(value, index) {
            is_total <- df_table[[desc1]][index] == "Sum" || colname == "Sum"
            val <- as.numeric(value)
            label <- format(
              val,
              big.mark = ",",
              decimal.mark = ".",
              scientific = FALSE
            )
            if (is_total) {
              return(label)
            }
            idx <- if (max_val > min_val) {
              max(
                1,
                min(
                  100,
                  floor(99 * (val - min_val) / (max_val - min_val)) + 1
                )
              )
            } else {
              50
            }
            div(
              style = paste0("background-color:", pal[idx], "; padding:4px;"),
              label
            )
          }
        )
      }
    })
    names(column_defs) <- names(df_table)

    return(tagList(
      div(class = "reactable-title", desc2),
      make_table(df_table, column_defs)
    ))
  }

  # For large tables (>25 cells), show optimal n×n submatrix
  # Find optimal n×n submatrix using MILP approach
  optimal_selection <- find_optimal_submatrix(contribution_pct, n)

  selected_rows <- optimal_selection$rows
  selected_cols <- optimal_selection$cols

  # Filter the original table (counts) and contributions for optimal categories
  filtered_tbl <- tbl[selected_rows, selected_cols, drop = FALSE]
  filtered_contributions <- contribution_pct[
    selected_rows,
    selected_cols,
    drop = FALSE
  ]

  # Calculate total contribution of selected cells
  total_selected_contribution <- sum(filtered_contributions)
  total_table_contribution <- sum(contribution_pct)

  # Create the display table with COUNTS (not percentages)
  display_table <- as.data.frame.matrix(filtered_tbl)
  display_table <- tibble::rownames_to_column(display_table, var = desc1)

  # Create column definitions with coloring based on CONTRIBUTION PERCENTAGE
  max_contribution <- max(filtered_contributions, na.rm = TRUE)

  column_defs <- lapply(seq_along(display_table), function(j) {
    colname <- names(display_table)[j]
    if (colname == desc1) {
      colDef(name = desc1, minWidth = 150)
    } else {
      colDef(
        name = colname,
        align = "center",
        cell = function(value, index) {
          if (is.na(value) || value == 0) {
            return("0")
          }

          # Get the corresponding contribution percentage for coloring
          row_name <- display_table[[desc1]][index]
          col_name <- colname

          # Find the contribution percentage for this cell
          contrib_value <- filtered_contributions[row_name, col_name]
          if (is.na(contrib_value) || contrib_value == 0) {
            contrib_value <- 0
          }

          # Color intensity based on contribution percentage
          intensity <- min(
            100,
            max(
              1,
              floor(99 * (as.numeric(contrib_value) / max_contribution)) + 1
            )
          )
          pal <- colorRampPalette(c("#f0f8ff", "#0066cc"))(100)

          # Format the count (without decimal places)
          count_label <- format(
            as.numeric(value),
            big.mark = ",",
            decimal.mark = ".",
            scientific = FALSE
          )

          div(
            style = paste0(
              "background-color:",
              pal[intensity],
              "; padding:4px; font-weight: bold;"
            ),
            count_label
          )
        }
      )
    }
  })
  names(column_defs) <- names(display_table)

  # Create the final output
  tagList(
    div(class = "reactable-title", desc2),
    div(
      style = "margin-bottom: 15px;",
      tags$p(
        style = "font-weight: bold; color: #333;",
        paste("Optimal", n, "×", n, "submatrix with maximum contribution")
      ),
      tags$p(
        style = "color: #666;",
        sprintf(
          "Selected cells account for %.2f%% of total chi-squared (%.2f/%.2f%%)",
          total_selected_contribution,
          total_selected_contribution,
          total_table_contribution
        )
      )
    ),
    make_table(display_table, column_defs),
    div(
      style = "margin-top: 15px; font-size: 0.9em; color: #666;",
      tags$p(
        "Cell values show the number of individuals in each category combination."
      ),
      tags$p(
        "Color intensity represents the percentage contribution to the chi-squared statistic."
      ),
      tags$p(
        "Darker blue indicates higher contribution to the association strength."
      ),
      tags$p(
        style = "font-style: italic;",
        "Selection optimized to maximize total contribution percentage."
      )
    )
  )
}
