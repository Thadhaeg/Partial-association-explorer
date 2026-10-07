source(file.path("R", "core_associations.R"), local = TRUE)

expect_true <- function(value, message) {
  if (!isTRUE(value)) {
    stop(message, call. = FALSE)
  }
}

expect_equal <- function(actual, expected, tolerance = 1e-7, message = NULL) {
  comparison <- all.equal(actual, expected, tolerance = tolerance, check.attributes = TRUE)
  if (!isTRUE(comparison)) {
    if (is.null(message)) {
      message <- paste(comparison, collapse = "; ")
    }
    stop(message, call. = FALSE)
  }
}

run_test <- function(name, test_function) {
  test_function()
  message("  PASS: ", name)
}

run_test("control residualization drops constant controls", function() {
  controls <- data.frame(active = 1:8, constant = rep(1, 8))
  response <- 3 * controls$active + c(-1, 1, -1, 1, -1, 1, -1, 1)
  residuals <- partial_residuals(response, controls)

  expect_equal(length(residuals), length(response))
  expect_true(abs(mean(residuals)) < 1e-10, "Residuals should be centered.")
  expect_equal(count_active_controls(controls), 1L)
})

run_test("partial-correlation p-values use the requested degrees of freedom", function() {
  p_value <- p_value_partial_cor(r = 0.5, n_eff = 30, k_controls = 2)
  expected <- 1 - stats::pf((0.5 * sqrt(26 / (1 - 0.5^2)))^2, 1, 26)

  expect_equal(p_value, expected)
  expect_true(p_value < 0.01, "The reference association should be significant.")
})

run_test("numerical-categorical association returns a bounded effect size", function() {
  groups <- factor(rep(c("A", "B"), each = 8))
  values <- c(1:8, 12:19)
  result <- calculate_partial_eta_squared_with_F(values, groups)

  expect_true(result$eta_sq > 0.5 && result$eta_sq <= 1, "Eta squared is out of bounds.")
  expect_true(is.finite(result$F) && result$F > 0, "The F statistic should be positive.")
  expect_true(is.finite(result$p_value) && result$p_value < 0.05, "The group effect should be significant.")
})

run_test("categorical independence produces the expected table", function() {
  observed <- matrix(c(10, 10, 10, 10), nrow = 2)
  expected <- compute_marginal_expected(observed)
  diagnostics <- compute_local_tables(observed, expected)

  expect_equal(expected, observed)
  expect_equal(diagnostics$D, matrix(0, nrow = 2, ncol = 2))
  expect_equal(diagnostics$R, matrix(0, nrow = 2, ncol = 2))
})

run_test("unconditional categorical association is bounded", function() {
  x <- factor(rep(c("A", "B"), each = 24))
  y <- factor(c(rep("A", 21), rep("B", 3), rep("A", 4), rep("B", 20)))
  result <- compute_unconditional(x, y)

  expect_true(is.finite(result$VL), "V_L should be finite.")
  expect_true(result$VL >= 0 && result$VL <= 1, "V_L should lie in [0, 1].")
  expect_true(is.finite(result$p_value) && result$p_value < 0.05, "The constructed table should be associated.")
  expect_equal(sum(result$O), length(x))
})

run_test("conditional categorical analysis exposes the conditional coefficient", function() {
  set.seed(20261006)
  n <- 80
  control <- rep(c(0, 1), each = n / 2)
  x <- factor(ifelse(stats::runif(n) < ifelse(control == 0, 0.25, 0.75), "B", "A"))
  y <- factor(ifelse(stats::runif(n) < ifelse(control == 0, 0.30, 0.70), "B", "A"))
  result <- compute_conditional(x, y, data.frame(control = control))

  expect_true("VL_Z" %in% names(result), "Conditional results must expose VL_Z.")
  expect_true(is.na(result$VL_Z) || (result$VL_Z >= 0 && result$VL_Z <= 1), "VL_Z should be missing or bounded.")
  expect_equal(sum(result$O), n)
})

run_test("mixed-type association matrices are symmetric and labeled", function() {
  dataset <- data.frame(
    score_a = 1:12,
    score_b = 2 * (1:12) + rep(c(-1, 1), 6),
    group = factor(rep(c("low", "high"), each = 6))
  )
  result <- calculate_correlations(dataset)

  expect_equal(result$cor_matrix, t(result$cor_matrix))
  expect_equal(result$p_matrix, t(result$p_matrix))
  expect_equal(unname(diag(result$cor_matrix)), rep(1, ncol(dataset)))
  expect_true(result$cor_type_matrix["score_a", "score_b"] == "Pearson's r", "Numeric pairs should use Pearson's r.")
  expect_true(result$cor_type_matrix["score_a", "group"] == "Eta²", "Mixed pairs should use eta squared.")
})

run_test("threshold filters retain only values inside their ranges", function() {
  values <- matrix(c(1, 0.8, 0.3, 0.8, 1, 0.6, 0.3, 0.6, 1), nrow = 3)
  types <- matrix("Pearson's r", nrow = 3, ncol = 3)
  diag(types) <- ""
  filtered <- apply_association_thresholds(
    values,
    types,
    threshold_num = c(0.25, 1),
    threshold_cat = c(0, 1)
  )

  expect_true(filtered[1, 2] == 0.8, "R-squared 0.64 should be retained.")
  expect_true(filtered[1, 3] == 0, "R-squared 0.09 should be filtered.")
})
