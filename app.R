# =============================================================================
# Dependencies and shared helpers
# =============================================================================

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(scales)
library(reactable)
library(visNetwork)
library(readxl)
library(janitor)
library(shinyjs)
library(tibble)
library(shinycssloaders)

source(file.path("R", "core_associations.R"), local = TRUE)

# =============================================================================
# Application user interface
# =============================================================================
# The UI is intentionally declared in one place so the navigation order and
# output identifiers are easy to audit against their server implementations.

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

        # Controls affect association estimates but are not displayed as nodes.
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
# Shiny server
# =============================================================================

server <- function(input, output, session) {
  # ---------------------------------------------------------------------------
  # Reactive state and view selection
  # ---------------------------------------------------------------------------

  data <- reactiveVal(NULL)
  full_data <- reactiveVal(NULL)
  var_descriptions <- reactiveVal(NULL)
  reversed_axes <- reactiveValues()
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

  # ---------------------------------------------------------------------------
  # Control selection and marginal/conditional view controls
  # ---------------------------------------------------------------------------

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

  # Controls may also be selected as observed variables; exclude them from the
  # displayed association network while retaining them in full_data().
  visualization_vars <- reactive({
    req(input$selected_vars)
    if (!is.null(input$control_vars) && length(input$control_vars) > 0) {
      setdiff(input$selected_vars, input$control_vars)
    } else {
      input$selected_vars
    }
  })

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

  # ---------------------------------------------------------------------------
  # Data import and description metadata
  # ---------------------------------------------------------------------------

  observeEvent(input$process_data, {
    req(input$data_file)

    # Read the uploaded dataset according to its file type.
    data_path <- input$data_file$datapath
    if (grepl("\\.csv$", data_path, ignore.case = TRUE)) {
      data_df <- read.csv(data_path, stringsAsFactors = TRUE)
    } else if (grepl("\\.(xlsx|xls)$", data_path, ignore.case = TRUE)) {
      data_df <- read_excel(data_path) |>
        mutate(across(where(is.character), as.factor))
    } else {
      stop("Unsupported file format for data file.")
    }

    # Remove variables with fewer than two distinct non-missing values.
    original_names <- names(data_df)
    data_df <- data_df[,
      sapply(data_df, function(x) length(unique(x[!is.na(x)])) > 1),
      drop = FALSE
    ]

    # Both holders currently receive the filtered dataset. full_data() keeps
    # control columns available when selected_data_reactive() narrows the
    # observed variables later in the analysis pipeline.
    data(data_df)
    full_data(data_df)
    cache_keys <- ls(envir = pair_cache, all.names = TRUE)
    if (length(cache_keys) > 0) {
      rm(list = cache_keys, envir = pair_cache)
    }

    # Tell the user which non-varying variables were removed.
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

    # Use variable names as descriptions unless valid metadata override them.
    default_descriptions <- data.frame(
      variable = names(data_df),
      description = names(data_df),
      stringsAsFactors = FALSE
    )

    # Read and validate the optional description file.
    if (!is.null(input$desc_file)) {
      # Read the uploaded file.
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

      # Normalize harmless whitespace before validating the required names.
      colnames(user_desc) <- trimws(colnames(user_desc))

      # Require exactly the two documented metadata columns.
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
        # Normalize valid metadata to the internal lowercase schema.
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

    # Continue the workflow on the variable-selection tab.
    updateTabsetPanel(session, "main_tabs", selected = "variables_tab")
  })

  # ---------------------------------------------------------------------------
  # Variable selection and navigation
  # ---------------------------------------------------------------------------

  output$variable_checkboxes_ui <- renderUI({
    req(data())
    selectizeInput(
      inputId = "selected_vars",
      label = "Select variables to include:",
      choices = names(data()),
      selected = names(data()),
      multiple = TRUE,
      width = "100%",
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
    # Hide the table unless the user supplied custom descriptions.
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

  # ---------------------------------------------------------------------------
  # Association computation and thresholded pair sets
  # ---------------------------------------------------------------------------

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
      full_data = full_data(),
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

  # ---------------------------------------------------------------------------
  # Association network and CSV export
  # ---------------------------------------------------------------------------

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

    compare_type_mat <- if (is.null(compare_result)) {
      matrix("", nrow = nrow(active_mat), ncol = ncol(active_mat), dimnames = dimnames(active_mat))
    } else {
      safe_named_square_subset(compare_result$cor_type_matrix, keep_names, fill = "")
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

  # ---------------------------------------------------------------------------
  # Pair diagnostics
  # ---------------------------------------------------------------------------

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

    # Register each reverse-axis observer once, even when renderUI invalidates.
    isolate({
      for (i in seq_len(nrow(pairs))) {
        local({
          idx <- i
          plot_id <- paste0("plot_", idx)
          button_id <- paste0("reverse_", idx)

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

      # Resolve descriptions once for every pair and reuse them in all views.
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

      # Pair plots use complete cases for the two displayed variables. Branches
      # with controls construct their stricter complete-case dataset below.
      plot_data <- df %>%
        filter(!is.na(.data[[v1]]), !is.na(.data[[v2]]))

      # Numerical-numerical diagnostics ---------------------------------------
      if (is_num1 && is_num2) {
        controls_exist <- has_controls()
        view_control_vars <- if (controls_exist) input$control_vars else NULL

        if (controls_exist) {
          # Conditional view: added-variable (partial regression) plot.
          output[[plotname]] <- renderPlot({
            # Declare the reverse-axis state as a reactive dependency.
            force(reversed_axes[[plotname]])

            # Use the full dataset because selected_data_reactive() excludes
            # control columns.
            full_df <- full_data()
            if (is.null(full_df)) {
              full_df <- data()
            }

            # Combine the displayed pair and controls before dropping cases.
            all_vars <- c(v1, v2, view_control_vars)

            # Report missing columns explicitly instead of failing inside lm().
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

            # Match the complete-case sample used by the association estimate.
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
                # Residualize both variables on the same controls.
                control_data <- plot_data_full[,
                  view_control_vars,
                  drop = FALSE
                ]

                resid_x <- partial_residuals(plot_data_full[[v1]], control_data)
                resid_y <- partial_residuals(plot_data_full[[v2]], control_data)

                # Compute the partial correlation and its test annotation.
                partial_cor <- cor(resid_x, resid_y, use = "complete.obs")
                partial_r2_text <- format_plot_stat(partial_cor^2)
                n_eff <- length(resid_x)
                k_controls <- count_active_controls(control_data)
                p_val_text <- format_plot_p_value(
                  p_value_partial_cor(partial_cor, n_eff, k_controls)
                )

                # Read the user's current axis orientation.
                is_reversed <- if (is.null(reversed_axes[[plotname]])) {
                  FALSE
                } else {
                  reversed_axes[[plotname]]
                }

                # Apply the orientation consistently to values and labels.
                x_resid <- if (is_reversed) resid_y else resid_x
                y_resid <- if (is_reversed) resid_x else resid_y
                x_desc <- if (is_reversed) desc2 else desc1
                y_desc <- if (is_reversed) desc1 else desc2

                # Fit the displayed residual-on-residual slope.
                if (is_reversed) {
                  lm_resid <- lm(resid_x ~ resid_y)
                } else {
                  lm_resid <- lm(resid_y ~ resid_x)
                }
                slope <- coef(lm_resid)[2]
                slope_text <- ifelse(is.na(slope), "NA", round(slope, 3))

                # Draw the added-variable diagnostic.
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
                # Preserve a useful plot if the adjusted fit cannot be drawn.
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
          # Unconditional view: ordinary scatter plot and Pearson correlation.
          output[[plotname]] <- renderPlot({
            # Declare the reverse-axis state as a reactive dependency.
            force(reversed_axes[[plotname]])

            if (nrow(plot_data) > 0) {
              # Read the user's current axis orientation.
              is_reversed <- if (is.null(reversed_axes[[plotname]])) {
                FALSE
              } else {
                reversed_axes[[plotname]]
              }

              # Apply the orientation consistently to values and labels.
              x_var <- if (is_reversed) v2 else v1
              y_var <- if (is_reversed) v1 else v2
              x_desc <- if (is_reversed) desc2 else desc1
              y_desc <- if (is_reversed) desc1 else desc2

              # Compute the Pearson annotation on the complete pair sample.
              current_cor <- cor(
                plot_data[[v1]],
                plot_data[[v2]],
                use = "complete.obs"
              )
              r2_text <- format_plot_stat(current_cor^2)
              p_val_text <- format_plot_p_value(
                p_value_partial_cor(current_cor, nrow(plot_data), 0)
              )

              # Fit the displayed slope in the selected orientation.
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

        # Each numerical pair exposes the same axis-reversal control.
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
            # Reverse both axes and refit the displayed slope.
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
        # Categorical-categorical diagnostics ---------------------------------

        output[[plotname]] <- renderUI({
          if (nrow(plot_data) == 0) {
            return(div(
              "No valid data available",
              style = "padding: 20px; text-align: center;"
            ))
          }

          controls_exist <- has_controls()
          view_control_vars <- if (controls_exist) input$control_vars else NULL

          full_df <- full_data()
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

          # Display observed counts while coloring cells by Pearson residual.
          display_df <- as.data.frame.matrix(O_display)
          display_df <- tibble::rownames_to_column(display_df, var = v1)

          # Normalize color intensity to the largest displayed residual.
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

                  # Encode residual magnitude as color intensity.
                  intensity <- min(1, abs(r_val) / max_abs_r)

                  # Red denotes over-representation; blue denotes under-representation.
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

            full_df <- full_data()
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
        # Numerical-categorical diagnostics -----------------------------------
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

          # Unconditional view: raw group means.
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
            # Conditional view: residualized group means.
            full_df <- full_data()
            if (is.null(full_df)) {
              full_df <- data()
            }

            all_vars <- c(num_var, cat_var, view_control_vars)

            # Report missing columns explicitly instead of failing inside lm().
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

            # Build the complete modeling frame for this pair and its controls.
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

            # Match the effect-size routine by discarding constant controls.
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

            # Retain only the response, factor, and usable controls.
            df_full <- df_full[,
              c(response_name, cat_name, controls_kept),
              drop = FALSE
            ]

            # A constant numerical outcome has no group contrast to display.
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

            # Residualize the numerical outcome on controls only.
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
              # Preserve a useful unadjusted plot if residualization fails.
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
              # Summarize the remaining numerical signal within factor levels.
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
