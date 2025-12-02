library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(scales)
library(reactable)
library(tidygraph)
library(visNetwork)
library(readxl)
library(grid)
library(gridExtra)
library(janitor)
library(shinyjs)
library(tibble)
library(shinycssloaders)

ui <- tagList(
  tags$head(
    tags$title("AssociationExplorer App"),
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
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
    titlePanel(div("Association Explorer", class = "app-title")),
    br(),
    tabsetPanel(
      id = "main_tabs",
      type = "tabs",
      tabPanel(
        title = tags$strong("📁 Data"),
        value = "upload_tab",
        br(),
        br(),
        fileInput("data_file", "Upload your dataset (CSV or Excel)",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values",
                    "text/plain",
                    ".csv",
                    ".xlsx",
                    ".xls"
                  )
        ),
        fileInput("desc_file", "(Optional) Upload variable descriptions (CSV or Excel)",
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
            sliderInput("threshold_num", "Threshold for Quantitative-Quantitative and Quantitative-Categorical Associations (R²)",
                        min = 0, max = 1, value = 0.5, step = 0.05
            ),
            sliderInput("threshold_cat", "Threshold for Categorical-Categorical Associations (Cramer's V)",
                        min = 0, max = 1, value = 0.5, step = 0.05
            ),
            tags$i(tags$span(
              style = "color: #666666",
              "Only associations stronger than the thresholds will be displayed in the plot."
            )),
            br(),
            br(),
            fluidRow(
              column(12,
                     align = "center",
                     actionButton("go_to_pairs", "See pairs plots", class = "btn btn-primary")
              )
            )
          ),
          mainPanel(
            class = "panel-white",
            withSpinner(visNetworkOutput("network_vis", height = "600px", width = "100%"), type = 6, color = "#0072B2")
          )
        )
      ),
      tabPanel(
        title = tags$strong("📊 Pairs Plots"),
        value = "pairs_tab",
        fluidPage(
          class = "panel-white",
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
          h3("How to use the Association Explorer app?"),
          br(),
          tags$ul(
            tags$li("Upload your dataset (CSV or Excel) in the 'Data' tab. Optionally, upload a file with variable descriptions. This file must contain 2 columns called 'Variable' and 'Description'."),
            tags$li("In the 'Variables' tab, select the variables you want to explore. If you upload a file containing variables' descriptions, a summary table below shows the selected variables along with their descriptions."),
            tags$li("Click 'Visualize all associations' to access the correlation network."),
            tags$li("Adjust the thresholds to filter associations by strength. Only variables that have strong associations (as defined by the thresholds) will appear in the network and pairs plots."),
            tags$li("In the correlation network plot, thicker and shorter edges indicate stronger associations."),
            tags$li("Click 'See pairs plots' to display bivariate visualizations for retained associations.")
          )
        )
      )
    ),
    br(),
    tags$hr(),
    tags$footer(
      class = "app-footer",
      "v3.5.5.",
      tags$a(href = "https://github.com/AntoineSoetewey/AssociationExplorer", "Code", target = "_blank")
    ),
  )
)

server <- function(input, output, session) {
  data <- reactiveVal(NULL)
  var_descriptions <- reactiveVal(NULL)
  
  # NEW: Track which plots have reversed axes
  reversed_axes <- reactiveValues()

    # NEW: Control variables logic
    # Store full dataset for control variable access
    data_env <- new.env()
    data_env$full_data <- reactiveVal(NULL)
    
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
  
  observeEvent(input$process_data, {
    req(input$data_file)
    
    # Read the uploaded data
    data_path <- input$data_file$datapath
    if (grepl("\\.csv$", data_path, ignore.case = TRUE)) {
      data_df <- read.csv(data_path, stringsAsFactors = TRUE)
    } else if (grepl("\\.(xlsx|xls)$", data_path, ignore.case = TRUE)) {
      data_df <- read_excel(data_path, stringsAsFactors = TRUE)
    } else {
      stop("Unsupported file format for data file.")
    }
    
    # Remove variables with all equal values (e.g., variance zero)
    original_names <- names(data_df)
    data_df <- data_df[, sapply(data_df, function(x) length(unique(x[!is.na(x)])) > 1), drop = FALSE]
    
    # Store filtered data
    data(data_df)
    data_env$full_data(data_df)  # NEW: Store the full dataset with all variables
    
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
        user_desc <- read.csv(desc_path, stringsAsFactors = FALSE, check.names = FALSE)
      } else {
        user_desc <- read_excel(desc_path, stringsAsFactors = FALSE)
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
            "Found columns:", paste(sQuote(colnames(user_desc)), collapse = ", ")
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
  
  output$go_to_network_ui <- renderUI({
    req(input$selected_vars)
    actionButton("go_to_network", "Visualize all associations", class = "btn btn-primary")
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
  
  cor_matrix_reactive <- reactive({
    req(data())
    selected_vars <- visualization_vars()
    selected_data <- data()[, selected_vars, drop = FALSE]
    
    # NEW : Force reactivity to threshold AND control changes
    force(input$threshold_num)
    force(input$threshold_cat)
    force(input$control_vars)
    
    calculate_correlations(selected_data, input$threshold_num, input$threshold_cat, input$control_vars)
  })

  cor_matrix_vals <- reactive({
    cor_matrix_reactive()
  })
  
  filtered_data_for_pairs <- reactive({
    mat <- cor_matrix_vals()$cor_matrix
    nodes_to_keep <- rowSums(abs(mat) > 0) > 1
    filtered_matrix <- mat[nodes_to_keep, nodes_to_keep]
    data()[, colnames(filtered_matrix), drop = FALSE]
  })
  
  significant_pairs <- reactive({
    req(cor_matrix_vals())
    req(input$threshold_num)  # Add explicit dependency
    req(input$threshold_cat)  # Add explicit dependency
    
    mat <- cor_matrix_vals()$cor_matrix
    nodes_to_keep <- rowSums(abs(mat) > 0) > 1
    filtered_matrix <- mat[nodes_to_keep, nodes_to_keep]
    pairs <- which(filtered_matrix != 0 & upper.tri(filtered_matrix), arr.ind = TRUE)
    
    if (nrow(pairs) == 0) {
      return(NULL)
    }
    
    data.frame(
      var1 = rownames(filtered_matrix)[pairs[, 1]],
      var2 = colnames(filtered_matrix)[pairs[, 2]],
      stringsAsFactors = FALSE
    )
  })
  
  output$network_vis <- renderVisNetwork({
    cor_result <- cor_matrix_reactive()
    cor_matrix <- cor_result$cor_matrix
    cor_type_matrix <- cor_result$cor_type_matrix
    
    cor_matrix_clean <- cor_matrix
    cor_matrix_clean[is.na(cor_matrix_clean)] <- 0 # NEW : to deal with NAs
    nodes_to_keep <- rowSums(abs(cor_matrix_clean) > 0) > 1
    mat <- cor_matrix[nodes_to_keep, nodes_to_keep]
    type_mat <- cor_type_matrix[nodes_to_keep, nodes_to_keep]
    
    validate(
      need(
        ncol(mat) > 0,
        "No associations above the thresholds. Please adjust the thresholds or select different variables."
      )
    )
    
    # 1) Prepare nodes with descriptions instead of names
    nodes <- data.frame(id = colnames(mat), stringsAsFactors = FALSE) |>
      left_join(var_descriptions(), by = c("id" = "variable")) |>
      mutate(
        label = id, # keep the variable code as label
        title = description, # on hover, the description will be shown
        size = 15 # default size
      ) |>
      select(id, label, title, size)
    
    # 2) Prepare edges with appropriate correlation type
    edgelist <- which(mat != 0 & upper.tri(mat), arr.ind = TRUE)
    edges <- data.frame(
      from = rownames(mat)[edgelist[, 1]],
      to = colnames(mat)[edgelist[, 2]],
      width = 1 + 4 * (abs(mat[edgelist]) - min(abs(mat[edgelist]))) / (max(abs(mat[edgelist])) - min(abs(mat[edgelist]))),
      color = ifelse(mat[edgelist] > 0, "steelblue", "darkred"),
      title = paste0(type_mat[edgelist], " = ", round(mat[edgelist], 2)),
      stringsAsFactors = FALSE
    )
    
    # Adjust edge lengths based on association strengths
    strengths <- abs(mat[edgelist])
    min_len <- 100 # min length (strong association)
    max_len <- 500 # max length (weak association)
    edges$length <- (1 - strengths) * (max_len - min_len) + min_len
    
    # 3) Build the plot
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
    
    # Create observers for reverse buttons OUTSIDE the renderUI
    # We'll use a different approach - create them once when pairs change
    isolate({
      for (i in seq_len(nrow(pairs))) {
        local({
          idx <- i
          plot_id <- paste0("plot_", idx)
          button_id <- paste0("reverse_", idx)
          
          # Only create observer if it doesn't exist
          if (is.null(reversed_axes[[paste0("obs_", button_id)]])) {
            observeEvent(input[[button_id]], {
              current_state <- reversed_axes[[plot_id]]
              reversed_axes[[plot_id]] <- if (is.null(current_state)) TRUE else !current_state
              cat("Button", button_id, "clicked! New state:", reversed_axes[[plot_id]], "\n")
            }, ignoreInit = TRUE)
            reversed_axes[[paste0("obs_", button_id)]] <- TRUE
          }
        })
      }
    })
    
    tabs <- lapply(seq_len(nrow(pairs)), function(i) {
      v1 <- pairs$var1[i]
      v2 <- pairs$var2[i]
      
      # Get the descriptions
      desc1 <- var_descriptions()$description[var_descriptions()$variable == v1]
      desc2 <- var_descriptions()$description[var_descriptions()$variable == v2]
      
      plotname <- paste0("plot_", i)
      is_num1 <- is.numeric(df[[v1]])
      is_num2 <- is.numeric(df[[v2]])
      
      # Create a clean subset without NAs for these variables
      plot_data <- df %>%
        filter(!is.na(.data[[v1]]), !is.na(.data[[v2]]))
      
      # Numeric vs numeric case
      if (is_num1 && is_num2) {
        
        # Check if we have controls selected - use the full data to access controls
        controls_exist <- has_controls() && 
          !is.null(input$control_vars) && 
          length(input$control_vars) > 0
        
        if (controls_exist) {
          # WITH CONTROLS: Added-variable plot (partial regression plot)
          output[[plotname]] <- renderPlot({
            # Force reactivity to reversed_axes changes
            force(reversed_axes[[plotname]])
            
            # GET DATA FROM FULL DATASET to access control variables
            full_df <- data_env$full_data()
            if (is.null(full_df)) {
              full_df <- data()  # Fallback to current data if full data not available
            }
            
            # Create complete dataset with the pair variables AND control variables
            all_vars <- c(v1, v2, input$control_vars)
            
            # Check if all required columns exist in full data
            missing_cols <- setdiff(all_vars, names(full_df))
            if (length(missing_cols) > 0) {
              plot.new()
              text(0.5, 0.5, paste("Missing columns in full data:", paste(missing_cols, collapse = ", ")), 
                   cex = 1.2, adj = 0.5)
              return()
            }
            
            # Get complete cases from FULL dataset
            complete_cases <- complete.cases(full_df[, all_vars])
            plot_data_full <- full_df[complete_cases, all_vars]
            
            if (nrow(plot_data_full) == 0) {
              plot.new()
              text(0.5, 0.5, "No complete data available after controlling for variables", 
                   cex = 1.2, adj = 0.5)
              return()
            }
            
            tryCatch({
              # Calculate residuals after controlling for other variables
              control_data <- plot_data_full[, input$control_vars, drop = FALSE]
              
              resid_x <- residuals(lm(plot_data_full[[v1]] ~ ., data = control_data))
              resid_y <- residuals(lm(plot_data_full[[v2]] ~ ., data = control_data))
              
              # Calculate partial correlation
              partial_cor <- cor(resid_x, resid_y, use = "complete.obs")
              partial_cor_text <- ifelse(is.na(partial_cor), "NA", round(partial_cor, 3))
              
              # Check if axes should be reversed
              is_reversed <- if (is.null(reversed_axes[[plotname]])) FALSE else reversed_axes[[plotname]]
              
              # Determine which residuals to use for X and Y
              x_resid <- if (is_reversed) resid_y else resid_x
              y_resid <- if (is_reversed) resid_x else resid_y
              x_desc <- if (is_reversed) desc2 else desc1
              y_desc <- if (is.null(reversed_axes[[plotname]])) FALSE else reversed_axes[[plotname]]
              
              # Calculate regression slope for the residuals (respect reversed axes)
              if (is_reversed) {
                lm_resid <- lm(resid_x ~ resid_y)
              } else {
                lm_resid <- lm(resid_y ~ resid_x)
              }
              slope <- coef(lm_resid)[2]
              slope_text <- ifelse(is.na(slope), "NA", round(slope, 3))
              
              # Create added-variable plot with slope
              ggplot(data.frame(x = x_resid, y = y_resid), aes(x = x, y = y)) +
                geom_point(alpha = 0.6, color = "steelblue", size = 2) +
                geom_smooth(method = "lm", se = TRUE, color = "darkred", linewidth = 1, 
                            fill = "pink", alpha = 0.2) +
                labs(
                  x = paste0("Residuals of ", x_desc, " | controls"),
                  y = paste0("Residuals of ", y_desc, " | controls"),
                  title = paste("Added-Variable Plot (Partial Regression)"),
                  subtitle = paste("Partial correlation =", partial_cor_text, 
                                   "| Slope =", slope_text,
                                   "| Controlling for:", paste(input$control_vars, collapse = ", "))
                ) +
                theme_minimal(base_size = 14) +
                theme(
                  plot.title = element_text(face = "bold"),
                  plot.subtitle = element_text(color = "gray40", size = 10)
                )
              
            }, error = function(e) {
              # Fallback to regular scatter plot if partial correlation fails
              cat("Partial correlation failed:", e$message, "\n")
              current_cor <- if (nrow(plot_data) > 0) {
                cor(plot_data[[v1]], plot_data[[v2]], use = "complete.obs")
              } else {
                NA
              }
              cor_text <- ifelse(is.na(current_cor), "NA", round(current_cor, 3))
              
              ggplot(plot_data, aes(x = .data[[v1]], y = .data[[v2]])) +
                geom_jitter(alpha = 0.6, color = "steelblue", width = 0.5, height = 0.5) +
                geom_smooth(method = "lm", se = FALSE, color = "darkred", linewidth = 1) +
                labs(
                  x = desc1,
                  y = desc2,
                  title = "Regular Scatter Plot (Partial Correlation Failed)",
                  subtitle = paste("Raw correlation =", cor_text, "| Error:", e$message)
                ) +
                scale_x_continuous(labels = label_number(big.mark = ",", decimal.mark = ".")) +
                scale_y_continuous(labels = label_number(big.mark = ",", decimal.mark = ".")) +
                theme_minimal(base_size = 14)
            })
          })
          
        } else {
          # WITHOUT CONTROLS: Regular scatter plot
          output[[plotname]] <- renderPlot({
            # Force reactivity to reversed_axes changes
            force(reversed_axes[[plotname]])
            
            if (nrow(plot_data) > 0) {
              # Check if axes should be reversed
              is_reversed <- if (is.null(reversed_axes[[plotname]])) FALSE else reversed_axes[[plotname]]
              
              # Determine which variable is X and which is Y
              x_var <- if (is_reversed) v2 else v1
              y_var <- if (is_reversed) v1 else v2
              x_desc <- if (is_reversed) desc2 else desc1
              y_desc <- if (is_reversed) desc1 else desc2
              
              # Calculate correlation
              current_cor <- cor(plot_data[[v1]], plot_data[[v2]], use = "complete.obs")
              cor_text <- ifelse(is.na(current_cor), "NA", round(current_cor, 3))
              
              # Calculate slope (respect reversed axes)
              if (is_reversed) {
                lm_regular <- lm(plot_data[[v1]] ~ plot_data[[v2]])
              } else {
                lm_regular <- lm(plot_data[[v2]] ~ plot_data[[v1]])
              }
              slope_regular <- coef(lm_regular)[2]
              slope_text_regular <- ifelse(is.na(slope_regular), "NA", round(slope_regular, 3))
              
              ggplot(plot_data, aes(x = .data[[x_var]], y = .data[[y_var]])) +
                geom_jitter(alpha = 0.6, color = "steelblue", width = 0.5, height = 0.5) +
                geom_smooth(method = "lm", se = FALSE, color = "darkred", linewidth = 1) +
                labs(
                  x = x_desc,
                  y = y_desc,
                  title = paste("Scatter Plot | Correlation =", cor_text, "| Slope =", slope_text_regular)
                ) +
                scale_x_continuous(labels = label_number(big.mark = ",", decimal.mark = ".")) +
                scale_y_continuous(labels = label_number(big.mark = ",", decimal.mark = ".")) +
                theme_minimal(base_size = 14)
            } else {
              plot.new()
              text(0.5, 0.5, "No valid data available", cex = 1.5, adj = 0.5)
            }
          })
        }
        
        # NEW : add a "reverse axes" button in the pair plots window
        nav_panel(
          paste0(v1, " vs ", v2),
          div(
            style = "position: relative;",
            plotOutput(plotname, height = "600px"),
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
      }
      # Categorical vs categorical case (table)
      else if (!is_num1 && !is_num2) {
        output[[plotname]] <- renderUI({
          if (nrow(plot_data) > 0) {
            build_contingency_table(plot_data, v1, v2)
          } else {
            div("No valid data available", style = "padding: 20px; text-align: center;")
          }
        })
        nav_panel(paste0(v1, " vs ", v2), uiOutput(plotname))
      }
      # Mixed case (numeric vs categorical)
      else {
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
          if (nrow(plot_data) > 0) {
            df_sum <- plot_data |>
              group_by(.data[[cat_var]]) |>
              summarise(
                mean_val = mean(.data[[num_var]], na.rm = TRUE),
                .groups = "drop"
              ) |>
              arrange(mean_val) |>
              mutate({{ cat_var }} := factor(.data[[cat_var]], levels = .data[[cat_var]]))
            
            ggplot(df_sum, aes(x = .data[[cat_var]], y = mean_val)) +
              geom_col(fill = "steelblue", width = 0.6) +
              geom_text(
                aes(label = format(round(mean_val, 2),
                                   big.mark = ",", decimal.mark = "."
                )),
                hjust = 1.1, color = "white", size = 4
              ) +
              labs(
                x = desc_cat,
                y = paste0('Mean of "', desc_num, '"')
              ) +
              scale_y_continuous(labels = label_number(big.mark = ",", decimal.mark = ".")) +
              theme_minimal(base_size = 14) +
              coord_flip()
          } else {
            plot.new()
            text(0.5, 0.5, "No valid data available",
                 cex = 1.5, adj = 0.5
            )
          }
        })
        nav_panel(paste0(v1, " vs ", v2), plotOutput(plotname, height = "600px"))
      }
    })
    tagList(navset_card_tab(id = "bivariate_tabs", !!!tabs))
  })
  
  # UPDATED : Function to calculate correlations and partial correlations
  calculate_correlations <- function(data, threshold_num, threshold_cat, control_vars = NULL) {
    vars <- names(data)
    n <- length(vars)
    cor_matrix <- matrix(0, n, n, dimnames = list(vars, vars))
    cor_type_matrix <- matrix("", n, n, dimnames = list(vars, vars))
    combs <- combn(vars, 2, simplify = FALSE)
    
    has_controls <- !is.null(control_vars) && length(control_vars) > 0
    
    # Get the full dataset with controls if needed
    full_data <- NULL
    if (has_controls) {
      full_data <- data_env$full_data()
    }
    
    for (pair in combs) {
      v1 <- pair[1]
      v2 <- pair[2]
      is_num1 <- is.numeric(data[[v1]])
      is_num2 <- is.numeric(data[[v2]])
      cor_val <- 0
      cor_type <- ""
      
      if (has_controls && !is.null(full_data)) {
        # Get complete cases from full dataset including control variables
        complete_cases <- complete.cases(
          full_data[[v1]], 
          full_data[[v2]], 
          full_data[, control_vars, drop = FALSE]
        )
        x <- full_data[[v1]][complete_cases]
        y <- full_data[[v2]][complete_cases]
        control_data <- full_data[complete_cases, control_vars, drop = FALSE]
      } else {
        # Get complete cases without controls
        complete_cases <- complete.cases(data[[v1]], data[[v2]])
        x <- data[[v1]][complete_cases]
        y <- data[[v2]][complete_cases]
        control_data <- NULL
      }
      
      # Numeric vs numeric case
      if (is_num1 && is_num2) {
        if (length(x) > 0 && length(y) > 0) {
          if (has_controls && !is.null(control_data)) {
            # Partial correlation
            tryCatch({
              resid_x <- residuals(lm(x ~ ., data = control_data))
              resid_y <- residuals(lm(y ~ ., data = control_data))
              r <- cor(resid_x, resid_y, use = "complete.obs")
              if (!is.na(r)) {
                cor_val <- ifelse(r^2 >= threshold_num, abs(r), 0)
                cor_type <- "Partial r"
              }
            }, error = function(e) {
              # Fallback to regular correlation if partial fails
              r <- cor(x, y, use = "complete.obs")
              if (!is.na(r)) {
                cor_val <- ifelse(r^2 >= threshold_num, abs(r), 0)
                cor_type <- "Pearson's r"
              }
            })
          } else {
            # Regular correlation
            r <- cor(x, y, use = "complete.obs")
            if (!is.na(r)) {
              cor_val <- ifelse(r^2 >= threshold_num, abs(r), 0)
              cor_type <- "Pearson's r"
            }
          }
        }
        
        # Categorical vs categorical case
      } else if (!is_num1 && !is_num2) {
        if (length(x) > 0 && length(y) > 0) {
          if (has_controls && !is.null(control_data)) {
            # NEW: Proper partial Cramér's V using multinomial likelihood
            cor_result <- calculate_partial_cramers_v(x, y, control_data)
            cor_val <- ifelse(cor_result$v >= threshold_cat, cor_result$v, 0)
            cor_type <- cor_result$type
          } else {
            # Regular Cramér's V
            cor_result <- calculate_regular_cramers_v(x, y)
            cor_val <- ifelse(cor_result$v >= threshold_cat, cor_result$v, 0)
            cor_type <- cor_result$type
          }
        }
        
        # Mixed case (numeric vs categorical)
      } else {
        if (is_num1) {
          num_var <- x
          cat_var <- y
        } else {
          num_var <- y
          cat_var <- x
        }
        
        if (length(num_var) > 0 && length(cat_var) > 0) {
          if (has_controls && !is.null(control_data)) {
            # Partial eta-squared via ANCOVA
            tryCatch({
              df_temp <- data.frame(num_var = num_var, cat_var = as.factor(cat_var))
              df_temp <- cbind(df_temp, control_data)
              
              # Fit models for partial association
              model_reduced <- lm(num_var ~ ., data = df_temp[, !names(df_temp) %in% "cat_var", drop = FALSE])
              model_full <- lm(num_var ~ ., data = df_temp)
              
              # Calculate partial eta-squared
              ss_residual_reduced <- sum(residuals(model_reduced)^2)
              ss_residual_full <- sum(residuals(model_full)^2)
              ss_explained <- ss_residual_reduced - ss_residual_full
              ss_total <- var(num_var) * (length(num_var) - 1)
              
              if (ss_total > 0 && ss_explained > 0) {
                partial_eta_sq <- ss_explained / ss_total
                cor_val <- ifelse(partial_eta_sq >= threshold_num, sqrt(partial_eta_sq), 0)
                cor_type <- "sqrt(Partial η²)"
              }
            }, error = function(e) {
              # Fallback to regular eta-squared
              cor_result <- calculate_eta_squared(num_var, cat_var)
              cor_val <- ifelse(cor_result$eta^2 >= threshold_num, cor_result$eta, 0)
              cor_type <- cor_result$type
            })
          } else {
            # Regular eta-squared
            cor_result <- calculate_eta_squared(num_var, cat_var)
            cor_val <- ifelse(cor_result$eta^2 >= threshold_num, cor_result$eta, 0)
            cor_type <- cor_result$type
          }
        }
      }
      
      cor_matrix[v1, v2] <- cor_matrix[v2, v1] <- cor_val
      cor_type_matrix[v1, v2] <- cor_type_matrix[v2, v1] <- cor_type
    }
    
    diag(cor_matrix) <- 1
    cor_matrix[is.na(cor_matrix)] <- 0 # NEW : to deal with NA values
    
    list(cor_matrix = cor_matrix, cor_type_matrix = cor_type_matrix)
  }
  
  # NEW: partial Cramér's V calculation using multinomial likelihood
  calculate_partial_cramers_v <- function(x, y, control_data) {
    require(nnet)
    
    # Convert to factors if not already
    x <- as.factor(x)
    y <- as.factor(y)
    
    # Create complete dataset
    df <- data.frame(var1 = x, var2 = y)
    df <- cbind(df, control_data)
    
    n_obs <- nrow(df)
    
    # Remove variables with zero variance in controls
    control_data_clean <- control_data[, sapply(control_data, function(col) length(unique(col)) > 1), drop = FALSE]
    
    if (ncol(control_data_clean) == 0) {
      # No valid controls, fall back to regular Cramér's V
      return(calculate_regular_cramers_v(x, y))
    }
    
    df_clean <- cbind(data.frame(var1 = x, var2 = y), control_data_clean)
    
    tryCatch({
      # Model A: Independence (var1 ~ controls + var2, but no var1-var2 relationship)
      # We test if var2 adds predictive power beyond controls for predicting var1
      formula_A <- as.formula(paste("var1 ~", paste(names(control_data_clean), collapse = " + ")))
      formula_B <- as.formula(paste("var1 ~ var2 +", paste(names(control_data_clean), collapse = " + ")))
      
      # Fit multinomial models
      model_A <- multinom(formula_A, data = df_clean, trace = FALSE)
      model_B <- multinom(formula_B, data = df_clean, trace = FALSE)
      
      # Likelihood ratio test
      deviance_A <- deviance(model_A)
      deviance_B <- deviance(model_B)
      
      partial_chi2 <- deviance_A - deviance_B
      
      # Degrees of freedom for the test
      r <- nlevels(x)
      c <- nlevels(y)
      df_test <- (r - 1) * (c - 1)
      
      if (partial_chi2 > 0 && df_test > 0) {
        # Calculate partial Cramér's V
        df_min <- min(r - 1, c - 1)
        partial_v <- sqrt(partial_chi2 / (n_obs * df_min))
        
        return(list(v = partial_v, type = "Partial Cramer's V", chi2 = partial_chi2))
      } else {
        # Fallback to regular Cramér's V
        return(calculate_regular_cramers_v(x, y))
      }
      
    }, error = function(e) {
      # Fallback to regular Cramér's V if anything fails
      return(calculate_regular_cramers_v(x, y))
    })
  }
  
  # NEW: Regular Cramér's V calculation
  calculate_regular_cramers_v <- function(x, y) {
    tbl <- table(x, y)
    
    if (nrow(tbl) > 1 && ncol(tbl) > 1) {
      chi <- tryCatch(chisq.test(tbl, simulate.p.value = TRUE), error = function(e) NULL)
      if (!is.null(chi)) {
        n_obs <- sum(tbl)
        r <- nrow(tbl)
        c <- ncol(tbl)
        df_min <- min(r - 1, c - 1)
        
        if (df_min > 0) {
          v_cramer <- sqrt(chi$statistic / (n_obs * df_min))
          return(list(v = as.numeric(v_cramer), type = "Cramer's V", chi2 = as.numeric(chi$statistic)))
        }
      }
    }
    
    return(list(v = 0, type = "Cramer's V", chi2 = 0))
  }
  
  # NEW: Eta-squared calculation for numeric vs categorical
  calculate_eta_squared <- function(num_var, cat_var) {
    means_by_group <- tapply(num_var, cat_var, mean, na.rm = TRUE)
    overall_mean <- mean(num_var, na.rm = TRUE)
    n_groups <- tapply(num_var, cat_var, length)
    
    bss <- sum(n_groups * (means_by_group - overall_mean)^2, na.rm = TRUE)
    tss <- sum((num_var - overall_mean)^2, na.rm = TRUE)
    
    if (tss > 0) {
      eta <- sqrt(bss / tss)
      return(list(eta = eta, type = "Eta"))
    } else {
      return(list(eta = 0, type = "Eta"))
    }
  }
  
  make_table <- function(df, columns_defs) {
    reactable(
      df,
      columns = columns_defs,
      bordered = TRUE,
      striped = TRUE,
      highlight = TRUE,
      defaultPageSize = 25,
      showPageSizeOptions = TRUE,
      pageSizeOptions = c(25, 50),
      theme = reactableTheme(headerStyle = list(fontWeight = "bold"))
    )
  }
  
  # NEW : Function to solve optimal submatrix selection using MILP 
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
      rep(0, N + M),                    # u_i and v_j coefficients
      as.vector(t(contribution_matrix))  # z_ij coefficients - NOTE: transposed!
    )
    
    # Build constraint matrix
    n_constraints <- 2 + 3 * N * M  # 2 cardinality constraints + 3 constraints per z_ij
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
    constraint_matrix[constraint_index, (N+1):(N+M)] <- 1
    constraint_dir[constraint_index] <- "=="
    constraint_rhs[constraint_index] <- n
    constraint_index <- constraint_index + 1
    
    # Constraints for z_ij = u_i * v_j
    for (i in 1:N) {
      for (j in 1:M) {
        z_index <- N + M + (i-1)*M + j  # Index of z_ij variable
        
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
    v_values <- solution$solution[(N+1):(N+M)]
    
    # Use proper rounding for binary variables
    selected_rows <- which(round(u_values) == 1)
    selected_cols <- which(round(v_values) == 1)
    
    # Verify the solution
    actual_objective <- sum(contribution_matrix[selected_rows, selected_cols])
    
    # Debug output
    cat("MILP reported objective:", solution$objval, "\n")
    cat("Actual submatrix objective:", actual_objective, "\n")
    cat("Selected", length(selected_rows), "rows and", length(selected_cols), "columns\n")
    
    return(list(
      rows = selected_rows,
      cols = selected_cols,
      objective = solution$objval,
      actual_objective = actual_objective
    ))
  }
  
  # Heuristic fallback
  find_optimal_submatrix_heuristic <- function(contribution_matrix, n = 5) {
    # Your proven heuristic approach
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
    top_rows <- unique(contribution_df$row[1:min(n* n, nrow(contribution_df))])
    top_cols <- unique(contribution_df$col[1:min(n* n, nrow(contribution_df))])
    
    # Take top n from each
    if (length(top_rows) > n) top_rows <- top_rows[1:n]
    if (length(top_cols) > n) top_cols <- top_cols[1:n]
    
    objective_value <- sum(contribution_matrix[top_rows, top_cols, drop = FALSE])
    
    return(list(
      rows = top_rows,
      cols = top_cols,
      objective = objective_value
    ))
  }
  
  # Fonction to build contigency table
  build_contingency_table <- function(df, v1, v2, n = 5) {
    desc1 <- var_descriptions()$description[var_descriptions()$variable == v1]
    desc2 <- var_descriptions()$description[var_descriptions()$variable == v2]
    
    # Ensure df has the correct columns
    if (!v1 %in% colnames(df) || !v2 %in% colnames(df)) {
      return(div("Invalid variable names for contingency table", style = "color:red"))
    }
    
    # Create a version of the data with NAs removed for the contingency table
    df_clean <- df[complete.cases(df[, c(v1, v2)]), ]
    
    if (nrow(df_clean) == 0) {
      return(div("No valid data available", style = "padding: 20px; text-align: center;"))
    }
    
    tbl <- table(df_clean[[v1]], df_clean[[v2]])
    
    if (length(tbl) == 0) {
      return(div("No valid data available", style = "padding: 20px; text-align: center;"))
    }
    
    # Calculate chi-squared test
    chi_test <- tryCatch({
      chisq.test(tbl, simulate.p.value = TRUE)
    }, error = function(e) NULL)
    
    if (is.null(chi_test)) {
      return(div("Cannot compute chi-squared test for this table", style = "padding: 20px; text-align: center;"))
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
    
    # For small tables (≤25 cells), show the traditional table
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
              label <- format(val, big.mark = ",", decimal.mark = ".", scientific = FALSE)
              if (is_total) {
                return(label)
              }
              idx <- if (max_val > min_val) {
                max(1, min(100, floor(99 * (val - min_val) / (max_val - min_val)) + 1))
              } else {
                50
              }
              div(style = paste0("background-color:", pal[idx], "; padding:4px;"), label)
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
    filtered_contributions <- contribution_pct[selected_rows, selected_cols, drop = FALSE]
    
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
            intensity <- min(100, max(1, floor(99 * (as.numeric(contrib_value) / max_contribution)) + 1))
            pal <- colorRampPalette(c("#f0f8ff", "#0066cc"))(100)
            
            # Format the count (without decimal places)
            count_label <- format(as.numeric(value), big.mark = ",", decimal.mark = ".", scientific = FALSE)
            
            div(style = paste0("background-color:", pal[intensity], "; padding:4px; font-weight: bold;"), 
                count_label)
          }
        )
      }
    })
    names(column_defs) <- names(display_table)
    
    # Create the final output
    tagList(
      div(class = "reactable-title", desc2),
      div(style = "margin-bottom: 15px;",
          tags$p(style = "font-weight: bold; color: #333;",
                 paste("Optimal", n, "×", n, "submatrix with maximum contribution")),
          tags$p(style = "color: #666;",
                 sprintf("Selected cells account for %.2f%% of total chi-squared (%.2f/%.2f%%)",
                         total_selected_contribution, total_selected_contribution, total_table_contribution))
      ),
      make_table(display_table, column_defs),
      div(style = "margin-top: 15px; font-size: 0.9em; color: #666;",
          tags$p("Cell values show the number of individuals in each category combination."),
          tags$p("Color intensity represents the percentage contribution to the chi-squared statistic."),
          tags$p("Darker blue indicates higher contribution to the association strength."),
          tags$p(style = "font-style: italic;", 
                 "Selection optimized to maximize total contribution percentage.")
      )
    )
  }
  
}

shinyApp(ui, server)
