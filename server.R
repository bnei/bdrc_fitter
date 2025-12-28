library(shiny)
library(markdown)
library(tidyverse)
library(bdrc)

# Source helper functions from functions.R
source("functions.R")

server <- function(input, output) {

  output$README <- renderUI({
    if(file.exists("../README.md")) {
       includeMarkdown("../README.md")
    } 
    else {
      # when deployed, README is copied to the same deployment folder
      includeMarkdown("./README.md")
    }
  })
  
  output$formula <- renderUI({
    withMathJax("$$Q = a(H-c)^b$$")
  })
  
  rc_points <- reactive({
    req(input$points_upload)
    tryCatch({
      data <- readr::read_csv(input$points_upload$datapath, show_col_types = FALSE)
      shiny::validate(
        shiny::need(all(c("H", "Q") %in% names(data)), 
                   "CSV must contain 'H' and 'Q' columns exactly")
      )
      shiny::validate(shiny::need(nrow(data) > 1, 
                                 "CSV must contain at least 2 rows"))
      data
    }, error = function(e) {
      shiny::validate(shiny::need(FALSE, paste("Error reading file:", e$message)))
    })
  })

  output$rc_table <- renderTable(
    head(rc_points(), 10)
  )

  # Single reactive for the fitted models (handles any number of breakpoints)
  curve_fit <- eventReactive(input$triggerFit, {
    data <- rc_points()
    num_bp <- input$num_breakpoints
    
    if (num_bp == 0) {
      # No breakpoints: fit single model
      list(bdrc::plm(Q ~ H, data))
    } else {
      # Estimate breakpoints and fit multiple models
      breaks <- estimate_breaks(Q ~ H, data, num_breaks = num_bp)
      fit_multiple(Q ~ H, data, breaks)
    }
  })
  
  # Extract all parameters from models
  parameters <- reactive({
    models <- curve_fit()
    extract_parameters(models)
  })
  
  # Refine breakpoints if we have more than 1 segment
  refined_breaks <- reactive({
    params <- parameters()
    if (nrow(params) > 1) {
      refine_breakpoints(params)
    } else {
      numeric(0)
    }
  })
  
  # Generate rating curves for all segments
  curve_plot <- reactive({
    models <- curve_fit()
    
    # Combine all rating curves with segment labels
    all_curves <- map_dfr(seq_along(models), function(i) {
      if (!is.null(models[[i]])) {
        models[[i]]$rating_curve %>%
          mutate(segment = i)
      } else {
        NULL
      }
    })
    
    all_curves
  })
  
  output_params <- reactive({
    params <- parameters()
    refined <- refined_breaks()
    
    if (nrow(params) == 0) {
      tibble(
        Parameter = character(),
        Value = character()
      )
    } else if (nrow(params) == 1) {
      # Single segment - just parameters
      tibble(
        Parameter = c("a", "b", "c"),
        Value = c(
          round(params$a[1], 4),
          round(params$b[1], 4),
          round(params$c[1], 4)
        )
      )
    } else {
      # Multiple segments - show parameters and breakpoints
      param_rows <- params %>%
        pivot_longer(cols = c(a, b, c), names_to = "Parameter", values_to = "Value") %>%
        mutate(
          Parameter = paste0(Parameter, " (Segment ", segment, ")"),
          Value = round(Value, 4)
        ) %>%
        select(Parameter, Value)
      
      # Add breakpoint rows
      breakpoint_rows <- tibble(
        Parameter = paste0("Breakpoint ", seq_along(refined), " (H)"),
        Value = round(refined[!is.na(refined)], 4)
      )
      
      # Combine parameters and breakpoints
      bind_rows(param_rows, breakpoint_rows)
    }
  })
  
  output$rating_curve <- renderPlot({
    colors <- c("#1f77b4", "#d62728", "#2ca02c", "#ff7f0e", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f")
    
    # Build plot with observed data
    p <- ggplot(rc_points(), aes(x = Q, y = H)) +
      geom_point(aes(color = "Observed Data"), size = 2.5, alpha = 0.65)
    
    # Get the curve data
    curve_data <- curve_plot()
    models <- curve_fit()
    
    if (!is.null(curve_data) && nrow(curve_data) > 0) {
      # Add each segment's curve using map/Reduce pattern
      segment_layers <- map(unique(curve_data$segment), function(seg_id) {
        seg_data <- curve_data %>% filter(segment == seg_id)
        segment_label <- paste("Segment", seg_id)
        color <- colors[min(seg_id, length(colors))]
        
        geom_line(
          data = seg_data,
          aes(x = median, y = h, color = segment_label),
          linewidth = 1.3,
          inherit.aes = FALSE
        )
      })
      
      # Add layers to plot
      p <- Reduce(`+`, Filter(Negate(is.null), segment_layers), init = p)
      
      # Add refined breakpoint lines
      refined <- refined_breaks()
      if (length(refined) > 0 && sum(!is.na(refined)) > 0) {
        for (bp in refined[!is.na(refined)]) {
          p <- p + geom_hline(yintercept = bp, linetype = "dashed", color = "gray50", linewidth = 0.5, alpha = 0.7)
        }
      }
      
      # Build colors
      num_segs <- length(unique(curve_data$segment))
      segment_colors <- setNames(colors[1:num_segs], paste("Segment", 1:num_segs))
      all_colors <- c("Observed Data" = "gray20", segment_colors)
      
      # Finalize plot
      p +
        labs(
          title = "Discharge Rating Curve Fit",
          x = "Discharge Q [m³/s]",
          y = "Stage H [m]"
        ) +
        scale_color_manual(name = "Series", values = all_colors) +
        theme_minimal() +
        theme(
          legend.position = "right"
        )
    }
  })
  
  output$parameter_table <- renderTable(
    output_params()
  )
}

