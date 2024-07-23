library(shiny)
library(markdown)
library(tidyverse)
library(bdrc)

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
    read_csv(input$points_upload$datapath)
  })

  output$rc_table <- renderTable(
    head(rc_points(), 10)
  )

  curve.fit <- eventReactive(input$triggerFit, {
    plm(Q ~ H, rc_points())
  })
  
  curve_plot <- reactive({
    curve.fit()$rating_curve
  })
  
  parameters <- reactive({
    fitted_rc <- curve.fit()
    
    list(
      a = fitted_rc$param_summary$median[[1]],
      b = fitted_rc$param_summary$median[[2]],
      c = fitted_rc$param_summary$median[[3]]
    )
  })
  
  output_params <- reactive({
    params <- parameters()
    
    a <- round(params$a, 4)
    b <- round(params$b, 4)
    c <- round(params$c, 4)
    
    tribble(
      ~Parameter, ~Value,
      "a", a,
      "b", b,
      "c", c
    )
  })
  
  output$rating_curve <- renderPlot(
    ggplot() +
      geom_point(data = rc_points(), aes(Q, H)) +
      geom_line(data = curve_plot(), aes(median, h)) +
      geom_line(data = curve_plot(), aes(upper, h), linetype = "dotted") +
      geom_line(data = curve_plot(), aes(lower, h), linetype = "dotted"))
  
  output$parameter_table <- renderTable(
    output_params()
  )
}
