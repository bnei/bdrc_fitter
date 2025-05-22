library(shiny)
library(shinythemes)
library(tidyverse)
library(bdrc)
library(bslib)

ui <- fluidPage(

  theme = shinytheme("yeti"),
  tags$head(HTML("<title>Bayesian Discharge Rating Curve Fitter</title>")),

  titlePanel("Simple Rating Curve Fitter"),

  mainPanel(

    tabsetPanel(

      tabPanel("Read Me", htmlOutput("README")),

      tabPanel(
        "Curve Fitter",
        # Sidebar with file upload and action button
        sidebarLayout(
          sidebarPanel(
            width = 3,
            fileInput("points_upload",
                      "Upload Stage-Discharge Points:",
                      accept = ".csv"),
            actionButton("triggerFit",
                         "Fit Curve",
                         class = "btn btn-primary"),
            p("Click button once and wait."),
            tableOutput("rc_table"),
          ),
          # Main panel for displaying results
          mainPanel(
            uiOutput("formula"),
            br(),
            plotOutput("rating_curve"),
            br(),
            tableOutput("parameter_table")
          )
        )
      )
    )
  )
)