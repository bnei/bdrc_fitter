library(shiny)
library(shinythemes)
library(tidyverse)
library(bdrc)
library(bslib)

ui <- fluidPage(
  
  theme = shinytheme("yeti"),
  tags$head(HTML("<title>Bayesian Discharge Rating Curve Fitter</title>")),
  
  titlePanel("Bayesian Discharge Rating Curve Fitter"),
  
  mainPanel(
    
    tabsetPanel(
      
      tabPanel("Read Me", htmlOutput("README")),
      
      tabPanel("Curve Fitter",
               sidebarLayout(
                 
                 sidebarPanel(width = 3, 
                              fileInput("points_upload", 
                                        "Upload Stage-Discharge Points:", 
                                        accept = ".csv"),
                              
                              actionButton("triggerFit", 
                                           "Fit Curve (CLICK ONCE AND WAIT)", 
                                           class = "btn btn-primary"),
                              
                              tableOutput("rc_table"),
                              ),
                 
                 mainPanel(
                   uiOutput("formula"),
                   br(),
                   plotOutput("rating_curve"),
                   br(),
                   tableOutput("parameter_table")))
      )
    )
  )
)