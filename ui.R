library(shiny)
library(shinythemes)
library(tidyverse)
library(bdrc)

ui <- fluidPage(
  
  theme = shinytheme("yeti"),
  tags$head(HTML("<title>Bayesian Discharge Rating Curve Fitter</title>")),
  
  titlePanel("Bayesian Discharge Rating Curve Fitter"),
  
  mainPanel(
    
    tabsetPanel(
      
      tabPanel("Read Me", htmlOutput("README")),
      
      tabPanel("Curve Fitter",
               br(),

               fluidRow(
               fileInput("points_upload", "Upload Stage-Discharge Points:", 
                         accept = ".csv"),
               uiOutput("formula")),
               br(),
               fluidRow(
                 actionButton("triggerFit", "Fit Curve (CLICK ONCE AND WAIT)", 
                              class = "btn btn-primary")),
               fluidRow(
                 column(width = 3, tableOutput("rc_table")),
                 column(width = 7, plotOutput("rating_curve"))),
               
               fluidRow(
                 tableOutput("parameter_table"))
      )
    )
  )
)