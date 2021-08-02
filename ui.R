#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

source('helpers.R')

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Energy Efficiency"),
    navbarPage("Navigate",
        tabPanel("About",
                 p("https://archive.ics.uci.edu/ml/datasets/Energy+efficiency")
        ),
        tabPanel("Data",
                 downloadButton("downloadFull", "Download Full Dataset"),
                 br(),
                 p("\n\n"),
                 dataTableOutput("energyDataTable")
        ),
        tabPanel("Data Exploration",
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("plots",
                                     "Choose Summary/Plot",
                                     choices = c("Summary Statistics" = "summary_stats",
                                                 "Scatter Plot" = "scatter",
                                                 "Box Plot" = "box"),
                                     selected = NULL)
                         
                     ),
                     # show chosen plot or summary
                     mainPanel(
                         conditionalPanel(
                             condition = "input.plots == 'summary_stats'",
                             tableOutput("summaryStats")
                         ),
                         conditionalPanel(
                             condition =  "input.plots == 'scatter'",
                             varSelectInput("scatterVariable", "Variable:", energyData[, c(1:4)]),
                             plotOutput("scatterPlot")
                         ),
                         conditionalPanel(
                             condition =  "input.plots == 'box'",
                             sliderInput("HLRange", "Toggle Heating Load",
                                         min = floor(min(energyData$heating_load)),
                                         max = ceiling(max(energyData$heating_load)),
                                         value = c(quantile(energyData$heating_load)[2],
                                                   quantile(energyData$heating_load)[4]),
                                         step = 2
                             ),
                             varSelectInput("boxPlotVariable", "Variable:", energyData[, c(5, 10)]),
                             plotOutput("boxPlot")
                         )
                     )
                 )
        ),
        tabPanel("Modeling",
                 tabsetPanel(
                     tabPanel("Modeling Info",

                              
                     ),
                     tabPanel("Model Fitting",
                              sidebarLayout(
                                  sidebarPanel(
                                      sliderInput("trainSize", "Choose Train Set Size:",
                                                  min = 0, max = 1, value = 0.7),
                                      numericInput("cvFolds", "Number of Cross-Validation Folds",
                                                   value = 10),
                                      actionButton("fit_model", "Fit Model")
                                  ),
                                  mainPanel(
                                      verbatimTextOutput("modeling")
                                  )
                              )
                     ),
                     tabPanel("Prediction",
                              p("srgafe")
                     )
                 )
        )
    )
))
