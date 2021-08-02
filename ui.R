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
                                      checkboxGroupInput("varsToUse", "Choose Variables for Model",
                                                         choices = c("relative_compactness",
                                                                     "surface_area",
                                                                     "wall_area",
                                                                     "roof_area",
                                                                     "overall_height",
                                                                     "orientation",
                                                                     "glazing_area",
                                                                     "glazing_area_dist"),
                                                         selected = c("relative_compactness",
                                                                     "surface_area",
                                                                     "wall_area",
                                                                     "roof_area",
                                                                     "overall_height",
                                                                     "orientation",
                                                                     "glazing_area",
                                                                     "glazing_area_dist")
                                                         ),
                                      actionButton("fit_model", "Fit Model")
                                  ),
                                  mainPanel(
                                      h4("Linear Regression Model Fit Result Summary"),
                                      verbatimTextOutput("lmFitResults"),
                                      h4("Boosted Tree Model Fit Result Summary"),
                                      verbatimTextOutput("boostFitResults"),
                                      h4("Random Forest Fit Result Summary"),
                                      verbatimTextOutput("rfFitResults"),
                                      h4("Fit Statistics For Each Model Comparing to Test Set"),
                                      tableOutput("fitStatistics"),
                                      h4("Best Model To Use"),
                                      verbatimTextOutput("finalModel"),
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
