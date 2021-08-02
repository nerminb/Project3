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
                 h3("Purpose of App"),
                 p("The purpose of this app is to analyze building paramters and the energy efficency
                   based on these parameters. The response variable in this app is the heating load
                   requirements of buildings to measure energy efficiency as a function of building
                   paramters. You can use this app to plots several graphs and numerical summaries
                   of the building parameters and heating load, fit regression and ensemble models,
                   compare these models to find the best one, and predict heating load based on
                   building parameter value inputs."),
                 h3("More About the Data"),
                 p("The data is made available by the UCI Machine Learning Repository."),
                 a("Click here to view more information and the source of the data",
                   href = "https://archive.ics.uci.edu/ml/datasets/Energy+efficiency",
                   target = "_blank"),
                 br(),
                 p("The dataset contains 8 building parameters (predictor variables). They are:
                 Surface Area, Wall Area, Roof Area, Overall Height, Orientation, Glazing Area,
                 and Glazing Area Distribution. The response variables are Heating Load and Cooling
                 Load. For the purpose of this app, we are focusing on Heating Load.
                 The heating load is the amount of heat energy that would need to be added to a
                 space to maintain the temperature in an acceptable range."),
                 a("Click here for more information about heating and cooling loads",
                   href = "https://basix.nsw.gov.au/iframe/thermal-help/heating-and-cooling-loads.html",
                   target = "blank"),
                 h3("App Components"),
                 p("This app contains four tabs: About, Data, Data Exploration, Modeling"),
                 p("The About tab contains some background information about the data this app is
                   based around. The Data tab contains the actual data, which you can download in CSV
                   format. In the Data tab, you can also filter the data based on column values.
                   The Data Exploration tab contains several numerical summaries and plots on the data.
                   This tab is interactive and you can toggle between the variables you want to
                   analyze. In the Modeling tab, you can fit several linear and ensemble models based
                   on the response variable Heating Load. Here, you can also predict a heating load
                   based on user inputs. This tab also contains background information on each model
                   being used."),
                 img(width = "500px",
                     src = "https://github.com/nerminb/Project3/blob/main/heading-load-picture.png"),
                 p("(Picture is from: https://www.finehomebuilding.com/2019/02/04/choose-right-size-heating-cooling-system)")
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
                              sidebarLayout(
                                  sidebarPanel(
                                      h3("Options"),
                                      p("Note: Fit models in previous tab before predicting."),
                                      br(),
                                      selectInput("modelPredChoice",
                                                   "Choose Model",
                                                   choices = c("Linear Regression",
                                                               "Boosted Tree",
                                                               "Random Forest"),
                                                   selected = "Linear Regression"),
                                       sliderInput("relative_compactnessPred",
                                                   "Relative Compactness",
                                                   min = 0.5,
                                                   max = 1,
                                                   value = 0.75,
                                                   step = 0.02),
                                       numericInput("surface_areaPred",
                                                    "Surface Area",
                                                    value = 750),
                                       numericInput("wall_areaPred",
                                                    "Wall Area",
                                                    value = 300),
                                       numericInput("roof_areaPred",
                                                    "Roof Area",
                                                    value = 200),
                                       selectInput("overall_heightPred",
                                                   "Overall Height",
                                                   choices = c(3.5, 7),
                                                   selected = 3.5),
                                       selectInput("orientationPred",
                                                   "Orientation",
                                                   choices = c(2, 3, 4, 5),
                                                   selected = 2),
                                       selectInput("glazing_areaPred",
                                                   "Glazing Area",
                                                   choices = c(2, 3, 4, 5),
                                                   selected = 2),
                                       sliderInput("glazing_areaPred",
                                                   "Glazing Area",
                                                   min = 0,
                                                   max = 0.5,
                                                   value = 0.25,
                                                   step = 0.02),
                                       selectInput("glazing_area_distPred",
                                                   "Glazing Area Distribution",
                                                   choices = c(0:5),
                                                   selected = 0),
                                       actionButton("predict_values", "Predict")
                                  ),
                                  mainPanel(
                                      h3("Heating Load Prediction"),
                                      textOutput("prediction")
                                  )
                              )
                     )
                 )
        )
    )
))
