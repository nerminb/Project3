# include code from external file
source('helpers.R')

shinyUI(fluidPage(
    # application title
    titlePanel("Energy Efficiency in Buildings"),
    # top-level navigation
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
                 br(),br(),
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
                     src = "https://github.com/nerminb/Project3/blob/main/heating-load-picture.png?raw=true"),
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
                         # select which plot to display
                         selectInput("plots",
                                     "Choose Summary/Plot",
                                     choices = c("Summary Statistics" = "summary_stats",
                                                 "Scatter Plot" = "scatter",
                                                 "Box Plot" = "box",
                                                 "Correlation Plot" = "corrplot"),
                                     selected = NULL),
                         actionButton("plotRun", "Plot")
                     ),
                     mainPanel(
                         # show chosen plot or summary
                         conditionalPanel(
                             condition = "input.plots == 'summary_stats'",
                             # display summary statistics
                             h3("Summary Statistics"),
                             hr(),
                             tableOutput("summaryStats")
                         ),
                         conditionalPanel(
                             condition =  "input.plots == 'scatter'",
                             # plot scatter plot
                             h3("Scatterplot"),
                             hr(),
                             p("Scatterplots show us relationships between two parameters.
                               The plot allows us to visually notice any positive or negative
                               relationships between parameters as well as the strength of the
                               relationship."),
                             hr(),
                             p("Select variable below and click Plot."),
                             p("To zoom in: drag inside the plot to create a zoom area,
                               double-click the zoom area, click Plot button in side panel.
                               To reset the plot, double-click on plot and press Plot."),
                             hr(),
                             # variable selection
                             varSelectInput("scatterVariable", "Variable:", energyData[, c(1:4)]),
                             # allow for zooming
                             plotOutput("scatterPlot", height = 300,
                                        dblclick = "plot_dblclick",
                                        brush = brushOpts(
                                            id = "plot_brush",
                                            resetOnNew = TRUE
                                        )
                             ),
                             # download plot
                             downloadButton("downloadScatterPlot", "Download Plot")
                         ),
                         conditionalPanel(
                             condition =  "input.plots == 'box'",
                             # plot boxplot
                             h3("Box Plot"),
                             hr(),
                             p("Box plots shows us the overall spread and variability of
                             the data. Summary statistics are mapped, like mean, minimum,
                             maximum, and interquartile ranges. We can notice any outliers
                             by looking at the farthest dots in the plot."),
                             hr(),
                             p("Toggle heating load, select variable below, and click Plot."),
                             hr(),
                             # toggle parameter values
                             sliderInput("HLRange", "Toggle Heating Load",
                                         min = floor(min(energyData$heating_load)),
                                         max = ceiling(max(energyData$heating_load)),
                                         value = c(quantile(energyData$heating_load)[2],
                                                   quantile(energyData$heating_load)[4]),
                                         step = 2
                             ),
                             # variable selection
                             varSelectInput("boxPlotVariable", "Variable:", energyData[, c(5, 10)]),
                             plotOutput("boxPlot"),
                             # download plot
                             downloadButton("downloadBoxPlot", "Download Plot"),
                             br(), br()
                         ),
                         conditionalPanel(
                             condition =  "input.plots == 'corrplot'",
                             # show correlations
                             h3("Correlation Plot"),
                             hr(),
                             p("We are able to see relationships between each parameter which
                             each other in the below correlation matrix. Blue color indicates a
                             positive effect between two parameters and red color indicates a
                             negative effect"),
                             hr(),
                             plotOutput("corrPlot")
                         )
                     )
                 )
        ),
        tabPanel("Modeling",
                 tabsetPanel(
                     tabPanel("Modeling Info",
                              withMathJax(),
                              h3("Multiple Linear Regression"),
                              p("Multiple linear regression models are statistical techniques used to 
                              fit the relationship between predictor variables and response
                              variables.  We can also use fitted multiple regression models to predict
                              future values. Linear regression models can contain multiple variables
                              that may interact with each other - we can add interaction terms to
                              improve linear regression models."),
                              uiOutput("MLRFormula"),
                              p("A benefit to multiple linear regression models is that they are
                                simple to model compared to other more complex models. We also have
                                the ability to easily see the relative influence each predictor
                                variables has on the response variable. This means multiple linear
                                regression models are easy to interpret. Another advantage is that 
                                modeling multiple linear regression is very fast because it doesn't
                                involve too many calculations. This is especially useful if the
                                dataset is too large. Several drawbacks include: MLR assumes
                                homoskedacity, is too simplistic when dealing with complex real-world
                                data, and tends to be affected by outliers easily."),
                              h3("Regression Tree"),
                              p("Tree-based methods involve splitting predictor spaces into regions,
                                and there are different predictions for each region. We use a
                                classification tree if we want to classify membership
                                to specific groups. We use a regression tree if we want to predict a
                                continuous response - we typically use mean of observations as
                                prediction for a given region. In this app, we are using a boosted tree
                                classification. Boosting trees grow sequentially, with each subsequent
                                tree grown on a modified version of the original data. Predictions
                                update as trees grow. The trees train slowly so there is no overfit.
                                A benefit to boosted trees is it curbs over-fitting. Regression trees
                                are simple to understand and it is easy to interpret output. Predicors
                                don't need to be scaled. There is built-in variable selection with this
                                method and there are no statistical assumptions necessary. A drawback
                                is that small changes in the data can change the tree by a lot. Another
                                drawback is that greedy algorithms are necessary and we often need to
                                prune."),
                              h3("Random Forest"),
                              p("Random forests extend the idea of bagging. With random forests
                                we create multiple trees from bootstrap samples and then average the
                                results. A benefit to this is it improves predictions because random
                                forests use a random subset of predictors for each bootstrap, so the
                                trees are more independent. If there is a strong predictor, each
                                bootstrap tree would likely use it for the 1st split, making the bagged
                                trees predictions more correlated as well. The biggest drawback to
                                random forests is that we lose interpretability as the model gets more
                                complex. While we gain in prediction, we lose in interpretability."),
                              br(), br(), br()
                     ),
                     tabPanel("Model Fitting",
                              sidebarLayout(
                                  sidebarPanel(
                                      h3("Options"),
                                      # toggle model fitting options
                                      sliderInput("trainSize", "Choose Train Set Size:",
                                                  min = 0, max = 1, value = 0.7),
                                      numericInput("cvFolds", "Number of Cross-Validation Folds",
                                                   value = 10),
                                      # variable selection
                                      checkboxGroupInput("varsToUse",
                                                         "Choose Variables for Model. All variables
                                                         selected by default. Un-check variables you
                                                         do not want to use in the model.",
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
                                      h3("Model Fitting"),
                                      hr(),
                                      p("Note: The NULL values are placeholders until model fitting
                                        is complete. Model fitting typically takes less than 30
                                        seconds when CV fold is set to 10."),
                                      hr(),
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
                                      # input parameter values in order to predict heating load
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
                                      hr(),
                                      p("Note: Fit models in previous tab before trying to predict."),
                                      p("Predicted heating load value will display below."),
                                      hr(),
                                      textOutput("prediction")
                                  )
                              )
                     )
                 )
        )
    )
))
