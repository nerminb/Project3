library(shiny)

source('helpers.R')

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    output$energyDataTable <- renderDataTable(
        datatable(energyData,
                  filter = "top"),
        server = FALSE
    )
    output$filtered_row <- 
        renderPrint({
            input[["dt_rows_all"]]
        })
    output$download_filtered <- 
        downloadHandler(
            filename = "Filtered Data.csv",
            content = function(file){
                write.csv(energyData[input[["dt_rows_all"]], ],
                          file)
            }
        )
    output$downloadFull <- downloadHandler('energy_efficiency.csv',
                                            content = function(con) {
                                                write.csv(energyData, con)
                                            }
    )
    # Data Exploration: Summary Statistics
    output$summaryStats <- renderTable({
        # Subset by columns we want to analyze
        energyStats <- energyData[ , c("relative_compactness",
                                       "surface_area",
                                       "wall_area",
                                       "roof_area",
                                       "heating_load")]
        
        # Function for summary statistics for energy stats
        summaryStats <- data.frame(do.call(cbind, lapply(energyStats, summary, digits = 3)))
        summaryStats$stat <- rownames(summaryStats)
        summaryStats <- summaryStats[ , c("stat", "relative_compactness", "surface_area",
                                          "wall_area", "roof_area", "heating_load")]
        colnames(summaryStats) <- c("Stat", "Relative Compactness", "Surface Area",
                                    "Wall Area", "Roof Area", "Heating Load")
        summaryStats
    })
    # Data Exploration: Scatter Plots
    output$scatterPlot <- renderPlot({
        ggplot(energyData, aes(x = !!input$scatterVariable, y = heating_load)) +
            geom_point(stat = "identity") +
            geom_smooth(data = energyData, aes(x = !!input$scatterVariable, y = heating_load),
                        method = "lm") +
            labs(x = "Surface Area", y = "Heating Load")
    })
    # Data Exploration: Box Plots
    output$boxPlot <- renderPlot({
        subsetEnergyHL <- subset(energyData,
                                 heating_load >= input$HLRange[1] &
                                 heating_load <= input$HLRange[2])
        subsetEnergyHL$overall_height <- as.factor(subsetEnergyHL$overall_height)
        levels(subsetEnergyHL$overall_height) <- list("Low" = 3.5,
                                                      "High" = 7)
        
        # Numerical summaries
        g <- ggplot(subsetEnergyHL, aes(x = !!input$boxPlotVariable, y = heating_load))
        g + geom_boxplot() +
            geom_point(aes(col = !!input$boxPlotVariable), alpha = 1, size = 1, position = "jitter") +
            labs(title = "Boxplot for Heating Load by Overall Height or Relative Compactness",
                 y = "Heating Load")
    })
    
    # Modeling
    
    # About mathJax formulas
    output$MLRFormula <- renderUI({
        withMathJax(
            helpText('Multiple Linear Regression equation:
                     $$
                     y_i = \\beta_0\\ + \\beta_1x_{i1} + \\beta_2x_{i2} + ... + \\beta_px_{ip} + 
                     \\epsilon\\
                     $$'
            ),
            helpText('$$y_i = response$$'),
            helpText('$$x_i = predictor$$'),
            helpText('$$\\beta_0 = y-intercept$$'),
            helpText('$$\\beta_p = slope$$'),
            helpText('$$\\epsilon = error$$')
         )
    })
    
    values <- reactiveValues()
    
    observeEvent(input$fit_model,{
        set.seed(1)
        energyIndex <- createDataPartition(energyData$heating_load, p = input$trainSize, list = FALSE)
        energyTrain <- energyData[energyIndex, ]
        energyTest <- energyData[-energyIndex, ]
        
        # Define training control
        trctrl <- trainControl(method = "cv", number = input$cvFolds)
        
        # Set seed for reproducible
        set.seed(5)
        
        modelVars <- paste(input$varsToUse, collapse = "+")
        modelFormula <- as.formula(paste('heating_load ~', modelVars))
        
        # Linear Regression Model
        lmFit <- train(modelFormula,
                       data = select(energyTrain, -c(compactness_level)),
                       method = "lm",
                       preProcess = c("center", "scale"),
                       trControl = trctrl)
        
        # Boosted Tree Model
        set.seed(5)
        boostFit <- train(modelFormula,
                          data = select(energyTrain, -c(compactness_level)),
                          method = "gbm",
                          trControl = trctrl,
                          preProcess = c("center", "scale"),
                          verbose = FALSE)
        
        # Random Forest Model
        set.seed(5)
        # Fit the random forest model on training set
        rfFit <- train(modelFormula,
                       data = select(energyTrain, -c(compactness_level)),
                       method = "rf",
                       preProcess = c("center", "scale"),
                       trControl = trctrl)
        
        # predict on test set
        predfitLm <- predict(lmFit, newdata = energyTest)
        predfitBoost <- predict(boostFit, newdata = energyTest)
        predfitRF <- predict(rfFit, newdata = energyTest)
        
        # evaluate the model performances by comparing the testing RMSE values
        testResults <- rbind(postResample(predfitLm, energyTest$heating_load),
                             postResample(predfitBoost, energyTest$heating_load),
                             postResample(predfitRF, energyTest$heating_load))
        testResults <- data.frame(Model = c("Linear Regression", "Boosted Tree", "Random Forest"),
                                  testResults)
        row.names(testResults) <- c("Linear Regression",
                                    "Boosted Tree",
                                    "Random Forest")

        # Find the best model with lowest RMSE value
        bestModel <- rownames(testResults[testResults$RMSE == min(testResults$RMSE), ])
        
        values$lmFit <- lmFit
        values$boostFit <- boostFit
        values$rfFit <- rfFit
        values$testResults <- testResults
        values$bestModel <- bestModel
    })
    
    # Output Modeling Results and Comparisons
    output$lmFitResults <- renderPrint({
        values$lmFit
    })
    output$boostFitResults <- renderPrint({
        values$boostFit
    })
    output$rfFitResults <- renderPrint({
        values$rfFit
    })
    output$fitStatistics <- renderTable({
        values$testResults[] <- lapply(values$testResults, format, decimal.mark = ",", digits = 5)
        values$testResults
    })
    output$finalModel <- renderPrint({
        values$bestModel
    })
    
    # Output Predictions
    observeEvent(input$predict_values, {
        pred_values <- data.frame(
            relative_compactness = input$relative_compactnessPred,
            surface_area = input$surface_areaPred,
            wall_area = input$wall_areaPred,
            roof_area = input$roof_areaPred,
            overall_height = as.numeric(input$overall_heightPred),
            orientation = input$orientationPred,
            glazing_area = as.numeric(input$glazing_areaPred),
            glazing_area_dist = input$glazing_area_distPred
        )
        cols <- c("orientation", "glazing_area_dist")
        pred_values[cols] <- lapply(pred_values[cols], factor)
        
        if (input$modelPredChoice == "Linear Regression") {
            predResult <- predict(lmFit, newdata = pred_values)
        } else if (input$modelPredChoice == "Boosted Tree") {
            predResult <- predict(boostFit, newdata = pred_values)
        } else if (input$modelPredChoice == "Random Forest") {
            predResult <- predict(rfFit, newdata = pred_values)
        }
        values$predResult <- predResult
    })
    output$prediction <- renderText({
        values$predResult
    })
})
