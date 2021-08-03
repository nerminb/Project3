# include code from external file
source('helpers.R')

shinyServer(function(input, output, session) {
    
    # -------------------------------- Data  -----------------------------------
    # data table with ability to toggle parameter values to subset data
    output$energyDataTable <- renderDataTable(
        datatable(energyData,
                  filter = "top"),
        server = FALSE
    )

    # download full dataset code
    output$downloadFull <- downloadHandler('energy_efficiency.csv',
                                           content = function(con) {
                                                write.csv(energyData, con)
                                            }
    )
    
    # ---------------------------- Data Exploration  ---------------------------
    
    # initialize reactive values to use later
    plots <- reactiveValues()
    ranges <- reactiveValues(x = NULL, y = NULL)
    
    observeEvent(input$plotRun, {
        # subset by columns we want to analyze
        energyStats <- energyData[ , c("relative_compactness",
                                       "surface_area",
                                       "wall_area",
                                       "roof_area",
                                       "heating_load")]
        
        # summary statistics
        summaryStats <- data.frame(do.call(cbind, lapply(energyStats, summary, digits = 3)))
        summaryStats$stat <- rownames(summaryStats)
        summaryStats <- summaryStats[ , c("stat", "relative_compactness", "surface_area",
                                          "wall_area", "roof_area", "heating_load")]
        colnames(summaryStats) <- c("Stat", "Relative Compactness", "Surface Area",
                                    "Wall Area", "Roof Area", "Heating Load")
        # save summary statistics as reactive value
        plots$summaryStats <- summaryStats
        
        # scatter plot with variable chosen by user
        plots$scatterPlot <- ggplot(energyData, aes(x = !!input$scatterVariable, y = heating_load)) +
            geom_point(stat = "identity") +
            geom_smooth(data = energyData, aes(x = !!input$scatterVariable, y = heating_load),
                        method = "lm") +
            labs(y = "Heating Load") +
            coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
        
        # box plot
        # subset data by heating load range picked by user
        subsetEnergyHL <- subset(energyData,
                                 heating_load >= input$HLRange[1] &
                                 heating_load <= input$HLRange[2])
        # convert to factor
        subsetEnergyHL$overall_height <- as.factor(subsetEnergyHL$overall_height)
        levels(subsetEnergyHL$overall_height) <- list("Low" = 3.5,
                                                      "High" = 7)
        # save box plot as reactive value
        g <- ggplot(subsetEnergyHL, aes(x = !!input$boxPlotVariable, y = heating_load))
        plots$boxPlot <- g + geom_boxplot() +
            geom_point(aes(col = !!input$boxPlotVariable), alpha = 1, size = 1, position = "jitter") +
            labs(title = "Boxplot for Heating Load by Overall Height or Relative Compactness",
                 y = "Heating Load")
    })
    
    # download code for scatter plot
    output$downloadScatterPlot <- downloadHandler(
        filename = "energy-efficiency-scatterplot.png",
        content = function(file) {
            ggsave(file, plot = plots$scatterPlot)
        }
    )
    # download code for box plot
    output$downloadBoxPlot <- downloadHandler(
        filename = "energy-efficiency-boxplot.png",
        content = function(file) {
            ggsave(file, plot = plots$boxPlot)
        }
    )
    
    # render tables and plots
    output$summaryStats <- renderTable({
        plots$summaryStats
    })
    output$scatterPlot <- renderPlot({
        plots$scatterPlot
    })
    output$boxPlot <- renderPlot({
        plots$boxPlot
    })
    output$corrPlot <- renderPlot({
        cm <- cor(energyData[, c("relative_compactness", "surface_area", "wall_area",
                                 "roof_area", "heating_load")])
        corrplot(round(cm, 2), method="circle")
    })
    
    # zoom code
    observeEvent(input$plot_dblclick, {
        brush <- input$plot_brush
        if (!is.null(brush)) {
            ranges$x <- c(brush$xmin, brush$xmax)
            ranges$y <- c(brush$ymin, brush$ymax)
            
        } else {
            ranges$x <- NULL
            ranges$y <- NULL
        }
    })
    
    # ------------------------------- Modeling  --------------------------------
    
    # About page mathJax() MLR function
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
        # set seed for reproducible
        set.seed(1)
        # partition data to training and test sets
        energyIndex <- createDataPartition(energyData$heating_load, p = input$trainSize, list = FALSE)
        energyTrain <- energyData[energyIndex, ]
        energyTest <- energyData[-energyIndex, ]
        # define training control and allow user input for # of CV folds
        trctrl <- trainControl(method = "cv", number = input$cvFolds)
        
        # create model formula based on variables user selected
        modelVars <- paste(input$varsToUse, collapse = "+")
        modelFormula <- as.formula(paste('heating_load ~', modelVars))
        
        # linear regression model fit
        set.seed(5)
        lmFit <- train(modelFormula,
                       data = select(energyTrain, -c(compactness_level)),
                       method = "lm",
                       preProcess = c("center", "scale"),
                       trControl = trctrl)
        
        # boosted tree model fit
        set.seed(5)
        boostFit <- train(modelFormula,
                          data = select(energyTrain, -c(compactness_level)),
                          method = "gbm",
                          trControl = trctrl,
                          preProcess = c("center", "scale"),
                          verbose = FALSE)
        
        # random forest model fit
        set.seed(5)
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

        # find the best model with lowest RMSE value
        bestModel <- rownames(testResults[testResults$RMSE == min(testResults$RMSE), ])
        
        # save fit results and comparisons
        values$lmFit <- lmFit
        values$boostFit <- boostFit
        values$rfFit <- rfFit
        values$testResults <- testResults
        values$bestModel <- bestModel
    })
    
    # show modeling results and comparisons
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
    
    # output predictions
    observeEvent(input$predict_values, {
        # user selected parameter values
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
        # convert to factors
        cols <- c("orientation", "glazing_area_dist")
        pred_values[cols] <- lapply(pred_values[cols], factor)
        
        # predict heating load
        if (input$modelPredChoice == "Linear Regression") {
            predResult <- predict(values$lmFit, newdata = pred_values)
        } else if (input$modelPredChoice == "Boosted Tree") {
            predResult <- predict(values$boostFit, newdata = pred_values)
        } else if (input$modelPredChoice == "Random Forest") {
            predResult <- predict(values$rfFit, newdata = pred_values)
        }
        # save prediction as reactive value
        values$predResult <- predResult
    })
    # show prediction result
    output$prediction <- renderText({
        values$predResult
    })
})
