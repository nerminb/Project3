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
    
    
    fitModel <- eventReactive(input$fit_model,{
        set.seed(1)
        energyIndex <- createDataPartition(energyData$heating_load, p = input$trainSize, list = FALSE)
        energyTrain <- energyData[energyIndex, ]
        energyTest <- energyData[-energyIndex, ]
        
        # Define training control
        trctrl <- trainControl(method = "cv", number = input$cvFolds)
        
        # Set seed for reproducible
        set.seed(5)
        
        # Linear Regression Model
        lmFit <- train(heating_load ~ .,
                       data = select(energyTrain, -c(compactness_level)),
                       method = "lm",
                       preProcess = c("center", "scale"),
                       trControl = trctrl)
        print(lmFit)
        print("-------------------------------")
        # Boosted Tree Model
        set.seed(5)
        boostFit <- train(heating_load ~.,
                          select(energyTrain, -c(compactness_level)),
                          method = "gbm",
                          trControl = trctrl,
                          preProcess = c("center", "scale"),
                          verbose = FALSE)
        print(boostFit)
    })
    
    output$modeling <- renderPrint({
        fitModel()
    })
    
})
