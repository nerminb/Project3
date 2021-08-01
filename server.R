#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

source('helpers.R')

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    subsetSA <- reactive({
        subsetEnergy <- subset(energyData,
                               surface_area >= input$SARange[1] &
                               surface_area <= input$SARange[2] &
                               wall_area >= input$WARange[1] &
                               wall_area <= input$WARange[2] &
                               roof_area >= input$RARange[1] &
                               roof_area <= input$RARange[2] &
                               orientation %in% input$orientationChoices)

        subsetEnergy
    })
    
    output$tbl <- renderDataTable({
        subsetSA()
    })
    # download data as csv
    output$downloadSubset <- downloadHandler('energy_efficiency_subset.csv',
                                              content = function(con) {
                                                write.csv(subsetSA(), con)
                                              }
    )
    output$downloadFull <- downloadHandler('energy_efficiency.csv',
                                            content = function(con) {
                                                write.csv(energyData, con)
                                            }
    )
})
