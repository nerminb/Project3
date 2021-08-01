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
                 
                 https://archive.ics.uci.edu/ml/datasets/Energy+efficiency
        ),
        tabPanel("Data",
                 # Sidebar with a slider input for number of bins
                 sidebarLayout(
                     sidebarPanel(
                         sliderInput("SARange", "Surface Area",
                                     min = floor(min(energyData$surface_area)),
                                     max = ceiling(max(energyData$surface_area)),
                                     value = c(quantile(energyData$surface_area)[2],
                                               quantile(energyData$surface_area)[4]),
                                     step = 30
                         ),
                         sliderInput("WARange", "Wall Area",
                                     min = floor(min(energyData$wall_area)),
                                     max = ceiling(max(energyData$wall_area)),
                                     value = c(quantile(energyData$wall_area)[2],
                                               quantile(energyData$wall_area)[4]),
                                     step = 20
                         ),
                         sliderInput("RARange", "Roof Area",
                                     min = floor(min(energyData$roof_area)),
                                     max = ceiling(max(energyData$roof_area)),
                                     value = c(quantile(energyData$roof_area)[2],
                                               quantile(energyData$roof_area)[4]),
                                     step = 10
                         ),
                         checkboxGroupInput("orientationChoices",
                                            "Orientation",
                                            choices = c(2, 3, 4, 5),
                                            selected = c(2, 3, 4, 5)
                         ),
                         h4("Download Subset"),
                         downloadButton("downloadSubset", "Download"),
                         h4("Download Full Dataset"),
                         downloadButton("downloadFull", "Download")
                     ),
                     
                     # Show a plot of the generated distribution
                     mainPanel(
                         dataTableOutput("tbl")
                     )
                 )
        )
    )
))
