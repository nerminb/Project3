# load libraries
library(shiny)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(caret)
library(DT)

# read data
energyData <- read_csv("energy_efficiency.csv")

# remove undesirable response variable, we want to focus on heating_load
energyData <- select(energyData, -cooling_load)

# convert categorical variables to factors
cols <- c("orientation", "glazing_area_dist")
energyData[cols] <- lapply(energyData[cols], factor)

# create new variable for compactness level
energyData$compactness_level <- cut(energyData$relative_compactness, c(0.6, 0.68, 0.76, 0.84, 0.92, 1))
levels(energyData$compactness_level) = c("Very Low", "Low", "Medium", "High", 'Very High')
