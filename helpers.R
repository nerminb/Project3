library(tidyverse)
library(dplyr)
library(ggplot2)
library(caret)
library(DT)

energyData <- read_csv("energy_efficiency.csv")
energyData <- select(energyData, -cooling_load)
