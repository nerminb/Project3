# Project3

## Energy Efficiency in Buildings

This app analyzes energy efficiency in buildings using a host of building parameters. The app is broken up by tabs: About, Data, Data Exploration, and Modeling. The source and description of the data being used can be found in the About tab of the app. This interactive app will allow you to scroll through and filter the data being used, download the data in CSV format, plot several numerical and graphical summaries and plots of the data, fit the data based on several linear and ensemble models, and predict the heating load (the amount of heat energy that would need to be added to a space to maintain the temperature in an acceptable range). You are able to toggle which variable you want to use when fitting the model. This app contains useful background information, definitions, benefits, and drawbacks of the different types of linear regression and ensemble models being used.

### Install Packages

```{r}
install.packages(c("shiny", "tidyverse", "dplyr", "ggplot2", "caret", "DT"))
```

### Packages required to run app

```{r}
library(shiny)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(caret)
library(DT)
library(corrplot)
```

### Run below code in RStudio to run this app

```{r}
shiny::runGitHub("Project3", "nerminb", ref="main")
```
