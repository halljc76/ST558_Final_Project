# ST 558 Final Project: Classifying Forest Cover Types

## Note to the Instructor(s)

Stratified sampling was used to reduce the dataset used for modeling down to 150K
rows from the original 581K -- the random forest still takes a considerable time to 
train!

## Purpose of Application

This application is the final project of the Fall 2023 course ST 558: Data 
Science for Statisticians at North Carolina State University. It houses
an exploration into a dataset involving soil cover types in forests through
both an interactive and predictive medium, allowing users the ability to 
fit, test, and evaluate appropriate statistical models.

Within the application, users may interact with the data through an explorative 
and predictive medium, investigating univariate and bivariate relationships in 
an effort to identify, and then train models on, predictors they believe could
be significant in achieving correct classifications of soil cover types.

More information regarding the background of the dataset can be found in the first tab of the application.
(This will download the original data dictionary.)

## Packages Needed for the Application

The following packages were installed under `R`, version 4.3.1:

- shiny
- ggplot2
- dplr
- shinyalert
- caret
- nnet
- DT
- tidyr
- shinythemes
- shinyWidgets

The code below installs these packages:

```r 
install.packages(c("shiny", "ggplot2", "dplyr", "shinyalert", "caret", "nnet",
"DT", "tidyr", "shinythemes", "shinyWidgets"))
```

The code below loads these packages into the session:


```r 
library(shiny)
library(ggplot2)
library(dplyr)
library(shinyalert)
library(caret)
library(nnet)
library(DT)
library(tidyr)
library(shinythemes)
library(shinyWidgets)
```

## Run this Application from This Repository

```r
shiny::runGitHub("halljc76/ST558_Final_Project", subdir = "app/")
```