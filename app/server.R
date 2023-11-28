library(shiny)
library(ggplot2)
library(dplyr)

shinyServer(
  function(input, output, session) {
    values <- reactiveValues(data = NULL, conditionIndex = 1)
    
    observe({
      if (is.null(values$data)) {
        values$data <- read.table(gzfile("data/covtype.data.gz"),header = F,sep = ",")
        output$conditionButton <- renderUI({
          fluidRow(
            actionButton("addFilterCond", label = "Click!",
                         class = "btn-primary")
          )
        })
      }
    })
    
    observeEvent(input$addFilterCond, {
        output$conditions <- insertUI(selector = "#conditions", 
          ui = fluidRow(selectInput(inputId = paste0("filterCond", toString(values$conditionIndex)),
                      choices = colnames(data), selected = "Elevation",
                      label = paste0("Add Condition #", toString(values$conditionIndex))),
                      style = "overflow: visible !important")
          )
        values$conditionIndex <- values$conditionIndex + 1
    })
  }
)