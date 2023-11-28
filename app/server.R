library(shiny)
library(ggplot2)
library(dplyr)

shinyServer(
  function(input, output, session) {
    values <- reactiveValues(data = NULL, conditionIndex = 0,
                             types = c(rep("Quant", 10), rep("Qual", 45)),
                             dataExplore = NULL)
    
   
    
    observe({
      if (is.null(values$data)) {
        values$data <- read.table(gzfile("data/covtype.data.gz"),header = F,sep = ",")
        updateActionButton(inputId = "addFilterCond", label = "Click!")
      } 
    })
    
    observeEvent(input$addFilterCond, {
      if (!is.null(values$data)) {
        values$conditionIndex <- values$conditionIndex + 1
        output$conditions <- insertUI(selector = "#conditions", 
                                      ui = fluidRow(
                                        column(width = 4,
                                               selectInput(inputId = paste0("filterSel", toString(values$conditionIndex)),
                                                           choices = colnames(values$data), selected = "Elevation",
                                                           label = paste0("Add Condition #", toString(values$conditionIndex)))),
                                        column(width = 8,
                                               uiOutput(paste0("filterUIPanel", toString(values$conditionIndex)))),
                                        style = "overflow: visible !important")
        ) 
      }
    })
    
    observeEvent(values$conditionIndex, {
      if (!is.null(values$data)) {
        for (i in 1:values$conditionIndex) {
          observeEvent(input[[paste0("filterSel", toString(i))]], {
            idx <- which(colnames(values$data) == input[[paste0("filterSel", 
                                                         toString(i))]])
            if (values$types[idx] == "Quant") {
              output[[paste0("filterUIPanel", toString(values$conditionIndex))]] <- renderUI({
                sliderInput(inputId = paste0("filterCond", toString(values$conditionIndex)),
                            label = "Select Range of Values (Slide to Select)",
                            min = min(values$data[,idx]),
                            max = max(values$data[,idx]),
                            value = c(min(values$data[,idx]), max(values$data[,idx])))
              })
            } else if (values$types[idx] == "Qual") {
              output[[paste0("filterUIPanel", toString(values$conditionIndex))]] <- renderUI({
                selectInput(inputId = paste0("filterCond", toString(values$conditionIndex)),
                            label = "Select Range of Values (Click to Select)",
                            choices = unique(values$data[,idx]),
                            selected = unique(values$data[,idx])[1], 
                            multiple = T)
              })
            }
          })
        } 
      }
    })
    
    observeEvent(input$doFilter, {
      if (!is.null(values$data)) {
        temp <- values$data
        for (i in 1:values$conditionIndex) {
          idx <- which(colnames(values$data) == input[[paste0("filterSel", 
                                                              toString(i))]])
          if (values$types[idx] == "Quant") {
            temp <- temp %>% filter(
              temp[,idx] >= input[[paste0("filterCond", toString(i))]][1] &
              temp[,idx] <= input[[paste0("filterCond", toString(i))]][2]
            )
          } else if (values$types[idx] == "Qual") {
            temp <- temp %>% filter(
              temp[,idx] %in% input[[paste0("filterCond", toString(i))]]
            )
          }
        }
        values$dataExplore <- temp
      }
    })
  }
)