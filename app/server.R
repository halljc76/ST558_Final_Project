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
        values$dataExplore <- values$data
        updateActionButton(inputId = "addFilterCond", label = "Click!")
        sapply(c("varSelect", "plot1Vars", "plot2XVar", "plot2YVar",
                 "plot2Grouping", "summaryVars"), function(id) {
                   updateSelectInput(inputId = id, choices = colnames(values$data),
                                     selected = colnames(values$data)[1])
                 })
        
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
    
    observeEvent({input$plot1Vars
                  input$plot1Type
                  values$dataExplore}, {
                    if (!is.null(values$dataExplore) && input$plot1Vars != "") {
                      temp <- data.frame(y = values$dataExplore[,input$plot1Vars])
                      if (input$plot1Type == "Histogram") {
                        output$plot1 <- renderPlot({
                          ggplot(data = temp) + 
                            geom_histogram(aes(x = y),bins = input$numHistBins) + 
                            labs(
                              x = input$plot1Vars,
                              y = "Count"
                            ) + theme_classic()
                        })
                      } else {
                        output$plot1 <- renderPlot({
                          ggplot(data = temp) + 
                            geom_boxplot(aes(y = y)) + labs(
                              y = input$plot1Vars
                            ) + theme_classic() +
                            theme(axis.title.x=element_blank(),
                                  axis.text.x=element_blank(),
                                  axis.ticks.x=element_blank())
                        })
                      } 
                    }
                  })
    
    observeEvent({input$plot2XVar
                  input$plot2YVar
                  input$plot2Grouping
                  input$plot2Group
                  input$plot2Type
                  values$dataExplore}, {
                    if (!is.null(values$dataExplore) &&
                        input$plot2XVar != "" &&
                        input$plot2YVar != "" 
                        ) {
                      temp <- data.frame(
                        x = values$dataExplore[,input$plot2XVar],
                        y = values$dataExplore[,input$plot2YVar]
                      )
                        if (input$plot2Type == "Scatterplot") {
                          if (input$plot2Group) {
                            temp <- cbind(temp, 
                                          data.frame(group = values$dataExplore[,input$plot2Grouping]))
                            output$plot2 <- renderPlot({
                              ggplot(data = temp) + geom_point(
                                aes(x = x, y = y, color = group)) +
                                theme_classic() + labs(
                                  x = input$plot2XVar,
                                  y = input$plot2YVar,
                                  color = input$plot2Grouping
                                )
                            })
                          } 
                        }
                      }
                  })
  }
)