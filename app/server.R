library(shiny)
library(ggplot2)
library(dplyr)
library(shinyalert)
library(caret)
library(nnet)
library(DT)
library(tidyr)

shinyServer(
  function(input, output, session) {
    
    ### Reactive Values used throughout the app
    values <- reactiveValues(data = NULL, conditionIndex = 0,
                             types = c(rep("Quant", 10), rep("Qual", 45)),
                             dataExplore = NULL,
                             readyForModelTab = F,
                             readyForTrain = F,
                             dataModel = NULL,
                             mlr = NULL,
                             rf = NULL,
                             mlrParams = NULL,
                             rfParams = NULL,
                             readyForPredict = F)
    
   
    
    observe({
      if (is.null(values$data)) {
        print("Read in Data")
        load("./data/preprocessed.RData")
        # values$data <- read.table(gzfile("data/covtype.data.gz"),header = F,sep = ",")
        # colnames(values$data) <- append(c(
        #   "Elevation", "Aspect", "Slope", "Horizontal_Distance_To_Hydrology",
        #   "Vertical_Distance_To_Hydrology", "Horizontal_Distance_To_Roadways",
        #   "Hillshade_9AM", "Hillshade_12PM", "Hillshade_3PM",
        #   "Horizontal_Distance_To_Fire_Points", 
        #   "Wilderness_Rawah", "Wilderness_Neota", "Wilderness_Comanche",
        #   "Wilderness_Cache"
        # ), c(sapply(1:40, function(j) {paste0("Soil_Type_",toString(j))}), 
        #      "Cover_Type"))
        values$data <- data
        values$dataModel <- dataModel     # Still currently the 600K x ...
        values$dataExplore <- values$data 
        
        print("Update Cols")
        updateActionButton(inputId = "addFilterCond", label = "Click!")
        sapply(c("varSelect", "plot1Vars", "plot2XVar", "plot2YVar",
                 "plot2Grouping", "summaryVars"), function(id) {
                   updateSelectInput(inputId = id, choices = colnames(values$data),
                                     selected = colnames(values$data)[1])
                 })
      } 
    })
    
    # This checks if the above observer is finished!
    observeEvent({input$summaryVars}, {
      if (!values$readyForModelTab) {
        values$readyForModelTab <- T
        print("Onto Modeling Tab")
      }
    })
    
    # This supplies predictors that can be selected for the model
    observeEvent(values$readyForModelTab, {
      if (!is.null(values$data) && values$readyForModelTab &&
          !is.null(input$summaryVars)) {
        
        print("Finishing Model Tab")
        
        sapply(c("rfParams", "mlrParams"), function(id) {
          updateSelectInput(inputId = id, 
                            choices = setdiff(colnames(values$dataModel),c("Cover_Type")),
                            selected = colnames(values$dataModel)[1])
        })
        updateSelectInput(session = session, 
                           "mtrySel", choices = c(1:(ncol(values$dataModel)-1)),
                          selected = sort(sample(1:(ncol(values$dataModel)-1),
                                            size = 3)))
      }
    })
    
    ### Dynamic UI creation for additional conditions on which to filter
    ### in the Exploration tab only!
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
              output[[paste0("filterUIPanel", toString(i))]] <- renderUI({
                sliderInput(inputId = paste0("filterCond", toString(i)),
                            label = "Select Range of Values (Slide to Select)",
                            min = min(values$data[,idx]),
                            max = max(values$data[,idx]),
                            value = c(min(values$data[,idx]), max(values$data[,idx])))
              })
            } else if (values$types[idx] == "Qual") {
              output[[paste0("filterUIPanel", toString(i))]] <- renderUI({
                selectInput(inputId = paste0("filterCond", toString(i)),
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
    
    # These observers control visualizations in the Data Exploration 
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
    # 
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
                          } else {
                            output$plot2 <- renderPlot({
                              ggplot(data = temp) + geom_point(
                                aes(x = x, y = y)) +
                                theme_classic() + labs(
                                  x = input$plot2XVar,
                                  y = input$plot2YVar
                                )
                            })
                          }
                        } else {
                          if (input$plot2Group) {
                            temp <- cbind(temp,
                                          data.frame(group = values$dataExplore[,input$plot2Grouping]))
                            output$plot2 <- renderPlot({
                              ggplot(data = temp) + geom_bar(
                                aes(x = x,
                                    color = as.factor(group)),
                                    stat = "count") +
                                facet_wrap(~as.factor(y)) +
                                theme_classic() + labs(
                                  x = input$plot2XVar,
                                  y = input$plot2YVar,
                                  color = input$plot2Grouping
                                )
                            })
                          } else {
                            output$plot2 <- renderPlot({
                              ggplot(data = temp) + geom_bar(aes(x = x),
                                                             stat = "count") +
                                facet_wrap(~as.factor(y)) +
                                theme_classic() + labs(
                                  x = input$plot2XVar,
                                  y = "Count"
                                )
                            })
                          }
                        }
                      }
                  })

    observeEvent({input$summaryVars
                  input$summaryType
                  values$dataExplore}, {
                    if (!is.null(values$dataExplore) && !is.null(input$summaryVars)) {
                      if (input$summaryType == "Contingency Table") {
                        output$summary <- renderPrint({
                          table(values$dataExplore[,input$summaryVars])
                        })
                      } else {
                        output$summary <- renderPrint({
                          summary(values$dataExplore[,input$summaryVars])
                        }) 
                      }
                    }
                  })

    # This observer executes code to train BOTH models.
    observeEvent(input$trainModels, {

      if (length(input$rfParams) == 0 || length(input$mlrParams) == 0) {
        shinyalert(title = "No Parameters Specified for [At Least] One Model",
                   type = "error",
                   text = "Please Specify a Non-Empty List of Parameters for Both Models.")
      } else if (length(input$rfParams) < max(as.numeric(input$mtrySel))) {
        shinyalert(title = "Insufficient Number of Candidate Parameters for Random Forest Model",
                   type = "error",
                   text = paste0("Please Specify At Least ", toString( max(as.numeric(input$mtrySel))),
                                 " Parameters for Random Forest Model"))
      } else if (input$splitProp == 0 || input$splitProp == 1) {
        shinyalert(title = "Invalid Proportion for Training/Testing Set Split",
                   type = "error",
                   text = "Please Specify a Number Between 0 and 1, Exclusive.")
      } else {
        withProgress(
          message = "Training models (this could take a while...)",
          expr = {
            print("Creating Partition")
            idxs <- createDataPartition(1:nrow(values$dataModel),
                                        p = input$splitProp)[[1]]
            # If using the mini dataset, no need for this line (done in preprocess)
            values$dataModel$Cover_Type <- factor(values$dataModel$Cover_Type,labels=sapply(1:7,function(x){paste0("Cover_Type_",toString(x))}))
            modelTrain <- values$dataModel[idxs,]
            modelTest  <- values$dataModel[-idxs,]
            incProgress(amount = 0.1, message = "Partitioned data! Now training GLM...")
            
            set.seed(558)
            print("Training GLM")
            multiLogReg <- multinom(Cover_Type ~ .,
                                    data = modelTrain %>% select(input$mlrParams, "Cover_Type"),
                                    maxit = 100, trace = T)
            mlrP <- predict(multiLogReg, newdata = modelTest %>% select(input$mlrParams, "Cover_Type"))
            mlrPTrain <- predict(multiLogReg, newdata = modelTrain %>% select(input$mlrParams, "Cover_Type"))
            # print(mlrP)
            # print(modelTest$Cover_Type)
            values$mlr <- multiLogReg
            values$mlrParams <- input$mlrParams
            
            output$mlrTrainTable <- renderPrint({
              confusionMatrix(mlrPTrain,modelTrain$Cover_Type)
            })
            output$mlrTable <- renderPrint({
              confusionMatrix(mlrP,modelTest$Cover_Type)
            })
            
            output$mlrSummaryText <- renderPrint({summary(values$mlr)})
            incProgress(amount = 0.2, message = "GLM Finished! Now training random forest...")
            
            print("Training Random Forest")
            print(as.vector(as.numeric(input$mtrySel)))
            control <- trainControl(method="cv", number=as.numeric(input$kfoldCVSel),
                                    classProbs=TRUE, summaryFunction=mnLogLoss)
            print(head(modelTrain %>% select(input$rfParams, "Cover_Type")))
            rfFit <- train(Cover_Type ~ ., # example preds
                           data = modelTrain %>% select(input$rfParams, "Cover_Type"),
                           method = "rf",
                           metric = "logLoss",
                           trControl = control,
                           tuneGrid = expand.grid(mtry = as.vector(as.numeric(input$mtrySel))))
            rfPred <- predict(rfFit, newdata = modelTest %>% select(input$rfParams, "Cover_Type"),
                              type = "raw")
            rfPredTrain <- predict(rfFit, newdata = modelTrain %>% select(input$rfParams, "Cover_Type"),
                                   type = "raw")
            
            values$rf <- rfFit
            values$rfParams <- input$rfParams
            
            incProgress(amount = 0.3, message = "Random Forest finished! Computing results...")
            
            output$rfTrainTable <- renderPrint({
              confusionMatrix(rfPredTrain,modelTrain$Cover_Type)
            })
            output$rfTable <- renderPrint({
              confusionMatrix(rfPred,modelTest$Cover_Type)
            })
            
            varImpRF <- varImp(rfFit)
            output$varImpRF <- renderPlot({plot(varImpRF)})
            
            isolate({
              output$mlrSummary <- insertUI(selector = "#mlrSummary",
                                            ui = verbatimTextOutput("mlrSummaryText"))
              
              output$rfSummary <- insertUI(selector = "#rfSummary",
                                           ui = plotOutput("varImpRF"))
            })
            
            incProgress(amount = 0.4, message = "Done!")
          }
        )
      }
    })
    
    # This code checks if a model has been created, and then populates
    # UI for predictions!
    observe({
      if (!is.null(values$rf) && !is.null(values$mlr)) {

          output$dynamicPredictors <- insertUI(
            selector = "#dynamicPredictors",
            where = "beforeEnd",
            ui = fluidRow(
              h4("Specify Values for Multinomial Logistic Regression Predictors:")
            )
          )
          sapply(1:length(values$mlrParams), function(p) {
            if (is.numeric(values$dataModel[,values$mlrParams[p]])) {
              output$dynamicPredictors <- insertUI(selector = "#dynamicPredictors",
                                                   where = "beforeEnd",
                                                   ui = fluidRow(
                                                     numericInput(inputId = paste0("mlrPred", toString(p)),
                                                                  label = paste0("Value for ", values$mlrParams[p]),
                                                                  min = min(values$dataModel[,p]),
                                                                  max = max(values$dataModel[,p]),
                                                                  value = min(values$dataModel[,p]))
                                                   ))
            } else {
              output$dynamicPredictors <- insertUI(selector = "#dynamicPredictors",
                                                   where = "beforeEnd",
                                                   ui = fluidRow(
                                                     selectInput(paste0("mlrPred", toString(p)),
                                                                 label = paste0("Value for ", p),
                                                                 choices = unique(values$dataModel[,p]),
                                                                 selected = unique(values$dataModel[,p])[1])
                                                   ))
            }
          })
          
          output$dynamicPredictors <- insertUI(
            selector = "#dynamicPredictors",
            where = "beforeEnd",
            ui = fluidRow(
              hr(),
              h4("Specify Values for Random Forest Predictors:")
            )
          )
          sapply(1:length(values$rfParams), function(p) {
            if (is.numeric(values$dataModel[,values$rfParams[p]])) {
              output$dynamicPredictors <- insertUI(selector = "#dynamicPredictors",
                                                   where = "beforeEnd",
                                                   ui = fluidRow(
                                                     numericInput(inputId = paste0("rfPred", toString(p)),
                                                                  label = paste0("Value for ", values$rfParams[p]),
                                                                  min = min(values$dataModel[,values$rfParams[p]]),
                                                                  max = max(values$dataModel[,values$rfParams[p]]),
                                                                  value = min(values$dataModel[,values$rfParams[p]]))))
                                                   
            } else {
              output$dynamicPredictors <- insertUI(selector = "#dynamicPredictors",
                                                   where = "beforeEnd",
                                                   ui = fluidRow(
                                                     selectInput(paste0("rfPred", toString(p)),
                                                                 label = paste0("Value for ", values$rfParams[p]),
                                                                 choices = unique(values$dataModel[,values$rfParams[p]]),
                                                                 selected = unique(values$dataModel[,values$rfParams[p]])[1])
                                                   ))
            }
          })
          
          output$dynamicPredictors <- insertUI(
            selector = "#dynamicPredictors",
            where = "beforeEnd",
            ui = fluidRow(
              hr(),
              actionButton("doPred", label = "Predict")
            )
          )
          
          values$readyForPredict <- T

        } 
    })
    
    # This obsever performs the predictions!
    observeEvent({input$doPred}, {
                    if (!is.null(values$rf) && !is.null(values$mlr)) {
                      
                      mlrNewObs <- as.data.frame(lapply(1:length(values$mlrParams), function(p) {
                        if (is.numeric(values$dataModel[,values$mlrParams[p]])) {
                          return(as.numeric(input[[paste0("mlrPred", toString(p))]]))
                        } else {
                          return(as.character(input[[paste0("mlrPred", toString(p))]]))
                        }
                      }))
                      names(mlrNewObs) <- values$mlrParams
                      
                      rfNewObs <- as.data.frame(lapply(1:length(values$rfParams), function(p) {
                        print(values$rfParams[[p]])
                        print(values$dataModel[,values$rfParams[p]])
                        if (is.numeric(values$dataModel[,values$rfParams[p]])) {
                          return(as.numeric(input[[paste0("rfPred", toString(p))]]))
                        } else {
                          return(as.character(input[[paste0("rfPred", toString(p))]]))
                        }
                      }))
                      names(rfNewObs) <- values$rfParams
                      
                      output$mlrPredResults <- renderPrint({predict(values$mlr, newdata = mlrNewObs)})
                      output$rfPredResults <- renderPrint({predict(values$rf, newdata = rfNewObs, type = "raw")})
                      
                      output$predResults <- renderUI({fluidRow(
                                                       h4("Multinomial Logistic Regression Results"),
                                                       verbatimTextOutput("mlrPredResults"),
                                                       hr(),
                                                       h4("Random Forest Results"),
                                                       verbatimTextOutput("rfPredResults")
                                                     )})
                    }
                  })
  }
)