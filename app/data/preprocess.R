library(dplyr)
data <- read.table(gzfile("covtype.data.gz"),header = F,sep = ",")
colnames(data) <- append(c(
  "Elevation", "Aspect", "Slope", "Horizontal_Distance_To_Hydrology",
  "Vertical_Distance_To_Hydrology", "Horizontal_Distance_To_Roadways",
  "Hillshade_9AM", "Hillshade_12PM", "Hillshade_3PM",
  "Horizontal_Distance_To_Fire_Points", 
  "Wilderness_Rawah", "Wilderness_Neota", "Wilderness_Comanche",
  "Wilderness_Cache"
), c(sapply(1:40, function(j) {paste0("Soil_Type_",toString(j))}), "Cover_Type"))

types <- c(rep("Quant", 10), rep("Qual", 45))

## Model Fitting

### Multinomial Logistic Regression
library(caret)
idxs <- createDataPartition(1:nrow(data),p = 0.1)[[1]]
data$Soil_Type <- as.factor(sapply(1:nrow(data), function(x) {
  return(names(which.max(data[x,] %>% select(Soil_Type_1:Soil_Type_40))))}))
train <- data[idxs,]
test <- data[-idxs,]
train$Cover_Type <- as.factor(train$Cover_Type)
test$Cover_Type <- as.factor(test$Cover_Type)

preds <- c("Elevation", "Aspect", "Slope", "Horizontal_Distance_To_Hydrology",
           "Vertical_Distance_To_Hydrology", "Horizontal_Distance_To_Roadways",
           "Hillshade_9AM", "Hillshade_12PM", "Hillshade_3PM",
           "Horizontal_Distance_To_Fire_Points", "Soil_Type")
set.seed(558)  
multiLogReg <- multinom(Cover_Type ~ Elevation, 
                        data = dataModel[idxs,],
                        maxit = 100, trace = T)
mlrP <- predict(multiLogReg, newdata = dataModel[-idxs,])
cm <- confusionMatrix(mlrP,as.factor(dataModel[-idxs,]$Cover_Type))

control <- trainControl(method="cv", number=5,
                        classProbs=TRUE, summaryFunction=mnLogLoss)
train$Cover_Type <- factor(train$Cover_Type,labels=sapply(1:7,function(x){paste0("Cover_Type_",toString(x))}))
test$Cover_Type <- factor(test$Cover_Type,labels=sapply(1:7,function(x){paste0("Cover_Type_",toString(x))}))
rfFit <- train(Cover_Type ~ Elevation + Aspect + Slope, # example preds
               data = train,
               method = "rf",
               metric = "logLoss",
               trControl = control,
               tuneGrid = expand.grid(mtry = c(1,2)),
               trace = T)
rfPred <- predict(rfFit, newdata = test[1:10000,],type = "raw")
confusionMatrix(rfPred, test$Cover_Type[1:10000])


