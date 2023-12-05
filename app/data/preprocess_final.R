library(dplyr)
set.seed(558)
data <- read.table(gzfile("covtype.data.gz"),header = F,sep = ",")
colnames(data) <- append(c(
  "Elevation", "Aspect", "Slope", "Horizontal_Distance_To_Hydrology",
  "Vertical_Distance_To_Hydrology", "Horizontal_Distance_To_Roadways",
  "Hillshade_9AM", "Hillshade_12PM", "Hillshade_3PM",
  "Horizontal_Distance_To_Fire_Points", 
  "Wilderness_Rawah", "Wilderness_Neota", "Wilderness_Comanche",
  "Wilderness_Cache"
), c(sapply(1:40, function(j) {paste0("Soil_Type_",toString(j))}), "Cover_Type"))
props <- table(data$Cover_Type) / nrow(data)
CT_prob <- sapply(1:nrow(data), function(x) {props[data$Cover_Type[x]]})
idxsForApp <- sample(1:nrow(data),size = 150000,replace = F,prob = CT_prob)

dataModel <- data %>% mutate(
  Soil_Type = as.factor(sapply(1:nrow(data), function(x) {
    names(which.max(data[x,] %>% select(Soil_Type_1:Soil_Type_40)))}))
)
dataModel$Cover_Type <- factor(dataModel$Cover_Type,labels=sapply(1:7,function(x){paste0("Cover_Type_",toString(x))}))
dataModel$Soil_Type <- as.factor(sapply(1:nrow(dataModel), function(x) {
  return(names(which.max(dataModel[x,] %>% select(Soil_Type_1:Soil_Type_40))))}))

dataModel<- dataModel[idxsForApp,] 
dataModel<- dataModel %>% select(-c(Soil_Type_1:Soil_Type_40))

temp <- dataModel

temp <- temp %>% mutate(
  Hillshade = as.factor(sapply(1:nrow(temp), function(x) {
    names(which.max(temp[x,] %>% select(Hillshade_9AM:Hillshade_3PM)))}))
)
temp <- temp %>% select(-c(Hillshade_9AM:Hillshade_3PM))
temp <- temp %>% mutate(
  Wilderness = as.factor(sapply(1:nrow(temp), function(x) {
    names(which.max(temp[x,] %>% select(Wilderness_Rawah:Wilderness_Cache)))}))
)
temp <- temp %>% select(-c(Wilderness_Rawah:Wilderness_Cache))

dataModel <- temp
rm(temp)
rm(CT_prob)
rm(idxsForApp)
rm(props)

# Save the data in the Environment tab