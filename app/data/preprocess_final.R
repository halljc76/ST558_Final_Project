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

dataModel <- data %>% mutate(
  Soil_Type = as.factor(sapply(1:nrow(data), function(x) {
    names(which.max(data[x,] %>% select(Soil_Type_1:Soil_Type_40)))}))
)
dataModel$Cover_Type <- factor(dataModel$Cover_Type,labels=sapply(1:7,function(x){paste0("Cover_Type_",toString(x))}))
dataModel$Soil_Type <- as.factor(sapply(1:nrow(dataModel), function(x) {
  return(names(which.max(dataModel[x,] %>% select(Soil_Type_1:Soil_Type_40))))}))