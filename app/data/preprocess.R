data <- read.table(gzfile("covtype.data.gz"),header = F,sep = ",")
colnames(data) <- append(c(
  "Elevation", "Aspect", "Slope", "Horizontal_Distance_To_Hydrology",
  "Vertical_Distance_To_Hydrology", "Horizontal_Distance_To_Roadways",
  "Hillshade_9AM", "Hillshade_12PM", "Hillshade_3PM",
  "Horizontal_Distance_To_Fire_Points", 
  "Wilderness_Rawah", "Wilderness_Neota", "Wilderness_Comanche",
  "Wilderness_Cache"
), c(sapply(1:40, function(j) {paste0("Soil_Type_",toString(j))}), "Cover_Type"))
