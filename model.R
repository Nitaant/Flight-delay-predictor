library(RCurl) # for downloading the flight CSV file
library(randomForest)
library(caret)

flight <- read.csv("flight_data.csv")
flight <- na.omit(flight)
data <- flight[sample(nrow(flight), 10000, replace = FALSE), ]
set.seed(123) # for reproducibility
TrainingIndex <- createDataPartition(data$dep_delay, p = 0.8, list = FALSE)
TrainingSet <- flight[TrainingIndex,] # Training Set
TestingSet <- flight[-TrainingIndex,]
write.csv(TrainingSet, "training.csv")
write.csv(TestingSet, "testing.csv")

TrainSet <- read.csv("training.csv", header = TRUE)
TrainSet <- TrainSet[,-1]

model_formula <- as.formula("arr_delay ~ dep_delay + carrier + origin + dest")
model <- randomForest(model_formula, data = TrainingSet)

# Save model to RDS file
saveRDS(model, "model.rds")
