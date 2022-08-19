# Step 0
install.packages("randomForest", repos = "https://cran.microsoft.com/")

# Load libraries
library(caret)
library(rpart)
library(rattle)
library(e1071)
library(randomForest)

# Set random seed
set.seed(123)

SBUX_Data<-read.csv("starbucks_drinkMenu_expanded.csv", stringsAsFactors = FALSE)
head(SBUX_Data)
dim(SBUX_Data)