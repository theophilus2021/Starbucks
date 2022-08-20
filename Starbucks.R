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

# Step 1: Data cleaning 

# Step 2: Data exploration and analysis
# 2.1 structure, features, target
dim(SBUX_Data)
str(SBUX_Data)
view(SBUX_Data)

# 2.2 plots and correlation 
# 2.2.a turn features to numeric 



# correlation table 
cor(SBUX_Data[15:18])

# scatterplot 
pairs(SBUX_Data[15:18])

