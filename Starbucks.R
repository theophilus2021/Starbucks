# Step 0
#install.packages("randomForest", repos = "https://cran.microsoft.com/")

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

# Fill NA with 0, etc 
SBUX_Data$Caffeine[is.na(SBUX_Data$Caffeine)]<-0
SBUX_Data$Total_Fat[is.na(SBUX_Data$Total_Fat)]<-0

# Step 2: Data exploration and analysis
# 2.1 structure, features, target
dim(SBUX_Data)
str(SBUX_Data)
view(SBUX_Data)

# Rename columns 
names(SBUX_Data)<-c("Beverage_cat","Beverage","Beverage_prep",
                    "Calories","Total_Fat","Trans_Fat","Sat_Fat",
                    "Sodium","Carb","Chole","Fibre","Sugars","Protein",
                    "VitA","VitC","Calcium","Iron","Caffeine")

# Remove % then convert chr to numeric
SBUX_Data$VitA<-gsub("%","", as.character(SBUX_Data$VitA))
SBUX_Data$VitC<-gsub("%","", as.character(SBUX_Data$VitC))
SBUX_Data$Calcium<-gsub("%","", as.character(SBUX_Data$Calcium))
SBUX_Data$Caffeine<-gsub("%","", as.character(SBUX_Data$Caffeine))
SBUX_Data$Iron<-gsub("%","", as.character(SBUX_Data$Iron))

# Convert character class to numeric data class
SBUX_Data$VitA <-as.numeric(SBUX_Data$VitA)
SBUX_Data$VitC <-as.numeric(SBUX_Data$VitC)
SBUX_Data$Calcium <-as.numeric(SBUX_Data$Calcium)
SBUX_Data$Caffeine <-as.numeric(SBUX_Data$Caffeine)
SBUX_Data$Iron <-as.numeric(SBUX_Data$Iron)
SBUX_Data$Total_Fat <-as.numeric(SBUX_Data$Total_Fat)

# boxplot
boxplot(SBUX_Data$VitA)
boxplot(SBUX_Data$VitC)
boxplot(SBUX_Data$Calcium)
boxplot(SBUX_Data$Iron)


# 2.5 plots and correlation 

# correlation table 
cor(SBUX_Data[4:18])

# correlation plots 
pairs(SBUX_Data[4:10])
pairs(SBUX_Data[10:18])

# 2.6 Eliminate features/attributes not useful 
#SBUX <- SBUX_data %>% select(c())

