# Step 0
#install.packages("randomForest", repos = "https://cran.microsoft.com/")
#install.packages("rattle")

# Load libraries
library(caret)
library(rpart)
library(rattle)
library(e1071)
library(randomForest)

# Set random seed
#set.seed(123)

SBUX_Data<-read.csv("starbucks_drinkMenu_expanded.csv", stringsAsFactors = FALSE)

head(SBUX_Data)
dim(SBUX_Data)

# Step 1: Data cleaning 

# Rename columns 
names(SBUX_Data)<-c("Beverage_cat","Beverage","Beverage_prep",
                    "Calories","Total_Fat","Trans_Fat","Sat_Fat",
                    "Sodium","Carb","Chole","Fibre","Sugars","Protein",
                    "VitA","VitC","Calcium","Iron","Caffeine")


### we only needed one 'factor' variable for our analysis so, it is better to delete the Beverage and Beverage_prep variables.

SBUX_Data$Beverage<-NULL
SBUX_Data$Beverage_prep<-NULL
dim(SBUX_Data)

# Fill NA cells in data with 0
SBUX_Data[is.na(SBUX_Data)] <- 0
# Fill NA with 0, etc 
# Set 0 caffeine to Iced Tea, Chocolate Smoothies,23 NAs 
SBUX_Data$Caffeine[is.na(SBUX_Data$Caffeine)] <- 0

# Set 3.4 Total Fat to Frappucino Blended Creme 
SBUX_Data$Total_Fat[is.na(SBUX_Data$Total_Fat)]<-3.4

# Step 2: Data exploration and analysis
# 2.1 structure, features, target
dim(SBUX_Data)
str(SBUX_Data)
view(SBUX_Data)


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

# Fill NA cells in data with 0
SBUX_Data[is.na(SBUX_Data)] <- 0
# boxplot
boxplot(SBUX_Data$VitA)
boxplot(SBUX_Data$VitC)
boxplot(SBUX_Data$Calcium)
boxplot(SBUX_Data$Iron)


# 2.5 plots and correlation 

# correlation table 
cor(SBUX_Data[4:9])
cor(SBUX_Data[8:13])
cor(SBUX_Data[12:16])


# correlation plots 
pairs(SBUX_Data[4:9])
pairs(SBUX_Data[8:13])
pairs(SBUX_Data[12:16])

# 2.6 Eliminate features/attributes not useful 
#SBUX <- SBUX_data %>% select(c())

SBUX_Data<-SBUX_Data %>% dplyr::mutate(Beverage_cat = as.factor(Beverage_cat))

# Parition 70% for training
#set.seed(777)
#sample_size = floor(0.7*nrow(SBUX_Data))
Train<-createDataPartition(SBUX_Data$Beverage_cat, p=.70, list = FALSE)
Train_SBUX<-SBUX_Data[Train,] 

# Parition 30% for valuation and test 
Test_SBUX<-SBUX_Data[-Train,]

# Parition 15% for valuation 
#Val_SBUX<-createDataPartition(Val_Test_SBUX$Sugars, p=.50, list = FALSE)

# Parition 15% for test  
#Test_SBUX<-Val_Test_SBUX[-Train,]

# Model in terms of sugars
Model_sugars<-lm(Sugars~Calories+Total_Fat+Trans_Fat+Sat_Fat+Sodium+Carb+Chole+Fibre+Protein+VitA+VitC+Calcium+Iron+Caffeine,Train_SBUX)
summary(Model_sugars)

# Model in terms of calories
Model_calories<-lm(Calories~Sugars+Total_Fat+Trans_Fat+Sat_Fat+Sodium+Carb+Chole+Fibre+Protein+VitA+VitC+Calcium+Iron+Caffeine,Train_SBUX)
summary(Model_calories)

# Model in terms of protein
Model_protein<-lm(Protein~Calories+Sugars+Total_Fat+Trans_Fat+Sat_Fat+Sodium+Carb+Chole+Fibre+VitA+VitC+Calcium+Iron+Caffeine,Train_SBUX)
summary(Model_protein)

#############
# Model #1 ##
#############
# ----- Validate Linear Regression models ------------
predicted_sugars <- predict(Model_sugars, Test_SBUX)
predicted_calories <- predict(Model_calories, Test_SBUX)
predicted_protein <- predict(Model_protein, Test_SBUX)

# compute model performance metrics for sugars, calories, protein model
data.frame(R2= R2(predicted_sugars, Test_SBUX$Sugars),
           RMSE = RMSE(predicted_sugars, Test_SBUX$Sugars),
           MAE = MAE(predicted_sugars, Test_SBUX$Sugars))

data.frame(R2= R2(predicted_calories, Test_SBUX$Calories),
           RMSE = RMSE(predicted_calories, Test_SBUX$Calories),
           MAE = MAE(predicted_calories, Test_SBUX$Calories))

data.frame(R2= R2(predicted_protein, Test_SBUX$Protein),
           RMSE = RMSE(predicted_protein, Test_SBUX$Protein),
           MAE = MAE(predicted_protein, Test_SBUX$Protein))


##sets our classification variable to a factor variable
SBUX_Data$Beverage_cat<-factor(SBUX_Data$Beverage_cat)

Train_SBUX$Beverage_cat<-factor(Train_SBUX$Beverage_cat)
Test_SBUX$Beverage_cat<-factor(Test_SBUX$Beverage_cat)

class(Train_SBUX$Beverage_cat)
class(Test_SBUX$Beverage_cat)
dim(Train_SBUX)
# add all categories of beverages to test data, since after splitting the data to train and test, the test data lacks two categories
#levels(Test_SBUX$Beverage_cat) <- c("Classic Espresso Drinks" ,          "Coffee" ,                           "Frappuccino?? Blended Coffee",      
#                                    "Frappuccino?? Blended Cr??me",        "Frappuccino?? Light Blended Coffee", "Shaken Iced Beverages" ,           
#                                    "Signature Espresso Drinks"  ,       "Smoothies" ,                       "Tazo?? Tea Drinks"        )
#############
# Model #2 ##
#############
## ---------------CART Implementation  ---------------
CART_Model <- train(Beverage_cat ~ ., data = Train_SBUX, method = "rpart",
                    trControl = trainControl("cv", number = 10),
                    tuneLength = 10)  # 10-fold cross validation on the training data
plot(CART_Model)   #produces plot of cross-validation results
CART_Model$bestTune #returns optimal complexity parameter

##Validation
confusionMatrix(predict(CART_Model, Test_SBUX), Test_SBUX$Beverage_cat) 

# Decision tree for the CART_Model
par(xpd=NA)
plot(CART_Model$finalModel)
text(CART_Model$finalModel, digits = 3)

#############
# Model #3 ##
#############
#................... Random Forest Implementation.....................
#caret package implementation with 10-fold cross validation
Forest_Model <- train(Beverage_cat ~ ., method="rf", 
                      trControl=trainControl(method = "cv", number = 10),
                      preProcess=c("center", "scale"), data=Train_SBUX)
print(Forest_Model)


confusionMatrix(predict(Forest_Model, Test_SBUX), Test_SBUX$Beverage_cat)

#############
# Model Use RF from Caret package instead  ##
#############
## -------------RF package -----------
#random forest package implementation
Forest_Model_2 <- randomForest(Beverage_cat ~., Train_SBUX)
print(Forest_Model_2)


#Support Vector Machines Implementation
SVM1<-svm(Beverage_cat~., data = Train_SBUX, cost=1000, cross = 10, gamma=.001)
confusionMatrix(predict(SVM1, Test_SBUX), Test_SBUX$Beverage_cat)

#############
# Model #4 ##
#############
#------------------- knn -----------##
library(class)
library(e1071)
SBUX_Data.cat <-SBUX_Data[Train,]
knn.30 <-knn(train = Train_SBUX[,-1], test = Test_SBUX[,-1], cl = SBUX_Data.cat[,1], k=30)
cm = table(Test_SBUX[,1], knn.30)
confusionMatrix(table(Test_SBUX[,1], knn.30))


####### Tuning is not necessary because we already know re-tune didn't improve accuracy #######
#tuning the SVM (validation)
#svm_tune <- tune(svm, train.x=Train_SBUX[,-1], train.y=Train_SBUX[,1], 
#                 kernel="linear", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))
#print(svm_tune) ###printed cott=10 and gamma=.5

#re-estimate the model with the optimally tuned parameters
#SVM_RETUNE<-svm(Beverage_cat~., data = Train_SBUX, cost=10, cross = 10, gamma=.5)
#confusionMatrix(predict(SVM_RETUNE, Test_SBUX), Test_SBUX$Beverage_cat)


### naive bayes ########
## Assume variables are completely independent of each other but reality is not
## therefore we are not considering this model 
NB <- naiveBayes(Beverage_cat ~ ., data=Train_SBUX)
NB









