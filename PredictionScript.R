#importing required libraries
library(caret)
library(glmnet)
library(MASS)
library(tidyverse)


# read data Please download Appointments.csv
weth <- read.csv("C:\\Users\\Nehal\\OneDrive\\Documents\\MSBA_StudyMaterial\\Fall2022\\AdvancedDataAnalytics\\Project\\WETHDailyDayData.csv", stringsAsFactors = TRUE)
str(weth)

#Converting date into date-time and creating a categorical variable Weekdays for analysis
weth$date<- as.Date(weth$date)
weth$weekday <- weekdays(weth$date)
weth$weekday <- as.factor(weth$weekday)
weth = subset(weth, select = -c(X,date) )

#Structure of dataframe
str(weth)
#Dimensions of dataframe
dim(weth)
#Descriptive statistics of dataframe
summary(weth)


cv_5 = trainControl(method = "cv", number = 5)

####################################################################################
#FIRST MODEL
#Train and test split 
set.seed(222)
ind <- sample(2, nrow(weth), replace = TRUE, prob = c(0.7, 0.3))
train <- weth[ind==1,]
head(weth)
test <- weth[ind==2,]

#creating custom Control Parameters
custom <- trainControl(method = "repeatedcv", number = 10, repeats = 5, verboseIter = TRUE)

#fitting Elastic Net Regression model
set.seed(1234)
en <- train(Target ~. - id, train, method='glmnet', tuneGrid =expand.grid(alpha=seq(0,1,length=10),
                                                                          lambda = seq(0.0001,0.2,length=5)), trControl=custom)
en

#mean validation score
mean(en$resample$RMSE)

#plotting the model
plot(en, main = "Elastic Net Regression")

#plotting important variables
plot(varImp(en,scale=TRUE))

calc_acc = function(actual, predicted) {
  mean(actual == predicted)
}

test$PredProb <- predict(en, newdata = test)
FinalData <- test[order(-test$PredProb),]

# test acc
calc_acc(actual = test$Target,
         predicted = ifelse(predict(en, newdata = test)>0.6,1,0))

