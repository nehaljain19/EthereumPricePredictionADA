#importing required libraries
library(caret)
library(glmnet)
library(MASS)


# read data Please download Appointments.csv
weth <- read.csv("C:\\Users\\Nehal\\OneDrive\\Documents\\MSBA_StudyMaterial\\Fall2022\\AdvancedDataAnalytics\\Project\\WETHDailyDayData.csv", stringsAsFactors = TRUE)
str(weth)

#Converting date into date-time and creating a categorical variable Weekdays for analysis
weth$date<- as.Date(weth$date)
weth$weekday <- weekdays(weth$date)
weth$weekday <- as.factor(weth$weekday)
weth = subset(weth, select = -c(X,date) )
str(weth)



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
en <- train(Target ~. -Target - id, train, method='glmnet', tuneGrid =expand.grid(alpha=seq(0,1,length=10),
            lambda = seq(0.0001,0.2,length=5)), trControl=custom)
en

#mean validation score
mean(en$resample$RMSE)

#plotting the model
plot(en, main = "Elastic Net Regression")

#plotting important variables
plot(varImp(en,scale=TRUE))

##SECOND MODEL
set.seed(42)
cv_5 = trainControl(method = "cv", number = 5)

default_idx = createDataPartition(weth$Target, p = 0.75, list = FALSE)
default_trn = weth[default_idx, ]
default_tst = weth[-default_idx, ]

#Method 1
def_elnet = train(
  Target ~ . - Target - id, data = default_trn,
  method = "glmnet",
  trControl = cv_5
)
def_elnet

calc_acc = function(actual, predicted) {
  mean(actual == predicted)
}

# test acc
calc_acc(actual = default_tst$Target,
         predicted = predict(def_elnet, newdata = default_tst))


##Method 2
# Build the model
set.seed(123)
elastic <- train(
  Y ~., data = train, method = "glmnet",
  family = "binomial",
  trControl = trainControl("cv", number = 10),
  tuneLength = 5
)
# Best tuning parameter
elastic$bestTune

elastic_model <- glmnet(x, y, family = "binomial", alpha = elastic$bestTune$alpha, lambda = elastic$bestTune$lambda) #lambda = NULL
# Display regression coefficients
#coef(lasso_model)

#Make predictions on the test data

probabilities <- elastic_model %>% predict(x.test)
predicted.classes <- ifelse(probabilities > 0.5, "1", "0")
#Model accuracy
observed.classes <- test$Y
mean(predicted.classes == observed.classes)


##Method 3
set.seed(123)
lambda <- seq(-50, 50)
alpha <- seq(-10, 10, length = 20)
searchGrid = expand.grid(.alpha = alpha, .lambda = lambda)

# Set training control
tc <- trainControl(method = "repeatedcv",
                   number = 5,
                   repeats = 3)

# Train the model
en.model <- train(Target ~. -Target - id,
                  data = train,
                  method = "glmnet",
                  tuneGrid = searchGrid,
                  metric = "Accuracy",
                  trControl = tc)
