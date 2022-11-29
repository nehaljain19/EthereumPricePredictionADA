eth <- read.csv("/Users/kritika/Downloads/EthereumPricePredictionADA-main/WETHDailyDayData.csv")
eth
eth_df=eth

head(eth_df)
summary(eth_df)
dim(eth_df)
sum(is.na(eth_df))
typeof(eth_df)
str(eth_df)
drop = c("X","id","date","totalValueLockedUSD","volumeUSD")
eth_new_df = eth_df[,!(names(eth_df) %in% drop)]
dim(eth_new_df)
str(eth_new_df)
ncol(eth_new_df)
names(eth_new_df)

eth_new_df$Target =as.factor(eth_new_df$Target)

eth_new_df <- data.frame(eth_new_df)
str(eth_new_df)

#unwanted_col <- c("id", "date")

#EC1_cor <- correlate(eth_new_df[ , !names(eth_new_df) %in% unwanted_col],
#method = 'pearson',
#use = 'pairwise.complete.obs')
#EC1_cor %>% rplot()

#-----------------------------------------------------------------
#Random Forest
install.packages("randomForest")
install.packages("randomForestExplainer")
library(randomForest)
library(randomForestExplainer)
library(caret)
set.seed(122)


split <- sample.split(eth_new_df, SplitRatio = 0.8)
split

train_reg <- subset(eth_new_df, split == "TRUE")
test_reg <- subset(eth_new_df, split == "FALSE")

errorvalues <- vector()
for (i in 1:3){
  temprf <- randomForest(Target ~open + high + low +close,data = train_reg,ntree = 2000,mtry = 3)
  errorvalues[i] <- temprf$err.rate[nrow(temprf$err.rate),1]
}
min_depth_frame <- min_depth_distribution(temprf)
plot_min_depth_distribution(min_depth_frame)

str(test_reg)
typeof(test_reg)
str(test_reg)
#a=c("open","high","low","close","Target")
y_pred = predict(temprf, newdata = test_reg)
#y_pred = predict(temprf, newdata = test_reg[,a])
table(y_pred)
typeof(y_pred)

confusion_mtx = confusionMatrix(y_pred, test_reg$Target)
confusion_mtx
plot(errorvalues)
importance(temprf)
varImpPlot(temprf)
plot(temprf)

#------------------------------------------------------------

#knn
install.packages("e1071")
install.packages("caTools")
install.packages("class")

# Loading package
library(e1071)
library(caTools)
library(class)


train_scale <- scale(train_reg[, 1:4])
test_scale <- scale(test_reg[, 1:4])

classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_reg$Target,
                      k = 3)
classifier_knn

# Confusiin Matrix
cm <- table(test_reg$Target, classifier_knn)
cm
# Model Evaluation - Choosing K
# Calculate out of Sample error
misClassError <- mean(classifier_knn != test_reg$Target)
print(paste('Accuracy =', 1-misClassError))

#-----------------------------------------------------------
#svm

split <- sample.split(eth_new_df, SplitRatio = 0.6)
split

train_reg_svm <- subset(eth_new_df, split == "TRUE")
test_reg_svm <- subset(eth_new_df, split == "FALSE")
str(test_reg_svm)

train_reg_svm[-5] = scale(train_reg_svm[-5])
test_reg_svm[-5] = scale(test_reg_svm[-5])

classifier = svm(formula = Target ~ .,
                 data = train_reg_svm,
                 type = 'C-classification',
                 kernel = 'linear')
classifier
y_pred_svm = predict(classifier, newdata = test_reg_svm[-5])
#y_pred = predict(classifier, newdata = test_reg[-5])
cm = table(test_reg_svm[, 5], y_pred_svm)
cm

#------------------------------------------------------------

