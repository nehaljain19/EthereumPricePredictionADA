
eth <- read.csv("/Users/kritika/Downloads/EthereumPricePredictionADA-main/WETHDailyDayData.csv")
eth
install.packages("ggplot2")
install.packages("dplyr")
install.packages("ghql")
install.packages("jsonlite")
 

# logistic                
model <- lm(priceUSD ~ open + high + low, data = df)
summary(model)
coef(model)

model1 <- lm(priceUSD ~ open + high + low + volumeUSD, data = df)
summary(model1)


install.packages("predict3d")

ggPredict(model,digits=1)
ggPredict(model,digits=1,show.error = TRUE,facet.modx = TRUE,show.text=FALSE)

install.packages("caTools")    # For Logistic regression
install.packages("ROCR")
install.packages("glmnet")
library(caTools)
library(ROCR) 
library(glmnet)
require(nnet)
library(caret)
library(MASS)


eth_df=eth
head(eth_df)
summary(eth_df)
dim(eth_df)
sum(is.na(eth_df))
typeof(eth_df)
str(eth_df)
drop = c("id","date","totalValueLockedUSD")
eth_new_df = eth_df[,!(names(eth_df) %in% drop)]
dim(eth_new_df)
str(eth_new_df)
ncol(eth_new_df)
names(eth_new_df)


eth_new_df$Target =as.factor(eth_new_df$Target)

eth_new_df <- data.frame(eth_new_df)


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
set.seed(49)


split <- sample.split(eth_new_df, SplitRatio = 0.7)
split

train_reg <- subset(eth_new_df, split == "TRUE")
test_reg <- subset(eth_new_df, split == "FALSE")

errorvalues <- vector()
for (i in 1:3){
  temprf <- randomForest(Target ~open + high + low +close,data = train_reg,ntree = 1800,mtry = 3)
  errorvalues[i] <- temprf$err.rate[nrow(temprf$err.rate),1]
}
min_depth_frame <- min_depth_distribution(temprf)
plot_min_depth_distribution(min_depth_frame)
#table(y_pred)
#typeof(y_pred)
str(test_reg)
typeof(test_reg)
str(test_reg)
a=c("open","high","low","close","Target")

y_pred = predict(temprf, newdata = test_reg[,a])
library(caret)
confusion_mtx = confusionMatrix(y_pred, test_reg$Target)
confusion_mtx
plot(errorvalues)
importance(temprf)
varImpPlot(temprf)
plot(temprf)




