rm(list=ls(all=T))
setwd("E:/Python")
getwd()
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')
#install.packages(x)
lapply(x, require, character.only = TRUE)
loan = read.csv("bank-loan.csv", header = TRUE)
str(Rental)
install.packages("GGally")
library(GGally)
ggpairs(loan)
##################################Missing Values Analysis###############################################

missing_val = data.frame(apply(Rental,2,function(x){sum(is.na(x))}))

missing_val$columns=row.names(missing_val)

missing_val
names(missing_val)[1] =  "Missing_percentage"

missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(Rental)) * 100
missing_val = missing_val[order(-missing_val$Missing_percentage),]
row.names(missing_val) = NULL
missing_val = missing_val[,c(2,1)]

######imputing the missing values#######
loan$default[is.na(loan$default)] = 0
sum(is.na(loan$default))###checking if the values are imputed or not


# Encoding the target feature as factor
loan$default = factor(loan$default)
str(loan)                           
########################model development ########################

##-- Decision Tree
##-- Random Forest
##-- logistic  Regression 

# Splitting the dataset into the Training set and Test set
set.seed(2020)
train_index = sample(1:nrow(loan), 0.8 * nrow(loan))
train = loan[train_index,]
test = loan[-train_index,]

#Logistic Regression
logit_model = glm(default ~ ., data = train, family = "binomial")
#summary of the model
summary(logit_model)

logit_Predictions = predict(logit_model, newdata = test, type = "response")
logit_Predictions = ifelse(logit_Predictions > 0.5, 1, 0)
ConfMatrixL = table(test$default, logit_Predictions)
Accuracy_LOR = sum(diag(ConfMatrixL)/sum(ConfMatrixL))
####Accuracy_LOR = 82.94%

library(ROCR)
ROCRPred = prediction(logit_Predictions,test$default)
ROCRPref = performance(ROCRPred,"tpr","fpr")

plot(ROCRPref, colorize=TRUE,print.cutoffs.at=seq(0.1,by=0.1))

#####Decision tree for classification#####
#Develop Model on training data
C50_model = C5.0(default ~., train, trials = 100, rules = TRUE)

#Summary of DT model
summary(C50_model)

#Lets predict for test cases
C50_Predictions = predict(C50_model, test[,-9], type = "class")

##Evaluate the performance of classification model
ConfMatrix_C50 = table(test$default, C50_Predictions)
confusionMatrix(ConfMatrix_C50)
##Accuracy : 76.47%

########Random forest###########
classifier = randomForest(default~.,data=train, ntree = 500,  importance = TRUE)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test[-9])

##Evaluate the performance of classification model
ConfMatrixR = table(test$default, y_pred)
confusionMatrix(ConfMatrixR)

Accuracy_RF = sum(diag(ConfMatrixR)/sum(ConfMatrixR))
###Accuracy_RF = 84.71%
