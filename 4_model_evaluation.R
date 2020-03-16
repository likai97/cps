#Model Auswertung
# From_e ------------------------------------------------------------------
memory.limit(24000)
library(ranger)
library(skimr)
library(caret)
library(UBL)
library(DMwR)
# call all our datasets df in order not to write to much code
load("Data/Data 2017/from_e.RData")
df <- from_e
#for reproducability
set.seed(420)

df$Transition_new <- as.factor(df$Transition_new)
weight <- df$HWTFINL

variables<- c("Transition_new","us_region","Education","Birth","Hispanic","Asian",
              "Black","Metropolitan","hh_single","veteran","Male","Recession","Before_Rec","After_Rec",
              "goverment_employee","self_employed","mult_job","own_house","rent_house",
              "industry","hh_income","absent_lw","AGE","NCHILD")
df <- df[,variables]
#Get row numbers for the training data
trainRowNumbers <- createDataPartition(df$Transition_new, p=0.7, list=FALSE)

# Step 2: Create the training dataset
df_train<- df[trainRowNumbers,]
df_test <- df[-trainRowNumbers,]

#load models
from_e_normal <- readRDS("Data/Model/from_e_normal.rds")
pred_ranger <- predict(from_e_normal, df_test)
confusionMatrix(data=pred_ranger$predictions,reference = df_test$Transition_new, mode = "prec_recall")

from_e_down <- readRDS("Data/Model/from_e_down.rds")
pred_ranger <- predict(from_e_down, df_test)
confusionMatrix(data=pred_ranger$predictions,reference = df_test$Transition_new, mode = "prec_recall")


# From_u ------------------------------------------------------------------
rm(list=ls())
memory.limit(24000)
library(ranger)
library(skimr)
library(caret)
library(UBL)
library(DMwR)
# call all our datasets df in order not to write to much code
load("Data/Data 2017/from_u.RData")
df <- from_u
#for reproducability
set.seed(420)

df$Transition_new <- as.factor(df$Transition_new)


variables<- c("Transition_new","us_region","Education","Birth","Hispanic","Asian",
              "Black","Metropolitan","hh_single","veteran","Male","Recession","Before_Rec","After_Rec",
              "own_house","rent_house",
              "weeks_unemp","hh_income","AGE","NCHILD")

df <- df[,variables]
#Get row numbers for the training data
trainRowNumbers <- createDataPartition(df$Transition_new, p=0.7, list=FALSE)
# Step 2: Create the training dataset
df_train<- df[trainRowNumbers,]
df_test <- df[-trainRowNumbers,]

from_u_normal <- readRDS("Data/Model/from_u_normal.rds")
pred_ranger <- predict(from_u_normal, df_test)
confusionMatrix(data=pred_ranger$predictions,reference = df_test$Transition_new, mode = "prec_recall")

from_u_down <- readRDS("Data/Model/from_u_down.rds")
pred_ranger <- predict(from_u_down, df_test)
confusionMatrix(data=pred_ranger$predictions,reference = df_test$Transition_new, mode = "prec_recall")

from_u_up <- readRDS("Data/Model/from_u_up.rds")
pred_ranger <- predict(from_u_up, df_test)
confusionMatrix(data=pred_ranger$predictions,reference = df_test$Transition_new, mode = "prec_recall")

# from_u_smote <- readRDS("Data/Model/from_u_smote.rds")
# pred_ranger <- predict(from_u_smote, df_test)
# confusionMatrix(data=pred_ranger$predictions,reference = df_test$Transition_new, mode = "prec_recall")


#scale
from_u_scale <- readRDS("Data/Model/from_u_scale.rds")
preprocessor <- preProcess(df_train[!sapply(df_train, is.factor)], method = c('center',"scale"))
df_train <- predict(preprocessor, df_train)
df_test <- predict(preprocessor, df_test)

# one-hot encode our categorical variables
one_hot <- dummyVars(~., data = df_train[,-1], fullRank = TRUE)
df_train <- data.frame(predict(one_hot,newdata= df_train))
df_test <- data.frame(predict(one_hot,newdata= df_test))
# make ranger compatible names
names(df_train) <- make.names(names(df_train), allow_ = FALSE)
names(df_test) <- make.names(names(df_test), allow_ = FALSE)
# Reattach the target variable to the training data that has been
# dropped by predict(dummies,...)
df_train$Transition_new <- df$Transition_new[trainRowNumbers]
df_test$Transition_new <- df$Transition_new[-trainRowNumbers]

pred_ranger <- predict(from_u_scale, df_test)
confusionMatrix(data=pred_ranger$predictions,reference = df_test$Transition_new, mode = "prec_recall")


# From_o ------------------------------------------------------------------
rm(list=ls())
memory.limit(24000)
library(ranger)
library(skimr)
library(caret)
library(UBL)
library(DMwR)
# call all our datasets df in order not to write to much code
load("Data/Data 2017/from_o.RData")
df <- from_o
#for reproducability
set.seed(420)

df$Transition_new <- as.factor(df$Transition_new)
variables<- c("Transition_new","us_region","Education","Birth","Hispanic","Asian",
              "Black","Metropolitan","hh_single","veteran","Male","Recession","Before_Rec","After_Rec",
              "own_house","rent_house",
              "hh_income","AGE","NCHILD")

df <- df[,variables]
#Get row numbers for the training data
trainRowNumbers <- createDataPartition(df$Transition_new, p=0.7, list=FALSE)

# Step 2: Create the training dataset
df_train<- df[trainRowNumbers,]
df_test <- df[-trainRowNumbers,]


from_o_normal <- readRDS("Data/Model/from_o_normal.rds")
pred_ranger <- predict(from_o_normal, df_test)
confusionMatrix(data=pred_ranger$predictions,reference = df_test$Transition_new, mode = "prec_recall")

from_o_down <- readRDS("Data/Model/from_o_down.rds")
pred_ranger <- predict(from_o_down, df_test)
confusionMatrix(data=pred_ranger$predictions,reference = df_test$Transition_new, mode = "prec_recall")

from_o_up <- readRDS("Data/Model/from_o_up.rds")
pred_ranger <- predict(from_o_up, df_test)
confusionMatrix(data=pred_ranger$predictions,reference = df_test$Transition_new, mode = "prec_recall")


# center and scale
preprocessor <- preProcess(df_train[!sapply(df_train, is.factor)], method = c('center',"scale"))
df_train <- predict(preprocessor, df_train)
df_test <- predict(preprocessor, df_test)

# one-hot encode our categorical variables
one_hot <- dummyVars(~., data = df_train[,-1], fullRank = TRUE)
df_train <- data.frame(predict(one_hot,newdata= df_train)) 
df_test <- data.frame(predict(one_hot,newdata= df_test)) 
# make ranger compatible names
names(df_train) <- make.names(names(df_train), allow_ = FALSE)
names(df_test) <- make.names(names(df_test), allow_ = FALSE)

# Reattach the target variable to the training data that has been 
# dropped by predict(dummies,...)
df_train$Transition_new <- df$Transition_new[trainRowNumbers]
df_test$Transition_new <- df$Transition_new[-trainRowNumbers]

from_o_scale <- readRDS("Data/Model/from_o_scale.rds")
pred_ranger <- predict(from_o_scale, df_test)
confusionMatrix(data=pred_ranger$predictions,reference = df_test$Transition_new, mode = "prec_recall")

