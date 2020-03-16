# From_e ------------------------------------------------------------------
library(ranger)
library(skimr)
library(caret)
library(UBL)
library(DMwR)
# call all our datasets df in order not to write to much code
load("Dropbox/Data 2017/from_e.RData")
df <- from_e
#for reproducability
set.seed(420)

df$Transition_new <- as.factor(df$Transition_new)

variables<- c("Transition_new","us_region","Education","Birth","Hispanic","Asian",
              "Black","Metropolitan","hh_single","veteran","Male","Recession","Before_Rec","After_Rec",
              "goverment_employee","self_employed","mult_job","own_house","rent_house",
              "industry","hh_income","absent_lw","AGE","NCHILD")

df <- df[,variables]


################################### Create Test and Training Set #######################################

#Get row numbers for the training data
trainRowNumbers <- createDataPartition(df$Transition_new, p=0.7, list=FALSE)

# Step 2: Create the training dataset
df_train<- df[trainRowNumbers,]
df_test <- df[-trainRowNumbers,]

# create feature names
y<- "Transition"
x<- setdiff(names(df),y)

#########################################################################################################
####################################### Normal Random Forest ############################################
#########################################################################################################

model <- ranger(
  formula         = Transition_new ~ .,
  data            = df_train,
  num.trees       = 100,
  mtry            = 12,
  min.node.size   = 50,
  sample.fraction = .7,
  importance      = 'impurity',
  seed            = 420
)

# save the model to disk
saveRDS(model, "Dropbox/Model/from_e_normal.rds")
pred_ranger <- predict(model, df_test)

# confusion matrix
confusionMatrix(data=pred_ranger$predictions,reference = df_test$Transition_new, mode = "prec_recall")
######################################################################################################### 
######################################## RF with resampling  ############################################
#########################################################################################################

#downsampling
down_train <- downSample(x=df_train[,-which(names(df_train)=="Transition_new")], y = df_train$Transition_new,yname="Transition_new")

model_down <- ranger(
  formula         = Transition_new ~ .,
  data            = down_train,
  num.trees       = 100,
  mtry            = 12,
  min.node.size   = 1000,
  sample.fraction = .7,
  importance      = 'impurity',
  seed            = 420
)

pred_ranger <- predict(model_down, df_test)

# confusion matrix
confusionMatrix(data=pred_ranger$predictions,reference = df_test$Transition_new, mode = "prec_recall")



# save the model to disk
saveRDS(model_down, "Dropbox/Model/from_e_down.rds")

#upsampling
up_train <- upSample(x=df_train[,-which(names(df_train)=="Transition_new")], y = df_train$Transition_new, yname = "Transition_new")

model_up <- ranger(
  formula         = Transition_new ~ .,
  data            = up_train,
  num.trees       = 1000,
  mtry            = 6,
  min.node.size   = 50,
  sample.fraction = .7,
  importance      = 'impurity',
  seed            = 420
)

# save the model to disk
saveRDS(model_up, "Dropbox/Model/from_e_up.rds")
# SMOTE
C.perc = list(E2E=.05,E2U=2,E2O=2)
smote_train <- SmoteClassif(Transition_new~.,as.data.frame(df_train),C.perc, k = 5, repl = TRUE, dist = "HEOM")


model_smote <- ranger(
  formula         = Transition_new ~ .,
  data            = smote_train,
  num.trees       = 100,
  mtry            = 12,
  min.node.size   = 50,
  sample.fraction = .7,
  importance      = 'impurity',
  seed            = 420
)

# save the model to disk
saveRDS(model_smote, "Dropbox/Model/from_e_smote.rds")

######################################################################################################### 
########################### RF with resampling, scaling, imputing and ohc ###############################
#########################################################################################################


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

# Reattach the target variable to the training data that has been 
# dropped by predict(dummies,...)
df_train$Transition_new <- df$Transition_new[trainRowNumbers]
df_test$Transition_new <- df$Transition_new[-trainRowNumbers]

model_scale <- ranger(
  formula         = Transition_new ~ .,
  data            = trainTransformed,
  num.trees       = 100,
  mtry            = 12,
  min.node.size   = 50,
  sample.fraction = .7,
  importance      = 'impurity',
  seed            = 420
)

# save the model to disk
saveRDS(model_scale, "Model/from_e_scale.rds")

rm(list=ls())


# From_u ------------------------------------------------------------------
library(ranger)
library(skimr)
library(caret)
library(UBL)
library(DMwR)
# call all our datasets df in order not to write to much code
load("Dropbox/Data 2017/from_u.RData")
df <- from_u
#for reproducability
set.seed(420)

df$Transition_new <- as.factor(df$Transition_new)

variables<- c("Transition_new","us_region","Education","Birth","Hispanic","Asian",
              "Black","Metropolitan","hh_single","veteran","Male","Recession","Before_Rec","After_Rec",
              "own_house","rent_house",
              "weeks_unemp","hh_income","AGE","NCHILD")

df <- df[,variables]


################################### Create Test and Training Set #######################################

#Get row numbers for the training data
trainRowNumbers <- createDataPartition(df$Transition_new, p=0.7, list=FALSE)

# Step 2: Create the training dataset
df_train<- df[trainRowNumbers,]
df_test <- df[-trainRowNumbers,]

# create feature names
y<- "Transition"
x<- setdiff(names(df),y)

#########################################################################################################
####################################### Normal Random Forest ############################################
#########################################################################################################

model <- ranger(
  formula         = Transition_new ~ .,
  data            = df_train,
  num.trees       = 100,
  mtry            = 6,
  min.node.size   = 200,
  sample.fraction = .7,
  importance      = 'impurity',
  seed            = 420
)

# save the model to disk
saveRDS(model, "Dropbox/Model/from_u_normal.rds")
pred_ranger <- predict(model, df_test)

# confusion matrix
confusionMatrix(data=pred_ranger$predictions,reference = df_test$Transition_new, mode = "prec_recall")
######################################################################################################### 
######################################## RF with resampling  ############################################
#########################################################################################################

#downsampling
down_train <- downSample(x=df_train[,-which(names(df_train)=="Transition_new")], y = df_train$Transition_new,yname="Transition_new")

model_down <- ranger(
  formula         = Transition_new ~ .,
  data            = down_train,
  num.trees       = 100,
  mtry            = 6,
  min.node.size   = 200,
  sample.fraction = .7,
  importance      = 'impurity',
  seed            = 420
)

pred_ranger <- predict(model_down, df_test)

# confusion matrix
confusionMatrix(data=pred_ranger$predictions,reference = df_test$Transition_new, mode = "prec_recall")
# save the model to disk
saveRDS(model_down, "Dropbox/Model/from_u_down.rds")

#upsampling
up_train <- upSample(x=df_train[,-which(names(df_train)=="Transition_new")], y = df_train$Transition_new, yname = "Transition_new")

model_up <- ranger(
  formula         = Transition_new ~ .,
  data            = up_train,
  num.trees       = 2000,
  mtry            = 6,
  min.node.size   = 50,
  sample.fraction = .7,
  importance      = 'impurity',
  seed            = 420
)

# save the model to disk
saveRDS(model_up, "Dropbox/Model/from_u_up.rds")
# SMOTE
C.perc = list(U2E=1.5,U2U=.8,U2O=2)
smote_train <- SmoteClassif(Transition_new~.,as.data.frame(df_train),C.perc, k = 5, repl = TRUE, dist = "HEOM")


model_smote <- ranger(
  formula         = Transition_new ~ .,
  data            = smote_train,
  num.trees       = 100,
  mtry            = 12,
  min.node.size   = 50,
  sample.fraction = .7,
  importance      = 'impurity',
  seed            = 420
)

# save the model to disk
saveRDS(model_smote, "Dropbox/Model/from_u_smote.rds")

######################################################################################################### 
########################### RF with resampling, scaling, imputing and ohc ###############################
#########################################################################################################

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

# Reattach the target variable to the training data that has been 
# dropped by predict(dummies,...)
df_train$Transition_new <- df$Transition_new[trainRowNumbers]
df_test$Transition_new <- df$Transition_new[-trainRowNumbers]

model_scale <- ranger(
  formula         = Transition_new ~ .,
  data            = df_train,
  num.trees       = 100,
  mtry            = 6,
  min.node.size   = 100,
  sample.fraction = .7,
  importance      = 'impurity',
  seed            = 420
)

# save the model to disk
saveRDS(model_scale, "Model/from_u_scale.rds")

rm(list=ls())


# From_o ------------------------------------------------------------------
library(ranger)
library(skimr)
library(caret)
library(UBL)
library(DMwR)
# call all our datasets df in order not to write to much code
load("Dropbox/Data 2017/from_o.RData")
df <- from_o
#for reproducability
set.seed(420)

df$Transition_new <- as.factor(df$Transition_new)

variables<- c("Transition_new","us_region","Education","Birth","Hispanic","Asian",
              "Black","Metropolitan","hh_single","veteran","Male","Recession","Before_Rec","After_Rec",
              "own_house","rent_house",
              "hh_income","AGE","NCHILD")

df <- df[,variables]


################################### Create Test and Training Set #######################################

#Get row numbers for the training data
trainRowNumbers <- createDataPartition(df$Transition_new, p=0.7, list=FALSE)

# Step 2: Create the training dataset
df_train<- df[trainRowNumbers,]
df_test <- df[-trainRowNumbers,]

# create feature names
y<- "Transition"
x<- setdiff(names(df),y)

#########################################################################################################
####################################### Normal Random Forest ############################################
#########################################################################################################

model <- ranger(
  formula         = Transition_new ~ .,
  data            = df_train,
  num.trees       = 100,
  mtry            = 12,
  min.node.size   = 50,
  sample.fraction = .7,
  importance      = 'impurity',
  seed            = 420
)

# save the model to disk
saveRDS(model, "Dropbox/Model/from_o_normal.rds")
pred_ranger <- predict(model, df_test)

# confusion matrix
confusionMatrix(data=pred_ranger$predictions,reference = df_test$Transition_new, mode = "prec_recall")
######################################################################################################### 
######################################## RF with resampling  ############################################
#########################################################################################################

#downsampling
down_train <- downSample(x=df_train[,-which(names(df_train)=="Transition_new")], y = df_train$Transition_new,yname="Transition_new")

model_down <- ranger(
  formula         = Transition_new ~ .,
  data            = down_train,
  num.trees       = 100,
  mtry            = 6,
  min.node.size   = 200,
  sample.fraction = .7,
  importance      = 'impurity',
  seed            = 420
)

pred_ranger <- predict(model_down, df_test)

# confusion matrix
confusionMatrix(data=pred_ranger$predictions,reference = df_test$Transition_new, mode = "prec_recall")
# save the model to disk
saveRDS(model_down, "Dropbox/Model/from_o_down.rds")

#upsampling
up_train <- upSample(x=df_train[,-which(names(df_train)=="Transition_new")], y = df_train$Transition_new, yname = "Transition_new")

model_up <- ranger(
  formula         = Transition_new ~ .,
  data            = up_train,
  num.trees       = 2000,
  mtry            = 6,
  min.node.size   = 50,
  sample.fraction = .7,
  importance      = 'impurity',
  seed            = 420
)

# save the model to disk
saveRDS(model_up, "Dropbox/Model/from_o_up.rds")
# SMOTE
C.perc = list(O2E=3,O2U=6,O2O=.5)
smote_train <- SmoteClassif(Transition_new~.,as.data.frame(df_train),C.perc, k = 5, repl = TRUE, dist = "HEOM")


model_smote <- ranger(
  formula         = Transition_new ~ .,
  data            = smote_train,
  num.trees       = 100,
  mtry            = 6,
  min.node.size   = 50,
  sample.fraction = .7,
  importance      = 'impurity',
  seed            = 420
)

# save the model to disk
saveRDS(model_smote, "Dropbox/Model/from_o_smote.rds")

######################################################################################################### 
########################### RF with resampling, scaling, imputing and ohc ###############################
#########################################################################################################

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

# Reattach the target variable to the training data that has been 
# dropped by predict(dummies,...)
df_train$Transition_new <- df$Transition_new[trainRowNumbers]
df_test$Transition_new <- df$Transition_new[-trainRowNumbers]

model_scale <- ranger(
  formula         = Transition_new ~ .,
  data            = df_train,
  num.trees       = 100,
  mtry            = 6,
  min.node.size   = 100,
  sample.fraction = .7,
  importance      = 'impurity',
  seed            = 420
)

# save the model to disk
saveRDS(model_scale, "Model/from_o_scale.rds")

rm(list=ls())



# From_e_single ------------------------------------------------------------------
library(ranger)
library(skimr)
library(caret)
library(UBL)
library(DMwR)
# call all our datasets df in order not to write to much code
load("Dropbox/Data 2017/from_e_single.RData")
df <- from_e_single
#for reproducability
set.seed(420)

df$Transition_new <- as.factor(df$Transition_new)

variables<- c("Transition_new","us_region","Education","Birth","Hispanic","Asian",
              "Black","Metropolitan","hh_single","veteran","Male","Recession","Before_Rec","After_Rec",
              "goverment_employee","self_employed","mult_job","own_house","rent_house",
              "industry","hh_income","absent_lw","AGE","NCHILD")

df <- df[,variables]


################################### Create Test and Training Set #######################################

#Get row numbers for the training data
trainRowNumbers <- createDataPartition(df$Transition_new, p=0.7, list=FALSE)

# Step 2: Create the training dataset
df_train<- df[trainRowNumbers,]
df_test <- df[-trainRowNumbers,]

# create feature names
y<- "Transition"
x<- setdiff(names(df),y)

#########################################################################################################
####################################### Normal Random Forest ############################################
#########################################################################################################

model <- ranger(
  formula         = Transition_new ~ .,
  data            = df_train,
  num.trees       = 100,
  mtry            = 12,
  min.node.size   = 50,
  sample.fraction = .7,
  importance      = 'impurity',
  seed            = 420
)

# save the model to disk
saveRDS(model, "Dropbox/Model/from_e_single_normal.rds")
pred_ranger <- predict(model, df_test)

# confusion matrix
confusionMatrix(data=pred_ranger$predictions,reference = df_test$Transition_new, mode = "prec_recall")
######################################################################################################### 
######################################## RF with resampling  ############################################
#########################################################################################################

#downsampling
down_train <- downSample(x=df_train[,-which(names(df_train)=="Transition_new")], y = df_train$Transition_new,yname="Transition_new")

model_down <- ranger(
  formula         = Transition_new ~ .,
  data            = down_train,
  num.trees       = 100,
  mtry            = 12,
  min.node.size   = 1000,
  sample.fraction = .7,
  importance      = 'impurity',
  seed            = 420
)

pred_ranger <- predict(model_down, df_test)

# confusion matrix
confusionMatrix(data=pred_ranger$predictions,reference = df_test$Transition_new, mode = "prec_recall")



# save the model to disk
saveRDS(model_down, "Dropbox/Model/from_e_single_down.rds")

#upsampling
# up_train <- upSample(x=df_train[,-which(names(df_train)=="Transition_new")], y = df_train$Transition_new, yname = "Transition_new")
# 
# model_up <- ranger(
#   formula         = Transition_new ~ .,
#   data            = up_train,
#   num.trees       = 1000,
#   mtry            = 6,
#   min.node.size   = 50,
#   sample.fraction = .7,
#   #case.weights    = weight,
#   importance      = 'impurity',
#   seed            = 420
# )
# 
# # save the model to disk
# saveRDS(model_up, "Dropbox/Model/from_e_single_up.rds")
# SMOTE
C.perc = list(E2E=.2,E2U=5,E2O=5)
smote_train <- SmoteClassif(Transition_new~.,as.data.frame(df_train),C.perc, k = 5, repl = TRUE, dist = "HEOM")


model_smote <- ranger(
  formula         = Transition_new ~ .,
  data            = smote_train,
  num.trees       = 100,
  mtry            = 12,
  min.node.size   = 50,
  sample.fraction = .7,
  importance      = 'impurity',
  seed            = 420
)

# save the model to disk
saveRDS(model_smote, "Dropbox/Model/from_e_single_smote.rds")

######################################################################################################### 
########################### RF with resampling, scaling, imputing and ohc ###############################
#########################################################################################################


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

# Reattach the target variable to the training data that has been 
# dropped by predict(dummies,...)
df_train$Transition_new <- df$Transition_new[trainRowNumbers]
df_test$Transition_new <- df$Transition_new[-trainRowNumbers]

model_scale <- ranger(
  formula         = Transition_new ~ .,
  data            = trainTransformed,
  num.trees       = 100,
  mtry            = 12,
  min.node.size   = 50,
  sample.fraction = .7,
  importance      = 'impurity',
  seed            = 420
)

# save the model to disk
saveRDS(model_scale, "Model/from_e_single_scale.rds")

rm(list=ls())


# From_u_single ------------------------------------------------------------------
library(ranger)
library(skimr)
library(caret)
library(UBL)
library(DMwR)
# call all our datasets df in order not to write to much code
load("Dropbox/Data 2017/from_u_single.RData")
df <- from_u_single
#for reproducability
set.seed(420)

df$Transition_new <- as.factor(df$Transition_new)
weight <- df$HWTFINL

variables<- c("Transition_new","us_region","Education","Birth","Hispanic","Asian",
              "Black","Metropolitan","hh_single","veteran","Male","Recession","Before_Rec","After_Rec",
              "own_house","rent_house",
              "weeks_unemp","hh_income","AGE","NCHILD")

df <- df[,variables]


################################### Create Test and Training Set #######################################

#Get row numbers for the training data
trainRowNumbers <- createDataPartition(df$Transition_new, p=0.7, list=FALSE)

# Step 2: Create the training dataset
df_train<- df[trainRowNumbers,]
df_test <- df[-trainRowNumbers,]

# create feature names
y<- "Transition"
x<- setdiff(names(df),y)

#########################################################################################################
####################################### Normal Random Forest ############################################
#########################################################################################################

model <- ranger(
  formula         = Transition_new ~ .,
  data            = df_train,
  num.trees       = 100,
  mtry            = 6,
  min.node.size   = 200,
  sample.fraction = .7,
  importance      = 'impurity',
  seed            = 420
)

# save the model to disk
saveRDS(model, "Dropbox/Model/from_u_single_normal.rds")
pred_ranger <- predict(model, df_test)

# confusion matrix
confusionMatrix(data=pred_ranger$predictions,reference = df_test$Transition_new, mode = "prec_recall")
######################################################################################################### 
######################################## RF with resampling  ############################################
#########################################################################################################

#downsampling
down_train <- downSample(x=df_train[,-which(names(df_train)=="Transition_new")], y = df_train$Transition_new,yname="Transition_new")

model_down <- ranger(
  formula         = Transition_new ~ .,
  data            = down_train,
  num.trees       = 100,
  mtry            = 6,
  min.node.size   = 200,
  sample.fraction = .7,
  importance      = 'impurity',
  seed            = 420
)

pred_ranger <- predict(model_down, df_test)

# confusion matrix
confusionMatrix(data=pred_ranger$predictions,reference = df_test$Transition_new, mode = "prec_recall")
# save the model to disk
saveRDS(model_down, "Dropbox/Model/from_u_single_down.rds")

#upsampling
up_train <- upSample(x=df_train[,-which(names(df_train)=="Transition_new")], y = df_train$Transition_new, yname = "Transition_new")

model_up <- ranger(
  formula         = Transition_new ~ .,
  data            = up_train,
  num.trees       = 2000,
  mtry            = 6,
  min.node.size   = 50,
  sample.fraction = .7,
  importance      = 'impurity',
  seed            = 420
)

# save the model to disk
saveRDS(model_up, "Dropbox/Model/from_u_single_up.rds")
# SMOTE
C.perc = list(U2E=1.5,U2U=.8,U2O=2)
smote_train <- SmoteClassif(Transition_new~.,as.data.frame(df_train),C.perc, k = 5, repl = TRUE, dist = "HEOM")


model_smote <- ranger(
  formula         = Transition_new ~ .,
  data            = smote_train,
  num.trees       = 100,
  mtry            = 12,
  min.node.size   = 50,
  sample.fraction = .7,
  importance      = 'impurity',
  seed            = 420
)

# save the model to disk
saveRDS(model_smote, "Dropbox/Model/from_u_single_smote.rds")

######################################################################################################### 
########################### RF with resampling, scaling, imputing and ohc ###############################
#########################################################################################################

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

# Reattach the target variable to the training data that has been 
# dropped by predict(dummies,...)
df_train$Transition_new <- df$Transition_new[trainRowNumbers]
df_test$Transition_new <- df$Transition_new[-trainRowNumbers]

model_scale <- ranger(
  formula         = Transition_new ~ .,
  data            = df_train,
  num.trees       = 100,
  mtry            = 6,
  min.node.size   = 100,
  sample.fraction = .7,
  importance      = 'impurity',
  seed            = 420
)

# save the model to disk
saveRDS(model_scale, "Model/from_u_single_scale.rds")

rm(list=ls())


# From_o_single ------------------------------------------------------------------
library(ranger)
library(skimr)
library(caret)
library(UBL)
library(DMwR)
# call all our datasets df in order not to write to much code
load("Dropbox/Data 2017/from_o_single.RData")
df <- from_o_single
#for reproducability
set.seed(420)

df$Transition_new <- as.factor(df$Transition_new)

variables<- c("Transition_new","us_region","Education","Birth","Hispanic","Asian",
              "Black","Metropolitan","hh_single","veteran","Male","Recession","Before_Rec","After_Rec",
              "own_house","rent_house",
              "hh_income","AGE","NCHILD")

df <- df[,variables]


################################### Create Test and Training Set #######################################

#Get row numbers for the training data
trainRowNumbers <- createDataPartition(df$Transition_new, p=0.7, list=FALSE)

# Step 2: Create the training dataset
df_train<- df[trainRowNumbers,]
df_test <- df[-trainRowNumbers,]

# create feature names
y<- "Transition"
x<- setdiff(names(df),y)

#########################################################################################################
####################################### Normal Random Forest ############################################
#########################################################################################################

model <- ranger(
  formula         = Transition_new ~ .,
  data            = df_train,
  num.trees       = 100,
  mtry            = 12,
  min.node.size   = 50,
  sample.fraction = .7,
  importance      = 'impurity',
  seed            = 420
)

# save the model to disk
saveRDS(model, "Dropbox/Model/from_o_single_normal.rds")
pred_ranger <- predict(model, df_test)

# confusion matrix
confusionMatrix(data=pred_ranger$predictions,reference = df_test$Transition_new, mode = "prec_recall")
######################################################################################################### 
######################################## RF with resampling  ############################################
#########################################################################################################

#downsampling
down_train <- downSample(x=df_train[,-which(names(df_train)=="Transition_new")], y = df_train$Transition_new,yname="Transition_new")

model_down <- ranger(
  formula         = Transition_new ~ .,
  data            = down_train,
  num.trees       = 100,
  mtry            = 6,
  min.node.size   = 200,
  sample.fraction = .7,
  importance      = 'impurity',
  seed            = 420
)

pred_ranger <- predict(model_down, df_test)

# confusion matrix
confusionMatrix(data=pred_ranger$predictions,reference = df_test$Transition_new, mode = "prec_recall")
# save the model to disk
saveRDS(model_down, "Dropbox/Model/from_o_single_down.rds")

#upsampling
up_train <- upSample(x=df_train[,-which(names(df_train)=="Transition_new")], y = df_train$Transition_new, yname = "Transition_new")

model_up <- ranger(
  formula         = Transition_new ~ .,
  data            = up_train,
  num.trees       = 2000,
  mtry            = 6,
  min.node.size   = 50,
  sample.fraction = .7,
  importance      = 'impurity',
  seed            = 420
)

# save the model to disk
saveRDS(model_up, "Dropbox/Model/from_o_single_up.rds")
# SMOTE
C.perc = list(O2E=3,O2U=6,O2O=.5)
smote_train <- SmoteClassif(Transition_new~.,as.data.frame(df_train),C.perc, k = 5, repl = TRUE, dist = "HEOM")


model_smote <- ranger(
  formula         = Transition_new ~ .,
  data            = smote_train,
  num.trees       = 100,
  mtry            = 6,
  min.node.size   = 50,
  sample.fraction = .7,
  importance      = 'impurity',
  seed            = 420
)

# save the model to disk
saveRDS(model_smote, "Dropbox/Model/from_o_single_smote.rds")

######################################################################################################### 
########################### RF with resampling, scaling, imputing and ohc ###############################
#########################################################################################################

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

# Reattach the target variable to the training data that has been 
# dropped by predict(dummies,...)
df_train$Transition_new <- df$Transition_new[trainRowNumbers]
df_test$Transition_new <- df$Transition_new[-trainRowNumbers]

model_scale <- ranger(
  formula         = Transition_new ~ .,
  data            = df_train,
  num.trees       = 100,
  mtry            = 6,
  min.node.size   = 100,
  sample.fraction = .7,
  importance      = 'impurity',
  seed            = 420
)

# save the model to disk
saveRDS(model_scale, "Model/from_o_single_scale.rds")

rm(list=ls())



# From_e_cp ------------------------------------------------------------------
library(ranger)
library(skimr)
library(caret)
library(UBL)
library(DMwR)
# call all our datasets df in order not to write to much code
load("Dropbox/Data 2017/from_e_cp.RData")
df <- from_e_cp
#for reproducability
set.seed(420)

df$Transition_new <- as.factor(df$Transition_new)

variables<- c("Transition_new","us_region","Education","Birth","Hispanic","Asian",
              "Black","Metropolitan","hh_single","veteran","Male","Recession","Before_Rec","After_Rec",
              "goverment_employee","self_employed","mult_job","own_house","rent_house",
              "industry","hh_income","absent_lw","AGE","NCHILD")

df <- df[,variables]


################################### Create Test and Training Set #######################################

#Get row numbers for the training data
trainRowNumbers <- createDataPartition(df$Transition_new, p=0.7, list=FALSE)

# Step 2: Create the training dataset
df_train<- df[trainRowNumbers,]
df_test <- df[-trainRowNumbers,]

# create feature names
y<- "Transition"
x<- setdiff(names(df),y)

#########################################################################################################
####################################### Normal Random Forest ############################################
#########################################################################################################

model <- ranger(
  formula         = Transition_new ~ .,
  data            = df_train,
  num.trees       = 100,
  mtry            = 12,
  min.node.size   = 50,
  sample.fraction = .7,
  importance      = 'impurity',
  seed            = 420
)

# save the model to disk
saveRDS(model, "Dropbox/Model/from_e_cp_normal.rds")
pred_ranger <- predict(model, df_test)

# confusion matrix
confusionMatrix(data=pred_ranger$predictions,reference = df_test$Transition_new, mode = "prec_recall")
######################################################################################################### 
######################################## RF with resampling  ############################################
#########################################################################################################

#downsampling
down_train <- downSample(x=df_train[,-which(names(df_train)=="Transition_new")], y = df_train$Transition_new,yname="Transition_new")

model_down <- ranger(
  formula         = Transition_new ~ .,
  data            = down_train,
  num.trees       = 100,
  mtry            = 12,
  min.node.size   = 1000,
  sample.fraction = .7,
  importance      = 'impurity',
  seed            = 420
)

pred_ranger <- predict(model_down, df_test)

# confusion matrix
confusionMatrix(data=pred_ranger$predictions,reference = df_test$Transition_new, mode = "prec_recall")



# save the model to disk
saveRDS(model_down, "Dropbox/Model/from_e_cp_down.rds")

#upsampling
# up_train <- upSample(x=df_train[,-which(names(df_train)=="Transition_new")], y = df_train$Transition_new, yname = "Transition_new")
# 
# model_up <- ranger(
#   formula         = Transition_new ~ .,
#   data            = up_train,
#   num.trees       = 1000,
#   mtry            = 6,
#   min.node.size   = 50,
#   sample.fraction = .7,
#   #case.weights    = weight,
#   importance      = 'impurity',
#   seed            = 420
# )
# 
# # save the model to disk
# saveRDS(model_up, "Dropbox/Model/from_e_up.rds")
# SMOTE
C.perc = list(E2E=.2,E2U=4,E2O=4)
smote_train <- SmoteClassif(Transition_new~.,as.data.frame(df_train),C.perc, k = 5, repl = TRUE, dist = "HEOM")


model_smote <- ranger(
  formula         = Transition_new ~ .,
  data            = smote_train,
  num.trees       = 100,
  mtry            = 12,
  min.node.size   = 50,
  sample.fraction = .7,
  importance      = 'impurity',
  seed            = 420
)

# save the model to disk
saveRDS(model_smote, "Dropbox/Model/from_e_cp_smote.rds")

######################################################################################################### 
########################### RF with resampling, scaling, imputing and ohc ###############################
#########################################################################################################


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

# Reattach the target variable to the training data that has been 
# dropped by predict(dummies,...)
df_train$Transition_new <- df$Transition_new[trainRowNumbers]
df_test$Transition_new <- df$Transition_new[-trainRowNumbers]

model_scale <- ranger(
  formula         = Transition_new ~ .,
  data            = trainTransformed,
  num.trees       = 100,
  mtry            = 12,
  min.node.size   = 50,
  sample.fraction = .7,
  importance      = 'impurity',
  seed            = 420
)

# save the model to disk
saveRDS(model_scale, "Model/from_e_cp_scale.rds")

rm(list=ls())


# From_u_cp ------------------------------------------------------------------
library(ranger)
library(skimr)
library(caret)
library(UBL)
library(DMwR)
# call all our datasets df in order not to write to much code
load("Dropbox/Data 2017/from_u_cp.RData")
df <- from_u_cp
#for reproducability
set.seed(420)

df$Transition_new <- as.factor(df$Transition_new)

variables<- c("Transition_new","us_region","Education","Birth","Hispanic","Asian",
              "Black","Metropolitan","hh_single","veteran","Male","Recession","Before_Rec","After_Rec",
              "own_house","rent_house",
              "weeks_unemp","hh_income","AGE","NCHILD")

df <- df[,variables]


################################### Create Test and Training Set #######################################

#Get row numbers for the training data
trainRowNumbers <- createDataPartition(df$Transition_new, p=0.7, list=FALSE)

# Step 2: Create the training dataset
df_train<- df[trainRowNumbers,]
df_test <- df[-trainRowNumbers,]

# create feature names
y<- "Transition"
x<- setdiff(names(df),y)

#########################################################################################################
####################################### Normal Random Forest ############################################
#########################################################################################################

model <- ranger(
  formula         = Transition_new ~ .,
  data            = df_train,
  num.trees       = 100,
  mtry            = 6,
  min.node.size   = 200,
  sample.fraction = .7,
  #case.weights    = weight,
  importance      = 'impurity',
  seed            = 420
)

# save the model to disk
saveRDS(model, "Dropbox/Model/from_u_cp_normal.rds")
pred_ranger <- predict(model, df_test)

# confusion matrix
confusionMatrix(data=pred_ranger$predictions,reference = df_test$Transition_new, mode = "prec_recall")
######################################################################################################### 
######################################## RF with resampling  ############################################
#########################################################################################################

#downsampling
down_train <- downSample(x=df_train[,-which(names(df_train)=="Transition_new")], y = df_train$Transition_new,yname="Transition_new")

model_down <- ranger(
  formula         = Transition_new ~ .,
  data            = down_train,
  num.trees       = 100,
  mtry            = 6,
  min.node.size   = 200,
  sample.fraction = .7,
  importance      = 'impurity',
  seed            = 420
)

pred_ranger <- predict(model_down, df_test)

# confusion matrix
confusionMatrix(data=pred_ranger$predictions,reference = df_test$Transition_new, mode = "prec_recall")
# save the model to disk
saveRDS(model_down, "Dropbox/Model/from_u_cp_down.rds")

#upsampling
up_train <- upSample(x=df_train[,-which(names(df_train)=="Transition_new")], y = df_train$Transition_new, yname = "Transition_new")

model_up <- ranger(
  formula         = Transition_new ~ .,
  data            = up_train,
  num.trees       = 2000,
  mtry            = 6,
  min.node.size   = 50,
  sample.fraction = .7,
  importance      = 'impurity',
  seed            = 420
)

# save the model to disk
saveRDS(model_up, "Dropbox/Model/from_u_cp_up.rds")
# SMOTE
C.perc = list(U2E=1.5,U2U=.8,U2O=2)
smote_train <- SmoteClassif(Transition_new~.,as.data.frame(df_train),C.perc, k = 5, repl = TRUE, dist = "HEOM")


model_smote <- ranger(
  formula         = Transition_new ~ .,
  data            = smote_train,
  num.trees       = 100,
  mtry            = 12,
  min.node.size   = 50,
  sample.fraction = .7,
  importance      = 'impurity',
  seed            = 420
)

# save the model to disk
saveRDS(model_smote, "Dropbox/Model/from_u_cp_smote.rds")

######################################################################################################### 
########################### RF with resampling, scaling, imputing and ohc ###############################
#########################################################################################################

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

# Reattach the target variable to the training data that has been 
# dropped by predict(dummies,...)
df_train$Transition_new <- df$Transition_new[trainRowNumbers]
df_test$Transition_new <- df$Transition_new[-trainRowNumbers]

model_scale <- ranger(
  formula         = Transition_new ~ .,
  data            = df_train,
  num.trees       = 100,
  mtry            = 6,
  min.node.size   = 100,
  sample.fraction = .7,
  importance      = 'impurity',
  seed            = 420
)

# save the model to disk
saveRDS(model_scale, "Model/from_u_cp_scale.rds")

rm(list=ls())


# From_o_cp ------------------------------------------------------------------
library(ranger)
library(skimr)
library(caret)
library(UBL)
library(DMwR)
# call all our datasets df in order not to write to much code
load("Dropbox/Data 2017/from_o_cp.RData")
df <- from_o_cp
#for reproducability
set.seed(420)

df$Transition_new <- as.factor(df$Transition_new)
weight <- df$HWTFINL

variables<- c("Transition_new","us_region","Education","Birth","Hispanic","Asian",
              "Black","Metropolitan","hh_single","veteran","Male","Recession","Before_Rec","After_Rec",
              "own_house","rent_house",
              "hh_income","AGE","NCHILD")

df <- df[,variables]


################################### Create Test and Training Set #######################################

#Get row numbers for the training data
trainRowNumbers <- createDataPartition(df$Transition_new, p=0.7, list=FALSE)

# Step 2: Create the training dataset
df_train<- df[trainRowNumbers,]
df_test <- df[-trainRowNumbers,]

# create feature names
y<- "Transition"
x<- setdiff(names(df),y)

#########################################################################################################
####################################### Normal Random Forest ############################################
#########################################################################################################

model <- ranger(
  formula         = Transition_new ~ .,
  data            = df_train,
  num.trees       = 100,
  mtry            = 12,
  min.node.size   = 50,
  sample.fraction = .7,
  importance      = 'impurity',
  seed            = 420
)

# save the model to disk
saveRDS(model, "Dropbox/Model/from_o_cp_normal.rds")
pred_ranger <- predict(model, df_test)

# confusion matrix
confusionMatrix(data=pred_ranger$predictions,reference = df_test$Transition_new, mode = "prec_recall")
######################################################################################################### 
######################################## RF with resampling  ############################################
#########################################################################################################

#downsampling
down_train <- downSample(x=df_train[,-which(names(df_train)=="Transition_new")], y = df_train$Transition_new,yname="Transition_new")

model_down <- ranger(
  formula         = Transition_new ~ .,
  data            = down_train,
  num.trees       = 100,
  mtry            = 6,
  min.node.size   = 200,
  sample.fraction = .7,
  importance      = 'impurity',
  seed            = 420
)

pred_ranger <- predict(model_down, df_test)

# confusion matrix
confusionMatrix(data=pred_ranger$predictions,reference = df_test$Transition_new, mode = "prec_recall")
# save the model to disk
saveRDS(model_down, "Dropbox/Model/from_o_cp_down.rds")

#upsampling
up_train <- upSample(x=df_train[,-which(names(df_train)=="Transition_new")], y = df_train$Transition_new, yname = "Transition_new")

model_up <- ranger(
  formula         = Transition_new ~ .,
  data            = up_train,
  num.trees       = 2000,
  mtry            = 6,
  min.node.size   = 50,
  sample.fraction = .7,
  importance      = 'impurity',
  seed            = 420
)

# save the model to disk
saveRDS(model_up, "Dropbox/Model/from_o_cp_up.rds")
# SMOTE
C.perc = list(O2E=3,O2U=6,O2O=.5)
smote_train <- SmoteClassif(Transition_new~.,as.data.frame(df_train),C.perc, k = 5, repl = TRUE, dist = "HEOM")


model_smote <- ranger(
  formula         = Transition_new ~ .,
  data            = smote_train,
  num.trees       = 100,
  mtry            = 6,
  min.node.size   = 50,
  sample.fraction = .7,
  importance      = 'impurity',
  seed            = 420
)

# save the model to disk
saveRDS(model_smote, "Dropbox/Model/from_o_cp_smote.rds")

######################################################################################################### 
########################### RF with resampling, scaling, imputing and ohc ###############################
#########################################################################################################

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

# Reattach the target variable to the training data that has been 
# dropped by predict(dummies,...)
df_train$Transition_new <- df$Transition_new[trainRowNumbers]
df_test$Transition_new <- df$Transition_new[-trainRowNumbers]

model_scale <- ranger(
  formula         = Transition_new ~ .,
  data            = df_train,
  num.trees       = 100,
  mtry            = 6,
  min.node.size   = 100,
  sample.fraction = .7,
  importance      = 'impurity',
  seed            = 420
)

# save the model to disk
saveRDS(model_scale, "Model/from_o_cp_scale.rds")

rm(list=ls())
