library(ranger)
library(ggplot2)
library(dplyr)
library(DMwR)
library(pdp)
############################## Visualizations ##################################
load("Data/Data Clean/cps_cleaned.RData")


ggplot(cps, aes(x=AGE)) +
  geom_histogram(color="#c8d3d5",fill="mediumpurple",bins = 31) +
  ggtitle("Histogram of Participant's Age") +
  labs(y='Count', x="Participant's Age")


######### Transition matrixes
cps <- cps[(cps$YEAR>=2006)&(cps$YEAR<=2016),]
single <- cps[cps$hh_single==1,]
couple <- cps[cps$hh_single!=1,]

# create transition matrix
x <- table(cps$Transition_new)
x1 <- cbind(x[1],x[3],x[2])
x2 <- cbind(x[7],x[9],x[8])
x3 <- cbind(x[4],x[6],x[5])

x1_freq <- prop.table(x1)
x2_freq <- prop.table(x2)
x3_freq <- prop.table(x3)
transition_matrix <- matrix(c(x1_freq,x2_freq,x3_freq), byrow = TRUE, nrow = 3)
transition_matrix <- round(transition_matrix,3)
rownames(transition_matrix) <- c("E","U","O")
colnames(transition_matrix) <- c("E","U","O")





########## variable importance ##################
from_e_normal <- readRDS("Data/Model/from_e_normal.rds")
from_e_normal$variable.importance %>% 
  tidy() %>%
  arrange(desc(x)) %>%
  top_n(10) %>%
  ggplot(aes(reorder(names,x),x)) + 
  geom_col(fill="mediumpurple") +
  coord_flip() +
  ggtitle("Top 10 important variables") +
  labs(x= "Variables", y = "Variable Importance")

from_e_down <- readRDS("Data/Model/from_e_down.rds")
from_e_down$variable.importance %>% 
  tidy() %>%
  arrange(desc(x)) %>%
  top_n(10) %>%
  ggplot(aes(reorder(names,x),x)) + 
  geom_col(fill="mediumpurple") +
  coord_flip() +
  ggtitle("Top 10 important variables") +
  labs(x= "Variables", y = "Variable Importance")

from_u_normal <- readRDS("Data/Model/from_u_normal.rds")
from_u_normal$variable.importance %>% 
  tidy() %>%
  arrange(desc(x)) %>%
  top_n(10) %>%
  ggplot(aes(reorder(names,x),x)) + 
  geom_col(fill="mediumpurple") +
  coord_flip() +
  ggtitle("Top 10 important variables") +
  labs(x= "Variables", y = "Variable Importance")

from_u_down <- readRDS("Data/Model/from_u_down.rds")
from_u_down$variable.importance %>% 
  tidy() %>%
  arrange(desc(x)) %>%
  top_n(10) %>%
  ggplot(aes(reorder(names,x),x)) + 
  geom_col(fill="mediumpurple") +
  coord_flip() +
  ggtitle("Top 10 important variables") +
  labs(x= "Variables", y = "Variable Importance")

from_u_up <- readRDS("Data/Model/from_u_up.rds")
from_u_up$variable.importance %>% 
  tidy() %>%
  arrange(desc(x)) %>%
  top_n(10) %>%
  ggplot(aes(reorder(names,x),x)) + 
  geom_col(fill="mediumpurple") +
  coord_flip() +
  ggtitle("Top 10 important variables") +
  labs(x= "Variables", y = "Variable Importance")

from_u_scale <- readRDS("Data/Model/from_u_scale.rds")
from_u_scale$variable.importance %>% 
  tidy() %>%
  arrange(desc(x)) %>%
  top_n(10) %>%
  ggplot(aes(reorder(names,x),x)) + 
  geom_col(fill="mediumpurple") +
  coord_flip() +
  ggtitle("Top 10 important variables") +
  labs(x= "Variables", y = "Variable Importance")

from_o_normal <- readRDS("Data/Model/from_o_normal.rds")
from_o_normal$variable.importance %>% 
  tidy() %>%
  arrange(desc(x)) %>%
  top_n(10) %>%
  ggplot(aes(reorder(names,x),x)) + 
  geom_col(fill="mediumpurple") +
  coord_flip() +
  ggtitle("Top 10 important variables") +
  labs(x= "Variables", y = "Variable Importance")

from_o_down <- readRDS("Data/Model/from_o_down.rds")
from_o_down$variable.importance %>% 
  tidy() %>%
  arrange(desc(x)) %>%
  top_n(10) %>%
  ggplot(aes(reorder(names,x),x)) + 
  geom_col(fill="mediumpurple") +
  coord_flip() +
  ggtitle("Top 10 important variables") +
  labs(x= "Variables", y = "Variable Importance")

from_o_up <- readRDS("Data/Model/from_o_up.rds")
from_o_up$variable.importance %>% 
  tidy() %>%
  arrange(desc(x)) %>%
  top_n(10) %>%
  ggplot(aes(reorder(names,x),x)) + 
  geom_col(fill="mediumpurple") +
  coord_flip() +
  ggtitle("Top 10 important variables") +
  labs(x= "Variables", y = "Variable Importance")


from_o_scale <- readRDS("Data/Model/from_o_scale.rds")
from_o_scale$variable.importance %>% 
  tidy() %>%
  arrange(desc(x)) %>%
  top_n(10) %>%
  ggplot(aes(reorder(names,x),x)) + 
  geom_col(fill="mediumpurple") +
  coord_flip() +
  ggtitle("Top 10 important variables") +
  labs(x= "Variables", y = "Variable Importance")

################# Partial dependence plot
memory.limit(24000)
library(ranger)
library(ggplot2)
library(dplyr)
library(DMwR)
library(pdp)
library(caret)
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

from_e_down_prob <- readRDS("Data/Prob/from_e_down_prob.rds")
down_train <- downSample(x=df_train[,-which(names(df_train)=="Transition_new")], y = df_train$Transition_new,yname="Transition_new")

pd<-NULL
for(i in 1:3){
  tmp <- partial(from_e_down_prob, pred.var = c("AGE"),
                 which.class = i, grid.resolution = 101, progress = "text")
  pd <- rbind(pd, cbind(tmp, Transition_new = levels(df$Transition_new)[i]))
}  

### log odds, positive value -> positive relationship between feature and target
ggplot(pd, aes(x = AGE, y = yhat, z = Transition_new)) +
  geom_point(col="mediumpurple") +facet_grid(~ Transition_new)+
  labs(x= "Age", y = "Log-odds")

pd2<-NULL
for(i in 1:3){
  tmp <- partial(from_e_down_prob, pred.var = c("industry"),
                 which.class = i, grid.resolution = 101, progress = "text")
  pd2 <- rbind(pd2, cbind(tmp, Transition_new = levels(df$Transition_new)[i]))
}  

pd3<-NULL
for(i in 1:3){
  tmp <- partial(from_e_down_prob, pred.var = c("Male"),
                 which.class = i, grid.resolution = 101, progress = "text")
  pd3 <- rbind(pd3, cbind(tmp, Transition_new = levels(df$Transition_new)[i]))
}  

pd4<-NULL
for(i in 1:3){
  tmp <- partial(from_e_down_prob, pred.var = c("NCHILD"),
                 which.class = i, grid.resolution = 101, progress = "text")
  pd4 <- rbind(pd4, cbind(tmp, Transition_new = levels(df$Transition_new)[i]))
}  
pd5<-NULL
for(i in 1:3){
  tmp <- partial(from_e_down_prob, pred.var = c("Education"),
                 which.class = i, grid.resolution = 101, progress = "text")
  pd5 <- rbind(pd5, cbind(tmp, Transition_new = levels(df$Transition_new)[i]))
}  


### log odds, positive value -> positive relationship between feature and target
ggplot(pd2, aes(x = industry, y = yhat, z = Transition_new)) +
  geom_point(col="mediumpurple") +facet_grid(~ Transition_new)+
  labs(x= "Industry", y = "Log-odds") + theme(axis.text.x=element_text(angle = 45, hjust = 1))



### log odds, positive value -> positive relationship between feature and target
ggplot(pd3, aes(x = Male, y = yhat, z = Transition_new)) +
  geom_point(col="mediumpurple") +facet_grid(~ Transition_new)+
  labs(x= "Gender", y = "Log-odds")

### log odds, positive value -> positive relationship between feature and target
ggplot(pd4, aes(x = NCHILD, y = yhat, z = Transition_new)) +
  geom_point(col="mediumpurple") +facet_grid(~ Transition_new)+
  labs(x= "Number of Children", y = "Log-odds")

### log odds, positive value -> positive relationship between feature and target
ggplot(pd5, aes(x = Education, y = yhat, z = Transition_new)) +
  geom_point(col="mediumpurple") +facet_grid(~ Transition_new)+
  labs(x= "Education", y = "Log-odds") + theme(axis.text.x=element_text(angle = 45, hjust = 1))



########################from_u ######################
rm(list=ls())
library(ranger)
library(ggplot2)
library(dplyr)
library(DMwR)
library(pdp)
library(caret)
# call all our datasets df in order not to write to much code
load("Data/Data 2017/from_u.RData")
df <- from_u
#for reproducability
set.seed(420)

df$Transition_new <- as.factor(df$Transition_new)
weight <- df$HWTFINL

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

from_o_up_prob <- readRDS("Data/Prob/from_u_normal_prob.rds")
up_train <- upSample(x=df_train[,-which(names(df_train)=="Transition_new")], y = df_train$Transition_new, yname = "Transition_new")

pd<-NULL
for(i in 1:3){
  tmp <- partial(from_o_up_prob, pred.var = c("AGE"),
                 which.class = i, grid.resolution = 101, progress = "text")
  pd <- rbind(pd, cbind(tmp, Transition_new = levels(df$Transition_new)[i]))
}  

### log odds, positive value -> positive relationship between feature and target
ggplot(pd, aes(x = AGE, y = yhat, z = Transition_new)) +
  geom_point(col="mediumpurple") +facet_grid(~ Transition_new)+
  labs(x= "Age", y = "Log-odds")

pd2<-NULL
for(i in 1:3){
  tmp <- partial(from_o_up_prob, pred.var = c("us_region"),
                 which.class = i, grid.resolution = 101, progress = "text")
  pd2 <- rbind(pd2, cbind(tmp, Transition_new = levels(df$Transition_new)[i]))
}  

pd3<-NULL
for(i in 1:3){
  tmp <- partial(from_o_up_prob, pred.var = c("Male"),
                 which.class = i, grid.resolution = 101, progress = "text")
  pd3 <- rbind(pd3, cbind(tmp, Transition_new = levels(df$Transition_new)[i]))
}  

pd4<-NULL
for(i in 1:3){
  tmp <- partial(from_o_up_prob, pred.var = c("NCHILD"),
                 which.class = i, grid.resolution = 101, progress = "text")
  pd4 <- rbind(pd4, cbind(tmp, Transition_new = levels(df$Transition_new)[i]))
}  
pd5<-NULL
for(i in 1:3){
  tmp <- partial(from_o_up_prob, pred.var = c("Education"),
                 which.class = i, grid.resolution = 101, progress = "text")
  pd5 <- rbind(pd5, cbind(tmp, Transition_new = levels(df$Transition_new)[i]))
}  

pd6<-NULL
for(i in 1:3){
  tmp <- partial(from_o_up_prob, pred.var = c("Recession"),
                 which.class = i, grid.resolution = 101, progress = "text")
  pd6 <- rbind(pd6, cbind(tmp, Transition_new = levels(df$Transition_new)[i]))
}  

pd7<-NULL
for(i in 1:3){
  tmp <- partial(from_o_up_prob, pred.var = c("weeks_unemp"),
                 which.class = i, grid.resolution = 101, progress = "text")
  pd7 <- rbind(pd7, cbind(tmp, Transition_new = levels(df$Transition_new)[i]))
}

### log odds, positive value -> positive relationship between feature and target
ggplot(pd2, aes(x = us_region, y = yhat, z = Transition_new)) +
  geom_point(col="mediumpurple") +facet_grid(~ Transition_new)+
  labs(x= "Region", y = "Log-odds") + theme(axis.text.x=element_text(angle = 45, hjust = 1))



### log odds, positive value -> positive relationship between feature and target
ggplot(pd3, aes(x = Male, y = yhat, z = Transition_new)) +
  geom_point(col="mediumpurple") +facet_grid(~ Transition_new)+
  labs(x= "Gender", y = "Log-odds")

### log odds, positive value -> positive relationship between feature and target
ggplot(pd4, aes(x = NCHILD, y = yhat, z = Transition_new)) +
  geom_point(col="mediumpurple") +facet_grid(~ Transition_new)+
  labs(x= "Number of Children", y = "Log-odds")

### log odds, positive value -> positive relationship between feature and target
ggplot(pd5, aes(x = Education, y = yhat, z = Transition_new)) +
  geom_point(col="mediumpurple") +facet_grid(~ Transition_new)+
  labs(x= "Education", y = "Log-odds") + theme(axis.text.x=element_text(angle = 45, hjust = 1))


### log odds, positive value -> positive relationship between feature and target
ggplot(pd6, aes(x = Recession, y = yhat, z = Transition_new)) +
  geom_point(col="mediumpurple") +facet_grid(~ Transition_new)+
  labs(x= "Recession", y = "Log-odds") + theme(axis.text.x=element_text(angle = 45, hjust = 1))

### log odds, positive value -> positive relationship between feature and target
ggplot(pd7, aes(x = weeks_unemp, y = yhat, z = Transition_new)) +
  geom_point(col="mediumpurple") +facet_grid(~ Transition_new)+
  labs(x= "Weeks unemployed", y = "Log-odds") + theme(axis.text.x=element_text(angle = 45, hjust = 1))


########################from_o###############################
rm(list=ls())
library(ranger)
library(ggplot2)
library(dplyr)
library(DMwR)
library(pdp)
library(caret)
# call all our datasets df in order not to write to much code
load("Data/Data 2017/from_o.RData")
df <- from_o
#for reproducability
set.seed(420)

df$Transition_new <- as.factor(df$Transition_new)
weight <- df$HWTFINL

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

from_o_up_prob <- readRDS("Data/Prob/from_o_up_prob.rds")
up_train <- upSample(x=df_train[,-which(names(df_train)=="Transition_new")], y = df_train$Transition_new, yname = "Transition_new")

pd<-NULL
for(i in 1:3){
  tmp <- partial(from_o_up_prob, pred.var = c("AGE"),
                 which.class = i, grid.resolution = 101, progress = "text")
  pd <- rbind(pd, cbind(tmp, Transition_new = levels(df$Transition_new)[i]))
}  

### log odds, positive value -> positive relationship between feature and target
ggplot(pd, aes(x = AGE, y = yhat, z = Transition_new)) +
  geom_point(col="mediumpurple") +facet_grid(~ Transition_new)+
  labs(x= "Age", y = "Log-odds")

pd2<-NULL
for(i in 1:3){
  tmp <- partial(from_o_up_prob, pred.var = c("us_region"),
                 which.class = i, grid.resolution = 101, progress = "text")
  pd2 <- rbind(pd2, cbind(tmp, Transition_new = levels(df$Transition_new)[i]))
}  

pd3<-NULL
for(i in 1:3){
  tmp <- partial(from_o_up_prob, pred.var = c("Male"),
                 which.class = i, grid.resolution = 101, progress = "text")
  pd3 <- rbind(pd3, cbind(tmp, Transition_new = levels(df$Transition_new)[i]))
}  

pd4<-NULL
for(i in 1:3){
  tmp <- partial(from_o_up_prob, pred.var = c("NCHILD"),
                 which.class = i, grid.resolution = 101, progress = "text")
  pd4 <- rbind(pd4, cbind(tmp, Transition_new = levels(df$Transition_new)[i]))
}  
pd5<-NULL
for(i in 1:3){
  tmp <- partial(from_o_up_prob, pred.var = c("Education"),
                 which.class = i, grid.resolution = 101, progress = "text")
  pd5 <- rbind(pd5, cbind(tmp, Transition_new = levels(df$Transition_new)[i]))
}  

pd6<-NULL
for(i in 1:3){
  tmp <- partial(from_o_up_prob, pred.var = c("Recession"),
                 which.class = i, grid.resolution = 101, progress = "text")
  pd6 <- rbind(pd6, cbind(tmp, Transition_new = levels(df$Transition_new)[i]))
}  

### log odds, positive value -> positive relationship between feature and target
ggplot(pd2, aes(x = us_region, y = yhat, z = Transition_new)) +
  geom_point(col="mediumpurple") +facet_grid(~ Transition_new)+
  labs(x= "Industry", y = "Log-odds") + theme(axis.text.x=element_text(angle = 45, hjust = 1))



### log odds, positive value -> positive relationship between feature and target
ggplot(pd3, aes(x = Male, y = yhat, z = Transition_new)) +
  geom_point(col="mediumpurple") +facet_grid(~ Transition_new)+
  labs(x= "Gender", y = "Log-odds")

### log odds, positive value -> positive relationship between feature and target
ggplot(pd4, aes(x = NCHILD, y = yhat, z = Transition_new)) +
  geom_point(col="mediumpurple") +facet_grid(~ Transition_new)+
  labs(x= "Number of Children", y = "Log-odds")

### log odds, positive value -> positive relationship between feature and target
ggplot(pd5, aes(x = Education, y = yhat, z = Transition_new)) +
  geom_point(col="mediumpurple") +facet_grid(~ Transition_new)+
  labs(x= "Education", y = "Log-odds") + theme(axis.text.x=element_text(angle = 45, hjust = 1))


### log odds, positive value -> positive relationship between feature and target
ggplot(pd6, aes(x = Recession, y = yhat, z = Transition_new)) +
  geom_point(col="mediumpurple") +facet_grid(~ Transition_new)+
  labs(x= "Recession", y = "Log-odds") + theme(axis.text.x=element_text(angle = 45, hjust = 1))