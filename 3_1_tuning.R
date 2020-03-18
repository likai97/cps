library(ranger)
library(rsample)

# call all our datasets df in order not to write to much code

#for reproducability
set.seed(420)

#create training (75%) and test (25%) sets
df_split <- initial_split(df,prob=.75)
df_train <- training(df_split)
df_test <- testing(df_split)

# create feature names
y<- "Transition"
x<- setdiff(names(df),y)



# one-hot encode our categorical variables
one_hot <- dummyVars(~ ., df_train, fullRank = FALSE)
df_train_hot <- predict(one_hot, df_train) %>% as.data.frame()

# make ranger compatible names
names(df_train_hot) <- make.names(names(df_train_hot), allow_ = FALSE)


#grid search
hyper_grid<- expand.grid(
  num.trees = c(100,200,300),
  mtry =  c(5,10,15),
  sample_size = c(.632 , .75),
  min.node.size = c(10,40,70),
  OOB_RMSE= 0
)

#total nmber of combinations
nrow(hyper_grid)


# RF model with grid search
for(i in 1:nrow(hyper_grid)){
  #train model
  model <- ranger(
    formula         = Transition ~ us_region + Education + Birth+Hispanic+Asian+Black+Metropolitan+hh_single+
      veteran+Male+Recession+Before_Rec+After_Rec+goverment_employee+self_employed+
      mult_job+industry+hh_income+absent_lw+weeks_unemp+AGE+NCHILD,
    data            = df_train,
    num.trees       = hyper_grid$num.trees[i],
    mtry            = hyper_grid$mtry[i],
    min.node.size   = hyper_grid$node_size[i],
    sample.fraction = hyper_grid$sample_size[i],
    seed            = 420,
    case.weights = HWTINFL
  )
  
  # add OOB error to grid
  hyper_grid$OOB_RMSE[i] <- sqrt(model$prediction.error)
}

hyper_grid %>% arrange(OOB_RMSE) %>% head(10)



# best model found so far is ...
# repeat this model to get better expecation of error rate

OOB_RMSE <- vector(mode = "numeric", length =1)
for(i in seq_along(OOB_RMSE)) {
  optimal_ranger <- ranger(
    formula         = Transition ~ .,
    data            = df_train,
    num.trees       = 200,
    mtry            = 6,
    min.node.size   = 50,
    sample.fraction = .75,
    importance      = 'impurity',
    probability = TRUE
  )
  
  OOB_RMSE[i] <- sqrt(optimal_ranger$prediction.error)
}

hist(OOB_RMSE, breaks = 10)

#variable importance
optimal_ranger$variable.importance %>% 
  tidy() %>%
  arrange(desc(x)) %>%
  #order(x,decreasing = TRUE) %>%
  top_n(10) %>%
  ggplot(aes(reorder(names,x),x)) + 
  geom_col() +
  coord_flip() +
  ggtitle("Top 10 important variables")


#Predicting
pred_ranger <- predict(optimal_ranger, df_test)

# confusion matrix
confusionMatrix(data=pred_ranger$predictions,reference = df_test$Transition, mode = "prec_recall")

#balanced accuracy
