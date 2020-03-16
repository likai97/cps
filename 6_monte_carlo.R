#library
library(stats)
library(dplyr)
library(caret)
library(ranger)
#set seed for reproducability
set.seed(69)
#Definition: Define state = 1 as Employment, state = 2 as Unemployment and state = 3 as Out of Labour Force

# random transition matrix
transition_matrix = function(Nstates,min_rate=0,max_rate=1){
  # create matrix
  trans <- matrix(0,Nstates,Nstates)
  cumulative <- matrix(0,Nstates,Nstates)
  for(i in 1:Nstates){
    # create vector of probabilities from 0 to 1
    a<- runif(runif(Nstates,min_rate,max_rate))
    # normalize the vector
    trans[i,]<- a/sum(a)
    cumulative[i,] <- cumsum(a/sum(a))
  }
  #return matrix
  #return(trans)
  return(cumulative)
}

# monte carlo simulation, transition matrix P
mc.sim <- function(P, num.iters= 8){
  # number of possible states
  num.states <- nrow(P)
  # stores the states X_t through time
  states <- numeric(num.iters)
  # initialize variable for first state 
  states[1] <- 1
  for(t in 2:num.iters) {
    # probability vector to simulate next state X_{t+1}
    p  <- P[states[t-1], ]
    #generate random number
    x <- runif(1)
    states[t]<-min(which(x<=p))
    ## draw from multinomial and determine state
    #states[t] <- which(rmultinom(1, 1, p) == 1)
  }
  return(states)
}

### generate transition between states
observations <- 10000
num.iters = 8
# matrix of the size (observations*num.iters)x2
market.states <- matrix(0,ncol = 2, nrow = num.iters*observations)
# generat
for(i in seq_len(observations)){
  # create random 3x3 transition matrix
  P <- transition_matrix(3)
  # simulate transition
  market.states[(1+(i-1)*8):(8+(i-1)*8),1] <- i
  market.states[(1+(i-1)*8):(8+(i-1)*8),2] <- mc.sim(P)
}


# convert matrix to dataframe
df <- as.data.frame(market.states)
names(df) = c("ID","LMSTATE")


# calculate flows
df <- df %>% group_by(ID) %>% mutate(flow = lead(LMSTATE, n=1, default = first(LMSTATE)) -LMSTATE)

#create Transition variable
df<- df %>% mutate(Transition =
                        case_when(
                          LMSTATE==1 & flow == 0  ~ "E2E",
                          LMSTATE==1 & flow == 1  ~ "E2U",
                          LMSTATE==1 & flow == 2  ~ "E2O",
                          LMSTATE==2 & flow == -1 ~ "U2E",
                          LMSTATE==2 & flow == 0  ~ "U2U",
                          LMSTATE==2 & flow == 1  ~ "U2O",
                          LMSTATE==3 & flow == -2 ~ "O2E",
                          LMSTATE==3 & flow == -1 ~ "O2U",
                          LMSTATE==3 & flow == 0  ~ "O2O"
                        ))

#create random covariates
for(i in 1:10){
  df[paste("V",i,sep="")] <- sample(0:1,size = nrow(df), replace = TRUE)
}
from_e <- df[df$LMSTATE==1,]
from_u <- df[df$LMSTATE==2,]
from_o <- df[df$LMSTATE==3,]

# drop unecessary variables
from_e <- from_e[,-which(names(from_e) %in% c("ID","LMSTATE","flow"))]
from_u <- from_u[,-which(names(from_u) %in% c("ID","LMSTATE","flow"))]
from_o <- from_o[,-which(names(from_o) %in% c("ID","LMSTATE","flow"))]


  #turn variables into factor
from_e<-  from_e %>% mutate_if(is.character, as.factor)
from_e<-  from_e %>% mutate_if(is.numeric, as.factor)
save(from_e,file="Data/MC/from_e.RData")

from_u<-  from_u %>% mutate_if(is.character, as.factor)
from_u<-  from_u %>% mutate_if(is.numeric, as.factor)
save(from_u,file="Data/MC/from_u.RData")

from_o<-  from_o %>% mutate_if(is.character, as.factor)
from_o<-  from_o %>% mutate_if(is.numeric, as.factor)
save(from_o,file="Data/MC/from_o.RData")


#ranger from_e
df<-from_e
#Create Test and Training Set
#Get row numbers for the training data
trainRowNumbers <- createDataPartition(df$Transition, p=0.8, list=FALSE)
# Step 2: Create the training dataset
df_train<- df[trainRowNumbers,]
df_test <- df[-trainRowNumbers,]
#rf
model <- ranger(
  formula         = Transition ~ .,
  data            = df_train
  #case.weights    = weight,
)

pred_ranger <- predict(model, df_test)
# confusion matrix
confusionMatrix(data=pred_ranger$predictions,reference = df_test$Transition, mode = "prec_recall")














# all individuals same transition matrix


### generate transition between states
observations <- 10000
num.iters = 8
# matrix of the size (observations*num.iters)x2
case_2 <- matrix(0,ncol = 1, nrow = num.iters*observations)

df<-as.data.frame(case_2)
for(i in 1:10){
  df[paste("V",i,sep="")] <- sample(0:1,size = nrow(df), replace = TRUE)
}

# create random 10x1 vector
c <- runif(10,0,1)
#transform df to matrix
df_matrix <- as.matrix(df)
# create states
# p = exp(df_matrix %*% c) / (1+exp(df_matrix %*% c) )
# logit = log(p/(1-p))
transition <- df_matrix %*% c
normalized = (transition-min(transition))/(max(transition)-min(transition))
normalized <- as.data.frame(normalized)
names(normalized) = c("prob")
df <- cbind(normalized,df)
df <- df %>% mutate(LMSTATE = case_when(prob<=1/3~1,prob<=2/3~2, prob<=3/3 ~3))

df<- df %>% mutate(Transition = case_when( LMSTATE==1 ~ "E2E",
                                           LMSTATE==2 ~ "E2U",
                                           LMSTATE==3 ~ "E2O"))
df <-df[,-which(names(df) %in% c("prob","LMSTATE"))]
df<-  df %>% mutate_if(is.numeric, as.factor)
df<-  df %>% mutate_if(is.character, as.factor)

trainRowNumbers <- createDataPartition(df$Transition, p=0.8, list=FALSE)
# Step 2: Create the training dataset
df_train<- df[trainRowNumbers,]
df_test <- df[-trainRowNumbers,]
#rf
model <- ranger(
  formula         = Transition ~ .,
  data            = df_train
  # sample.fraction = .7,
  #case.weights    = weight,
)




pred_ranger <- predict(model, df_test)
# confusion matrix
confusionMatrix(data=pred_ranger$predictions,reference = df_test$Transition, mode = "prec_recall")

