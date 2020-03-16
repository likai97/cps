#library
library(expss)
library(plyr)
library(dplyr)
library(caret)

#increase memory for data handeling
memory.limit(24000)
options(scipen=999)

#load data
load("Data/Data Raw/cps.RData")
# clean data
#test <- cps[cps$HRHHID==110360812356,]

# restrict datasets to observations ranging the age 25-50
cps <- cps[cps$AGE>=25 & cps$AGE <= 55,]


# remove observations with missing data in EMPSTAT as we cant do an analysis on these observations
cps <- cps[cps$EMPSTAT!=0,]
# -> unnecessary move as all these observations get removed by restricting the dataset to observation age 25-55

#exclude the armed forces as they dont appear in unemployment rate by the BLS
# and were not asked all questions -> useful filter for NIU
cps<- cps[cps$POPSTAT==1,]


#3 labourforce states

#flow is for transition t-1 to t and flow1  is for t to t+1
cps <- cps %>% mutate(LMSTATE = case_when(EMPSTAT==1 | EMPSTAT==10 | EMPSTAT==12 ~ 1,
                                          EMPSTAT==20 | EMPSTAT==21 | EMPSTAT==22 ~ 2,
                                          EMPSTAT==31 | EMPSTAT==32 | EMPSTAT==33 | EMPSTAT==34 |EMPSTAT==35 |EMPSTAT==36  ~ 3)) %>% 
              group_by(CPSIDP) %>%mutate(flow = LMSTATE - lag(LMSTATE, n=1, default = first(LMSTATE))) %>%
                                  mutate(flow1 = lead(LMSTATE, n=1, default = last(LMSTATE)) -LMSTATE)
cps <- cps %>% mutate(Transition =
                        case_when(
                          LMSTATE==1 & flow == 0  ~ "E2E",
                          LMSTATE==1 & flow == -1 ~ "U2E",
                          LMSTATE==1 & flow == -2  ~ "O2E",
                          LMSTATE==2 & flow == 1  ~ "E2U",
                          LMSTATE==2 & flow == 0  ~ "U2U",
                          LMSTATE==2 & flow == -1  ~ "O2U",
                          LMSTATE==3 & flow == 2 ~ "E2O",
                          LMSTATE==3 & flow == 1 ~ "U2O",
                          LMSTATE==3 & flow == 0  ~ "O2O"
                        ))
  cps <- cps %>% mutate(Transition_new =
                          case_when(
                            LMSTATE==1 & flow1 == 0  ~ "E2E",
                            LMSTATE==1 & flow1 == 1  ~ "E2U",
                            LMSTATE==1 & flow1 == 2  ~ "E2O",
                            LMSTATE==2 & flow1 == -1 ~ "U2E",
                            LMSTATE==2 & flow1 == 0  ~ "U2U",
                            LMSTATE==2 & flow1 == 1  ~ "U2O",
                            LMSTATE==3 & flow1 == -2 ~ "O2E",
                            LMSTATE==3 & flow1 == -1 ~ "O2U",
                            LMSTATE==3 & flow1 == 0  ~ "O2O"
                          ))


cps <- cps %>% mutate(Employment_status = case_when(EMPSTAT==1 | EMPSTAT==10 | EMPSTAT==12 ~ "Employed",
                                          EMPSTAT==20 | EMPSTAT==21 | EMPSTAT==22 ~ "Unemployed",
                                          EMPSTAT==31 | EMPSTAT==32 | EMPSTAT==33 | EMPSTAT==34 |EMPSTAT==35 |EMPSTAT==36  ~ "Not in LF")) %>%
              mutate(us_region = case_when(REGION == 11| REGION == 12 ~ "Northeast Region",
                                           REGION == 21| REGION == 22 ~ "Midwest Region",
                                           REGION == 31| REGION == 32|REGION == 33 ~"South Region",
                                           REGION == 41| REGION == 42 ~ "West Region",
                                           REGION == 97 ~ "State unknown")) %>%
              mutate(Education = case_when(EDUC >= 2 & EDUC <= 71 ~ "No Diploma",
                                           EDUC >= 73 & EDUC <= 92 ~ "High School Diploma",
                                           EDUC >= 111 ~ "Bachelor's Degree or Higher")) 
cps <-cps %>% mutate(Birth = case_when(CITIZEN>=1 & CITIZEN <=3 ~ "US Citizen",
                                       CITIZEN==4 ~ "Naturalized Citizen",
                                       CITIZEN==5~ "Not US Citizen")) %>%
              mutate(Hispanic = case_when(HISPAN == 0 ~ 0,
                                          HISPAN>0 ~ 1)) %>%
              mutate(Asian = case_when(RACE==651~1,
                                       RACE!=651~0))%>%
              mutate(Black = case_when(RACE==200~1,
                                       RACE!=200~0))%>%
              mutate(Metropolitan = case_when(METRO==0 | METRO==1 ~ "Not in metro area",
                                              METRO>=2 ~ "In Metro area"))

cps <- cps %>% mutate(hh_single = case_when(MARST<=2 ~ 0,
                                            MARST>=3 & MARST <= 7 ~ 1)) %>%
  mutate(veteran = case_when(VETSTAT==2~1,
                             VETSTAT!=2~0))

# Stand 04022020
cps <- cps %>% mutate(Male = case_when(SEX==1~1,
                                       SEX==2~0)) %>% 
               mutate(Recession = if_else((YEAR==2007 & MONTH >= 10)|YEAR == 2008 | (YEAR==2009 & MONTH <=6),1,0)) %>%
              # indicators for one year before and after recession
               mutate(Before_Rec = if_else((YEAR==2007 & MONTH < 10)|(YEAR==2006 & MONTH >= 10),1,0)) %>%
               mutate(After_Rec  = if_else((YEAR==2009 & MONTH > 7) |(YEAR==2010 & MONTH <= 6),1,0)) %>%
              mutate(goverment_employee= ifelse(CLASSWKR>=24 & CLASSWKR <=28,1,0)) %>%
              mutate(self_employed = ifelse(CLASSWKR== 13 | CLASSWKR == 14,1,0)) %>%
              mutate(own_house = ifelse(HHTENURE == 1,1,0)) %>%
              mutate(rent_house = ifelse(HHTENURE == 2,1,0)) %>%
              mutate(disability = ifelse(DIFFANY==2,1,0))%>%
              mutate(mult_job = ifelse(MULTJOB==2,1,0))

cps <- cps %>% mutate(industry = case_when(IND>=170 & IND <=290 ~ "Agriculture",
                                           IND>=370 & IND <=490 ~ "Mining",
                                           IND==770 ~ "Construction",
                                           IND>=1070 & IND <=3990 ~ "Manufacturing",
                                           (IND>=6070 & IND<=6390)|(IND>=570 & IND<=690) ~ "Transportation",
                                           IND>=4070 & IND <=5790 ~ "Wholesale and Retail",
                                           IND>=6870 & IND <=7190 ~ "Finance",
                                           IND>=7270 & IND <=7790 ~ "Busines and Adminstrative Services",
                                           IND>=6470& IND <=6780 ~ "Information",
                                           IND>=7860 & IND <=8470 ~ "Education and Health Services",
                                           IND>=8560 & IND <=8690 ~ "Leisure and Hospitality Services",
                                           IND>=8770 & IND <=9290 ~ "Other Services",
                                           IND>=9370 & IND <=9590 ~ "Public Services",
                                           IND>=9670 & IND <=9890 ~ "Military",
                                           IND==0 ~ "Not Applicable"))
              
#farmrel -> spouse? spouse got work last period?

######################################################################################################### 
################################# FEATURES WITH MISSING INFO ############################################
######################################################################################################### 

#median household income 
med_inc <- c(59712,60178,60985,58811,58400,56873,56006,55900,57856,56969,59901,61779,62626,63179)
mean(med_inc)
#divide households into brackets
cps <- cps %>% mutate(hh_income = case_when(FAMINC<=730~"Lower class",
                                            FAMINC>=740 & FAMINC <=842~"Middle class",
                                            FAMINC==843~"Upper class",
                                            FAMINC>=996~"Missing or refused to answer")) %>%
               mutate(absent_lw = ifelse(EMPSTAT==12,1,0)) %>%
               mutate(weeks_unemp= case_when(DURUNEM2==0 ~ "0 Weeks",
                                             DURUNEM2>=1 & DURUNEM2 <=8 ~"1-14 Weeks",
                                             DURUNEM2>=9 & DURUNEM2 <=15 ~"15-52 Weeks",
                                             DURUNEM2 == 16 ~ "More than 52 Weeks")) %>%
              mutate(spouse_emp = ifelse(EMPSTAT_SP==10 | EMPSTAT_SP==12,1,0))
  
save(cps,file = "Data/Data Clean/cps.RData")

features<- c("Employment_status","us_region","Education","Birth","Hispanic","Asian",
            "Black","Metropolitan","hh_single","veteran","Male","Recession","Before_Rec","After_Rec",
            "goverment_employee","self_employed","own_house","rent_house","disability","mult_job",
            "industry","hh_income","absent_lw","weeks_unemp","spouse_emp")


cps[features] <- lapply(cps[features],factor)


# drop unecessary variables
# ASECFLAG,
cps <- cps[,-which(names(cps) %in% c("REGION","FAMINC","RACE","flow","EDUC","CITIZEN","METRO","HISPAN","MARST",
                                     "DURUNEM2","EMPSTAT_SP","SEX","flow1","CLASSWKR","SERIAL",
                                     "CPSID","ASECFLAG","GQTYPE","CPSIDP","POPSTAT","VETSTAT","FTYPE","FAMREL",
                                     "NATIVITY","UHRSWORKT","AHRSWORKT","MULTJOB","AHRSWORK1","AHRSWORK2","ABSENT",
                                     "EMPSAME","NUMJOB","NILFACT","EDCYC","EDDIPGED","SCHLCOLL","HOURWAGE","PAIDHOUR",
                                     "EARNWEEK","OTPAY"))]
  
  #turn variables to categorical
  #cps <- cps %>% mutate_if(is.character, as.factor)


# divide datasets into datasets from employment, from unemployment, and from OLF
from_e <- cps[cps$Employment_status=="Employed",]
from_u <- cps[cps$Employment_status=="Unemployed",]
from_o <- cps[cps$Employment_status=="Not in LF",]

# save clean datasets
save(from_e, file = "Data/Data Train/from_e.RData")
save(from_u, file = "Data/Data Train/from_u.RData")
save(from_o, file = "Data/Data Train/from_o.RData")


# single households
single <- cps[cps$hh_single==1,]
# divide datasets into datasets from employment, from unemployment, and from OLF
from_e_single <- single[single$Employment_status=="Employed",]
from_u_single <- single[single$Employment_status=="Unemployed",]
from_o_single <- single[single$Employment_status=="Not in LF",]
save(from_e_single, file = "Data/Data Train/from_e_single.RData")
save(from_u_single, file = "Data/Data Train/from_u_single.RData")
save(from_o_single, file = "Data/Data Train/from_o_single.RData")
couple <- cps[cps$hh_single!=1,]
# divide datasets into datasets from employment, from unemployment, and from OLF
from_e_cp <- couple[couple$Employment_status=="Employed",]
from_u_cp <- couple[couple$Employment_status=="Unemployed",]
from_o_cp <- couple[couple$Employment_status=="Not in LF",]
save(from_e_cp, file = "Data/Data Train/from_e_cp.RData")
save(from_u_cp, file = "Data/Data Train/from_u_cp.RData")
save(from_o_cp, file = "Data/Data Train/from_o_cp.RData")

             
             
             
