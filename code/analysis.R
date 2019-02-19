# - - - - - - - - - - - - - - - - - - - - #
# File:             analysis.R
# Objective:        Obtain 'patients-like-me' prediction for Timed Up and Go and WOMAC scores
# Date created:     5/17/2018
# Modified by:      C.K.
# Date modified:    11/14/2018
# - - - - - - - - - - - - - - - - - - - - #



# - - - - - - - - - - - - - - - - - - - - #
# Environment configuration
# - - - - - - - - - - - - - - - - - - - - #

## Remove objects in workspace
rm(list=ls())
## Set working directory
getwd() 
# if you are in the code folder get out!
setwd('../')
source("./code/functions.R")

# - - - - - - - - - - - - - - - - - - - - #
# Data Import and testing
# - - - - - - - - - - - - - - - - - - - - #

library(readxl) # read original data
library(dplyr) # ez data manipulation
library("rio") # ez data manipulation
# load all outcome data
alldf <- import("./data/all_tka.csv")
amp  <- import("./data/amp_data_test.csv")
# load only the TUG dataset
full  <- readxl::read_xlsx("./data/TUG_070118.xlsx")

# need to exclude the above patients
# exclude also time > 200
full <- full %>%
    #filter(!patient_id %in% exclude$patient_id & time < 200)
    filter(time < 200) %>%
    mutate(gender = as.factor(gender))

# Select patient id's that have TUG < 2 or > 70 after time > 3 
exclude <- full %>% filter(tug < 2 | (tug > 70 & time > 3)) %>% dplyr::select(patient_id) %>%
    bind_rows(
              # need to exclude patients that have no post operative time beyond 2 from the train pre and possibly test pre because if people don't have post operative time in test it doesn't make sense
              full %>%
                  group_by(patient_id) %>%
                  filter(max(time) < 3) %>%
                  distinct(patient_id)
              )


full <- full %>%
    filter(!patient_id %in% exclude$patient_id & time < 200)

# Make clean data for alldf (This needs to be done to run other functions)
alldf <- alldf %>%
    filter(!is.na(time) & !is.na(age) & !is.na(gender) & !is.na(bmi) & !is.na(knee_flex)) %>%
    dplyr::select(time, age, gender, bmi, knee_flex, source, patient_id )


# ONLY TUG outcomes
#train <- full %>% filter(train_test == 1)
#test <- full %>% filter(train_test == 2)

# Train and Test split for all TKA outcomes: create 
set.seed(1234)
alldf <- alldf %>%
    left_join(
              alldf %>%
                  distinct(patient_id) %>%
                  mutate(train_test = sample(c(1,2), nrow(.), replace=TRUE)) 
    )

full <- baselinemk(full, "patient_id", "time")
alldf <- baselinemk(alldf, "patient_id", "time")
amp <- baselinemk(amp, "patient_id", "time")



#train <- amp %>% filter(train_test == 1)
#test <- amp %>% filter(train_test == 2)
train <- alldf %>% filter(train_test == 1)
test <- alldf %>% filter(train_test == 2)
# data description: this data contains id, age, sex, age at surg, knee_arom_flexion, time elapsed

# quick data check
# make sure there aren't any missing data and etc
summary(full) ;  sapply(full, function(x) {
                          table(is.na(x))})

summary(alldf) ;  sapply(alldf, function(x) {
                          table(is.na(x))})

summary(amp) ;  sapply(amp, function(x) {
                          table(is.na(x))})

# check if there are TUG < 2

# train pre data
train_pre <- train %>%
    filter(time < 0)
# train post data select only days beyond 3 bc the 2nd day estimates are all ove rthe place.
train_post  <- train %>%
    filter(time > 3)

test_post  <- test %>%
    filter(time > 3)

# Spaghetti plot of original data
par(mfrow=c(1,2))
library(ggplot2)
ggplot(full, aes(x=time, y=tug)) +
    geom_line() + guides(colour=FALSE) + xlab("Time") +
    ylab("TUG") + aes(colour = factor(patient_id)) + ggtitle("Full data Plot") +
    geom_smooth(se=FALSE, colour="black", size=2)

ggplot(alldf, aes(x=time, y=knee_flex)) +
    geom_line() + guides(colour=FALSE) + xlab("Time") +
    ylab("Knee Flexion") + aes(colour = factor(patient_id)) + ggtitle("Full data Plot") +
    geom_smooth(se=FALSE, colour="black", size=2)

ggplot(train_post, aes(x=time, y=tug)) +
    geom_line() + guides(colour=FALSE) + xlab("Time") +
    ylab("TUG") + aes(colour = factor(patient_id)) + ggtitle("Training Post") +
    geom_smooth(se=FALSE, colour="black", size=2)

# - - - - - - - - - - - - - - - - - - - - #
# Table 1 .RDS
# - - - - - - - - - - - - - - - - - - - - #

tab1 <- full %>%
    filter(time < 1 & patient_id %in% c(test_proc$train_o$id, test_proc$test_o$id)) %>%
    dplyr::select(train_test, bmi, gender, age, b_tug) %>%
    data.frame

saveRDS(tab1, "./data/tab1.RDS")
tab1 <- readRDS("./data/tab1.RDS")

library(tableone)
library(labelled)
library(kableExtra)
testtotcount <- paste0("N = ",nrow(test_proc$test_o) ,", # of Obs = ",nrow(test_post))
traintotcount <- paste0("N = ",nrow(test_proc$train_o), ", # of Obs = ", nrow(train_post))
#tab1_out <- tab1 %>%
tab1_o <- tab1 %>%
    mutate(gender = ifelse(gender == 2, "Female","Male"),
           train_test = ifelse(train_test == 1, "Train", "Test")) %>%
    set_variable_labels(gender = "Gender", age = "Age (years)", 
                        bmi = "BMI (kg/m^2)", b_tug = "Baseline TUG (sec)") %>% 
    CreateTableOne(vars = c("age","gender","bmi","b_tug"), factorVars = c("gender"), strata = c("train_test"), data=.) %>%
    print(., quote = FALSE, noSpace = TRUE, printToggle = FALSE, varLabels = TRUE) %>%
    .[, 1:3] %>%
    kable(., caption = "Baseline Characteristics of Training and Testing Set", booktabs=TRUE, format="latex" ) %>%
    kable_styling(latex_options = c("scale_down")) %>%
    gsub(pattern="n \\& (\\d){3} \\& (\\d){3} \\& (\\\\)+","", x=.) %>%
    gsub(pattern=" \\& Test \\& Train \\& p\\\\",
         ## replacement
         paste0(" \\& Test (",testtotcount,") \\& Train (",traintotcount,") \\& p\\\\"), x=.) 

    #add_header_above(c(" ", 
                       #"# TUG Obs = 604"= 1, 
                       #"# TUG Obs = 1339" = 1,
                       #" "
                       #))

# Add # of TUG observations in train and test 

train_post %>%
    summarise(train = n()) %>%
    cbind(., test_post %>%
          summarise(test = n())) %>% dplyr::select(test) %>%
    paste0(.)




#- - - - - - - - - - - - - - - - - - - - - - - - - - #
## Run the Long LOOCV Training Set Matchem Up!
#- - - - - - - - - - - - - - - - - - - - - - - - - - #

##extract fitted values for the linear model (PREOP only here)
#-- preprocess train and test data: involves
#1. Taking training post data and running broken stick to get y90 or yNU where NU is value chosen
#2. Processes test data set so that yNU is matched with training data
#3. The matched test and train data according to yNU is used to later match the personalized predicted gamlss

source("./code/functions.R")

# Process the full data to get
# 1. Post operative test data 
# 2. Fitted 90-Day TUG for test patients
# 3. Post operative train data 
# 4. Fitted 90-Day TUG for train patients
runpreproc <- function(outc = "tug"){
    if(outc == "tug" ){
        temp <- preproc(
                     dff=full,  # specify full dataset name
                    split_var = 'train_test', # train test split variable
                    trainval = 1, # training set value
                    testval = 2, # test set value
                     knots_exp = c(0, 14, 50, 90), # Specify broken stick knots
                     out_time = 90,  # specify which timepoint to use 
                     outcome = "tug", # specify outcome variable name
                     time_var = "time", # specify time variable name
                     pat_id = "patient_id", # specify patient id variable name
                     varlist = c("age","gender","bmi"), # specify list of covariates for pmm
                     filter_exp = NULL               
        )
    } else if(outc == "knee"){
        temp <- preproc(
                     dff=alldf, 
                    split_var = 'train_test', # train test split variable
                    #split_var = 'source', # for all_tka dataset
                    trainval = 1, # training set value
                    testval = 2, # test set value
                     knots_exp = c(0, 14, 50, 90), # Specify broken stick knots
                     out_time = 90,  # specify which timepoint to use 
                     #outcome = "tug", # specify outcome variable name
                     outcome = "knee_flex",
                     time_var = "time", # specify time variable name
                     pat_id = "patient_id", # specify patient id variable name
                     varlist = c("age","gender","bmi"), # specify list of covariates for pmm
                     filter_exp = NULL               
                     )
    } else if(outc == "plusm"){
        temp <- preproc(
                     dff=amp, 
                    split_var = 'train_test', # train test split variable
                    #split_var = 'source', # for all_tka dataset
                    trainval = 1, # training set value
                    testval = 2, # test set value
                     knots_exp = c(0, 50, 181), # Specify broken stick knots
                     out_time = 181,  # specify which timepoint to use 
                     #outcome = "tug", # specify outcome variable name
                     outcome = "plusm",
                     time_var = "time", # specify time variable name
                     pat_id = "patient_id", # specify patient id variable name
                     varlist = c("age","gender","bmi"), # specify list of covariates for pmm
                     filter_exp = NULL               
                     )
    }
    return(temp)
}

test_proc <- runpreproc("plusm")
test_proc <- runpreproc("knee")
test_proc <- runpreproc("tug")

# - - - - - - - - - - - - - - -#
# TRAINING DATA: LOOCV Result
# - - - - - - - - - - - - - - -#


# Specify Distribution
dists <- c(BCCGo)
#dists <- c(BCCGo)
#dists <- c(NO,GA, BCT, BCPE, BCCGo)
#0 = NO, 1 = GA, 2 = BCT, 3 = BCPE, 4 = BCCG

# Run for loop
cnt = 0
for(d in dists) {
    print(cnt)
    fin <- loocv_function(
                          
                          # specify number or vector of numbers from {1,...,total number of patients in training data} 
                          nearest_n = c(seq(10,nrow(test_proc$train_o),10),max(nrow(test_proc$train_o))-1),
                          # enter training and testing post operative and fitted y90 dataset
                          train_post = test_proc$train_post,
                          ord_data = test_proc$train_o,
                          test_post = test_proc$test_post,
                          test_o = test_proc$test_o,

                          # Specify outcome variable and time variable name
                          #outcome = "tug",
                          outcome = "tug",
                          #outcome = "knee_flex",
                          time_elapsed = "time",

                          # Specify use of cubic spline or not
                          cs=TRUE,

                          # specify degrees of freedom use or not
                          #dfspec=NULL,
                          dfspec=TRUE,

                          # specify degree of freedom for location, scale and shape (d_f_* where * = {m, s} for location and scale default for shape is 1.
                          # specify power transformation of location (ptr_m)
                          d_f_m=3, ptr_m=0.5,
                          #d_f_m=3, ptr_m=1,
                          d_f_s=1,

                          # Specify distribution for location, scale and shape 
                          dist_fam = d)

    #saveRDS(fin,paste0("./data/fin_BCCGo_plusm_10",cnt,".RDS"))# save the output
    saveRDS(fin,paste0("./data/fin_NO_tug_35",cnt,".RDS"))# save the output
    #saveRDS(fin,paste0("./data/fin_NO",cnt,".RDS"))# save the output
    rm(fin) # remove the object (can be quite large)
    #cnt = cnt + 1 # increment by 1 and do another distribution
    #cnt = cnt + 4
}


# Non for loop running

#-- Run BCCGo from 25-55 by 1's
fin <- loocv_function(nearest_n = seq(25,55,1),
                      train_post = test_proc$train_post,
                      ord_data = test_proc$train_o,
                      outcome = "tug",time_elapsed = "time",
                      cs=TRUE,
                      dfspec=TRUE,
                      d_f_m=3, ptr_m=0.5,
                      d_f_s=1,
                      dist_fam = BCCGo
                      )
fin <- loocv_function(nearest_n = seq(150,150,1),
#fin <- loocv_function(nearest_n = seq(20,20,1),
                      train_post = test_proc$train_post,
                      ord_data = test_proc$train_o,
                      test_post = test_proc$test_post,
                      test_o = test_proc$test_o,
                      outcome = "plusm",time_elapsed = "time",
                      #outcome = "tug",time_elapsed = "time",
                      cs=TRUE,
                      dfspec=TRUE,
                      matchprobweight=FALSE,
                      d_f_m=3, ptr_m=0.5,
                      d_f_s=1,
                      dist_fam = BCCGo
                      #dist_fam = NO
                      )
saveRDS(fin,paste0("./data/fin_NO_3536_cs_dfspec_plusm.RDS"))# saved as the Gamma distribution
rm(fin)
length(seq(34,34, 1))
# Running LOOCV on Knee Flexion as Outcome
debug(loocv_function)
undebug(loocv_function)
test_proc <- runpreproc("plusm")
test_proc <- runpreproc("knee")
#test_proc <- runpreproc("tug")
source("./code/functions.R")
fin <- loocv_function(nearest_n = seq(34,35, 1),
                      train_post = test_proc$train_post,
                      ord_data = test_proc$train_o,
                      test_post = test_proc$test_post,
                      test_o = test_proc$test_o,
                      outcome = "plusm",time_elapsed = "time",
                      #outcome = "tug",time_elapsed = "time",
                      #outcome = "knee_flex",time_elapsed = "time",
                      cs=TRUE,
                      dfspec=TRUE,
                      d_f_m=3, ptr_m=0.5,
                      d_f_s=1,
                      dist_fam = NO
                      )



#-- load all data
myfiles <- lapply(
                  #list.files(path="./data/", pattern="f.+([0]).RDS$"), function(x){
                  #list.files(path="./data/", pattern="f.+n(_35_.*|_4).RDS$"), function(x){
                  #list.files(path="./data/", pattern="f.+(n_|n_pre.+_)(\\d).RDS$"), function(x){
                  #list.files(path="./data/", pattern="fin_(\\d).RDS$"), function(x){
                  #list.files(path="./data/", pattern="f.+(_ns_)+(\\d).RDS$"), function(x){
                  #list.files(path="./data/", pattern="fin(_ns_\\d|_\\d)+.RDS$"), function(x){
                  list.files(path="./data/", pattern="fin_4.RDS$"), function(x){
                  #list.files(path="./data/", pattern="fin_kf_.*.RDS$"), function(x){
                      readRDS(paste0("./data/",x))
                  }
                  )
#names(myfiles) <- c("BCCGo_pow1"+"BCCGop_ns","BCCGop_fs")
#names(myfiles) <- c("NO","GA","BCTo","BCPEo","BCCGo")
#names(myfiles) <- c("NO","GA","BCTo","BCPEo","BCCGo","NOns","GAns","BCTons","BCPEons","BCCGons")
#names(myfiles) <- c("NO")
names(myfiles) <- c("BCCGo")
#names(myfiles) <- c("NO","BCCGo")
#lapply(myfiles, function(x) { x$nearest_n })


# OBJECT CHECK: Check bias, zscore, coverage, and etc
do.call(cbind, lapply(myfiles, function(x) { 
           sapply(x$loocv_res, function(y) {
                      y$dropped_cases
                      #mean(y$bias, na.rm=TRUE)
                      #mean(y$iqrcoverage, na.rm=TRUE)
                      #mean(y$coveragevec95, na.rm=TRUE)
                      #mean(y$rmse, na.rm=TRUE)
                      #mean(y$zscore, na.rm=TRUE)
                      #mean(y$precisionvec, na.rm=TRUE)
                })
          })
)





# - - - - - - - - - - - - - - - - - - - - #
# Plots: Calibration, Bias/RMSE and Coverage
# - - - - - - - - - - - - - - - - - - - - #

#-- Bias, Cov, Pred
png("./report/figure/LOOCV_BCCGo_tug_resamp.png")
plot_cal(
         # Specify plotobj which is the object saved from the loocv_func()
         # if multiple distributes were used then specify the distribution (e.g. myfiles$BCCGo) otherwise specify just the object (e.g. fin)
         #plotobj=myfiles$BCCGo,
         plotobj=fin,
         # specify the processed file to match test data and train data for calibration
          test_proc=test_proc,
         # specify calibration plot x 
          pred_sum='mean',
          # specify observed distribution to use (default = "median")
          obs_dist="median"
          )
dev.off()

#-- Calibration
dev.new()
#plot_cal(fin,
#plot_cal(plotobj=myfiles$NO,
#plot_cal(plotobj=myfiles$BCCGo,
test_proc <- runpreproc("tug")
fin  <- readRDS("./data/fin_NO_35_cs_dfspec_plusm.RDS")
test_proc <- runpreproc("plusm")
source("./code/functions.R")
png("./report/figure/CAL_BCCGo_tug_resamp.png")
#fin  <- readRDS("./data/fin_NO_35_cs_dfspec_tug.RDS")
plot_cal(
         #plotobj=myfiles$BCCGo,
         plotobj=fin,
          test_proc=test_proc,
          outcome = 'plusm',
          #outcome = 'tug',
          #outcome = 'knee_flex',
          pred_sum='mean',
          #obs_dist="gaussian",
          #obs_dist="poisson",
          obs_dist="median",
          # specify loocv=FALSE to check calibration
          loocv = FALSE
          )
dev.off()


#-- save plots
#lapply(c("NO","GA","BCTo","BCPEo","BCCGo","BCCGons"), function(x) {
#lapply(c("NO","GA","BCTo","BCPEo","BCCGo","BCCGop"), function(x) {
#lapply(c("BCCGo_pow1","BCCGop_ns","BCCGop_fs"), function(x) {
lapply(c("BCCGo"), function(x) {
#lapply(c("NO","GA","BCTo","BCPEo","BCCGo","NOns","BCCGons"), function(x) {
#lapply(c("NO","GA","BCTo","BCPEo","BCCGo","NOns","GAns","BCTons","BCPEons","BCCGons"), function(x) {
           ggsave(
                  filename=paste0("./report/figure/fig_",x,"_loocv.png"),
                  width=23, height=18, units="cm", 
                  plot= plot_cal(plotobj=myfiles[[x]],
                                 test_proc=test_proc,
                                 pred_sum='mean',
                                 obs_dist="median"
                                 #obs_dist='poisson'
                                 )
                  )
           ggsave(
                  filename=paste0("./report/figure/fig_",x,"_cal.png"),
                  width=38, height=28, units="cm", 
                  plot= plot_cal(plotobj=myfiles[[x]],
                                 test_proc=test_proc,
                                 pred_sum='mean',
                                 obs_dist="median",
                                 loocv = FALSE
                                 #obs_dist='poisson'
                                 )
                  )
          })

# - - - - - - - - - - - - - - - - - - - - #
# Internal Validation Performance Measures
# - - - - - - - - - - - - - - - - - - - - #

fin  <- readRDS("./data/fin_4.RDS")
fin  <- readRDS("./data/fin_0.RDS")
fin  <- readRDS("./data/fin_NO_35_cs_dfspec.RDS")
fin  <- readRDS("./data/fin_NO_35_cs_dfspec_plusm.RDS")
test_proc <- runpreproc("plusm")
loocvperf(fin$loocv_res)
intmeas <- loocvperf(myfiles$BCCGo$loocv_res)
intmeas[which(intmeas$nearest_n == myfiles$BCCGo$nearest_n),]

# - - - - - - - - - - - - - - - - - - - - #
# External Validation Performance Measures
# - - - - - - - - - - - - - - - - - - - - #

debug(extvalid)
extvalid(fin, test_proc)$zscore
extvalid(fin, test_proc)$coverage
extvalid(fin, test_proc)$precision
extvalid(myfiles$BCCGo, test_proc)$zscore
extmeas <- extvalid(myfiles$BCCGo, test_proc)


# - - - - - - - - - - - - - - - - - - - - #
# Debugging Section
# - - - - - - - - - - - - - - - - - - - - #

tempfit <- full %>%
    group_by(train_test) %>%
    do(fitmed = MedianCI(.[,"tug"][[1]], conf.level= 0.95,
                         method = "exact", R = 10000))

tempfitcoef <- tidy(tempfit, fitmed)


# - - - - - - - - - - - - - - - - - - - - #
# Report Generation Code From Here
# - - - - - - - - - - - - - - - - - - - - #

# Nearest Neighbors Selection and Model Tuning

# Factors and matching
temp <- data.frame(summary(test_proc$reg_obj)$coefficients)
## beta, and p-value for age
formatC(temp[match("age", rownames(temp)),1], format="e", digits = 2)
formatC(temp[match("age", rownames(temp)),4], format="e", digits = 2)
## baseline TUG
formatC(temp[match("get(outcome)", rownames(temp)),1], format="e", digits = 2)
formatC(temp[match("get(outcome)", rownames(temp)),4], format="e", digits = 2)


# standardized beta coefficients
library("QuantPsyc")
lm.beta(test_proc$reg_obj)
names(lm.beta(test_proc$reg_obj)[1]) # first is the largest influence
lm.beta(test_proc$reg_obj)[1] # first is the largest influence
lm.beta(test_proc$reg_obj)[[length(lm.beta(test_proc$reg_obj))]] # 4th is last
length(lm.beta(test_proc$reg_obj))
stdbeta <- lm.beta(test_proc$reg_obj)
names(stdbeta) <- c("baseline TUG", "age","gender","BMI")
lm.beta(test_proc$reg_obj)[1] / lm.beta(test_proc$reg_obj)[4]


myfiles <- lapply(
                  list.files(path="./data/", pattern="fin_4.RDS$"), function(x){
                      readRDS(paste0("./data/",x))
                  }
                  )
names(myfiles) <- c("BCCGo")
myfiles$BCCGo$nearest_n


# selection of bias coverage and precision score for reporting 
myfiles$BCCGo$loocv_score %>%
    filter(nearest_n == myfiles$BCCGo$nearest_n) %>%
    dplyr::select(zscore) %>% unlist(.) %>% as.vector %>% formatC(., 1,format="e", digits=2)
    #dplyr::select(precision) %>% unlist(.) %>% as.vector
    #dplyr::select(coverage) %>% unlist(.) %>% as.vector


# Rversion
substr(R.version.string,1,15)


# External and Internal Validation in Result
The average bias, coverage, and precision for all TUG predictions in the training data was \Sexpr{}, \Sexpr{}, and \Sexpr{}, respectively (Figure ~\ref{fig:fig_BCCGo_loocv}). Model calibration was good, with close agreement between predicted and observed values of post-operative TUG times (Figure ~\ref{fig:fig_BCCGop_cal}). 

The model showed similar prediction performance on the testing data based on the average bias, coverage, and precision for all TUG predictions (\Sexpr{}, \Sexpr{}, and \Sexpr{}, respectively). Model calibration was good, with slight underestimation of predicted TUG times with regards to observed values in the testing data (Figure ~\ref{fig:fig_BCCGop_cal}). 











# appendix table
library(kableExtra)
library(tableone)
library(labelled)
library(dplyr)
myfiles$BCCGo$loocv_score  %>%
    set_variable_labels(zscore = "Z-score", nearest_n = "# of Nearest Matches", 
                        coverage = "Coverage", precision = "Precision" ,
                        dcase = "Dropped Cases", totscore = "Weighted Total Score") %>% 
    print(., quote = FALSE, noSpace = TRUE, printToggle = FALSE, varLabels = TRUE) %>%
    kable(., caption = "\\label{tab:atab1}Appendix Table 1. Weighted total score of bias, coverage, and precision for choosing optimal # of nearest matches", booktabs=TRUE, format="latex" , align=c("c","r","r","r","r","r"), digits = 4) %>%
    kable_styling(latex_options = c("scale_down"), font_size = 7) %>%
    add_footnote(c("Continuous variables tested with one-way analysis of variance; Categorical variables tested with chisq test")) 



# - - - - - - - - - - - - - - - - - - - - #
# Presentation Code Example: reference chart
# - - - - - - - - - - - - - - - - - - - - #

library(gamlss)
fit<-gamlss(eval(as.formula(tug ~ cs(time^0.5, df=3))),
                    sigma.formula = as.formula(~cs(time^0.5, df=1)),
                    nu.formula = ~1,
                    tau.formula = ~1,
                    data=train_post, family=BCTo,
                    )

png("./report/figure/fig_refchart.png")
centiles.fan(fit, xvar=train_post$time, cent=c(3,10,25,50,75,90,97),
             colors="terrain",ylab="TUG", xlab="Time Post Surgery (days)", main="",
             ylim=c(0,45))
points(train_post$time, train_post$tug)
dev.off()

#-- example patient chart
# fit brokenstick
library(brokenstick)
fit <- brokenstick(y = unlist(train_post$tug),
                   x = unlist(train_post$time),
                   subjid = unlist(train_post$patient_id),
                   knots = c(0, 15, 50, 90)
                   )
est1 <- predict(fit, at="knots")

#-- fit pmm and arrange 
patlist <- est1[est1$x == 90 , ] %>% 
    dplyr::select(subjid, x, yhat) %>%
    rename(patient_id = subjid, time = x, y90 = yhat) %>% 
    mutate(patient_id = as.numeric(as.character(patient_id))) %>% 
    left_join(., 
              train_pre,
              by = "patient_id"
              ) %>% 
    filter(!is.na(!!"tug"), !is.na(y90), !is.na(age)) %>% 
    dplyr::select(tug, patient_id, y90, age, gender, bmi) %>% 
    cbind(
          Fitted = gamlss(y90 ~ tug + age + gender + bmi,family=NO, data=.) %>%
              .$mu.fv
          ) %>% 
    arrange(Fitted) %>% 
    dplyr::select(patient_id) %>% unlist(.) %>% as.vector %>% head(20)

#-- fit gamlss on 1st person's data

dfm <- train_post %>%
    filter(patient_id %in% patlist)

fit<-gamlss(eval(as.formula(tug ~ cs(time^0.5, df=3))),
                    sigma.formula = as.formula(~cs(time^0.5, df=1)),
                    nu.formula = ~1,
                    tau.formula = ~1,
                    data=dfm, family=BCTo,
                    )

png("./report/figure/fig_exprchart.png")
centiles.fan(fit, xvar=dfm$time,
             cent=c(3,10,25,50,75,90,97),
             colors="terrain",ylab="TUG", xlab="Time Post Surgery (days)",main="",
             ylim=c(0,45))
points(dfm$time, dfm$tug)
dev.off()



temp <- centiles.pred(fit, type="z-scores",
              xname="time",
              #data=dfm,
              xval=test_proc$train_post[test_proc$train_post$patient_id %in% test_proc$train_o$id[c(2)],"time"][[1]],
              yval=test_proc$train_post[test_proc$train_post$patient_id %in% test_proc$train_o$id[c(2)],"tug"][[1]])




# - - - - - - - - - - - - - - - - - - - - #
# Development Code From Here
# - - - - - - - - - - - - - - - - - - - - #

# Optimization goals
# 1. Remove Output of GAMLSS iterations
# 2. Include status message (% based on nearest_n and number of patients
# 3. Vectorize the process


    # - - - - - - - - - - - - - - - - - - - - - - #
    plot_func <- function(plotobj = plotobj+ train = TRUE, filter=filter){
        library(data.table)
        val <- "c50"
        if(train == TRUE){

            if(filter==TRUE){

                temp <- rbindlist(fin$pred_res$pred) %>% filter_(paste0('abs(',val,')', ' < 20')) # first one is c50

            } else {

                temp <- rbindlist(plotobj$pred_res$pred) # first one is c50
            }

            filtdf <- temp %>%
                bind_cols(
                          dec = temp %>% 
                              dplyr::select_(val) %>%
                              as.data.frame() %>%
                              ntile(., 10)# create deciles
                          ) %>%
            left_join(
                      temp %>%
                          bind_cols(
                                    dec = temp %>% 
                                        dplyr::select_(val) %>%
                                        as.data.frame() %>%
                                        ntile(., 10)# create deciles
                                    ) %>%
                      group_by(dec) %>%
                      summarise_(avg_val =  paste0("avg_val = ", obs_sum, "(",val,")"))
                  )

            pred_tug <- temp  %>%
                bind_cols(
                          dec = temp %>% 
                              dplyr::select_(val) %>%
                              as.data.frame() %>%
                              ntile(., 10)# create deciles
                          ) %>%
            group_by(dec) %>%
            summarise_(avg_val = paste0("avg_val = ", obs_sum, "(",val,")"))
        #summarise(avg_val = mean(get(val))) 

        if(pred_sum == "mean") {
            print(paste0("Distribution within Each Decile of Predicted TUG"))
            library(broom)
            dffitpois <- filtdf %>%
                group_by(dec) %>%
                do(fitpois = glm(tug ~ 1, data= .))
            dfpoiscoef <- tidy(dffitpois, fitpois)
            
            # create poisson estimates and standard errors for each decile for plotting
            print(paste0("Plot DF creation"))
            plotdf <- pred_tug %>%
                left_join(dfpoiscoef %>% dplyr::select(dec, estimate, std.error)) %>%
                mutate(ul = estimate+1.96*std.error,
                       ll = estimate-1.96*std.error) 
        } else {
            quant95 <- filtdf %>%
                group_by(dec) %>%
                summarise(estimate = median(tug),
                          ll = quantile(tug, 0.25),
                          ul = quantile(tug, 0.95))
            plotdf <- pred_tug %>%
                left_join(quant95)
        }


            # set minimum and maximum based on the range of predicted and observed TUG values
            minc <- floor(min(plotdf$ll, plotdf$avg_val)) 
            maxc <- ceiling(max(plotdf$ul, plotdf$avg_val)) 

            # plot observed vs. predicted TUG on decile of predicted TUG
            cptrain <-  ggplot(plotdf, aes(x = avg_val, y = estimate, ymin = estimate-1.96*std.error, ymax=estimate+1.96*std.error)) + geom_pointrange() + 
                xlim(minc, maxc) + ylim(minc,maxc) + 
                geom_abline(slope=1, intercept=0) + 
                xlab("Predicted TUG") + 
                ylab("Observed TUG")

            return(cptrain)


# - - - - - - - - - - - - - - - - - - - - #
# doParallel to parallelize the matching
# - - - - - - - - - - - - - - - - - - - - #

library(doParallel)
cl <- makeCluster(4)
registerDoParallel(cl)


# Currently there are issues with parallelizing the gamlss predictions
# Link: https://stackoverflow.com/questions/38900489/parallel-foreach-changes-data-to-na-when-running-dropterm
# Link2: https://stackoverflow.com/questions/49187268/r-package-gamlss-used-inside-foreach-fails-to-find-an-object

temp <-  for(n in seq(20,20, 10)) {
    patlistint <- seq(1,length(ordered$id))
    cnt = 0
    ref <- gamlss(tug~pb(time),
                  sigma.formula=~pb(time),
                  nu.formula=~pb(time),
                  train_post,
                  family=BCCGo)
    temp2 <- foreach(i=patlistint, .packages=c("dplyr","gamlss")) %dopar% {

        # Create vector list
        biasvec<-vector()
        rmsevec <- vector()
        coveragevec<-vector()
        coveragevec95<-vector()
        iqrvec<-vector()
        ninefiveconf <- data.frame()

        cnt = cnt + 1

        message(paste0('current n is: ',n))
        message(paste0('current count is: ',cnt))
        message(paste0('current i is: ',i))
        # make sure the edge cases are matched up well
        if (i == 1) {
            matches<-ordered$id[c(2:(1+n))]
        } else if (i == 2) {
            matches <- ordered$id[c(1, (3):(1+n))]
        } else if ((i-(n/2)) < 0) {
            offset <- abs(i-(n/2)) + 1
            matches<-ordered$id[c(1:(i-1),(i+1):(i+(n/2)+offset))]
        } else {
            matches<-ordered$id[c((i-(n/2)):(i-1),(i+1):(i+(n/2)))]
        }
        matchmodel <- train_post[train_post$patient_id %in% matches, ]
        print(colnames(matchmodel))

        # run the gamlss with nearest neighbors
        plm <- update(ref, data=matchmodel)
        #plm <- function() {

            #out <- tryCatch(
                            #{
                                #message("TRY PART")
                                #gamlss(tug~pb(time),
                                       #sigma.formula=~pb(time),
                                       #nu.formula=~pb(time),
                                       #matchmodel,
                                       #family=BCCGo)
                            #},
                            #error = function(cond)
                            #{
                                #message(paste("ERROR in GAMLSS"))
                                #message("ORIGINAL ERROR:")
                                #message(cond)
                                #return(NA)
                            #},
                            #warning=function(cond)
                            #{
                                #message(paste("GAMLSS WARNING"))
                                #message("ORIIGNAL WARNING:")
                                #message(cond)
                                #return(NULL)
                            #},
                            #finally={
                                #message("PROCESSING GAMLSS")
                                #message("GAMLSS EOL SUCCESS")
                            #}
                            #)
            #return(out)
        #}

        # if something wrong with iteration... then just don't do prediction it eats up all the memory anyways
        if(typeof(plm) != 'list') {

            misses = misses + 1 # increment number of misses
            message('Something wrong with plm model. Prediction not included')

        } else {

            message(paste0("Getting predictions: "))

            # time window again
            if(is.null(time_window)){
                iqr<-centiles.pred(plm, type="centiles",  xname = time_elapsed, xvalues=c(mint:maxt),
                                   cent=c(25, 50, 75),
                                   data=matchmodel,
                                   plot=FALSE)
            } else {
                iqr<-centiles.pred(plm, type="centiles",  xname = time_elapsed, xvalues=c(time_window[1]:time_window[2]),
                                   cent=c(25,50,75),
                                   data=matchmodel,
                                   plot=FALSE)
            }

            # - - - - --  - -- - - - - - -- - - #
            # Code below is for testing purposes#
            # - - - - --  - -- - - - - - -- - - #

            #temp <- gamlss(tug~cs(time), sigma.formula=~cs(time), family=GA, data = train_post)
            #temp2 <- centiles.pred(temp, type='centiles', xname = 'time', xvalues=c(mint:400), cent = c(25,50,75), plot=FALSE)
            #str(temp2)

            # - - - - - - - -- - - - - #
            # code mostly for graphing #
            # - - - - - - - -- - - - - #

            iqr$iqr<-iqr$C75-iqr$C25
            #x<-iqr[,time_elapsed]
            #y<-iqr$iqr 

            #dfList[[cnt]] <- data.frame( x,y)

            iqrvec[cnt]<-mean(iqr$iqr)
            targetid<-ordered$id[c(i)]
            targetrec<-train_post[which(train_post$patient_id %in% targetid), ]

            bias<-merge(iqr,targetrec, by=time_elapsed)
            bias$diff<-bias$C50-bias[,outcome]

            #store mean of bias and rmse in biasvec, rmsevec
            biasvec[cnt] <- mean(bias$diff)
            rmsevec[cnt] <- sqrt(sum(na.omit(bias$diff)^2)/length(na.omit(bias$diff)))

            # coverage prob based on IQR
            bias$cov1<-bias[,outcome]-bias$C25
            bias$cov2<-bias$C75-bias[,outcome]
            bias$cov3<-ifelse(bias$cov1>0,1,0)
            bias$cov4<-ifelse(bias$cov2>0,1,0)
            bias$cov5<-bias$cov3+bias$cov4
            bias$coverage<-ifelse(bias$cov5>1,1,0)

            # store mean of the n coverage in vector
            coveragevec[cnt]<-mean(bias$coverage)

            # cov based on 95% CI
            message(paste0("Getting predictions for 95% CI: "))
            predandse <- function() {

                out <- tryCatch(
                                {
                                    message("TRY PART")
                                    predict(plm, se.fit=TRUE, type='response')
                                },
                                error = function(cond)
                                {
                                    message(paste("ERROR in GAMLSS"))
                                    message("ORIGINAL ERROR:")
                                    message(cond)
                                    return(NA)
                                },
                                warning=function(cond)
                                {
                                    message(paste("GAMLSS WARNING"))
                                    message("ORIIGNAL WARNING:")
                                    message(cond)
                                    return(NULL)
                                },
                                finally={
                                    message("PROCESSING GAMLSS")
                                    message("GAMLSS EOL SUCCESS")
                                }
                                )
                return(out)
            }
            predandse <- predandse()

            if(typeof(predandse) != 'list') {

                coveragevec95[cnt] <- NA

            } else {

                misses = misses + 1 # increment number of misses
                message('Something wrong with plm model. Prediction not included')
                predandse <- as.data.frame(predandse)
                message(paste0("DataFrame added: "))
                predandse$ll <- predandse$fit - 1.96*predandse$se.fit
                message(paste0("Confidence Intervals: "))
                predandse$ul <- predandse$fit + 1.96*predandse$se.fit
                predandse[,time_elapsed] <- plm$mu.x[,2]
                bias <- merge(bias,predandse, by=time_elapsed)
                bias$cov1<-bias[,outcome]-bias$ll
                bias$cov2<-bias$ul-bias[,outcome]
                bias$cov3<-ifelse(bias$cov1>0,1,0)
                bias$cov4<-ifelse(bias$cov2>0,1,0)
                bias$cov5<-bias$cov3+bias$cov4
                bias$coverage<-ifelse(bias$cov5>1,1,0)

                # store mean of the n coverage in vector
                coveragevec95[cnt]<-mean(bias$coverage)

            }

        }

        list(biasvec, coveragevec, coveragevec95, rmsevec, misses)

    }
}

library(microbenchmark)
microbenchmark(
               temp,
               temp2)

# stop clusters
stopCluster(cl)

# After we figure out the optimal number of matches we run gamlss for each subject in the test data by matching with the n closest in the trainig data and fit a gamlss model and calculate the bias and coverage in the test data!


# - - - - - - - - - - - - - - - - - - - - #
# Temporary Area
# - - - - - - - - - - - - - - - - - - - - #


# Matching and Bootstrap by weight

# Extract Matched Train Patient ID
matches <- test_proc$train_o %>% 
    bind_cols(diff = abs(test_proc$train_o$Fitted[1] - test_proc$train_o$Fitted)) %>%
    arrange(diff) %>%
    dplyr::select(id) %>%
    .[-1,] %>%
    head(n = 10) %>% unlist %>% as.vector

# get probabilty weighting by id based on n (1/n is prob weight)

matchprob <- test_proc$train_o %>% 
    bind_cols(diff = abs(test_proc$train_o$Fitted[1] - test_proc$train_o$Fitted)) %>%
    arrange(diff) %>%
    dplyr::select(id) %>%
    .[-1,] %>%
    head(n = 10) %>% cbind(weight=seq(10,1)/sum(seq(10,1))) %>%
    as.data.frame %>%
    rename(patient_id = 1)

# create dataset with weights
matchmodel <- test_proc$train_post[test_proc$train_post$patient_id %in% matches, ] %>%
    left_join(
              matchprob
              ) %>%
dplyr::select(patient_id, time, tug, weight) 

matchindex <- sample(x = seq_len(nrow(matchmodel)), size=1000, prob=matchmodel$weight, replace=TRUE)

# create final weighted matchdataset
matchmodel <- matchmodel[matchindex,]
