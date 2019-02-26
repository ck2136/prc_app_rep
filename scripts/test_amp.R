# - - - - - - - - - - - - - - - - - - - - - #
# File Name: 	'test_amp.R'
# Purpose:
# Created on: 	22-02-2019
# Modified on 	17-12-2018
# Created by:
# Contact Info: ck
# - - - - - - - - - - - - - - - - - - - - - #

# - - - - - - - - - - - - - - - - - - - - #
# Environment configuration
# - - - - - - - - - - - - - - - - - - - - #

## Remove objects in workspace
rm(list=ls())
## Set working directory
library(pacman)
p_load(here, readxl, dplyr, rio,pcr)
# if you are in the code folder get out!
#library("pcr")
setwd(here())

# - - - - - - - - - - - - - - - - - - - - #
# Data Import and testing
# - - - - - - - - - - - - - - - - - - - - #

amp  <- import("./data/amp_data_test_021519.csv")

# messed up train_test variable
amp <- amp %>% 
    dplyr::select(-train_test) %>%
    left_join(
              amp %>%
                  distinct(patient_id, .keep_all=TRUE) %>%
                  dplyr::select(patient_id, train_test)
    ) 

#-- add baseline indicator
amp <- baselinemk(amp, "patient_id", "time")

#-- add baseline value of plusm as predictor
amp <- amp %>% 
    left_join(
              amp %>% 
                  arrange(patient_id, time) %>% 
                  distinct(patient_id, .keep_all = TRUE) %>%
                  dplyr::select(patient_id, plusm) %>%
                  rename(b_plusm = plusm)
    ) 

#-- filter out duplicated values

amp <- amp %>%
    distinct(patient_id, time, .keep_all=TRUE) 

#-- Need to have patients that have both the pre op AND post op values
amp %>% 
    filter(baseline == 1) %>%
    dplyr::select(patient_id, plusm) %>% 
    left_join(
              amp %>% 
                  filter(baseline ==0) %>%
                  distinct(patient_id, .keep_all =TRUE) %>%
                  dplyr::select(patient_id, plusm) %>%
                  rename(p_plusm = plusm)
    ) %>%
    filter(is.na(p_plusm) | is.na(plusm)) %>% nrow

exclude <- amp %>% 
    filter(baseline == 1) %>%
    dplyr::select(patient_id, plusm) %>% 
    left_join(
              amp %>% 
                  filter(baseline ==0) %>%
                  distinct(patient_id, .keep_all =TRUE) %>%
                  dplyr::select(patient_id, plusm) %>%
                  rename(p_plusm = plusm)
    ) %>%
    filter(is.na(p_plusm) | is.na(plusm)) %>%
    dplyr::select(patient_id) %>% unlist %>% as.vector

amp <- amp %>%
    filter(!patient_id %in% exclude)

# quick data check
# make sure there aren't any missing data and etc
summary(amp) ;  sapply(amp, function(x) {
                          table(is.na(x))})



#- - - - - - - - - - - - - - - - - - - - - - - - - - #
## Run the Long LOOCV Training Set Matchem Up!
#- - - - - - - - - - - - - - - - - - - - - - - - - - #

##extract fitted values for the linear model (PREOP only here)
#-- preprocess train and test data: involves
#1. Taking training post data and running broken stick to get y90 or yNU where NU is value chosen
#2. Processes test data set so that yNU is matched with training data
#3. The matched test and train data according to yNU is used to later match the personalized predicted gamlss


# Process the full data to get
# 1. Post operative test data 
# 2. Fitted 90-Day TUG for test patients
# 3. Post operative train data 
# 4. Fitted 90-Day TUG for train patients
test_proc <- preproc(
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
                varlist = c("age","gender","bmi", "b_plusm", "cause","amp_laterality"), # specify list of covariates for pmm
                filter_exp = NULL               
)

summary(test_proc$reg_obj)

# - - - - - - - - - - - - - - -#
# TRAINING DATA: LOOCV Result
# - - - - - - - - - - - - - - -#

# need to run everything in parallel by 3 and store results for later use
# let's say we do 5 at a time for the 3 cores then we go up by 15

# run from 5 to 100, 5 to 14 run with just a sequential forlopp 
fin <- loocv_function(nearest_n = seq(25,205,5),
                      train_post = test_proc$train_post,
                      ord_data = test_proc$train_o,
                      test_post = test_proc$test_post,
                      test_o = test_proc$test_o,
                      outcome = "plusm",time_elapsed = "time",
                      cs=TRUE,
                      dfspec=TRUE,
                      matchprobweight=FALSE,
                      d_f_m=3, ptr_m=0.5,
                      d_f_s=1,
                      userchoose=NULL,                    # User gets to choose number 
                      seed=1234,
                      parallel=3,
                      dist_fam = gamlss.dist::NO
                      )
# run things in parallel but be cognizant about memory

saveRDS(fin,paste0("./data/fin_NO_25_205_5_cs_dfspec_plusm.RDS"))# saved as the Gamma distribution
rm(fin)


# - - - - - - - - - - - - - - - - - - - - #
# Plots: Calibration, Bias/RMSE and Coverage
# - - - - - - - - - - - - - - - - - - - - #

#-- Load Final Results

fin <- readRDS(paste0(here(),"/data/fin_NO_25_205_5_cs_dfspec_plusm.RDS"))

#-- Bias, Cov, Pred
dev.new()
ploocv <- plot_cal(
         # Specify plotobj which is the object saved from the loocv_func()
         # if multiple distributes were used then specify the distribution (e.g. myfiles$BCCGo) otherwise specify just the object (e.g. fin)
         #plotobj=finfilt,
         #plotobj=finnofilt,
         plotobj=fin,
         # specify the processed file to match test data and train data for calibration
          test_proc=test_proc,
         # specify calibration plot x 
          pred_sum='mean',
          # specify observed distribution to use (default = "median")
          obs_dist="median",
          wtotplot=FALSE
          )
save_plot("./report/figure/LOOCV_BCCGo_plusm_30_150_10.png",ploocv, base_width = 10, base_height = 5)

#-- Calibration
pcal <- plot_cal(
         #plotobj=myfiles$BCCGo,
         plotobj=fin,
         #plotobj=finfilt,
         #plotobj=finnofilt,
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
save_plot("./report/figure/CAL_NO_plusm_30_150_10.png",pcal, base_width = 10, base_height = 5)

# - - - - - - - - - - - - - - - - - - - - #
# Internal Validation Performance Measures
# - - - - - - - - - - - - - - - - - - - - #

loocvperf(fin$loocv_res, test_proc$train_o)

# - - - - - - - - - - - - - - - - - - - - #
# External Validation Performance Measures
# - - - - - - - - - - - - - - - - - - - - #

extvalid(fin, test_proc)

# - - - - - - - - - - - - - - - - - - - - #
# Reference Model Comparison
# - - - - - - - - - - - - - - - - - - - - #

library(gamlss)
library(cowplot)
refgamlss <- gamlss(plusm ~cs(time, df=3),
                sigma.formula = ~cs(time^0.5, df=1),
                family = NO,
                data = test_proc$train_post
)

### 2. Prediction at Outtime
mint <- min(c(min(test_proc$train_post$time), min(test_proc$test_post$time)))
maxt <- max(c(max(test_proc$train_post$time), max(test_proc$test_post$time)))
iqrfull <- centiles.pred(refgamlss, type='centiles', xname = "time", xvalues=c(mint:maxt),
                         data = test_proc$train_post,
                         cent=c(25,50,75), plot=FALSE)

complot <- plot_cal(
         plotobj=fin,
         test_proc=test_proc,
         outcome = 'plusm',
         pred_sum='mean',
         obs_dist="median",
         # specify loocv=FALSE to check calibration
         loocv = FALSE,
         iqrfull=iqrfull
)
save_plot("./report/figure/amp_refcomparison.png", complot, base_height=5, base_width = 10)
