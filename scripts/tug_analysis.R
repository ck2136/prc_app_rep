# - - - - - - - - - - - - - - - - - - - - - #
# File Name: 	'tug_analysis.R'
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
p_load(here, readxl, dplyr, rio,pcr, cowplot)
# if you are in the code folder get out!
#library("pcr")
setwd(here())

# - - - - - - - - - - - - - - - - - - - - #
# Data Import and testing
# - - - - - - - - - - - - - - - - - - - - #

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

# Train and Test split for all TKA outcomes: create 

full <- baselinemk(full, "patient_id", "time")

# quick data check
# make sure there aren't any missing data and etc
summary(full) ;  sapply(full, function(x) {
                          table(is.na(x))})


full <- full %>%
    distinct(patient_id, time, .keep_all=TRUE)


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
                dff=full,  # specify full dataset name
                split_var = 'train_test', # train test split variable
                trainval = 1, # training set value
                testval = 2, # test set value
                knots_exp = c(0, 14, 50, 90), # Specify broken stick knots
                out_time = 90,  # specify which timepoint to use 
                outcome = "tug", # specify outcome variable name
                time_var = "time", # specify time variable name
                pat_id = "patient_id", # specify patient id variable name
                varlist = c("age","gender","bmi","b_tug"), # specify list of covariates for pmm
                filter_exp = "time > 3"               
                #filter_exp = NULL
)
saveRDS(test_proc,"./data/test_proc.RDS")

# - - - - - - - - - - - - - - -#
# TRAINING DATA: LOOCV Result
# - - - - - - - - - - - - - - -#

# need to run everything in parallel by 3 and store results for later use
# let's say we do 5 at a time for the 3 cores then we go up by 15

## run from 5 to 100, 5 to 14 run with just a sequential forlopp 
#fin <- loocv_function(nearest_n = c(seq(15,100,1)),
                      #train_post = test_proc$train_post,
                      #ord_data = test_proc$train_o,
                      #test_post = test_proc$test_post,
                      #test_o = test_proc$test_o,
                      #outcome = "tug",time_elapsed = "time",
                      #cs=TRUE,
                      #dfspec=TRUE,
                      #matchprobweight=FALSE,
                      #d_f_m=3, ptr_m=0.5,
                      #d_f_s=1,
                      #userchoose=NULL,                    # User gets to choose number 
                      #seed=1234,
                      #parallel=3,
                      #dist_fam = gamlss.dist::BCCGo
                      #)
## run things in parallel but be cognizant about memory

#saveRDS(fin,paste0("./data/fin_BCCGo_15_100_cs_dfspec_tug.RDS"))# saved as the Gamma distribution
#rm(fin)

#fin <- loocv_function(nearest_n = c(5:14,200,300,400),
                      #train_post = test_proc$train_post,
                      #ord_data = test_proc$train_o,
                      #test_post = test_proc$test_post,
                      #test_o = test_proc$test_o,
                      #outcome = "tug",time_elapsed = "time",
                      #cs=TRUE,
                      #dfspec=TRUE,
                      #matchprobweight=FALSE,
                      #d_f_m=3, ptr_m=0.5,
                      #d_f_s=1,
                      #userchoose=NULL,                    # User gets to choose number 
                      #seed=1234,
                      #dist_fam = gamlss.dist::BCCGo
                      #)

#saveRDS(fin,paste0("./data/fin_BCCGo_5_400_cs_dfspec_tug.RDS"))# saved as the Gamma distribution
#rm(fin)



# - - - - - - - - - - - - - - - - - - - - #
# Plots: Calibration, Bias/RMSE and Coverage
# - - - - - - - - - - - - - - - - - - - - #

#-- Load Final Results

#-- filtering on time
fin <- readRDS(paste0(here(),"/data/fin_BCCGo_5_100_400_cs_dfspec_tug.RDS"))
#fin1 <- readRDS(paste0(here(),"/data/fin_BCCGo_5_400_cs_dfspec_tug.RDS"))
#fin2 <- readRDS(paste0(here(),"/data/fin_BCCGo_15_100_cs_dfspec_tug.RDS"))

#-- merge 2 together for viewing
#finfilt <- mapply(c, 
               #list(loocv_res = fin2$loocv_res, 
                             #loocv_score = fin2$loocv_score
                             #), 
               #list(loocv_res = fin1$loocv_res, 
                             #loocv_score = fin1$loocv_score
                             #), 
               #SIMPLIFY=FALSE)

#finnofilt <- mapply(c, 
               #list(loocv_res = fin3$loocv_res, 
                             #loocv_score = fin2$loocv_score
                             #), 
               #list(loocv_res = fin4$loocv_res, 
                             #loocv_score = fin1$loocv_score
                             #), 
               #SIMPLIFY=FALSE)
#finfilt$pred_res <- fin2$pred_res
#finfilt$nearest_n <- fin2$nearest_n

#rm(fin1,fin2)

#-- Bias, Cov, Pred

temp <- fin
temp$loocv_res %>% str(., max.level = 1)
names(temp$loocv_res) <- paste0("nearest_", seq(5,103))


png("./report/figure/LOOCV_BCCGo_tug.png")
dev.new()
p <- plot_cal(
         # Specify plotobj which is the object saved from the loocv_func()
         # if multiple distributes were used then specify the distribution (e.g. myfiles$BCCGo) otherwise specify just the object (e.g. fin)
         #plotobj=finfilt,
         #plotobj=finnofilt,
         plotobj=temp,
         # specify the processed file to match test data and train data for calibration
          test_proc=test_proc,
         # specify calibration plot x 
          pred_sum='mean',
          # specify observed distribution to use (default = "median")
          obs_dist="median",
          wtotplot=FALSE
          )
save_plot("./report/figure/LOOCV_BCCGo_tug.png", p, base_width=10, base_height=4)
dev.off()

#-- Calibration
plot_cal(
         #plotobj=myfiles$BCCGo,
         plotobj=temp,
         #plotobj=finfilt,
         #plotobj=finnofilt,
          test_proc=test_proc,
          #outcome = 'plusm',
          outcome = 'tug',
          #outcome = 'knee_flex',
          pred_sum='mean',
          #obs_dist="gaussian",
          #obs_dist="poisson",
          obs_dist="median",
          # specify loocv=FALSE to check calibration
          loocv = FALSE,
          #plotvals=TRUE
          )
save_plot("./report/figure/LOOCV_BCCGo_tug.png", p, base_width=10, base_height=4)

# - - - - - - - - - - - - - - - - - - - - #
# Reference chart and PLM plots
# - - - - - - - - - - - - - - - - - - - - #

refplm <- plot_ref_plm(temp,
             test_proc,
             outcome="tug",
             time_var="time",
             patnum = 219,
             dist=gamlss.dist::NO,
             df_m=2,
             df_s=1,
             xvalues=3:200
)
save_plot("./report/figure/refplmcompare.png", refplm, base_width = 10, base_height = 5)

# - - - - - - - - - - - - - - - - - - - - #
# Internal Validation Performance Measures
# - - - - - - - - - - - - - - - - - - - - #

loocvperf(fin$loocv_res, test_proc$train_o)

# - - - - - - - - - - - - - - - - - - - - #
# External Validation Performance Measures
# - - - - - - - - - - - - - - - - - - - - #

extvalid(fin, test_proc)

# - - - - - - - - - - - - - - - - - - - - #
# population vs. plm calibration
# - - - - - - - - - - - - - - - - - - - - #
### 1. GAMLSS Fitting
library(gamlss)
library(cowplot)
refgamlss <- gamlss(tug ~cs(time^0.5, df=3),
                sigma.formula = ~cs(time, df=1),
                family = BCCGo,
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
         outcome = 'tug',
         pred_sum='mean',
         obs_dist="median",
         # specify loocv=FALSE to check calibration
         loocv = FALSE,
         iqrfull=iqrfull
)
save_plot("./report/figure/CAL_REFvPLM_BCCGo_tug.png", complot, base_width=10, base_height=4)
