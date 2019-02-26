# File:             test_smocc.R
# Objective:        Try out functions on the SMOCC growth data

library(dplyr)
library(pcr)
smocc <- pcr::smocc

# include time-invariant covariates into long data
full <- smocc[[2]] %>% 
  dplyr::select(id, rec, age, sex, ga, bw, 
         hgt, wgt, hdc, bmi, 
         hgt.z, wgt.z, hdc.z, bmi.z) %>% 
  left_join(dplyr::select(smocc[[1]], id, edu, twin, agem, smo, hgtm, hgtf, hdc_0), 
            by = c("id")) %>% 
  dplyr::select(id, rec, age, sex, ga, bw, edu:hgtf, hgt:bmi.z, hdc_0)

# for testing, make random selection of 100 persons
set.seed(98881)
ids <- sample(unique(full$id), 100)
full <- full %>% 
  filter(id %in% ids)

# baseline indicator creation
full <- baselinemk(full, pat_id = "id", time_var = "age")

# check that baseline equals record no. 1 
with(full, table(baseline, rec))

# make sure there aren't any missing data and etc
summary(full)
sapply(full, function(x) table(is.na(x)))
full[is.na(full$hgt), ]


# define knots (knots should be integer values!)
knots <- round(c(0, 28/365.25, 56/365.25, 1/4, 1/3, 1/2,
                 7.5/12, 9/12, 11/12, 14/12, 18/12, 2), 4)
knots <- as.integer(knots*365.25)
#Boundary.knots <- c(0, 3)

#-- Missing covariate information will not allow predictive mean matching!
full[,c("hdc","sex", "ga", "bw", "edu", "hgtf", "hgtm","hdc_0")] %>% complete.cases %>% table

#-- remove rows with missing outcome
full %>%
    filter(is.na(hdc)) %>%
    dplyr::select(id, hdc, age, sex)

full %>% 
    filter(!is.na(hdc)) %>% 
    nrow

full <- full %>% 
    filter(!is.na(hdc))

#-- Need to have patients that have both the pre op AND post op values
full %>% 
    filter(baseline == 1) %>%
    dplyr::select(id, hdc) %>% 
    full_join(
              full %>% 
                  filter(baseline ==0) %>%
                  distinct(patient_id, .keep_all =TRUE) %>%
                  dplyr::select(id, hdc) %>%
                  rename(p_hdc = hdc)
    ) %>%
    filter(is.na(p_hdc) | is.na(hdc)) %>% nrow

#-- create vector of id's to exclude based on having only single observation
exclude <- full %>% 
    filter(baseline == 1) %>%
    dplyr::select(id, hdc) %>% 
    full_join(
              full %>% 
                  filter(baseline ==0) %>%
                  distinct(id, .keep_all =TRUE) %>%
                  dplyr::select(id, hdc) %>%
                  rename(p_hdc = hdc)
    ) %>%
    filter(is.na(p_hdc) | is.na(hdc)) %>%
    dplyr::select(id) %>% unlist %>% as.vector

full <- full %>%
    filter(!id %in% exclude)

# divide train and test
full <- full %>% 
    left_join(
              full %>% 
                  distinct(id)  %>%
                  bind_cols(
                            train_test = c(rep(1,round(full %>%
                                               distinct(id)  %>% 
                                               nrow *  (2/3))), rep(2, round(full %>%
                                           distinct(id)  %>% 
                                           nrow *  (1/3)))) 
                  )
    )

#-- specify integer for the time variable (i.e. age) for matching. Floating point won't provide right number to match in terms of the time points
# time has to be integer or else the algorithm won't match the times! we need to make it so that even if time isn't int the preproc automatically processes it and wars the user :)
full <- full %>% 
    dplyr::select(id, train_test, age,sex, agem, edu, hdc_0, hdc, baseline) %>%
    # time needs to be as.integer() for matching the patients!!!
    mutate(age = as.integer(age*365.25)) %>%
    rename(patient_id = id,
           time = age
    )

# pre-processing
test_proc <- preproc(
  dff = full,
  split_var = "train_test",
  trainval = 1,
  testval = 2, 
  knots_exp = knots,
  out_time = as.integer(round((14/12)*365.25,0)), 
  outcome = "hdc", 
  time_var = "time",
  pat_id = "patient_id", 
  varlist = c("sex","hdc_0"),
  filter_exp = NULL)

summary(test_proc$reg_obj)

# TRAINING DATA: LOOCV Result
fin <- loocv_function(
  nearest_n = seq(15,50,1),
  train_post = test_proc$train_post,
  ord_data = test_proc$train_o,
  test_post = test_proc$test_post,
  test_o = test_proc$test_o,
  outcome = "hdc",
  time_elapsed = "time",
  cs = TRUE,
  dfspec = TRUE,
  parallel = 3,
  #loocv=FALSE,
  #userchoose=120,
  d_f_m = 2, ptr_m = 1, d_f_s = 1,
  dist_fam = gamlss.dist::NO
)

# CHECK PERFORMANCE
loocvperf(fin$loocv_res, test_proc$train_o)

# Model performance plot with bias, coverage, width 50% pred in
plot_cal(
         # Specify plotobj which is the object saved from the loocv_func()
         plotobj=fin,
         # specify the processed file to match test data and train data for calibration
          test_proc=test_proc,
         # specify calibration plot x 
          pred_sum='mean',
          # specify observed distribution to use (default = "median")
          obs_dist="median",
          wtotplot=FALSE
          )

# Calibration plot 
plot_cal(
         plotobj=fin,
         test_proc=test_proc,
         outcome = 'hdc',
         pred_sum='mean',
         obs_dist="median",
         # specify loocv=FALSE to check calibration
         loocv = FALSE
)

# Internal Validation Performance Measures
loocvperf(fin$loocv_res, test_proc)

# External Validation Performance Measures
extvalid(fin, test_proc)

# Comparison with Reference models!
## GAMLSS predictions first

### 1. GAMLSS Fitting
library(gamlss)
library(cowplot)
refgamlss <- gamlss(hdc ~cs(time, df=3),
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
         outcome = 'hdc',
         pred_sum='mean',
         obs_dist="median",
         # specify loocv=FALSE to check calibration
         loocv = FALSE,
         iqrfull=iqrfull
)
save_plot("../report/figure/smocc_refcomparison.png", complot, base_height=5, base_width = 10)
