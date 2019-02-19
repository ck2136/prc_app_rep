#functions.R


# - - - - - - - - - - - - - - - - - - - - - - #
# PreProcess Function: create train/test filtering ----
# - - - - - - - - - - - - - - - - - - - - - - #
preproc <- function(dff=full,
                    split_var = 'train_test',
                    trainval = 1,
                    testval = 2,
                   filter_exp = NULL,               # expression can be of form "time > 3"
                   knots_exp = c(0, 14, 50, 90),    # select knots that are clinically relevat
                   out_time = 90,                   # this variable has to be within the knots above
                   outcome = "tug",
                   time_var = "time",
                   pat_id = "patient_id",           # Need the patient_id column to be specified as "patient_id"
                   baseline_var = "baseline",       # pre-op/post-op indicator variable. preop: baseline = 1; postop: baseline = 0 or otherwise
                   varlist = NULL,                  # need user to fill in var list otherwise will use all vars, categorical variables need to be factor or character
                   pmmform = NULL,
                   modelselect = FALSE,
                   ...) {

    # - - - - - - - - - - - - - - - - - - - - - - #
    # Load 'dplyr' for data manipulation
    # - - - - - - - - - - - - - - - - - - - - - - #
    library(dplyr)

    # - - - - - - - - - - - - - - - - - - - - - - #
    # If baseline_var not  supplied, then stop
    # - - - - - - - - - - - - - - - - - - - - - - #
    if(is.null(baseline_var)){
        stop("baseline_var is NULL. 
             Specify  baseline var as string. (e.g.baseline_var = 'baseline').
             Utility function baselinemk() may be used to create baseline variable.
             ")
    }

    # - - - - - - - - - - - - - - - - - - - - - - #
    # If varlist supplied, then form dataframe that only contains 
    # ID, Outcome, Time, train/test split variable, and Listed variables 
    # else stop function
    # - - - - - - - - - - - - - - - - - - - - - - #
    if(!is.null(varlist)){
        dff <- dff %>%
            .[,c(pat_id, outcome, time_var, split_var, baseline_var, varlist)]
    } else {
        stop("varlist not populated: specify varlist = c('var1','var2',...)")
    }

    # - - - - - - - - - - - - - - - - - - - - - - #
    # Split test/train by pre and post
    # - - - - - - - - - - - - - - - - - - - - - - #

    df_train <-dff  %>% filter_(paste0(split_var, "==", trainval))
    df_test <- dff %>% filter_(paste0(split_var, "==",testval))

    # - - - - - - - - - - - - - - - - - - - - - - #
    # Split test/train by pre and post using baseline_var
    # - - - - - - - - - - - - - - - - - - - - - - #
    #if (all(dff %>% arrange_(pat_id, time_var) %>% distinct_(pat_id, .keep_all = TRUE) %>% select_(time_var) %>% unlist %>% as.vector  < 0)) {
        #pre_train_df <- df_train %>% filter_(paste0(time_var,"< 0"))
        #pre_test_df <- df_test %>% filter_(paste0(time_var, "< 0"))
    #} else if(all(dff %>% arrange_(pat_id, time_var) %>% distinct_(pat_id, .keep_all = TRUE) %>% select_(time_var) %>% unlist %>% as.vector == 0)){
        #pre_train_df <- df_train %>% filter_(paste0(time_var,"== 0"))
        #pre_test_df <- df_test %>% filter_(paste0(time_var, "== 0"))
    #} else {
        #stop("Inconsistencies in baseline time variable 
             #(i.e. there are patients that have time values > 0).
             #Either all baseline values need to be < 0 or == 0")
    #}

    # - - - - - - - - - - - - - - - - - - - - - - #
    # Split test/train by pre and post using baseline_var
    # - - - - - - - - - - - - - - - - - - - - - - #

    pre_train_df <- df_train %>% filter_(paste0(baseline_var,"== 1"))
    pre_test_df <- df_test %>% filter_(paste0(baseline_var, "== 1"))

    # - - - - - - - - - - - - - - - - - - - - - - #
    # Allow user to specify filter_exp
    # - - - - - - - - - - - - - - - - - - - - - - #

    if(is.null(filter_exp)) {
        post_train_df <- df_train %>% filter_(paste0(time_var, "> 3"))
        post_test_df <- df_test %>% filter_(paste0(time_var, "> 3"))
    } else {
        post_train_df <- df_train %>% filter_(filter_exp)
        post_test_df <- df_test %>% filter_(filter_exp) 
    }

    # - - - - - - - - - - - - - - - - - - - - - - #
    # use brokenstick to predict values at knots_exp
    # - - - - - - - - - - - - - - - - - - - - - - #

    library(brokenstick)
    fit <- brokenstick(y = unlist(post_train_df[,outcome]),
                       x = unlist(post_train_df[,time_var]),
                       subjid = unlist(post_train_df[,pat_id]),
                       knots = knots_exp
                       )

    est1 <- predict(fit, at="knots")
    library("rlang")
    library("tidyverse")
    alldf <- left_join(
                     est1[est1$x== out_time, ] %>%
                         setNames(c(pat_id,"x","y","yhat","knot")) %>%
                          dplyr::select_(pat_id, "yhat") %>%
                          # need to change pat_id bc it is a factor when outputted through predict()
                          mutate(!!pat_id := !!parse_quosure(paste0("as.numeric(as.character(",pat_id,"))")))
                      ,
                      pre_train_df,
                      by = pat_id
                      ) %>%
    # - - - - - - - - - - - - - - - - - - - - - - #
    # only keep complete cases 
    # - - - - - - - - - - - - - - - - - - - - - - #
    .[complete.cases(.),]
    # - - - - - - - - - - - - - - - - - - - - - - #
    # lm() using gamlss package to get predicted outcome at out_time
    # - - - - - - - - - - - - - - - - - - - - - - #
    library(gamlss)
    #pmm <- gamlss(yhat ~ get(outcome) + age + gender + bmi,family=NO, data=alldf)
    #pmm <- lm(yhat ~ get(outcome) + age + gender + bmi, data=alldf)
    if(is.null(pmmform)){
        pmm <- lm(formula(paste0("yhat ~ ", outcome, " + ", paste0(varlist, collapse="+"))), data=alldf)
    } else {
        pmm <- lm(formula = pmmform, data=alldf )
    }

    # - - - - - - - - - - - - - - - - - - - - - - #
    # If modelselect = TRUE, use stepAIC to choose variables
    # - - - - - - - - - - - - - - - - - - - - - - #
    if(modelselect){
        library(MASS)
        pmm <- stepAIC(pmm)
    }

    # - - - - - - - - - - - - - - - - - - - - - - #
    # Created dataset with fitted outcome at out_time for training patients
    # - - - - - - - - - - - - - - - - - - - - - - #
    train_ordered <- alldf %>%
        dplyr::select_(pat_id) %>%
        cbind(pmm$fitted.values) %>%
        rename_("id"= pat_id,
               "Fitted"="`pmm$fitted.values`") %>%
               #Fitted=`pmm$mu.fv`) %>%
        arrange(Fitted)

    # - - - - - - - - - - - - - - - - - - - - - - #
    # Created dataset with fitted outcome at out_time for testing patients
    # Here we still use the linear model used to fit the training data (i.e. pmm)
    # - - - - - - - - - - - - - - - - - - - - - - #

    test_ordered <- pre_test_df %>% 
        dplyr::select_(pat_id) %>%
        bind_cols(pred = predict(pmm, data=alldf, 
                                 newdata=pre_test_df %>% 
                                            .[,c(outcome, varlist)]
                             )
        ) %>%
        rename_("id" = pat_id) %>%
        arrange(pred)

    return(list(train_post = post_train_df, 
                train_o =  train_ordered,
                reg_obj = pmm,
                test_post = post_test_df, 
                test_o =  test_ordered)
    )
}



# - - - - - - - - - - - - - - - - - - - -#
# LOOCV Function ----
# - - - - - - - - - - - - - - - - - - - -#
# now using that we need to come up with the right number of matches that will give us the best bias and coverage.
loocv_function <- function(nearest_n = seq(20,150,by=10), # number to play with 
                           dist_fam = NULL, # for gamlss distribution
                           train_post = train_post, 
                           ord_data = ordered, 
                           test_post = test_post, 
                           test_o = test_o,  # datasets
                           outcome, time_elapsed, plot = FALSE,
                           matchprobweight=FALSE,
                           time_window = c(min(min(test_proc$train_post$time),min(test_proc$test_post$time)),max(max(test_proc$train_post$time),max(test_proc$test_post$time))),
                           interval=NULL,
                           cs=FALSE,
                           dfspec=NULL,
                           d_f_m=1.8, ptr_m=1,
                           d_f_s=1.2,
                           d_f_n=1,
                           d_f_t=1,
                           thresh_val = 50,
                           printtrace=FALSE,
                           userchoose=FALSE,
                           seed=1234,
                           ...) {

    # - - - - - - - - - - - - - - - - - - - - - # 
    # load data.table for rbindlist(): binding multiple lists of performance 
    # - - - - - - - - - - - - - - - - - - - - - # 
    library("data.table")

    # - - - - - - - - - - - - - - - - - - - - - # 
    # Setting spline option
    # - - - - - - - - - - - - - - - - - - - - - # 
    if(cs){
        if(is.null(dfspec)) {
            spl=paste0("cs(")
            spls=paste0("cs(")
            spln=paste0("cs(")
            splt=paste0("cs(")
        } else {
            spl=paste0("cs(df=",d_f_m,",")
            spls=paste0("cs(df=",d_f_s,",")
            spln=paste0("cs(df=",d_f_n,",")
            splt=paste0("cs(df=",d_f_t,",")
        }
    } else {
        spl="pb("
    }

    # - - - - - - - - - - - - - - - - - - - - - # 
    # Setting distribution for GAMLSS
    # If NULL then use NO as default
    # - - - - - - - - - - - - - - - - - - - - - # 
    if(is.null(dist_fam)) {

        # - - - - - - - - - - - - - - - - - - - - - # 
        # Reference gamlss fitting 
        # - - - - - - - - - - - - - - - - - - - - - # 
        message("Distribution of GAMLSS not chosen")
        message("Will determine based on Normal, Gamma, and Box Cox Cole and Green")

        ref1<-gamlss(eval(as.formula(paste0(outcome," ~ ",spl, time_elapsed,"^",ptr_m,")"))),
                    sigma.formula = as.formula(paste0(" ~ ",spls, time_elapsed, ")")),
                    data=train_post, family=NO,
                    #gamlss control
                    ...)
        ref2<-gamlss(eval(as.formula(paste0(outcome," ~ ",spl, time_elapsed,"^",ptr_m,")"))),
                    sigma.formula = as.formula(paste0(" ~ ",spls, time_elapsed, ")")),
                    data=train_post, family=GA,
                    #gamlss control
                    ...)
        ref3<-gamlss(eval(as.formula(paste0(outcome," ~ ",spl, time_elapsed,"^",ptr_m, ")"))),
                    sigma.formula = as.formula(paste0(" ~ ",spls, time_elapsed, ")")),
                    nu.formula = as.formula(paste0(" ~ ",spln, time_elapsed, ")")),
                    data=train_post, family=BCCGo,
                    #gamlss control
                    ...)
        ref4<-gamlss(eval(as.formula(paste0(outcome," ~ ",spl, time_elapsed,"^",ptr_m, ")"))),
                    sigma.formula = as.formula(paste0(" ~ ",spls, time_elapsed, ")")),
                    nu.formula = as.formula(paste0(" ~ ",spln, time_elapsed, ")")),
                    data=train_post, family=BCTo,
                    #gamlss control
                    ...)
        ref5<-gamlss(eval(as.formula(paste0(outcome," ~ ",spl, time_elapsed,"^",ptr_m, ")"))),
                    sigma.formula = as.formula(paste0(" ~ ",spls, time_elapsed, ")")),
                    nu.formula = as.formula(paste0(" ~ ",spln, time_elapsed, ")")),
                    data=train_post, family=BCPEo,
                    #gamlss control
                    ...)
        # here the BCCGo distribution with tau is intentionally left out bc the fit is good but not as when we jsut do the nu
        #ref5<-gamlss(eval(as.formula(paste0(outcome," ~ ", "pb(", time_elapsed, ")"))),
                    #sigma.formula = as.formula(paste0(" ~ ", "pb(", time_elapsed, ")")),
                    #nu.formula = as.formula(paste0(" ~ ", "pb(", time_elapsed, ")")),
                    #tau.formula = as.formula(paste0(" ~ ", "pb(", time_elapsed, ")")),
                    #data=train_post, family=BCCGo)

        ref <- list(ref1,ref2,ref3,ref4,ref5)[which.min(lapply(list(ref1,ref2,ref3,ref4,ref5), function(x){x$aic}))][[1]]
    } else if(length(dist_fam()$parameters) == 2) {
        ref<-gamlss(eval(as.formula(paste0(outcome," ~ ",spl, time_elapsed,"^",ptr_m, ")"))),
                    sigma.formula = as.formula(paste0(" ~ ",spls, time_elapsed, ")")),
                    data=data.frame(train_post), family=dist_fam,
                    #gamlss control
                    ...)
    } else if(length(dist_fam()$parameters) == 3) {
        ref<-gamlss(eval(as.formula(paste0(outcome," ~ ",spl, time_elapsed,"^",ptr_m, ")"))),
                    sigma.formula = as.formula(paste0(" ~ ",spls, time_elapsed, ")")),
                    nu.formula = ~1,
                    data=train_post, family=dist_fam,
                    #gamlss control
                    ...)
    } else if(length(dist_fam()$parameters) == 4) {
        ref<-gamlss(eval(as.formula(paste0(outcome," ~ ",spl, time_elapsed,"^",ptr_m, ")"))),
                    sigma.formula = as.formula(paste0(" ~ ",spls, time_elapsed, ")")),
                    nu.formula = ~1,
                    tau.formula = ~1,
                    data=train_post, family=dist_fam,
                    ...
                    )
    }
    gamlss_dist <- ref$family[2]


    # - - - - - - - - - - - - - - - - - - - - - # 
    # Full training post operative data fitting (i.e reference data) with GAMLSS 
    # - - - - - - - - - - - - - - - - - - - - - # 

    # - - - - - - - - - - - - - - - - - - - - - # 
    # Specify time_window if NULL for GAMLSS centiles prediction
    # - - - - - - - - - - - - - - - - - - - - - # 
    if(is.null(time_window)){
        mint <- min(min(train_post[, time_elapsed]), min(test_post[, time_elapsed]))
        maxt <- max(max(train_post[, time_elapsed]), max(test_post[, time_elapsed]))
        iqrfull <- centiles.pred(ref, type='centiles', xname = time_elapsed, xvalues=c(mint:maxt),
                                data = train_post,
                                cent=c(25,75), plot=FALSE)
    } else {
        iqrfull <- centiles.pred(ref, type="centiles", xname = time_elapsed, xvalues=c(time_window[1]:time_window[2]),
                                 data = train_post,
                                 cent=c(25,75), plot=FALSE)
    }
    iqrfull$iqr<-iqrfull$C75-iqrfull$C25

    # - - - - - - - - - - - - - - - - - - - - - # 
    # NEAREST NEIGHBOR MATCHING
    # - - - - - - - - - - - - - - - - - - - - - # 

    # Outerloop will iterate through a list of nearest_n numbers
    # Innerloop will iterate through all patients and store the result of the whole iteration into an array
    # nearest neighbor should be an even number for at least those that aren't edge cases

    
    # iterate through the nearest_n list calculate bias coverage for each iteration
    # for e.g. if we go form 10:100 by 10's we will have a list with 10 results 

    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # Match training and testing patients based on minimum difference in the predicted outcome at out_time 
    # Need this to get test data predictions
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    temp <- data.frame(train_id = sapply(1:length(test_o$pred), function(x) {
                                             ord_data[which.min(abs(test_o$pred[x] - ord_data$Fitted)), "id"]
                }), 
                       test_id = test_o$id)

    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # pat_level_func() is the loocv function 
    # Patient level function this is the workhorse 
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    pat_level_func <- function(nearest=nearest_n, loocv=FALSE){

        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        # Empty list that will be populated with performance measures and predictions
        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        nn_arr = list()

        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        # Loop based on the nearest_n specified as user input 
        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

        for(n in nearest){
            cnt = 0
            misses = 0

            dfList <- list()                #-- list to store training data's predicted C50 from gamlss model
            dfList_test <- list()           #-- list to store testing data's predicted C50 from gamlss model
            centilepred <- list()           #-- store all centile for later test set merging
            biasvec<-vector()               #-- store mean of bias
            rmsevec <- vector()             #-- store rmse vector
            coveragevec<-vector()           #-- store coverage vector
            coveragevec95a<-vector()        #-- store mean of the n coverage in vector
            #coveragevec95<-vector()
            iqrvec<-vector()
            ninefiveconf <- data.frame()
            precisionvec <- list()
            crazymatch <- list()
            zsc_list <- list()

            # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
            # iterate through all patients and match patients according to order. ord_data is the training data
            # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
            if(is.null(interval)){
                patlistint <- seq(1,length(ord_data$id))
            } else {
                patlistint <- seq(1,length(ord_data$id), by=interval)
            }
            for (i in patlistint) {
                cnt = cnt + 1

                # - - - - - - - - - - - - - - - - - - - - - - #
                # Matching cases using absolute value of difference between Fitted values
                # - - - - - - - - - - - - - - - - - - - - - - #

                matches <- ord_data %>% 
                    bind_cols(diff = abs(ord_data$Fitted[i] - ord_data$Fitted)) %>%
                    arrange(diff) %>%
                    dplyr::select(id) %>%
                    .[-1,] %>%
                    head(n = n) %>% unlist %>% as.vector

                # - - - - - - - - - - - - - - - - - - - - - - #
                # Matching by probability weighting by id based on n (1/n is prob weight)
                # - - - - - - - - - - - - - - - - - - - - - - #
                if(matchprobweight){
                    matchprob <- ord_data %>% 
                        bind_cols(diff = abs(ord_data$Fitted[i] - ord_data$Fitted)) %>%
                        arrange(diff) %>%
                        dplyr::select(id) %>%
                        .[-1,] %>%
                        head(n = n) %>% cbind(weight=seq(n,1)/sum(seq(n,1))) %>%
                        as.data.frame %>%
                        rename(patient_id = 1)

                    # create dataset with weights
                    matchmodel <- train_post[train_post$patient_id %in% matches, ] %>%
                        left_join(
                                  matchprob
                                  ) %>%
                    dplyr::select_("patient_id", time_elapsed, outcome, "weight") 

                    set.seed(seed)
                    matchindex <- sample(x = seq_len(nrow(matchmodel)), size=nrow(matchmodel), prob=matchmodel$weight, replace=TRUE)

                    # create final weighted matchdataset
                    matchmodel <- matchmodel[matchindex,]
                } else {
                    matchmodel <- train_post[train_post$patient_id %in% matches, ]
                }

                # - - - - - - - - - - - - - - - - - - - - - - #
                # Fit GAMLSS model to nearest n matches
                # - - - - - - - - - - - - - - - - - - - - - - #

                plm <- function() {

                    out <- tryCatch(
                                    {
                                        message("TRY PART")
                                        update(ref, data=matchmodel)

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
                plmr <- plm()

                # if something wrong with iteration... then just don't do prediction it eats up all the memory anyways
                if(typeof(plmr) != 'list') {

                    misses = misses + 1 # increment number of misses
                    message('Something wrong with plm model. Prediction not included')

                } else {

                    message(paste0("Getting predictions: "))

                    # time window again
                    if(is.null(time_window)){
                        iqr<-centiles.pred(plmr, type="centiles",  xname = time_elapsed, xvalues=c(mint:maxt),
                                           cent=c(2.5, 25, 50, 75, 97.5),
                                           data=matchmodel,
                                           plot=FALSE)
                    } else {
                        iqr<-centiles.pred(plmr, type="centiles",  xname = time_elapsed, xvalues=c(time_window[1]:time_window[2]),
                                           cent=c(2.5, 25,50,75, 97.5),
                                           data=matchmodel,
                                           plot=FALSE)
                    }


                    # - - - - - - - - - - - - - - - - - - - - - - #
                    # If not LOOCV, meaning if we're doing just predictions obtain test data bias, coverage and precision
                    # - - - - - - - - - - - - - - - - - - - - - - #
                    if(loocv!=TRUE){

                        #
                        # - - - Zscore on test set
                        #

                        #-- first need to make the test set such that 
                        zscf <- function(){
                            out <- tryCatch(
                                            {
                                                message("ZSCORE PREDICTION TRY PART")
                                                message(paste0("PREDICTING FOR TRAIN = ",i))
                                                testpred=data.frame()
                                                # - - - - - - - - - - - - - - - - - - - - - - #
                                                # Iterate through each testing obs(x) closest to the current train obs(i) 
                                                # get predicted values using centles.pred()
                                                # - - - - - - - - - - - - - - - - - - - - - - #
                                                for(x in temp[temp$train_id %in% ord_data$id[c(i)], "test_id"]){
                                                message(paste0("PREDICTING FOR TEST = ",x))
                                                         testpred <- testpred %>% 
                                                             bind_rows(
                                                                       data.frame(
                                                                                  zsc = centiles.pred(plmr, type="z-scores",
                                                                                                      xname="time",
                                                                                                      data=matchmodel,
                                                                                                      xval=test_proc$test_post %>%
                                                                                                          filter(patient_id %in% x) %>%
                                                                                                          dplyr::select(time) %>%
                                                                                                          unlist %>% as.vector,
                                                                                                      yval=test_proc$test_post %>%
                                                                                                          filter(patient_id %in% x) %>%
                                                                                                          dplyr::select_(outcome) %>%
                                                                                                          unlist %>% as.vector
                                                                                                      ),
                                                                                  test_id = rep(x, length(test_proc$test_post %>%
                                                                                                          filter(patient_id %in% x) %>%
                                                                                                          dplyr::select(time) %>%
                                                                                                          unlist %>% as.vector
                                                                                                      )),
                                                                                  time = test_proc$test_post %>%
                                                                                      filter(patient_id %in% x) %>%
                                                                                      dplyr::select(time) %>%
                                                                                      unlist %>% as.vector
                                                                                  ))
                                                }

                                                return(testpred)
                                            },
                                            error = function(cond)
                                            {
                                                message(paste("ERROR in ZSCORE PREDICTION"))
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
                        zsc <- zscf()
                        if(!is.data.frame(zsc)) {
                            print(zsc)
                            print(str(zsc))
                            message(zsc)
                            message(str(zsc))

                            message('Something wrong with Z-SCORE prediction. Prediction not included')
                            zsc_list[[cnt]] <- data.frame(zsc = NA,
                                                          test_id = NA,
                                                          time = NA
                                                          )
                        } else {
                            zsc_list[[cnt]] <- zsc
                        }

                        # - - - - --  - - - - - - -#
                        # Calculate and store Test-set bia, coverage, and precision
                        # - - - - --  - - - - - - -#

                        # -- IQR for the test data set

                        iqr$iqr<-iqr$C75-iqr$C25
                        iqrvec[cnt]<-mean(iqr$iqr)

                        targetid<-temp[temp$train_id %in% ord_data$id[c(i)], "test_id"] # select the test id's that corresopnd to current train patient
                        targetrec<-test_post[which(test_post$patient_id %in% targetid), ] # ge tthe trainging set post op data

                        bias<-merge(iqr,targetrec, by=time_elapsed)
                        bias$diff<-bias$C50-bias[,outcome]

                        #store mean of bias and rmse in biasvec, rmsevec
                        biasvec[cnt] <- mean(bias$diff)
                        rmsevec[cnt] <- sqrt(sum(na.omit(bias$diff)^2)/length(na.omit(bias$diff)))

                        # coverage prob based on IQR 50%
                        bias$cov1<-bias[,outcome]-bias$C25
                        bias$cov2<-bias$C75-bias[,outcome]
                        bias$cov3<-ifelse(bias$cov1>0,1,0)
                        bias$cov4<-ifelse(bias$cov2>0,1,0)
                        bias$cov5<-bias$cov3+bias$cov4
                        bias$coverage<-ifelse(bias$cov5>1,1,0)

                        # store mean of the n coverage in vector
                        coveragevec[cnt]<-mean(bias$coverage)


                        if(any(bias$C50) > thresh_val){
                            crazymatch[[cnt]] <- matchmodel
                        } else {
                            crazymatch[[cnt]] <- NA
                        }

                        #-- precision
                        precisionvec[[cnt]] <-list(time= iqr[,time_elapsed], prec=iqr$iqr) 
                        #-- store Testing C50
                        dfList_test[[cnt]] <- test_post[which(test_post$patient_id %in% targetid), c("patient_id",time_elapsed,outcome)] %>%
                        #train_post[train_post$patient_id == ord_data$id[c(i)],c("patient_id",time_elapsed,outcome)] %>% 
                            left_join(
                                      data.frame(time=iqr[,time_elapsed],c50 = iqr$C50) 
                                      )

                        #-- store Training C50
                        dfList[[cnt]] <- train_post[which(train_post$patient_id %in% ord_data$id[c(i)]), c("patient_id",time_elapsed,outcome)] %>%
                            left_join(
                                      data.frame(time=iqr[,time_elapsed],c50 = iqr$C50) 
                                      )
                        #-- store all centile for later test set merging
                        centilepred[[cnt]] <- cbind(ord_data$id[c(i)], iqr$time, iqr$C50)

                    } else { # loocv = FALSE

                        # - - - -
                        # z-scores vector
                        # - - - - 
                        zscf <- function(){
                            out <- tryCatch(
                                            {
                                                message("ZSCORE PREDICTION LOOCV TRY PART")
                                                trainzsc <- data.frame(
                                                                       zsc = centiles.pred(plmr, type="z-scores",
                                                                                           xname=time_elapsed,
                                                                                           data=matchmodel,
                                                                                           xval=train_post[train_post$patient_id %in% ord_data$id[c(i)],time_elapsed][[1]],
                                                                                           yval=train_post[train_post$patient_id %in% ord_data$id[c(i)],outcome][[1]]),
                                                                       train_id = rep(ord_data$id[c(i)], length(train_post[train_post$patient_id %in% ord_data$id[c(i)],time_elapsed][[1]]))
                                                                       ,
                                                                       time = train_post[train_post$patient_id %in% ord_data$id[c(i)],time_elapsed][[1]]
                                                                       )
                                                return(trainzsc)

                                            },
                                            error = function(cond)
                                            {
                                                message(paste("ERROR in ZSCORE PREDICTION"))
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
                        zsc <- zscf()
                        if(!is.data.frame(zsc)) {

                            message('Something wrong with Z-SCORE prediction. Prediction not included')
                            #zsc_list[[cnt]] <- list(train = NA)
                            zsc_list[[cnt]] <- data.frame(zsc = NA,
                                                          test_id = NA,
                                                          time = NA
                                                          )

                        } else {
                            zsc_list[[cnt]] <-  zsc
                            #zsc_list[[cnt]] <- list(train = zsc)
                            # below is when we want to also include the time point for the individual which is probably iportant... 
                            #zsc_list[[cnt]] <- list(zsc = zsc, time = train_post[train_post$patient_id %in% ord_data$id[c(i)],time_elapsed][[1]])
                        }

                        message(paste0("Getting MLE  predictions: "))

                        # - - - - - - - -- - - - - - - - - - - - - - - - - - - - - - - - #
                        # code for storing either LOOCV result or extracting prediction #
                        # - - - - - - - -- - - - - - - - - - - - - - - - - - - - - - - - #

                        iqr$iqr<-iqr$C75-iqr$C25

                        iqrvec[cnt]<-mean(iqr$iqr)
                        targetid<-ord_data$id[c(i)]
                        targetrec<-train_post[which(train_post$patient_id %in% targetid), ]

                        bias<-merge(iqr,targetrec, by=time_elapsed)
                        bias$diff<-bias$C50-bias[,outcome]

                        #store mean of bias and rmse in biasvec, rmsevec
                        biasvec[cnt] <- mean(bias$diff)
                        rmsevec[cnt] <- sqrt(sum(na.omit(bias$diff)^2)/length(na.omit(bias$diff)))

                        # coverage prob based on IQR 50%
                        bias$cov1<-bias[,outcome]-bias$C25
                        bias$cov2<-bias$C75-bias[,outcome]
                        bias$cov3<-ifelse(bias$cov1>0,1,0)
                        bias$cov4<-ifelse(bias$cov2>0,1,0)
                        bias$cov5<-bias$cov3+bias$cov4
                        bias$coverage<-ifelse(bias$cov5>1,1,0)

                        # store mean of the n coverage in vector
                        coveragevec[cnt]<-mean(bias$coverage)

                        # coverage prob based on IQR 95%
                        bias$cov1<-bias[,outcome]-bias$C2.5
                        bias$cov2<-bias$C97.5 -bias[,outcome]
                        bias$cov3<-ifelse(bias$cov1>0,1,0)
                        bias$cov4<-ifelse(bias$cov2>0,1,0)
                        bias$cov5<-bias$cov3+bias$cov4
                        bias$coverage<-ifelse(bias$cov5>1,1,0)

                        # store mean of the n coverage in vector
                        coveragevec95a[cnt]<-mean(bias$coverage)

                        #-- precision potentially remove later because this is 7.5mb per n so 7.5*14 ~ 100MB
                        precisionvec[[cnt]] <-list(time= iqr[,time_elapsed], prec=iqr$iqr) 


                    }

                }
                message(paste0("Current count is: ",cnt))
                message(paste0("and Current n: ",n))
            }

            # - - - - - - - - - - - - - - - - - - - - - - - - -#  
            # Code to create dataframe of the time and C75-C25 #
            # - - - - - - - - - - - - - - - - - - - - - - - - -#  

            message(paste0("Merging measures: "))
            if(loocv!=TRUE){
                message(paste0("Constructing Prediction Data"))
 
                library(data.table)

                nn_arr  <- list(
                                pred_train = dfList, 
                                pred_test = dfList_test, 
                                biasvec = Filter(Negate(is.na),Filter(Negate(is.na), biasvec)),
                                coveragevec = Filter(Negate(is.na),Filter(Negate(is.na), coveragevec)),
                                centilerange = centilepred, 
                                precisionvec=precisionvec, 
                                zsc_list = zsc_list,
                                rmse = rmsevec,
                                crazymatch = crazymatch)


            } else {

                # Get rid of NA or NULL values that are created
                All_list <- list(
                                 #                          Filter(Negate(is.null),Filter(Negate(is.null), fin)),
                                 Filter(Negate(is.na),Filter(Negate(is.na), biasvec)),
                                 Filter(Negate(is.na),Filter(Negate(is.na), coveragevec)),
                                 Filter(Negate(is.na),Filter(Negate(is.na), coveragevec95a)),
                                 #Filter(Negate(is.na),Filter(Negate(is.na), coveragevec95)),
                                 Filter(Negate(is.na),Filter(Negate(is.na), iqrvec)),
                                 rmsevec,
                                 zsc_list,
                                 precisionvec = precisionvec,
                                 misses 
                                 )

                # name the list objects
                message(paste0("Assigning names"))
                #names(All_list) <- c("bias","iqrcoverage","coverage95c","coverage95m","iqr","rmse", "dropped_cases")
                names(All_list) <- c("bias","iqrcoverage","coverage95c","iqr","rmse","zscore","precisionvec", "dropped_cases")

                message(paste0("Putting in the list in array slot: ",n))
                # store the result of the n nearest_n result
                nn_arr[[which(nearest == n)]] = All_list

                # rename the list items to indicate nearestn
                names(nn_arr)[which(nearest == n)] <- paste0('nearest_',n)

                # Plotting
                if (plot==TRUE){

                    # create dataframe from compiled array for plotting

                    df <- data.table(mrmse = sapply(nn_arr, function(x) {
                                                        mean(x[['rmse']], na.rm=TRUE)
                              }),
                                     mcov = sapply(nn_arr, function(x) {
                                                       mean(x[['iqrcoverage']], na.rm=TRUE)
                              }),
                                     mcov95c = sapply(nn_arr, function(x) {
                                                          mean(x[['coverage95c']], na.rm=TRUE)
                              }),
                                     mzscore = sapply(nn_arr, function(x) {
                                                          mean(x[['zscore']], na.rm=TRUE)
                              }),
                                     mmis = sapply(nn_arr, function(x) {
                                                       mean(x[['dropped_cases']], na.rm=TRUE)
                              })
                                     )

                    # add timepoints 
                    df[,('nearest_n') := nearest_n]

                    #plot(fin[,time_elapsed], fin$final, type="l",col="red", ylim=range(0.5:1.5), ylab="Normalized IQR", xlab="Days following Surgery", lwd=3)

                    #Add the individual data
                    #for(i in 1:nearest_n) {
                    #lines(fin[,time_elapsed], fin[,paste0('d',i)], col="green")
                }

                # select of the n result, which has the smallest mean 'rmse' or selection criteria chosen by user

            }

        }

        # after loocv finishes for all nearest_n, print which n is the lowest


        # return either LOOCV array or final prediction array
        return(nn_arr)
    }

    # run the LOOCV function 
    pat_level_funcm <-pat_level_func

    # if we're doing this on training data
    loocv_test_result <- pat_level_funcm(nearest=nearest_n, loocv=TRUE)

    # - - - - - - - - - - - - - - - - - - - - - - #
    # Calculation of Weighted Z-score, Coverage, and Bias to select Optimal N #
    # - - - - - - - - - - - - - - - - - - - - - - #

    #opt_n_index <- which.min(as.vector(sapply(loocv_test_result, function(x) { mean(x$rmse, na.rm=TRUE)})))
    if(length(nearest_n) != 1){

        askuser <- function(){
            n <- readline(prompt="Choose Nearest N: ")
            if(n > max(nearest_n)-1){
                message(paste0("Outside of the possible number of matches = ", max(nearest_n) - 1))
                return(askuser())
            }  else {
                message(paste0("Using ", n, " number of mathces as desired."))
            }
            return(as.integer(n))
        }

        if(userchoose){
            usernum <- askuser()
            opt_n_index <- which(nearest_n == usernum)
        } else {
            opt_n_index <- loocvperf(loocv_test_result) %>% 
                dplyr::select(totscore) %>% unlist(.) %>% which.min(.) %>% as.vector(.)
        }

        print(paste0(mean(loocv_test_result[[opt_n_index]]$rmse, na.rm=TRUE), " from ", names(loocv_test_result))[[opt_n_index]])
        print(paste0("Number of misses is: ",loocv_test_result[[opt_n_index]]$dropped_cases,' cases'))
        print(paste0("Distribution chosen for matched GAMLSS: ", gamlss_dist))
        print(paste0("Optimal Number of Matches is: ", nearest_n[[opt_n_index]]))
        predict_test_result <- pat_level_funcm(nearest=nearest_n[opt_n_index], loocv=FALSE)
        # extract prediction results from the optimum nearest n 
        return(list(pred_res = predict_test_result,
                    loocv_res =  loocv_test_result,
                    loocv_score = loocvperf(loocv_test_result),
                    nearest_n=nearest_n[opt_n_index]))
    } else {
        predict_test_result <- pat_level_funcm(nearest=nearest_n, loocv=FALSE)
        return(list(pred_res = predict_test_result,
                    loocv_res =  loocv_test_result,
                    #loocv_score = loocvperf(loocv_test_result),
                    nearest_n=nearest_n))
    }

}




# - - - - - - - - - - - - - - -#
# Utility Function: Convert Performance metrics into dataframe RMSE and COVERAGE ----
# - - - - - - - - - - - - - - -#

#-- Function to aggregate all nearest_n results for each people

listtodf <- function(listfile){
    library(data.table)
    temp <- lapply(listfile, function(x) {
                       c(mean(x$bias, na.rm=TRUE),
                         mean(x$iqrcoverage, na.rm=TRUE),
                         mean(x$coverage95c, na.rm=TRUE),
                         mean(x$zscore, na.rm=TRUE),
                         mean(x$iqr, na.rm=TRUE),
                         mean(x$rmse, na.rm=TRUE),
                         x$dropped_cases)
                      })
    temp <- as.data.frame(temp)
    colnames(temp) <- paste0(seq(20,150,10))
    rownames(temp) <- paste0(c('bias','iqrcoverage','coverage95c','zscore','iqrdif','rmse','dropped_cases'))
    temp$measure  <-  c('bias','iqrcoverage','coverage95c','zscore','iqrdif',"rmse","dropped_cases")
    library(reshape2)
    temp <- melt(temp, id.vars=c("measure"))
    temp <- temp %>%
        mutate(variable = as.numeric(as.character(variable))) %>%
        rename(nearest_n = variable)

    return(temp)

}

#-- Function to add baseline variable

baselinemk <- function(dftotf = full,
                       pat_id = "patient_id",
                       time_var = "time"
                       ){
    library("dplyr")
    dftotf <- dftotf %>% 
        left_join(
                  dftotf %>% 
                      select_(pat_id, time_var) %>%
                      arrange_(pat_id, time_var) %>%
                      distinct_(pat_id, .keep_all=TRUE) %>%
                      mutate(baseline = 1)
                  ) %>% 
    mutate(baseline = if_else(is.na(baseline), 0,baseline)) 
}


# - - - - - - - - - - - - #
# Plotting Function: (calibration, rmse and etc) ----
# - - - - - - - - - - - - #

plot_cal <- function(plotobj,
                     test_proc=test_proc,
                     outcome = "tug",
                     filt=FALSE,
                     pred_sum="mean",
                     obs_dist="gaussian",
                     #plot_by=seq(10,150,5),
                     loocv=TRUE,
                     filter_exp = NULL,
                     plot_zscore=FALSE,
                     ...) {
    library(ggplot2)
    library(data.table)
    library(broom)
    library(tidyr)
    # plotting function for 4 plots
    # 1. Training set Calibration Plot
    # 2. Test set Calibration Plot
    # 3. Training set RMSE/Coverage Plot
    # 4. Test set RMSE/Coverage Plot

    # Main input: object from LOOCV function which spits out train data output
    # plotobj$pred_res$pred contains the training set predictions in dataframe
    # plotobj$pred_res

    # - - - - - - - - - - - - - - - - - - - - - - #
    # Calibration Plot function for test and train
    # - - - - - - - - - - - - - - - - - - - - - - #
    plot_func <- function(plotobj = plotobj, train = TRUE, filt=filt){
        library(data.table)
        val <- "c50"
        if(train == TRUE){

            if(filt==TRUE){

                temp <- rbindlist(plotobj$pred_res$pred_train) %>% filter_(filter_exp) # first one is c50
            } else {
                temp <- rbindlist(plotobj$pred_res$pred_train) # first one is c50
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
                      summarise_(avg_val =  paste0("avg_val = ", pred_sum, "(",val,")"))
                  ) 
            if(any(is.na(filtdf$dec))){
                print("NA's present in group")
                filtdf <- filtdf %>%
                    filter(!is.na(dec))
            }

            pred_tug <- temp  %>%
                bind_cols(
                          dec = temp %>% 
                              dplyr::select_(val) %>%
                              as.data.frame() %>%
                              ntile(., 10)# create deciles
                          ) %>%
            group_by(dec) %>%
            summarise_(avg_val = paste0("avg_val = ", pred_sum, "(",val,")"))


        #summarise(avg_val = mean(get(val))) 

        if(obs_dist == "gaussian" | obs_dist == "poisson") {
            print(paste0("Distribution within Each Decile of Predicted", toupper(outcome)))
            if(obs_dist == "gaussian") {
                dffitpois <- filtdf %>%
                    group_by(dec) %>%
                    do(fitpois = glm(get(outcome) ~ 1, data= .))

                dfpoiscoef <- tidy(dffitpois, fitpois)

                # create poisson estimates and standard errors for each decile for plotting
                print(paste0("Plot DF creation"))
                plotdf <- pred_tug %>%
                    left_join(dfpoiscoef %>% dplyr::select(dec, estimate, std.error)) %>%
                    mutate(ul = estimate+1.96*std.error,
                           ll = estimate-1.96*std.error) 
            } else {
                dffitpois <- filtdf %>%
                    group_by(dec) %>%
                    do(fitpois = glm(get(outcome) ~ 1, family=poisson(link="log"), data= .))

                dfpoiscoef <- tidy(dffitpois, fitpois)

                # create poisson estimates and standard errors for each decile for plotting
                print(paste0("Plot DF creation"))
                plotdf <- pred_tug %>%
                    left_join(dfpoiscoef %>% dplyr::select(dec, estimate, std.error)) %>%
                    mutate(
                           ul = exp(estimate+1.96*std.error),
                           ll = exp(estimate-1.96*std.error)) %>%
                    mutate(estimate = exp(estimate))
            }
            # set minimum and maximum based on the range of predicted and observed TUG values
            minc <- floor(min(plotdf$ll, plotdf$avg_val, na.rm=TRUE)) 
            maxc <- ceiling(max(plotdf$ul, plotdf$avg_val, na.rm=TRUE)) 

        } else { # for observed value as median and 95%CI for median
            library(DescTools)
            dffitmed <- filtdf %>%
                group_by(dec) %>%
                do(fitmed = MedianCI(.[,outcome][[1]], conf.level= 0.95,
                                     method = "exact", R = 10000))
                #do(fitpois = glm(tug ~ 1, data= .))


            dfmedcoef <- tidy(dffitmed, fitmed)

            plotdf <- pred_tug %>%
                left_join(spread(dfmedcoef, names, -dec)) %>%
                rename(ul = upr.ci,
                       ll = lwr.ci)
            # set minimum and maximum based on the range of predicted and observed TUG values
            minc <- floor(min(plotdf$ll, plotdf$avg_val, na.rm=TRUE)) 
            maxc <- ceiling(max(plotdf$ul, plotdf$avg_val, na.rm=TRUE)) 
            #minc <- floor(min(plotdf[,val], plotdf[,outcome], na.rm=TRUE)) 
            #maxc <- ceiling(max(boxplot.stats(plotdf[,val])$stats[5], boxplot.stats(plotdf[,outcome])$stats[5], na.rm=TRUE))*1.05
        }



            # plot observed vs. predicted TUG on decile of predicted TUG
            if(obs_dist == "gaussian" | obs_dist == "poisson"){
                if(obs_dist == "gaussian") {
                    cptrain <-  ggplot(plotdf, aes(x = avg_val, y = estimate, ymin = estimate-1.96*std.error, ymax=estimate+1.96*std.error)) + geom_pointrange() + 
                        #xlim(minc, maxc) + ylim(minc,maxc) + 
                        geom_abline(slope=1, intercept=0) + 
                        xlab(paste0("Predicted ",toupper(outcome))) + 
                        ylab(paste0("Observed ",toupper(outcome))) 
                } else {
                    cptrain <-  ggplot(plotdf, aes(x = avg_val, y = estimate, ymin = ll, ymax=ul)) + geom_pointrange() + 
                        #xlim(minc, maxc) + ylim(minc,maxc) + 
                        geom_abline(slope=1, intercept=0) + 
                        xlab(paste0("Predicted ",toupper(outcome))) + 
                        ylab(paste0("Observed ",toupper(outcome))) 
                }

            
            } else {
                # median and IQR
                cptrain <-  ggplot(plotdf, aes(x = avg_val,
                                               ymin = ll, ymax=ul,
                                               y = median,
                                               )) + geom_pointrange() + 
                    #xlim(minc, maxc) + ylim(minc,maxc) + 
                    geom_abline(slope=1, intercept=0) + 
                    xlab(paste0("Predicted ",toupper(outcome))) + 
                    ylab(paste0("Observed ",toupper(outcome))) 
            
                #cptrain <-  ggplot(plotdf, aes_string(x = "avg_val",
                                                      #y = outcome,
                                                      #group = "avg_val"
                                                      #)) + geom_boxplot(outlier.colour=NA) + 
                    ##xlim(minc, maxc) + ylim(minc,maxc) + 
                    #geom_abline(slope=1, intercept=0) + 
                    #xlab("Predicted TUG") + 
                    #ylab("Observed TUG")
            }
            cptrainzsc <-  ggplot(plotdf, aes_string(x = "avg_val",
                                           y = "zsc",
                                           group = "avg_val"
                                           )) + geom_boxplot(outlier.colour=NA) + 
                #xlim(minc, maxc) + ylim(minc,maxc) + 
                geom_hline(yintercept=0) + 
                xlab(paste0("Predicted ",toupper(outcome))) + 
                ylab("Z-score")

            mincz <- floor(min(plotdf$zsc, na.rm=TRUE)) 
            maxcz <- ceiling(max(plotdf$zsc, na.rm=TRUE)) 

            # correlation
            traincor <- cor(temp %>% 
                            dplyr::select_(outcome, "c50") %>%
                            as.matrix, method="spearman") %>%
                        .[1,2] %>%
                        round(3) %>%
                        paste0("r^2 ==", .)

            # other performance measures
            zscres <- loocvperf(plotobj$loocv_res)
            test_perf <- zscres[which.min(zscres$totscore),c("zscore","coverage","precision")]

            return(list(cptrain, minc, maxc, traincor, cptrainzsc, mincz, maxcz,tp=test_perf ))
            #return(list(cptrain, minc, maxc))

        } else { # for test set we need to merge from the full data

            #-- add the z-score dataset 
            #tempzsc <- rbindlist(lapply(plotobj$pred_res$zsc_list, list))[[1]][!is.na(rbindlist(lapply(plotobj$pred_res$zsc_list, list))) & !is.infinite(unlist(rbindlist(lapply(plotobj$pred_res$zsc_list, list))))]
            #tempzsc <- rbindlist(lapply(plotobj$pred_res$zsc_list, function(x) {list( zsc = x)}), idcol = "patient_id")

            #-- get id of train_df that has closests y90 value to test df
            print("creating temp and test matching data")

            temp <-  test_proc$test_post %>% 
                dplyr::select_("patient_id", "time", outcome) %>%
                left_join(
                          test_proc$test_o %>%
                              bind_cols(
                                        train_id = test_proc$train_o$id[sapply(1:length(test_proc$test_o$pred), function(x) {
                                                                                   which.min(abs(test_proc$test_o$pred[x] - test_proc$train_o$Fitted))                                         })]
                                        ) %>% 
                          dplyr::select(id, train_id) %>%
                          rename(patient_id = id)
                      ) %>%
                rename(test_id = patient_id)  %>%
                #-- join centiles.pred from the unique train id's that were selected based on minimum difference between y90 of train and test
                left_join(
                          data.frame(train_id = unique(test_proc$train_o$id[sapply(1:length(test_proc$test_o$pred), function(x) {
                                                                                       which.min(abs(test_proc$test_o$pred[x] - test_proc$train_o$Fitted))
})]) ) %>% 
                          full_join(
                                    rbindlist(lapply(plotobj$pred_res$centilerange, data.frame)) %>%
                                        rename(train_id = 1,
                                               time = 2,
                                               C50 = 3))
                          ,
                          by = c("time","train_id") 
                          ) 
                #-- bind with decile
                print("binding with decile")
                print("filt df made in test")
                filtdf <- temp %>%
                    bind_cols(
                              dec = temp %>%
                                  dplyr::select(C50) %>%
                                  as.data.frame() %>%
                                  ntile(., 10)
                              ) %>%
                left_join(
                          temp %>%
                              bind_cols(
                                        dec = temp %>%
                                            dplyr::select(C50) %>%
                                            as.data.frame() %>%
                                            ntile(., 10)
                                        ) %>%
                          group_by(dec) %>%
                          summarise_(avg_val = paste0("avg_val = ", pred_sum, "(C50)"))
                      ) 
                # merge zscore later on
                #%>%
                #left_join(
                          #tempzsc
                          #) %>%
                #filter(!is.na(zsc))
                if(any(is.na(filtdf$dec))){
                    warning("NA's present in group")
                    filtdf <- filtdf %>%
                        dplyr::filter(!is.na(dec))
                }

                print(paste0("Predicting TUG values"))

                pred_tug <-  temp %>%
                    bind_cols(
                              dec = temp %>%
                                  dplyr::select(C50) %>%
                                  as.data.frame() %>%
                                  ntile(., 10)
                              ) %>%
                group_by(dec) %>%
                summarise_(avg_val = paste0("avg_val = ", pred_sum, "(C50)"))


            # use normal, poisson, or median/95% IQR
            if(obs_dist == "gaussian" | obs_dist == "poisson") {
                print(paste0("Distribution within Each Decile of Predicted TUG"))
                library(broom)
                if(obs_dist == "gaussian"){
                    dffitpois <- filtdf %>%
                        group_by(dec) %>%
                        do(fitpois = glm(get(outcome)~ 1, data= .))
                    dfpoiscoef <- tidy(dffitpois, fitpois)

                    # create poisson estimates and standard errors for each decile for plotting
                    print(paste0("Plot DF creation"))
                    plotdf <- pred_tug %>%
                        left_join(dfpoiscoef %>% dplyr::select(dec, estimate, std.error)) %>%
                        mutate(ul = estimate+1.96*std.error,
                               ll = estimate-1.96*std.error) 
                } else {

                    dffitpois <- filtdf %>%
                        group_by(dec) %>%
                        do(fitpois = glm(get(outcome)~ 1,family=poisson(link="log"), data= .))

                    dfpoiscoef <- tidy(dffitpois, fitpois)

                    # create poisson estimates and standard errors for each decile for plotting
                    print(paste0("Plot DF creation"))
                    plotdf <- pred_tug %>%
                        left_join(dfpoiscoef %>% dplyr::select(dec, estimate, std.error)) %>%
                        mutate(ul = exp(estimate+1.96*std.error),
                               ll = exp(estimate-1.96*std.error)) %>%
                        mutate(estimate = exp(estimate))
                }


                # set minimum and maximum based on the range of predicted and observed TUG values
                minc <- floor(min(plotdf$ll, plotdf$avg_val, na.rm=TRUE)) 
                maxc <- ceiling(max(plotdf$ul, plotdf$avg_val, na.rm=TRUE)) 

            } else {
                print("Median calculation")

                library(DescTools)
                print(
                      filtdf %>% 
                          head
                )

                dffitmed <- filtdf %>%
                    group_by(dec) %>%
                    do(fitmed = MedianCI(.[,outcome][[1]], conf.level= 0.95,
                                         method = "exact", R = 10000, na.rm = TRUE))
                    #do(fitpois = glm(tug ~ 1, data= .))

                dfmedcoef <- tidy(dffitmed, fitmed)

                plotdf <- pred_tug %>%
                    left_join(spread(dfmedcoef, names, -dec)) %>%
                    rename(ul = upr.ci,
                           ll = lwr.ci)
                # set minimum and maximum based on the range of predicted and observed TUG values
                minc <- floor(min(plotdf$ll, plotdf$avg_val, na.rm=TRUE)) 
                maxc <- ceiling(max(plotdf$ul, plotdf$avg_val, na.rm=TRUE)) 
                #plotdf <- filtdf

                ## set minimum and maximum based on the range of predicted and observed TUG values
                #minc <- floor(min(plotdf$C50, unlist(plotdf[,outcome]), na.rm=TRUE)) 
                #maxc <- ceiling(max(boxplot.stats(plotdf$C50)$stats[5], boxplot.stats(unlist(plotdf[,outcome]))$stats[5], na.rm=TRUE))*1.05
            }


            # plot observed vs. predicted TUG on decile of predicted TUG
            if(obs_dist == "gaussian" | obs_dist == "poisson"){
                if(obs_dist == "gaussian"){
                    cptest <-  ggplot(plotdf, aes(x = avg_val, y = estimate, ymin = estimate-1.96*std.error, ymax=estimate+1.96*std.error)) + geom_pointrange() + 
                        #xlim(minc, maxc) + ylim(minc,maxc) + 
                        geom_abline(slope=1, intercept=0) + 
                        xlab(paste0("Predicted ",toupper(outcome))) + 
                        ylab(paste0("Observed ",toupper(outcome))) 
                
                } else {
                    cptest <-  ggplot(plotdf, aes(x = avg_val, y = estimate, ymin = ll, ymax=ul)) + geom_pointrange() + 
                        #xlim(minc, maxc) + ylim(minc,maxc) + 
                        geom_abline(slope=1, intercept=0) + 
                        xlab(paste0("Predicted ",toupper(outcome))) + 
                        ylab(paste0("Observed ",toupper(outcome))) 

                }

            } else {

                cptest <-  ggplot(plotdf, aes(x = avg_val,
                                              ymin = ll, ymax=ul,
                                              y = median
                                              #y = estimate
                                              )) + 
                    geom_pointrange() + 
                    #xlim(minc, maxc) + ylim(minc,maxc) + 
                    geom_abline(slope=1, intercept=0) + 
                    xlab(paste0("Predicted ",toupper(outcome))) + 
                    ylab(paste0("Observed ",toupper(outcome))) 

                #cptest <-  ggplot(plotdf, aes_string(x = "avg_val",
                                              #y = outcome,
                                              #group = "avg_val"
                                              #)) + 
                    #geom_boxplot(outlier.colour=NA) + 
                    ##xlim(minc, maxc) + ylim(minc,maxc) + 
                    #geom_abline(slope=1, intercept=0) + 
                    #xlab("Predicted TUG") + 
                    #ylab("Observed TUG")

            }
            cptestzsc <-  ggplot(plotdf, aes_string(x = "avg_val",
                                           y = "zsc",
                                           group = "avg_val"
                                           )) + geom_boxplot(outlier.colour=NA) + 
                #xlim(minc, maxc) + ylim(minc,maxc) + 
                geom_hline(yintercept=0) + 
                xlab(paste0("Predicted ",toupper(outcome))) + 
                ylab("Z-score")

            mincz <- floor(min(plotdf$zsc, na.rm=TRUE)) 
            maxcz <- ceiling(max(plotdf$zsc, na.rm=TRUE)) 

            # correlation
            testcor <- cor(temp %>% 
                            dplyr::select_(outcome, "C50") %>%
                            as.matrix, method="spearman", use="complete.obs") %>%
                        .[1,2] %>%
                        round(3) %>%
                        paste0("r^2 ==", .)

            # other performance measures
            test_perf <- extvalid(plotobj, test_proc)

            return(list(cptest, minc, maxc, testcor, cptestzsc, mincz, maxcz, tp=test_perf))
            #return(list(cptest, minc, maxc))

        }
    }


    # - - - - - - - - - - - - - - - - - - - - - - #
    # Instantiate all plot objects for viewing
    # - - - - - - - - - - - - - - - - - - - - - - #

    library(cowplot)
    #-- calibration plots only
    if(loocv){

        # - - - - - - - - - - - - - - - - - - - - - - #
        # RMSE/Coverage Plot function for test and train
        # - - - - - - - - - - - - - - - - - - - - - - #

        listtodf <- function(listfile){
            temp <- lapply(listfile, function(x) {
                               c(mean(x$bias, na.rm=TRUE),
                                 mean(x$iqrcoverage, na.rm=TRUE),
                                 mean(x$coverage95c, na.rm=TRUE),
                                 mean(rbindlist(x$zscore)$zsc[!is.na(rbindlist(x$zscore)$zsc) & !is.infinite(rbindlist(x$zscore)$zsc)]),
                                 mean(x$iqr, na.rm=TRUE),
                                 mean(x$rmse, na.rm=TRUE),
                                 x$dropped_cases)
                            })
            temp <- as.data.frame(temp)

            colnames(temp) <- regmatches(names(plotobj$loocv_res), regexpr("\\d+",names(plotobj$loocv_res)))
            #colnames(temp) <- paste0(plot_by)
            rownames(temp) <- paste0(c('bias','iqrcoverage','coverage95c','zscore','iqrdif','rmse','dropped_cases'))
            temp$measure  <-  c('bias','iqrcoverage','coverage95c','zscore','iqrdif',"rmse","dropped_cases")
            library(reshape2)
            temp <- melt(temp, id.vars=c("measure"))
            temp <- temp %>%
                mutate(variable = as.numeric(as.character(variable))) %>%
                rename(nearest_n = variable)

            return(temp)

        }

        tmp1 <-listtodf(plotobj$loocv_res) 

        train_bias <- ggplot(tmp1 %>%
                             filter(
                                    #abs(value) < 50,
                                    #measure == 'bias' | measure == 'rmse' | measure == 'zscore') 
                                    #measure == 'bias' | measure == 'zscore') 
                                    measure == 'zscore') 
                             ) + 
            xlab("Matches (N)") + ylab("Bias") +
            geom_point(aes(x=nearest_n, y=value, colour=measure)) + 
            theme_bw()  + theme(legend.position="none")

    # Coverage (Excluding extreme measures)
    train_cov <- ggplot(tmp1 %>%
                        filter(
                               #nearest_n > 10,
                               measure == 'iqrcoverage') 
                               #measure == 'iqrcoverage' | measure == 'coverage95c' ) 
                        ) + 
        geom_point(aes(x=nearest_n, y=value, colour=measure)) + 
        xlab("Matches (N)") + ylab("Coverage") +
        ylim(min(tmp1 %>% 
                 filter(measure == 'iqrcoverage') %>%
                 dplyr::select(value) %>%
                 unlist %>%
                 as.vector) * 0.95 ,
             max(tmp1 %>%
                 filter(measure == 'iqrcoverage') %>%
                 dplyr::select(value) %>%
                 unlist %>%
                 as.vector) * 1.05)+
        #ylim(0.3,1)+
        #scale_colour_manual(labels=c("95% IQR Coverage","50% IQR Coverage"), values=c("blue","red")) +
        scale_colour_manual(labels=c("50% IQR Coverage"), values=c("blue")) +
        theme_bw() + theme(legend.position="none")

    # - - - - - - - - - - - - - - - - - - - - - - #
    # Precision Plot: Mean IQR dif by Nearest N
    # - - - - - - - - - - - - - - - - - - - - - - #
    ppdf <- rbindlist(lapply(plotobj$loocv_res, function(y) {
                                 rbindlist(lapply(y$precisionvec, function(x) {
                                                      list(meaniqrdif = mean(x$prec, na.rm=TRUE))
}))
                               }), idcol='nearest_n')
    #- change values in id column to numeric
    ppdf <- ppdf %>%
        mutate(nearest_n = as.numeric(gsub("nearest_", "", nearest_n))) 
    ppdf_means <- ppdf %>%
        group_by(nearest_n) %>%
        summarise(meaniqrdif=mean(meaniqrdif, na.rm=TRUE))
    #-- ggplot boxplots by the id
    #ppp <- ggplot(data=ppdf,aes(y=meaniqrdif, x=as.factor(nearest_n))) + geom_boxplot() +
        #stat_summary(fun.y=mean, colour="darkred", geom="point", shape=18, size=3, show_guide=FALSE) +
        ##geom_text(data=ppdf_means, aes(label=meaniqrdif, y = meaniqrdif+0.08)) +
        #xlab("Nearest Neighbor (N)") + ylab("Mean IQR difference") +
        #theme(axis.text.x = element_text(angle = 60, hjust = 1, size=6))

    pppm <- ggplot(ppdf_means) + 
        geom_point(aes(x=nearest_n, y=meaniqrdif)) + 
        xlab("Matches (N)") + ylab("Mean IQR difference") +
        ylim(min(ppdf_means%>% 
                 dplyr::select(meaniqrdif) %>%
                 unlist %>%
                 as.vector) * 0.95 ,
             max(ppdf_means%>%
                 dplyr::select(meaniqrdif) %>%
                 unlist %>%
                 as.vector) * 1.05)+
        #ylim(0.3,1)+
        #scale_colour_manual(labels=c("95% IQR Coverage","50% IQR Coverage"), values=c("blue","red")) +
        #scale_colour_manual(labels=c("50% IQR Coverage"), values=c("blue")) +
        theme_bw() + theme(legend.position="none")

    # - - - - - - - - - - - - - - - - - - - - - - #
    # Weighted Total Score Plot
    # - - - - - - - - - - - - - - - - - - - - - - #

    wtspdf <- loocvperf(plotobj$loocv_res)

    #wtsp <- ggplot(wtspdf %>% filter(totscore <= 3)) + 
    wtsp <- ggplot(wtspdf) + 
        geom_point(aes(x=nearest_n, y=totscore)) + 
        xlab("Matches (N)") + ylab("Weighted Total Score") +
        theme_bw() + theme(legend.position="none")



        return(plot_grid(train_bias, train_cov, pppm, wtsp, labels="AUTO", ncol=2))

    } else {

        print("creating training calibration plot")
        cptrainlist = plot_func(plotobj = plotobj, train=TRUE, filt=filt)
        print("creating testing calibration plot")
        cptestlist = plot_func(plotobj = plotobj, train=FALSE, filt=filt)

        if(plot_zscore==FALSE){

            minc <- floor(min(cptrainlist[[2]], cptestlist[[2]], na.rm=TRUE))
            maxc <- ceiling(max(cptrainlist[[3]], cptestlist[[3]], na.rm=TRUE))

            #labels
            train_zs_lab <- paste0("zscore == ", cptrainlist$tp$zscore)
            train_cov_lab <- paste0("coverage == ", cptrainlist$tp$coverage)
            train_prec_lab <- paste0("precision == ", cptrainlist$tp$precision)

            test_zs_lab <- paste0("zscore == ", round(cptestlist$tp$zscore, 4))
            test_cov_lab <- paste0("coverage == ", round(cptestlist$tp$coverage, 4))
            test_prec_lab <- paste0("precision == ", round(cptestlist$tp$precision,4))

            # Calibration plots
            cptrain <- cptrainlist[[1]] + xlim(minc, maxc) + ylim(minc,maxc) + 
                #geom_text(aes(label=paste0(cptrainlist[[4]]), y=minc+(maxc-minc)/10+1, x=(minc+maxc)*0.6), parse= TRUE, color="red") +
                geom_text(aes(label=paste0(train_zs_lab), y=minc+(maxc-minc)/10+(maxc-minc)/10, x=(minc+maxc)*0.6), parse= TRUE, color="red") +
                geom_text(aes(label=paste0(train_cov_lab), y=minc+(maxc-minc)/10, x=(minc+maxc)*0.6), parse= TRUE, color="blue") +
                geom_text(aes(label=paste0(train_prec_lab), y=minc+(maxc-minc)/10-(maxc-minc)/10, x=(minc+maxc)*0.6), parse= TRUE, color="green")

            cptest <- cptestlist[[1]] + xlim(minc, maxc) + ylim(minc,maxc) + 
                #geom_text(aes(label=paste0(cptestlist[[4]]), y=minc+(maxc-minc)/10+1, x=(minc+maxc)*0.6), parse= TRUE, color="red")+
                geom_text(aes(label=paste0(test_zs_lab), y=minc+(maxc-minc)/10+(maxc-minc)/10, x=(minc+maxc)*0.6), parse= TRUE, color="red") +
                geom_text(aes(label=paste0(test_cov_lab), y=minc+(maxc-minc)/10, x=(minc+maxc)*0.6), parse= TRUE, color="blue") +
                geom_text(aes(label=paste0(test_prec_lab), y=minc+(maxc-minc)/10-(maxc-minc)/10, x=(minc+maxc)*0.6), parse= TRUE, color="green")

            return(plot_grid(cptrain, cptest, labels = "AUTO"))
        
        } else {

            minc <- floor(min(cptrainlist[[6]], cptestlist[[6]], na.rm=TRUE))
            maxc <- ceiling(max(cptrainlist[[7]], cptestlist[[7]], na.rm=TRUE))

            # Calibration plots
            cptrain <- cptrainlist[[5]] + xlim(minc, maxc) + ylim(minc,maxc) 
            cptest <- cptestlist[[5]] + xlim(minc, maxc) + ylim(minc,maxc) 
            return(plot_grid(cptrain, cptest, labels = "AUTO"))
        
        
        }
    
    }
    #return(plot_grid(cptrain, cptest, train_bias, train_cov, ppp, pppm, labels = "AUTO"))
    
}


#explained variance code here:
#round(var(fitted(fit))/var(newdata1$knee_arom_flexion),3)
#explained variance code here:
#round(var(fitted(fit))/var(newdata1$knee_arom_flexion),3)
#extract fitted outcome Y90

# - - - - - - - - - - - - #
# Plotting Function: Precision Curve ----
# - - - - - - - - - - - - #

plot_prec <- function(plotobj, n_range){
    library(data.table)
    library(ggplot2)
    library(dplyr)

    precplot <- rbindlist(lapply(plotobj$loocv_res, function(x) { 
                                     rbindlist(x$precisionvec, idcol="id") }), 
                          idcol="n") %>%
    filter(grepl(n_range, n)) %>%
    ggplot(., aes(x=time, y=prec, group=id)) +
    geom_line() +
    geom_point() +
    xlim(0, 365) +
    ylim(0, 10) +
    facet_wrap( ~n)

    return(precplot)

}


# - - - - - - - - - - - - - - - - - - - - #
# Performance Metric Calculation Function: After LOOCV ----
# - - - - - - - - - - - - - - - - - - - - #

loocvperf <- function(loocv_res){
    library("data.table")
    data.frame(
               zscore = sapply(loocv_res, function(x) { 
                  rbindlist(x$zscore, idcol="id") %>%
                      left_join(
                                test_proc$train_o %>% dplyr::select(id) %>%
                                    rename(patient_id = id) %>%
                                    cbind(., id=seq(1,nrow(test_proc$train_o))) 
                                )%>% 
                  filter(!is.na(zsc) & !is.infinite(zsc)) %>%
                  summarise(zscore = mean(zsc))
                  #unlist(x$zscore$zsc)[!is.na(unlist(x$zscore)) & !is.infinite(unlist(x$zscore))]
                            }) %>% unlist %>% as.vector,
               nearest_n = rownames( data.frame(sapply(sapply(loocv_res, function(x) { 
                                                                  unlist(x$zscore)[!is.na(unlist(x$zscore)) & !is.infinite(unlist(x$zscore))]
                         }), function(y) {
                   mean(y)
                         })
               )

               )
               ) %>% 
    mutate(nearest_n = as.numeric(gsub("nearest_", "", nearest_n)),
           # zscore standardizing
           zsc = abs(pnorm(zscore, 0, 1)-0.5)/0.5) %>%
    left_join(
              # coverage merge
              data.frame(
                         coverage = sapply(loocv_res, function(x) { 
                                               mean(x$iqrcoverage)
               }),
                         nearest_n = rownames(data.frame(sapply(loocv_res, function(x) { 
                                                                    mean(x$iqrcoverage)
                                                                                    })))
                         ) %>%
              mutate(nearest_n = as.numeric(gsub("nearest_", "", nearest_n)),
                     # coverage standardize
                     covsc = abs(coverage - 0.5)/0.5) 
              ) %>% 
    left_join(
              # precision merge
              data.frame(
                         precision = sapply(loocv_res, function(x) { 
                                                rbindlist(x$precisionvec, idcol = "id") %>%
                                                    left_join(
                                                              test_proc$train_o %>% dplyr::select(id) %>%
                                                                  rename(patient_id = id) %>%
                                                                  cbind(., id=seq(1,nrow(test_proc$train_o))) 
                                                              )%>% 
                                                summarise(precision = mean(prec))
                                            #mean(sapply(x$precisionvec, function(y) {
                                            #mean(y$prec)
                                            #}), na.rm=TRUE)
               }) %>% unlist %>% as.vector,
                         #precision = sapply(loocv_res, function(x) { 
                                                #mean(sapply(x$precisionvec, function(y) {
                                                                #mean(y$prec)
                                                                                    #}), na.rm=TRUE)
               #}),
                         nearest_n = rownames(data.frame(
                                                         sapply(loocv_res, function(x) { 
                                                                    mean(sapply(x$precisionvec, function(y) {
                                                                                    mean(y$prec)
}), na.rm=TRUE)
                                                                                    })                   
                                                         ))
                         ) %>%
              mutate(nearest_n = as.numeric(gsub("nearest_", "", nearest_n)),
                     # precision calc
                     #precision = as.vector(abs(pnorm(scale(precision), 0, 1)-0.5)/0.5) )
                     presc = precision/max(precision))
              ) %>%
    left_join(
              data.frame(
                         dcase = sapply(loocv_res, function(x) { 
                                            mean(x$dropped_cases)
               }),
                         nearest_n = rownames(
                                              data.frame(
                                                         sapply(loocv_res, function(x) { 
                                                                    mean(x$dropped_cases)
                                                                                    })

                                                         )
                                              )
                         ) %>%
              mutate(nearest_n = as.numeric(gsub("nearest_", "", nearest_n)))

          ) %>%
    mutate(totscore = zsc + covsc + presc + dcase)  %>%
    .[complete.cases(.),]
    #mutate(totscore = zscore + coverage + precision + dcase) %>%
    #dplyr::select(totscore) %>% unlist(.) %>% which.min(.) %>% as.vector(.)

}


#-- external validation measures of bias, coverage and precision

extvalid <- function(loocvobj,
                     test_proc){

    library("data.table")
    library("dplyr")
    # for matching train and test patients via min value difference in Fitted of train (i.e. pmm)
    temp <- data.frame(train_id = sapply(1:length(test_proc$test_o$pred), function(x) {
                                             #which.min(abs(test_proc$test_o$pred[x] - test_proc$train_o$Fitted))
                                             test_proc$train_o[which.min(abs(test_proc$test_o$pred[x] - test_proc$train_o$Fitted)),"id"]
                         }), 
                       test_id = test_proc$test_o$id)
    precisionvec = test_proc$test_post %>% 
                   dplyr::select(patient_id, time) %>% 
                   rename(test_id = patient_id) %>% 
                   left_join(
                             temp 
                             ) %>% 
                   left_join(
                             # merge training data precision by time and training id
                             rbindlist(lapply(loocvobj$pred_res$precisionvec, data.frame), idcol="id")  %>%
                                 left_join(
                                           # create a table that has ordered training id and sequence from 1 to 398 because the precisionvector doesn't have id's recorded
                                           test_proc$train_o %>% dplyr::select(id) %>%
                                               rename(train_id = id) %>%
                                               cbind(., id=seq(1,nrow(test_proc$train_o))) 
                                           ) 
                             ) %>%
                   dplyr::select(prec) %>% unlist %>% as.vector

    res <- data.frame(zscore = mean(rbindlist(loocvobj$pred_res$zsc_list)$zsc[!is.na(rbindlist(loocvobj$pred_res$zsc_list)$zsc) & !is.infinite(rbindlist(loocvobj$pred_res$zsc_list)$zsc)]),
               coverage = mean(loocvobj$pred_res$coveragevec),
               precision = test_proc$test_post %>% 
                   dplyr::select(patient_id, time) %>% 
                   rename(test_id = patient_id) %>% 
                   left_join(
                             temp 
                             ) %>% 
                   left_join(
                             # merge training data precision by time and training id
                             rbindlist(lapply(loocvobj$pred_res$precisionvec, data.frame), idcol="id")  %>%
                                 left_join(
                                           # create a table that has ordered training id and sequence from 1 to 398 because the precisionvector doesn't have id's recorded
                                           test_proc$train_o %>% dplyr::select(id) %>%
                                               rename(train_id = id) %>%
                                               cbind(., id=seq(1,nrow(test_proc$train_o))) 
                                           ) 
                             )  %>% 
                   # here we use na.rm but really need to think about if there are any dropped predictions.. probably not a good idea to use the model at all
                   summarise(precision = mean(prec, na.rm=TRUE))
               )
    if(any(is.na(precisionvec))){
           warning("NA values in precision")
               }
    if(any(is.na(rbindlist(loocvobj$pred_res$zsc_list)$zsc))){
           warning("NA values in zscore")
               }
    if(any(is.na(loocvobj$pred_res$coveragevec))){
           warning("NA values in coverage")
               }
    res
}
