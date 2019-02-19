# Temporary file for testing out integration of MLR and Caret for LOOCV into custom function

# start after running LOOCV_020118_ck.R from line 128 just before predictive mean matching

# data we will be using is after we've merged the broken stick (i.e. lmm prediction) model with the 90 day prediction and 20 day early covariate and other covariate mix

# First we'll see what predictions are most predictive

rm(list=ls())

# Here are the steps for the TUG prediction
# All of the k-fold cross validation will happen inside the mlr function through the resampling procedure so we won't need to split the data up
# load data
library(readxl)
df <- read_xlsx('../data/TUG_070118.xlsx')
colnames(df) # we are going to split by patient_id later

# The ultimate goal is to find a model that will best predict TUG for patients. This method is different here to the traditional methods in that we are using subjects that have similar 90 day (or 30 or 60) TUG values to be used in the GAMLSS model. Technically there is multiple splits of the patients for modeling.. complicated!!!

# so the traditional k-fold cv approach will be used the steps are:
# 1. Split train-test ( this will be the outer loop )
# 2. On the train data do broken stick to get 30 60 90 days estimate 
# 3. Run a linear model (predictive mean matching) on the data with the previous x day estimate then for each patient, select n patients closest (this n will also be cross validated)
# 4. Use GAMLSS on the matched patients in the training set
# 5. Use the GAMLSS model in 4 and predict new values using test set
library(dplyr)

df_train  <- df %>%
    filter(train_test == 1)
df_test  <- df %>%
    filter(train_test == 2)

# check


library(mlr)
all1 %>%
  dplyr::select(-patient_id) %>%
  filter(complete.cases(.))

reg.task = makeRegrTask(id = "pmm", 
                        data = all1 %>%
                          select(-patient_id) %>%
                          filter(complete.cases(.)),
                        target = "y90")
reg.task
getTaskDesc(reg.task)
str(getTaskData(reg.task))

# select glmnet as the learner to determine which predictors should be included
regr.lrn = makeLearner("regr.glmnet", par.vals = list(lambda = 0.1, family = "gaussian"))
regr.lrn2 = makeLearner("regr.cvglmnet", par.vals = list(alpha = 0.1))
# train learner
mod = train(regr.lrn, reg.task)
mod2 = train(regr.lrn2, reg.task)

#predict
task.pred = predict(mod, task = reg.task)
task.pred2 = predict(mod2, task = reg.task)
task.pred
task.pred2
head(getPredictionResponse(task.pred)) # get the MSE
?getPredictionResponse

# plot the regression
plotLearnerPrediction("regr.lm", features = "age_at_s", task = reg.task)
plotLearnerPrediction("regr.lm", features = c("age_at_s","bmi1"), task = reg.task)

# tuning
getParamSet(regr.lrn)

ps = makeParamSet(
  makeNumericParam("alpha", lower = 0.01, upper = 0.1), # here we set alpha lambda's upper and lower
  makeNumericParam("lambda", lower = 0.01, upper = 0.1)
)
ctrl = makeTuneControlRandom(maxit = 50L) # 100 iterations for each 10 folds... this will take some time 
rdesc = makeResampleDesc(method = "CV", stratify = FALSE, iters = 10) # 10-fold CV
res = tuneParams(regr.lrn, reg.task, par.set = ps, control = ctrl, show.info = FALSE, resampling = rdesc)
as.data.frame(res$opt.path)[1:3]
res$opt.path
res

# get performance measure
listMeasures(reg.task)
getDefaultMeasure(reg.task)
performance(task.pred, measures = list(mse, medse, mae))
performance(task.pred2, measures = list(mse, medse, mae))

# Resampling manually

all1 %>%
  group_by(patient_id) %>%
  filter(n() > 1)
# there are 4 responders with duplicated rows.. get rid of them   
all1 <- all1 %>%
  filter(!duplicated(patient_id))
rdesc = makeResampleDesc(method = "LOO") # 10-fold CV
r = resample("regr.lm", reg.task, rdesc)
