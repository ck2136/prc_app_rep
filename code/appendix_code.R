# - - - - - - - - - - - - - - - - - - -
# Appendix Code
# Objectives
# 1. Comparison of LM, GLM, GAM, and GAMLSS with TUG data
# 2. CV testbed using mlr (or caret)
# 3. Figures and Graphics Examples
# - - - - - - - - - - - - - - - - - - -


rm(list=ls())
library(gamlss)
library(brokenstick)
library(dplyr)

####Set working directory and import data
setwd("/srv/shiny-server/prc")

## Step 2: Read Data ###

## Updated data by Dr. Kittelson 3.16.2018
library(data.table) # for faster csv reading (not that significantly different in read speed but still)
df <- fread("flexdata_031618.csv")

## Step 3: Clean data ###

### remove rows where key measures are missing
### note: it would be nice to automate this in the future where the user 
### specifies the outcomes of interest and the timeframe
df<-df[!is.na(df$knee_arom_flexion) & !is.na(df$time_elapsed),]
head(df)

### keep only the postoperative data
df_pre <- df[(df$time_elapsed<0), ]
df_post <- df[(df$time_elapsed>0), ]
lapply(list(df_pre, df_post), head)


###  limit data to first 4 months due to sparsity of long term outcomes
df_post <- df_post[(df_post$time_elapsed<121), ]

### sort by patient ID and then by time
df_post <- df_post[order(df_post$patient_id, df_post$time_elapsed),]
df_post<-df_post[,c("patient_id", "knee_arom_flexion", "time_elapsed")]

full <- df_post

head(df_post)

# Prediction Model  

## Step 1: Using post operative data estimate 90 day outcome using brokenstick modeling

### brokenstick() from Dr. van Buren: Essentially a linear mixed model fit by REML
### Dr. van Buren published a paper indicating brokenstick outperforms other longitudinal modeling approach

require("brokenstick")

knots <- c(0, 20, 55, 90)

fit <- brokenstick(y = df_post$knee_arom_flexion,
                    x = df_post$time_elapsed,
                    subjid = df_post$patient_id,
                    knots = knots)

est1<-predict(fit, at="knots")
head(est1)


#explained variance code here:
#round(var(fitted(fit))/var(newdata1$knee_arom_flexion),3)
#extract fitted outcome Y90
df_all <- left_join(
                  left_join(
                            # 90 day covariates
                            est1[est1$x==90,] %>%
                                rename(y90 = yhat,
                                       patient_id = subjid),
                            # early covariates
                            est1[est1$x==20,] %>%
                                rename(y20 = yhat,
                                       patient_id = subjid),
                            by="patient_id"
                            )  %>% 
                  # select only the relevant variables 
                  select(patient_id, y90, y20)
              ,
              # preoperative data for baseline info like age bmi, kaf
              df_pre %>% 
                  mutate(patient_id = as.factor(patient_id))
              , by = "patient_id"
              ) %>%
# select those without missing variables and age above 20
filter(!is.na(knee_arom_flexion), !is.na(y90), !is.na(age_at_s), age_at_s > 20) %>%
mutate(sex1 = as.factor(sex1)) %>%
select(patient_id, age_at_s, bmi1, sex1, knee_arom_flexion, time_elapsed,
       y20, y90)
head(df_all)


##linear model with common sense PREOP predictors

pmm<-lm(y90~knee_arom_flexion + age_at_s, data=df_all)
summary(pmm)
par(mfrow=c(2,2))
# check diagnostics
plot(pmm)

###linear model with common sense PREOP and EARLY POSTOP predictors
pmm1<-lm(y90~knee_arom_flexion +y20 + age_at_s, data=df_all)
summary(pmm1)

##extract fitted values for the linear model (PREOP only here)
df_all <- df_all %>%
    select(patient_id, y20, y90, knee_arom_flexion, age_at_s) %>%
    cbind(pmm$fitted.values) %>%
    rename(id = patient_id, y20rom = y20, rom = knee_arom_flexion, age = age_at_s,
           Fitted = `pmm$fitted.values`) %>%
    #dataset ordered by fitted values
    arrange(Fitted)

head(df_all)

# CH 1: INTRODUCTION TO GAMLSS 

## Descriptive

par(mfrow=c(1,1))
plot(knee_arom_flexion ~ time_elapsed, data = full)

# Note: Here we will go through LM, GLM, GAM, and finally GAMLSS to see if there are any differences in the fit


## LM

r1 <- gamlss(knee_arom_flexion ~ time_elapsed, family=NO, data=full, trace=FALSE)
l1 <- lm(knee_arom_flexion ~ time_elapsed, data=full)

lapply(list(l1, r1), function(x) {
           coef(x)
           summary(x)
           })
summary(r1)
plot(r1)
par(mfrow=c(1,2))
termplot(l1, partial.resid = TRUE, se=T)
term.plot(r1,  partial = T)
# based on the diagnostics, we can see clear deviations from normality and homoscedasticity

## GLM: Gamma, Inv-Gamma
r2 <- gamlss(knee_arom_flexion ~ time_elapsed, family=GA, data=full, trace=FALSE)
r3 <- gamlss(knee_arom_flexion ~ time_elapsed, family=IG, data=full, trace=FALSE)
r4 <- gamlss(knee_arom_flexion ~ time_elapsed, family=NOF, data=full, trace=FALSE)
r5 <- gamlss(knee_arom_flexion ~ time_elapsed, family=LOGNO, data=full, trace=FALSE)
GAIC(r1, r2, r3, r4, r5,  k=0)
# interestingly the linear model does the best in terms of the aic :) 
par(mfrow=c(2,2))
plot(r4) # the normality and highly kurtotic distribution seems not to have been adressed...

## GAM

r6 <- gamlss(knee_arom_flexion~pb(time_elapsed), family=NO, data=full, trace=FALSE)
AIC(r1,r6)  # the smoothing function definitely increases the fit
plot(r6)

summary(r6)
drop1(r6)
term.plot(r6, pages=1, ask=FALSE)
wp(r6, ylim.all=.9)

## GAM with sigma modeling

# Note: Now we model the scale parameter. Namely as a function of the explanatory variable. (i.e. the \sigma^2 seems to be higher around lower time_elapsed)

r7 <- gamlss(knee_arom_flexion~pb(time_elapsed), sigma.formula = ~pb(time_elapsed),
             family=NO, data=full, trace=FALSE)
r8 <- gamlss(knee_arom_flexion~pb(time_elapsed), sigma.formula = ~pb(time_elapsed),
             family=GA, data=full, trace=FALSE)
r9 <- gamlss(knee_arom_flexion~pb(time_elapsed), sigma.formula = ~pb(time_elapsed),
             family=IG, data=full, trace=FALSE)
AIC(r1, r6,  r7, r8, r9) 
# clearly when we model the scale parameter explicitly without assuming a constant variance...
term.plot(r7, pages=1, what="sigma", ask=FALSE)
drop1(r7, what="sigma")

wp(r7, ylim.all=3)
# There still is many points that fall outside of the 95% pointwise ci indicating that the distribution may not be appropriate.The inverted U also indicated negative skewness and the Normal distribution may not be flexible enough to capture skewness in the data

## GAMLSS modeling all 4 moments

r10 <- gamlss(knee_arom_flexion~pb(time_elapsed), sigma.formula = ~pb(time_elapsed),
              nu.formula = ~1, family = BCCGo, data = full,
              trace = FALSE)
r11 <- gamlss(knee_arom_flexion~pb(time_elapsed), sigma.formula = ~pb(time_elapsed),
              nu.formula = ~pb(time_elapsed), family = BCCGo, data = full,
              trace = FALSE)

AIC(r7, r10, r11) 
# based on the AIC... the BCCGo distribution provides a superior fit
par(mfrow=c(1,2))
wp(r10, ylim.all = 3)
wp(r11, ylim.all = 3)


# CH 2 GAMLSS PACKAGE AND FUNCTIONS

# Note: Back to looking at the linear model and fitting polynomial!
## Polynomial Terms
r11 <- gamlss(knee_arom_flexion~time_elapsed+I(time_elapsed^2), family=NO, data=full, trace=FALSE)
r12 <- gamlss(knee_arom_flexion~time_elapsed+I(time_elapsed^2) + I(time_elapsed^3), family=NO, data=full, trace=FALSE)
summary(r11)

# Note: plot the fit
par(mfrow=c(1,2))
plot(knee_arom_flexion~time_elapsed, col = "lightblue", data=full)
lines(fitted(r11)[order(full$time_elapsed)]~full$time_elapsed[order(full$time_elapsed)])
plot(knee_arom_flexion~time_elapsed, col = "lightblue", data=full)
lines(fitted(r12)[order(full$time_elapsed)]~full$time_elapsed[order(full$time_elapsed)])

# Note: both quadratic and cubic fit might not make sense as once the patients arrive at a later time point, it's possible that they remain there rather than increasing or decreasing in arom_flexion.

# VCOV matrix
print(vcov(r11), digits=3)
plot(knee_arom_flexion~time_elapsed, col = "lightblue", data=full)
lines(fitted(r11)[order(full$time_elapsed)]~full$time_elapsed[order(full$time_elapsed)])


#VCOV matrix, Corr mat
print(vcov(r11), digits=3)
print(vcov(r11, type="cor"), digits=3)
#standard errors
print(vcov(r11, type="se"), digits=2)
print(vcov(r11, type="se", robust=TRUE), digits=2)

#fit r11 with orthogonal polynomials
r0 <- gamlss(knee_arom_flexion~poly(time_elapsed,3), data=full, family=NO)

# get correlation between parameters
library(corrplot)
col1 <- colorRampPalette(c("black","grey"))
corrplot(vcov(r11, type="cor"), col=col1(2), outline=TRUE, tl.col="black",addCoef.col="white")
corrplot(vcov(r12, type="cor"), col=col1(2), outline=TRUE, tl.col="black",addCoef.col="white")
corrplot(vcov(r0, type="cor"), col=col1(2), outline=TRUE, tl.col="black",addCoef.col="white")
# because we use orthogonal polynomials the parameters for model r0 are uncorrelated :)

#Fit Nonparametric smoothing model

# Note: Now use pb() (P-splines), cb() (cubic spliens) and lo() (Locally weighted, i.e. LOESS)  nn() (Nueral networks).

## P-splines (pb())

# Note: The linear model doesn't seem to fit too well... so let's try some splines

r1 <- gamlss(knee_arom_flexion~pb(time_elapsed),data=full, family=NO)
summary(r1)
par(mfrow=c(1,1))
plot(knee_arom_flexion~time_elapsed, col = "lightblue", data=full)
lines(fitted(r1)[order(full$time_elapsed)]~full$time_elapsed[order(full$time_elapsed)])

# Note: get effective degrees of freedom
edf(r1, c("mu"))

# Note: the coef and se of smoothing term only refers to the linear part (not the nonlinear component). Interpreting the values in terms of significance is not warranted (or with care for sure) Use term.plot() to interpret the whole smoothing function.
term.plot(r1, partial=TRUE)

# Note: also the way summary() method calculates the smoothing additive terms the vcov() is implemented in a way that assumes estimated smoothing terms are fixed at estimated values... use prof.dev() and prof.term() to get confidence intervals for parameters.

prof.dev(r1, min = 0, max = 20, which = "sigma", step = 1)

## Cubic splines (cs())
r2 <- gamlss(knee_arom_flexion~cs(time_elapsed,df=10), data=full, family=NO)
plot(knee_arom_flexion~time_elapsed, col = "lightblue", data=full)
lines(fitted(r1)[order(full$time_elapsed)]~full$time_elapsed[order(full$time_elapsed)])
lines(fitted(r2)[order(full$time_elapsed)]~full$time_elapsed[order(full$time_elapsed)],col = "red", lty=2, lwd=2)
legend("bottomright", legend=c("r1: P-splines", "r2: Cubic splines"), lty=1:2, col=c("black","red"), cex=1)

## LOESS
r4 <- gamlss(knee_arom_flexion~lo(~time_elapsed,span=.4), data=full, family=NO)
plot(knee_arom_flexion~time_elapsed, col = "lightblue", data=full)
lines(fitted(r1)[order(full$time_elapsed)]~full$time_elapsed[order(full$time_elapsed)])
lines(fitted(r2)[order(full$time_elapsed)]~full$time_elapsed[order(full$time_elapsed)],col = "red", lty=2, lwd=2)
lines(fitted(r4)[order(full$time_elapsed)]~full$time_elapsed[order(full$time_elapsed)],col = "blue", lty=3, lwd=3)
legend("bottomright", legend=c("r1: P-splines", "r2: Cubic splines","r3: LOESS"), lty=1:3, col=c("black","red","blue"), cex=1)

## Neural Networks
install.packages("gamlss.add")
library(gamlss.add)
rnt <- gamlss(knee_arom_flexion~nn(~time_elapsed,size=20,decay=0.1), data=full,family=NO)
plot(knee_arom_flexion~time_elapsed, col = "lightblue", data=full)
lines(fitted(r1)[order(full$time_elapsed)]~full$time_elapsed[order(full$time_elapsed)])
lines(fitted(r2)[order(full$time_elapsed)]~full$time_elapsed[order(full$time_elapsed)],col = "red", lty=2, lwd=2)
lines(fitted(r4)[order(full$time_elapsed)]~full$time_elapsed[order(full$time_elapsed)],col = "blue", lty=3, lwd=3)
lines(fitted(rnt)[order(full$time_elapsed)]~full$time_elapsed[order(full$time_elapsed)],col = "green", lty=3, lwd=3)
legend("bottomright", legend=c("r1: P-splines", "r2: Cubic splines","r3: LOESS","rnt: Neural network"), lty=1:4, col=c("black","red","blue","green"), cex=1)

# Note: can retrive coef from neural network model
coef(getSmo(rnt))

## Extract fitted values
plot(full$time_elapsed, fitted(r1,"mu"))

## Modeling scale and shape as function of explanatory variables
# Note: to model the predictors of both the mean and the scale parameter as a non-parametric smoothing function (i.e. p-spline) of the explanatory variable time_elapsed, use:

r3 <- gamlss(knee_arom_flexion~pb(time_elapsed), sigma.formula=~pb(time_elapsed), family=NO, data=full)
# get effective degrees of freedom for all parameters
edfAll(m3)
plot(knee_arom_flexion~time_elapsed, col = "lightblue", data=full)
lines(fitted(r1)[order(full$time_elapsed)]~full$time_elapsed[order(full$time_elapsed)])
lines(fitted(r2)[order(full$time_elapsed)]~full$time_elapsed[order(full$time_elapsed)],col = "red", lty=2, lwd=2)
lines(fitted(r4)[order(full$time_elapsed)]~full$time_elapsed[order(full$time_elapsed)],col = "blue", lty=3, lwd=3)
lines(fitted(rnt)[order(full$time_elapsed)]~full$time_elapsed[order(full$time_elapsed)],col = "green", lty=3, lwd=3)
lines(fitted(m3)[order(full$time_elapsed)]~full$time_elapsed[order(full$time_elapsed)], col="orange", lty=4, lwd=4)
legend("bottomright", legend=c("r1: P-splines", "r2: Cubic splines","r3: LOESS","rnt: Neural network","r4:r3+mean/scale"), lty=1:5, col=c("black","red","blue","green", "orange"), cex=1)

## Diagnostic Plots!
plot(r3)
wp(r3, ylim.all=5)
# The model is inadequate bc many pts lie outside of the 95% confidence interval and there is a slight quadratic shape

## Fitting different distributions
r5 <- gamlss(knee_arom_flexion~pb(time_elapsed), sigma.formula=~pb(time_elapsed), nu.formula=~pb(time_elapsed), data=full, family=BCCG)
r6 <- gamlss(knee_arom_flexion~pb(time_elapsed), sigma.formula=~pb(time_elapsed), nu.formula=~pb(time_elapsed), tau.formula = ~pb(time_elapsed), data=full, family=BCPE)

summary(r5)
summary(r6)

## Selection between models
GAIC(r1,r2,r3,r4,r5,r6) 
## Note: thE GAIC doesn't use the right kappa value... we need to adjust according to the k=log(n))
GAIC(r1,r2,r3,r4,r5,r6, k=log(r5$N))

fittedPlot(r5, r6, x=full$time_elapsed, line.type=TRUE)
centiles.fan(m5, xvar=full$time_elapsed, 
             cent=c(3,10,25,50,75,90,97),
             colors="terrain",ylab="Knee_arom_flexion",xlab="time_elapsed")

# centiles plot of 3, 10, 25, 50, 75, 90, 97% indicated for values of Knee_arom_flexion values below the curve if model m5 is correct model. 

install.packages("zoo")
install.packages("gamlss.util")
library(gamlss.util)
library(colorspace)
plotSimpleGamlss(knee_arom_flexion,time_elapsed,model=m5,data=full,
                 x.val=seq(3,120,8), val=50,N=1000,ylim=c(0,200),
                 cols=heat <- hcl(100))
# Note: fitted conditional distribution of the response knee_arom_flexion, how it changes for different values of time_elapsed.
