library(ggplot2)
library(gamlss)
library(gridExtra)
library(brokenstick)
#### 1.2 Set working directory and import data
setwd("../../data")

#read in skinny dataset with matching characteristics, time, and outcome
data <- read.csv("TUG_070118.csv")
data<-subset(data, train_test==1)
id<-"patient_id"
time<-"time"
m1<-"age"
m2<-"bmi"
m3<-"gender"
m4<-"b_flex"
m5<-"b_ext"
m6<-"b_pain"
m7<-"b_tug"
outcome<-"tug"
#proximal=100
distal=90


############################################################################
############################################################################



data$m1<-data[,m1]
data$m2<-data[,m2]
data$m3<-data[,m3]
# data$m4<-data[,m4]
# data$m5<-data[,m5]
# data$m6<-data[,m6]
data$m7<-data[,m7]

miny<-min(na.omit(data[,outcome]))
maxy<-max(na.omit(data[,outcome]))


##### keep only the postoperative data

newdata <- data #subset(data, source <3 | source>3)

mint<-min(na.omit(newdata[,time]))
maxt<-max(na.omit(newdata[,time]))


full<-newdata[c(id, time, outcome)]

full<-na.omit(full)

full<-subset(full, time>0)

#write.csv(full, file="tug_all_051718.csv")

full1<-full

mean(full1$tug)

#### visualize the data


ggplot(data = full1, aes(x = full1$time, y = full1[,outcome])) +       
  geom_line(aes(group = full1[,id]), colour="black", size=1) + 
  geom_point(color="black", size = 1)+
  theme_bw() + 
  ggtitle("Spaghetti") +
  labs(x=time,y=outcome) +
  theme(panel.grid.minor = element_line(colour="gray", size=0.5)) +
  scale_y_continuous(limits = c(0,60), minor_breaks = seq(mint,maxt , 10), breaks = seq(mint,maxt, 5)) 
  #scale_x_continuous(limits= c(-10,120), minor_breaks = seq(0 , 120, 5), breaks = seq(0, 120, 10)) 



#mess around with brokenstick. Set knots according to postoperative dates and examine model fit
#note: I would like to look at linear mixed model instead of brokenstick to see if it gives a different answer

#coeffs<-list()


require("brokenstick")


#for (i in c(1:20)){
  
  
 # i=30
  
  #knots <- c(0, distal)
   knots <- c(0, 14, 50,90)
  
  
  
  
  
  fit <- brokenstick(y = full[,outcome],
                     x = full[,time],
                     subjid = full[,id],
                     knots = knots)
  
  est1<-predict(fit, at="knots")
  
  
  
  #explained variance code here:
  #round(var(fitted(fit))/var(newdata1$knee_arom_flexion),3)
  
  
  #extract fitted outcome Y90
  
  long<-est1[est1$x==distal,]
  long$y<-long$yhat
  long$patient_id<-long$subjid
  head(long)
  
  #extract early covariate

  earlycov<-est1[est1$x==14,]
  earlycov$xearly<-earlycov$yhat
  earlycov$patient_id<-earlycov$subjid

  #create dataset of baseline covariates
  
  #preoperative data only
  baseline <- data[(data[,time]<0), ]
  
  ###combine all the relevent timepoints in a wide dataset
  
  #Also need to look here at how to automate selection of covariates and removal of NAs
  
  all.0<-merge(baseline, long, by="patient_id")
  
  all<-merge(all.0, earlycov, by="patient_id")
  
  all$y<-all$y.x
  # all1$m2<-factor(all1$m2)
  # # # 
  # all1 <-all1[!is.na(all1$knee_arom_flexion) & !is.na(all1$bmi1)    & !is.na(all1$age_at_s),]
  # # all1 <-all1[(all1$age_at_s>20),]
  # all1$sex1<-factor(all1$sex1)
  # head(all1)
  
  
  ######linear model with common sense PREOP predictors
  
  mod<-all # [c("y","m1","m2","m3","m4","m5","xearly", "m6","m7","patient_id")] 
  #mod<-na.omit(mod)
  
  pmm<-lm(y~m1 +
            m2 +
            m3 +
            #m4 +
            #xearly +
            #m5 +
            #m6 +
            m7 
          ,
          data=mod)
  summary(pmm)

  ggplot(mod) + geom_smooth(aes(x=m7, y=y)) +
      geom_point(aes(x=m7,y=y))
      geom_line(aes(x=m7,y=y))
  
plot(mod$m2, mod$y)

  
  fitted<- data.frame(id = mod$patient_id,  
   Fitted = pmm$fitted.values)
  
  
hist(mod$y, 50)  
  
  #write.csv(fitted, file="tug_yhats_postop_051718.csv")
  
  
  beta<-as.data.frame(pmm1$coefficients)
  
  beta<-as.data.frame(t(beta))
  beta$t1<-i
  beta$r2<-summary(pmm1)$r.squared
  coeffs[[i]]<-beta
  
  
  
  
}


parameters1<-data.frame(t1=unlist(lapply(coeffs,"[[",6)), 
                        intercept=unlist(lapply(coeffs,"[[",1)), 
                        preop=unlist(lapply(coeffs,"[[",2)),
                        x1=unlist(lapply(coeffs,"[[",3)),  
                        bmi=unlist(lapply(coeffs,"[[",4)),  
                        age=unlist(lapply(coeffs,"[[",5)),  
                        r2=unlist(lapply(coeffs,"[[",7)))

plot(parameters1$t1, parameters1$age)

plot(all1$xearly, all1$y)


coeffs[7]
unlist(lapply(coeffs,"[[",2))

data[,1]
