setwd("/Users/kittelsa/desktop/brokenstick")

dat <- read.csv('tug_yhats_postop_051718.csv')
all <- read.csv("tug_all_051718.csv")
all<-subset(all, patient_id != 488)
all$timed_up_and_go<-all$tug
all$time_elapsed<-all$time
all<-all[c("X", "patient_id", "timed_up_and_go", "time_elapsed")]

sex<-1
input<-c(11.06, 10.02)
time<-c(18,21)
preop<-8.0
bmi<-27.3
age<-70


patient<-data.frame(X=8.8,
                    patient_id=1000000,
                    timed_up_and_go=input,
                    time_elapsed=time)

all<-rbind(all, patient)


require("brokenstick")

knots <- c(0, 14, 50, 90)

fit <- brokenstick(y = all$timed_up_and_go,
                   x = all$time_elapsed,
                   subjid = all$patient_id,
                   knots = knots)

est1<-predict(fit, at="knots")
patientest<-subset(est1, est1$subjid==1000000)

patientest1<-subset(patientest, patientest$x==14)

point<-patientest1$yhat


try <- -0.46809+ 0.09313*preop + 0.05800*age + 0.63438*sex +0.05008*bmi +0.09140*point



df <- data.frame(x=-5, y=preop) 
df1 <- data.frame(x=time, y=input) 





dat$diff <- abs(dat$Fitted-try)
dat1 <- dat[order(dat$diff),]
dat2 <- dat1[1:40,"id"]

all1 <- subset(all, patient_id %in% dat2)
all1 <- all1[order(all1$patient_id),]
all2 <- subset(all1, is.na(timed_up_and_go)==F & time_elapsed>0)
all3 <- all2[c("patient_id", "timed_up_and_go", "time_elapsed")]

all5<-subset(all, is.na(timed_up_and_go)==F & time_elapsed>0 &timed_up_and_go>3)
all6<-all5[c("patient_id", "timed_up_and_go", "time_elapsed")]

mintime<-min(all3$time_elapsed)
maxtime<-max(all3$time_elapsed)

plm<-gamlss(timed_up_and_go~cs(time_elapsed^0.5, df=2),
            sigma.formula = ~cs(time_elapsed^0.5, df=1), 
            data=na.omit(all3), family=BCT, n.cyc=500)
plm<-refit(plm)

ref_txt<-centiles.pred(plm, type = c("centiles"), 
                       xname = "time_elapsed", xvalues = c(1:120), 
                       cent = c(10, 25, 50, 75, 90), 
                       plot = T, legend = F)



all4<-subset(all3, is.na(timed_up_and_go)==F & time_elapsed<121)
# 
# # extract model parameters
# xgrid <- c(2, seq(5, 120, 2))
# xnew <- data.frame(patient_id = NA,
#                    timed_up_and_go = NA, 
#                    time_elapsed = xgrid)
# 
# # 
# # 
# # ref_txt<-centiles.pred(plm, type = c("centiles"), 
# #                        xname = "time_elapsed", xvalues = c(mintime:maxtime), 
# #                        cent = c(2.5, 5, 10, 25, 50, 75, 90, 95, 97.5), 
# #                        plot = F, legend = F)
# # 
# 
# ref <- predictAll(plm, 
#                   newdata = xnew, type = "response",
#                   data = all3)
# 
# 
# ref_txt <- data.frame(x = round(xgrid, 4), 
#                       C50 = round(ref$mu, 2),
#                       sigma = round(ref$sigma, 4),
#                       C10 = (ref$mu+(ref$sigma*(-1.282))),
#                       C25 = (ref$mu+(ref$sigma*(-0.675))),
#                       C75 = (ref$mu+(ref$sigma*(0.675))),
#                       C90 = (ref$mu+(ref$sigma*(1.28))))
#population level
plm_pop<-gamlss(timed_up_and_go~cs(time_elapsed^0.5, df=2),
                sigma.formula = ~cs(time_elapsed, df=1),
                data=na.omit(all6), family=BCT)

all7<-subset(all6, is.na(timed_up_and_go)==F & time_elapsed<121)


pop_txt<-centiles.pred(plm_pop, type = c("centiles"), 
                       xname = "time_elapsed", xvalues = c(1:120), 
                       cent = c(5,10, 25, 50, 75, 90,95), 
                       plot = T, legend = F)




# 
# xpop <- data.frame(patient_id = NA,
#                    timed_up_and_go = NA,
#                    time_elapsed = xgrid)
# pop <- predictAll(plm_pop,
#                   newdata = xnew, type = "response",
#                   data = all6)
# pop_txt <- data.frame(x = round(xgrid, 4),
#                       C50 = round(pop$mu, 2),
#                       sigma = round(pop$sigma, 4),
#                       C10 = (pop$mu+(pop$sigma*(-1.282))),
#                       C25 = (pop$mu+(pop$sigma*(-0.675))),
#                       C75 = (pop$mu+(pop$sigma*(0.675))),
#                       C90 = (pop$mu+(pop$sigma*(1.28))))




p11<- ggplot(data = all4, aes(x = time_elapsed, y = timed_up_and_go)) +       
  geom_line(aes(group = patient_id), colour="gray", size=1) + 
  geom_point(color="gray", size = 2)+
  geom_line(data=ref_txt, aes(x=time_elapsed, y=C10), color='purple', size =1.3, linetype = 1) +
  geom_line(data=ref_txt, aes(x=time_elapsed, y=C25), color='blue', size =1.5, linetype = 2) +
  geom_line(data=ref_txt, aes(x=time_elapsed, y=C50), color='black', size = 2) +
  geom_line(data=ref_txt, aes(x=time_elapsed, y=C75), color='blue', size =1.5, linetype = 2) +
  geom_line(data=ref_txt, aes(x=time_elapsed, y=C90), color='purple', size =1.3, linetype = 1) +
  geom_point( data= df, aes(x=x, y=y),color="green", size = 5)+
  geom_point( data= df1, aes(x=x, y=y),color="green", size = 5)+
  geom_line( data= df1, aes(x=x, y=y),color="green", size = 1, linetype = 1)+
  theme_bw() + 
#  ggtitle("'Patients Like Me'") +
  labs(x="Time Following Surgery (Days)",y="TUG Time (sec)") +
  theme(panel.grid.minor = element_line(colour="gray", size=0.5)) +
  scale_y_continuous(limits = c(0,20), minor_breaks = seq(0 , 155, 1), breaks = seq(0, 155, 2)) +
  scale_x_continuous(limits = c(-10, 120), minor_breaks = seq(0 , 120, 5), breaks = seq(0, 120, 10)) 

p15<-ggplot(data = all7, aes(x = time_elapsed, y = timed_up_and_go)) +
  geom_line(aes(group = patient_id), colour="gray", size=1) + geom_point(color="gray", size = 2)+
  geom_line(data=pop_txt, aes(x=time_elapsed, y=C5), color='red', size =1.3, linetype = 1) +
  geom_line(data=pop_txt, aes(x=time_elapsed, y=C10), color='red', size =1.3, linetype = 1) +
  geom_line(data=pop_txt, aes(x=time_elapsed, y=C25), color='blue', size =1.5, linetype = 2) +
  geom_line(data=pop_txt, aes(x=time_elapsed, y=C50), color='black', size = 2) +
  geom_line(data=pop_txt, aes(x=time_elapsed, y=C75), color='blue', size =1.5, linetype = 2) +
  geom_line(data=pop_txt, aes(x=time_elapsed, y=C90), color='red', size =1.3, linetype = 1) +
  geom_line(data=pop_txt, aes(x=time_elapsed, y=C95), color='red', size =1.3, linetype = 1) +

   geom_point( data= df, aes(x=x, y=y),color="green", size = 5)+
  geom_point( data= df1, aes(x=x, y=y),color="green", size = 5)+
  geom_line( data= df1, aes(x=x, y=y),color="green", size = 1, linetype = 1)+
  theme_bw() +
 # ggtitle("All Patients") +
  labs(x="Time Following Surgery (Days)",y="TUG Time (sec)") +
  theme(panel.grid.minor = element_line(colour="gray", size=0.5)) +
  scale_y_continuous(limits = c(0,20), minor_breaks = seq(0 , 155, 1), breaks = seq(0, 155, 2)) +
  scale_x_continuous(limits = c(-10, 120), minor_breaks = seq(0 , 120, 5), breaks = seq(0, 120, 10)) 

grid.arrange(p11, p15,ncol=1)
grid.arrange(p15,ncol=1)
