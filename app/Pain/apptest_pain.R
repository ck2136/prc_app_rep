
install.packages("devtools")
Sys.setenv(GITHUB_PAT = "c2599f776cdc834470c765eb198c667e4d61477c")
devtools::install_github(repo = "stefvanbuuren/brokenstick", oath = Sys.getenv("c2599f776cdc834470c765eb198c667e4d61477c"))




setwd("/Users/andrewkittelson/Desktop/brokenstick")

library(gamlss)
library(ggplot2)



### READ DATA ###
dat <- read.csv('pain.csv')
all <- read.csv('data_120716.csv')



expectations<-100
basePAIN<- 0.5


try <- 0.239894 + basePAIN*0.176064 +expectations*-0.001408



dat$diff <- abs(dat$lmFit-try)
dat1 <- dat[order(dat$diff),]
dat2 <- dat1[1:80,"id"]

all1 <- subset(all, patient_id %in% dat2)
all1 <- all1[order(all1$patient_id),]
all2 <- subset(all1, is.na(w_pain_percent)==F & time_elapsed>0 & w_pain_percent<11)
all3 <- all2[c("patient_id", "w_pain_percent", "time_elapsed")]


mintime<-min(all3$time_elapsed)
maxtime<-max(all3$time_elapsed)
 
plm<-gamlss(w_pain_percent~cs(time_elapsed^0.5),
            sigma.formula = ~cs(time_elapsed),
            data=na.omit(all3), family=NO)


all4<-subset(all3, is.na(w_pain_percent)==F & time_elapsed<121)


# extract model parameters
xgrid <- c(2, seq(5, 120, 5))
xnew <- data.frame(patient_id = NA,
                   w_pain_percent = NA, 
                   time_elapsed = xgrid)
ref <- predictAll(plm, 
                  newdata = xnew, type = "response",
                  data = all3)
ref_txt <- data.frame(x = round(xgrid, 4), 
                      mu = round(ref$mu, 2),
                      sigma = round(ref$sigma, 4),
                      c10 = (ref$mu+(ref$sigma*(-1.282))),
                      c25 = (ref$mu+(ref$sigma*(-0.675))),
                      c75 = (ref$mu+(ref$sigma*(0.675))),
                      c90 = (ref$mu+(ref$sigma*(1.28)))
)
                    




ggplot(data = all4, aes(x = time_elapsed, y = w_pain_percent)) +       
  geom_line(aes(group = patient_id), colour="light gray", size=1) + geom_point(color="light gray", size = 2)+
  geom_line(data=ref_txt, aes(x=x, y=c10), color='red', size =1.3, linetype = 1) +
  geom_line(data=ref_txt, aes(x=x, y=c25), color='blue', size =1.5, linetype = 2) +
  geom_line(data=ref_txt, aes(x=x, y=mu), color='black', size = 2) +
  geom_line(data=ref_txt, aes(x=x, y=c75), color='blue', size =1.5, linetype = 2) +
  geom_line(data=ref_txt, aes(x=x, y=c90), color='red', size =1.3, linetype = 1) +
  theme_bw() + 
  ggtitle("Personalized Reference Chart") +
  labs(x="Time Following Surgery (Days)",y="Pain (%)") +
theme(panel.grid.minor = element_line(colour="gray", size=0.5)) +
  scale_y_continuous(limits = c(-.01,1), minor_breaks = seq(0 , 1, .05), breaks = seq(0, 1, .10)) +
  scale_x_continuous(minor_breaks = seq(0 , 120, 5), breaks = seq(0, 120, 10)) 
  

