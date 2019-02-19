library(shiny)
library(ggplot2)
library(gamlss)
library(shiny)
library(gridExtra)
library(dplyr)
library(DT)
library(plotly)

### Values that will be inputted into the existing data

fields <- c("patient_id","age_at_s", "sex1","bmi1", "y20ROM","y90ROM","baseROM", "knee_arom_flexion","knee_arom_extension", "walking_speed","w_pain_percent","expect","time_elapsed")


### READ EXISTING DATA ###
dat <- read.csv('./fittedLMy90.csv')
ext <- read.csv('./Extension/extension.csv')
pain <- read.csv('./Pain/pain.csv')
#ws <- read.csv('./ws.csv')
all <- read.csv("./data_120716.csv")


shinyServer(
  function(input, output, session) {
    
    ######################################
    
    
    ################# READ EXISTING DATA #################
    
    # Whenever a field is filled, aggregate all form data
    formData <- reactive({
      data <- sapply(fields, function(x) input[[x]])
      data
    })
    
    
    # When the Submit button is clicked, save the form data
    observeEvent(input$submit, {
      saveData(formData())
      session$sendCustomMessage(type = 'testmessage',
                                message = 'One Observation is Added')
    })
    
    # When Delete button clicked, prev response deleted
    observeEvent(input$delprev, {
      if(nrow(responses) >= 1) {
        delData(formData())
        session$sendCustomMessage(type = 'testmessage',
                                  message = 'One Observation is Deleted')
      } else {
        session$sendCustomMessage(type = 'testmessage',
                                  message = 'No Rows to Delete')
      }
      
    })
    
    ################### Create Merged Data with User Input #########################
    
    mergedData <- eventReactive(list(input$submit, input$delprev),  {
      if(!is.null(loadData())){
        newdata <- rbind(dat[,-1] %>%
                           mutate(patient_id = id,
                                  age_at_s = age) %>%
                           select(patient_id, y20ROM, baseROM, age_at_s, y90ROM, lmFit),
                         loadData() %>%
                           mutate(lmFit = 1) %>%
                           mutate_all(funs(as.numeric)) %>%
                           select(patient_id, y20ROM, baseROM, age_at_s, y90ROM, lmFit))
        
        ### Here we are getting the predicted y90ROM based on baseROM and age ###
        
        newdata$LmFit <- predict(lm(y90ROM ~ baseROM + age_at_s, newdata))
        newdata
        
      } else {
        dat[,-1] %>%
          mutate(patient_id = id,
                 age_at_s = age) %>%
          select(patient_id, y20ROM, baseROM, age_at_s, y90ROM, lmFit)
      }
      
    })
    
    output$table1 <- renderDataTable({
      rendered_table <- mergedData() 
      datatable(rendered_table) %>%
        formatRound(., columns = c(setdiff(colnames(rendered_table), c("patient_id","age_at_s"))), digits = 2)
    })
  
    
    
    
    ################## Plot Fitted vs. Observed y90ROM ######################
    
    output$y90ROMfit <- renderPlotly({
      
      
      theGraph <- ggplot(mergedData()) +
        geom_smooth(aes(x = baseROM, y = lmFit, color = "Fitted")) +
        geom_smooth(aes(x = baseROM, y = y90ROM, color = "Observed")) +
        xlab("Baseline ROM") +
        ylab("y90ROM") + labs(color = "Obs vs Pred")  +
        theme(legend.position="bottom")
      
      ggplotly(theGraph)
      
    })
    
    ########### KNEE ROM #################
    output$aromPRC1 <- renderPlotly({
      
      try <- 79.55 + input$age_at_s*0.25 + input$knee_arom_flexion*0.16
      
      df <- data.frame(x=-5, y=input$knee_arom_flexion) 
      
      dat$diff <- abs(dat$lmFit-try)
      dat1 <- dat[order(dat$diff),]
      dat2 <- dat1[1:60,"id"]
      
      all1 <- subset(all, patient_id %in% dat2)
      all1 <- all1[order(all1$patient_id),]
      all2 <- subset(all1, is.na(knee_arom_flexion)==F & time_elapsed>0)
      all3 <- all2[c("patient_id", "knee_arom_flexion", "time_elapsed")]
      
      
      all4<-subset(all3, is.na(knee_arom_flexion)==F & time_elapsed<121)
      
      
      plm<-gamlss(knee_arom_flexion~cs(time_elapsed^0.5),
                  sigma.formula = ~cs(time_elapsed),
                  data=na.omit(all3), family=NO)
      
      
      # extract model parameters
      xgrid <- c(2, seq(5, 120, 2))
      xnew <- data.frame(patient_id = NA,
                         knee_arom_flexion = NA, 
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
                            c90 = (ref$mu+(ref$sigma*(1.28))))
      
      
      
      p1<- ggplot(data = all4, aes(x = time_elapsed, y = knee_arom_flexion)) +       
        geom_line(aes(group = patient_id), colour="light gray", size=1) + geom_point(color="light gray", size = 2)+
        geom_line(data=ref_txt, aes(x=x, y=c10), color='red', size =1.3, linetype = 1) +
        geom_line(data=ref_txt, aes(x=x, y=c25), color='blue', size =1.5, linetype = 2) +
        geom_line(data=ref_txt, aes(x=x, y=mu), color='black', size = 2) +
        geom_line(data=ref_txt, aes(x=x, y=c75), color='blue', size =1.5, linetype = 2) +
        geom_line(data=ref_txt, aes(x=x, y=c90), color='red', size =1.3, linetype = 1) +
        geom_point( data= df, aes(x=x, y=y),color="green", size = 5)+
        theme_bw() + 
        ggtitle("Patients Like Me") +
        labs(x="Time Following Surgery (Days)",y="Knee Flexion (Degrees)") +
        theme(panel.grid.minor = element_line(colour="gray", size=0.5)) +
        scale_y_continuous(limits = c(55,145), minor_breaks = seq(0 , 155, 5), breaks = seq(0, 155, 10)) +
        scale_x_continuous(minor_breaks = seq(0 , 120, 5), breaks = seq(0, 120, 10)) 
      
      
      #grid.arrange(p1, p2, ncol=2)
      ggplotly(p1)
      
    })
    
    output$aromPRC2 <- renderPlotly({
      
      try <- 79.55 + input$age_at_s*0.25 + input$knee_arom_flexion*0.16
      
      df <- data.frame(x=-5, y=input$knee_arom_flexion) 
      
      dat$diff <- abs(dat$lmFit-try)
      dat1 <- dat[order(dat$diff),]
      dat2 <- dat1[1:60,"id"]
      
      all1 <- subset(all, patient_id %in% dat2)
      all1 <- all1[order(all1$patient_id),]
      all2 <- subset(all1, is.na(knee_arom_flexion)==F & time_elapsed>0)
      all3 <- all2[c("patient_id", "knee_arom_flexion", "time_elapsed")]
      
      all5<-subset(all, is.na(knee_arom_flexion)==F & time_elapsed>0)
      all6<-all5[c("patient_id", "knee_arom_flexion", "time_elapsed")]
      
      mintime<-min(all3$time_elapsed)
      maxtime<-max(all3$time_elapsed)
      
      plm<-gamlss(knee_arom_flexion~cs(time_elapsed^0.5),
                  sigma.formula = ~cs(time_elapsed),
                  data=na.omit(all3), family=NO)
      
      # extract model parameters
      xgrid <- c(2, seq(5, 120, 2))
      xnew <- data.frame(patient_id = NA,
                         knee_arom_flexion = NA, 
                         time_elapsed = xgrid)
      
      ##population level
      plm_pop<-gamlss(knee_arom_flexion~cs(time_elapsed^0.5),
                      sigma.formula = ~cs(time_elapsed),
                      data=na.omit(all6), family=NO)
      
      all7<-subset(all6, is.na(knee_arom_flexion)==F & time_elapsed<121)
      
      xpop <- data.frame(patient_id = NA,
                         knee_arom_flexion = NA, 
                         time_elapsed = xgrid)
      pop <- predictAll(plm_pop, 
                        newdata = xnew, type = "response",
                        data = all6)
      pop_txt <- data.frame(x = round(xgrid, 4), 
                            mu = round(pop$mu, 2),
                            sigma = round(pop$sigma, 4),
                            c10 = (pop$mu+(pop$sigma*(-1.282))),
                            c25 = (pop$mu+(pop$sigma*(-0.675))),
                            c75 = (pop$mu+(pop$sigma*(0.675))),
                            c90 = (pop$mu+(pop$sigma*(1.28))))
      
      p2<-ggplot(data = all7, aes(x = time_elapsed, y = knee_arom_flexion)) +       
        geom_line(aes(group = patient_id), colour="light gray", size=1) + geom_point(color="light gray", size = 2)+
        geom_line(data=pop_txt, aes(x=x, y=c10), color='red', size =1.3, linetype = 1) +
        geom_line(data=pop_txt, aes(x=x, y=c25), color='blue', size =1.5, linetype = 2) +
        geom_line(data=pop_txt, aes(x=x, y=mu), color='black', size = 2) +
        geom_line(data=pop_txt, aes(x=x, y=c75), color='blue', size =1.5, linetype = 2) +
        geom_line(data=pop_txt, aes(x=x, y=c90), color='red', size =1.3, linetype = 1) +
        geom_point( data= df, aes(x=x, y=y),color="green", size = 5)+
        theme_bw() + 
        ggtitle("All Patients") +
        labs(x="Time Following Surgery (Days)",y="Knee Flexion (Degrees)") +
        theme(panel.grid.minor = element_line(colour="gray", size=0.5)) +
        scale_y_continuous(limits = c(55,145), minor_breaks = seq(0 , 155, 5), breaks = seq(0, 155, 10)) +
        scale_x_continuous(minor_breaks = seq(0 , 120, 5), breaks = seq(0, 120, 10)) 
      
      #grid.arrange(p1, p2, ncol=2)
      ggplotly(p2)
      
    })


  }
)

saveData <- function(data) {
  data <- as.data.frame(t(data), stringsAsFactors = FALSE)
  if (exists("responses")) {
    responses <<- rbind(responses, data)
  } else {
    responses <<- data
  }
}

loadData <- function() {
  if (exists("responses")) {
    responses
  }
}

delData <- function(data) {
  data <- as.data.frame(t(data), stringAsFactors = FALSE)
  if (exists("responses")) {
    responses <<- head(responses, -1)
  } else {
    print("No response to delete")
  }
}
