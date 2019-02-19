library(shiny)
library(RMySQL)
library(ggplot2)
library(plotly)
suppressPackageStartupMessages(
  library(shinyjs)
)
########################################################
library(shinyjs)

shinyServer(function(input, output, session) {
  
  USER <- reactiveValues(Logged = FALSE , session = session$user) 
  
  source("./Login.R",  local = TRUE)
  
  extDat <- eventReactive(input$search,{
    withProgress(
      message = "Extracting Data!",
      detail = "Get data from database", value = 0, {
        
        con <- dbConnect(MySQL(),
                         user="user", password="password",
                         dbname="dbname", host="123.456.789.00")
        setnumPat <- isolate(input$num_pat)
        
        incProgress(0.5)
        
        if (!is.null(setnumPat)) {
          rs <- dbSendQuery(con, paste0("select * from fittedLMy90 limit", setnumPat, ";"))
          
          data <- fetch(rs, n=input$num_pat)
          dbClearResult(rs)
          dbDisconnect(con)
        } else {
          data <- NULL
        }
        setProgress(1)
      }
    )
    return(data)
  })
  
  
  
  ### logic for logged == TRUE
  output$obs <- renderUI({
    if (USER$Logged == TRUE) {
      list(
        h3("Specify your data"),
        dateRangeInput("dates", "Select range of date", min = "2015-01-01", format = "yyyy-mm-dd" ),
        sliderInput("num_pat", "Select number of patients", min = 100, max = 500, step = 1, value = 238),
        actionButton('search', 'EXTRACT'),
        actionButton('plot1',"PLOT")
      )
    }
  })
  
  # render table first
  output$table2 <- renderDataTable(
    extDat(),
    options = list(
      pageLength = 10,
      lengthMenu = c(10,25,50,100,200,500)
    )
  )
  
  # render the UI with the datatable
  output$dataTable <- renderUI({
    if (USER$Logged == TRUE) {
      dataTableOutput('table2')
    }
  })
  
 
  
  ### Plot Key Logic
  
  plotDat <- eventReactive(input$plot1,{
    withProgress(
      message = "Plotting Data!",
      detail = "Plotting of the dataframe", value = 0, {
        
        
        
        incProgress(0.5)
        
        theGraph <- ggplot(extDat()) +
          geom_smooth(aes(x = BASEROM, y = LMFIT, color = "Fitted")) +
          geom_smooth(aes(x = BASEROM, y = Y90ROM, color = "Observed")) +
          xlab("Baseline ROM") +
          ylab("y90ROM") + labs(color = "Obs vs Pred")  +
          theme(legend.position="bottom")
        
        ggplotly(theGraph)
        
        setProgress(1)
      }
    )
    return(ggplotly(theGraph))
  })
  
  output$dataplot <- renderPlotly({
    plotDat()
    
  })
  
  # render the UI with the datatable
  output$ggplotly1 <- renderUI({
    if (USER$Logged == TRUE) {
      plotlyOutput('dataplot')
    }
  })
  
  # hide table once PLOT button is hit this is using library(shinyjs)
  observeEvent(input$plot1, {
    hide("dataTable")
    # toggle("plot") if you want to alternate between hiding and showing
  })
  
  
  ################ Function for the Plotting ###########################
  
  
  
  
  
  
})
