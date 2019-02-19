library(shinyjs)

shinyUI(
  fluidPage(
    useShinyjs(),
    tagList(
      tags$head(
        tags$style(HTML("
      @import url('//fonts.googleapis.com/css?family=Bungee+Shade|Oranienbaum|Abril+Fatface|Ubuntu');
      label, input, button, select {
      font-family: 'Abril+Fatface';
      font-size: 12;
      }
      body {
      background-color: white;
      }

      h1 {
        font-family: 'Ubuntu';
        font-weight: 20;
        line-height: 1.0;
        color: #000000;
        font-size: 28px;
      }


    "))
      )
    ),

    ### First Fluid Row: 1. LOGO and 2. CHART TITLE
    fluidRow(
      column(4,
             img(src="http://www.ucdenver.edu/SiteCollectionImages/Logos/som_ptp.png", align = "left")
             ),
      column(4,
        headerPanel("Knee Replacement Reference Charts"), align = 'center', size = "300"
      )
    )
    ,
    
    fluidRow(
      column(11,
             div(class = "logininfo",
                 uiOutput("userPanel"),float = "center", align = "right", direction = "rtl")
      )
    ),
    
    ### Second Fluid Row: 1. user info and 2. logout 
    
    div(class = "login",
        uiOutput("uiLogin"),
        textOutput("pass"),
        tags$head(tags$style("#pass{color: red;"))
    ), 
    
    ### Third Fluid Row: Import Data from Database
    fluidRow(
      column(4,
             div(class = "span1",      
                 uiOutput("obs")
             )
      ),
      column(5,
             div(class = "DataTable",      
                 uiOutput('dataTable'),
                 uiOutput('ggplotly1')
             ))
    ),
    conditionalPanel(
      condition = "input.num_pat >= 0",
      selectInput("temp1","Temporary", c("Matt","M"))
    )
  )
)
