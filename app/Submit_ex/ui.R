library(shiny)
library(DT)
library(ggiraph)
library(plotly)
library(shinydashboard)

fluidPage(
  
  # JS tags
  
  ## tag 1: to add msg for adding and deleting observation
  tags$head(tags$script('Shiny.addCustomMessageHandler("testmessage",
                        function(message) {
                        alert(JSON.stringify(message));
                        }
  );'),
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
                            font-weight: 150;
                            line-height: 1;
                            color: #000000;
                            }
                            
                            "))),
  
  # Application title
  fluidRow(
    column(4,
           img(src="http://www.ucdenver.edu/SiteCollectionImages/Logos/som_ptp.png", align = "left")
    ),
    column(6,
           headerPanel("Knee Replacement Reference Charts"), align = 'center'
    )
  ),
  
  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the
  # br() element to introduce extra vertical spacing
  sidebarLayout(
    sidebarPanel(
      
      # Application title
      titlePanel("Patient information"),
      
      # Enter patient id with numericInput 
      numericInput(inputId="patient_id", label="ID", min=1, value = 1, width='100%'),
      
      
      # Enter patient age with numericInput 
      numericInput(inputId="age_at_s", label="Age", value=75, min=1, max=110, width='100%'),
      
      # Gender
      selectInput(inputId="sex1", label="Gender", 
                  choices=c("Female", "Male"), selected="Female", width='100%'),
      
      #Enter patient BMI
      numericInput(inputId="bmi1", label="BMI", 
                   value=28.6, min=5, max=50, width='100%'),
      
      #Enter patient y20ROM
      numericInput(inputId="y20ROM", label="y20ROM", 
                   value=90, min=1, max=150, width='100%'),
      
      #Enter patient y90ROM
      numericInput(inputId="y90ROM", label="y90ROM", 
                   value=120, min=1, max=150, width='100%'),
     
      #Enter patient baseROM
      numericInput(inputId="baseROM", label="baseROM", 
                   value=50, min=1, max=150, width='100%'),
      
      # Enter patient baseline knee flexion ROM with numericInput 
      numericInput(inputId="knee_arom_flexion", label="Pre-op Flexion ROM", 
                   value=127, min=0, max=180, width='100%'),
      
      
      # Enter patient baseline extension ROM
      numericInput(inputId="knee_arom_extension", label="Pre-op Extension ROM", 
                   value=-2, min=-25, max=20, width='100%'),
      
      
      # Enter patient baseline walking speed
      numericInput(inputId="walking_speed", label="Pre-op Walking Speed", 
                   value=1.33, min=0, max=5, width='100%'),
      
      
      # Enter patient baseline pain
      numericInput(inputId="w_pain_percent", label="Pre-op WOMAC Pain", 
                   value=0.33, min=0, max=1, width='100%', step = 0.01),
      
      # Enter patient baseline expectations
      numericInput(inputId="expect", label="Expectations for Success", 
                   value=100, min=0, max=100, width='100%'),
      
      # Enter patient time elapsed
      numericInput(inputId="time_elapsed", label="Time Elapsed", 
                   value=50, min=0, max=120, width='100%'),
      
      # Add on new observation or delete it
      p("Press below button to either 'Submit' or 'Delete' patient information in database"),
      actionButton("submit", "Submit"),
      actionButton("delprev","Delete"),
      
      width=3
      
      
    ),
    
    #mainPanel(plotOutput("aromPRC"))   
    mainPanel(tabsetPanel(type = "tabs", 
                          
                          ### First tab panel ###
                          
                          tabPanel("Data", 
                                   fluidRow(
                                     box(
                                       column(12, h3("Table 1: Patient Data") ,align = 'left') , width = 12
                                     )
                                   ),
                                   fluidRow(
                                     box(
                                       column(12, dataTableOutput("table1") ,align = 'center')
                                       , width = 8
                                     )
                                   ),
                                   fluidRow(
                                     box(
                                       column(12, h3("Figure 1: Observed vs. Fitted y90ROM") ,align = 'left') , width = 12
                                     ),
                                     
                                     box(
                                       plotlyOutput("y90ROMfit") , width = 12
                                     )
                                   )
                                   ),
                          
                          ### Second (flexion) tab panel ###
                          
                          tabPanel("Flexion",
                                   fluidRow(
                                     box(
                                       plotlyOutput("aromPRC1"), width = 6
                                     ),
                                     box(
                                       plotlyOutput("aromPRC2"), width = 6
                                     )
                                   )),
                          
                          ### Third (extension) tab panel ###
                          
                          tabPanel("Extension", plotOutput("extension")), 
                          tabPanel("Walking Speed", plotOutput("ws")), 
                          tabPanel("WOMAC Pain", plotOutput("pain"))
    ))
  )
  )
