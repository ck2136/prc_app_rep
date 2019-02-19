#### Log in module ###

PASSWORD <- data.frame(
  Brukernavn = c("ray","gil"), 
  Passord = c("0000","1234")
  )

output$uiLogin <- renderUI({
  if (USER$Logged == FALSE) {
    fluidRow(
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      column(width = 4, offset = 4,
             wellPanel(
               textInput("userName", "User Name:"),
               passwordInput("passwd", "Pass word:"),
               br(),
               actionButton("Login", "Log in")
             )
             )
    )
   
  }
})

output$pass <- renderText({  
  if (USER$Logged == FALSE) {
    USER$pass
  }  
})

# Login info during session ----
output$userPanel <- renderUI({
  if (USER$Logged == TRUE) {
    fluidRow(
      column(11,
             "User: ", USER$name, align = "right"
      ),
      column(1, actionLink("logout", "Logout"))
    )
  }  
})

# control login
observeEvent(input$Login , {
  Username <- isolate(input$userName)
  Password <- isolate(input$passwd)
  Id.username <- which(PASSWORD$Brukernavn == Username)
  Id.password <- which(PASSWORD$Passord    == Password)
  if (length(Id.username) > 0 & length(Id.password) > 0) {
    if (Id.username == Id.password) {
      USER$Logged <- TRUE
      USER$name <- Username      
    } 
  } else {
    USER$pass <- "User name or password failed!"
  }
})

# control logout
observeEvent(input$logout , {
  USER$Logged <- FALSE
  USER$pass <- ""
})

