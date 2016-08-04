ui <- fluidPage(
  tags$img(src="SuspectedSpoofingTrades.png"),
  #tags$img(src="check.jpg"),
  h3("Trading Data for E-mini S&P 500: July 01, 2011"), 
  #hr(),
  
  plotOutput('plot6P'),
  
  fluidRow(
    column(3, "", 
           numericInput("CaseNum", paste0("Case Number (1-",N,"):"), value = 1, 
                        min = 1, max = N, step = 1),
           numericInput("LevelNum", "Number of Levels (1-7)", value = 4, 
                        min = 1, max = 7, step = 1)
    ),
    
    column(5, offset=1, "", htmlOutput("text"), br(), tableOutput("table")),
    
    column(3,"", plotOutput('plot1P'))
    
  )
  
)   

