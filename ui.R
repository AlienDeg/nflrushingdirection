
shinyUI(fluidPage(
  titlePanel("NFL rush directions"), tags$head(includeScript("gtm_head.js")),
  fluidRow(column(1),
           column(2, radioButtons('plotType', 'select', choices = c('team','player'))),
           column(3,htmlOutput("seasonSelector")),
           column(3,htmlOutput("teamSelector")),
           conditionalPanel(
             condition = "input.plotType == 'player'", column(3,htmlOutput("playerSelector")))),
  fluidRow(
    
    
    conditionalPanel(
      condition = "input.plotType == 'team'",
      column(6,plotOutput("teamplot1")),column(6,plotOutput("teamplot2"))),
    
    conditionalPanel(
      condition = "input.plotType == 'player'",
      column(6, plotOutput("playerplot1")),column(6,plotOutput("playerplot2")))),
  fluidRow(tags$body(tags$p(style = "padding: 30px; width:822px",align = 'left', 'If you want to learn more about me check out my ', tags$a('Twitter (@AlienDeg)',target="_blank" ,href='https://twitter.com/aliendeg'), ' or ', tags$a('Linkedin.',target="_blank", href='https://uk.linkedin.com/in/pawelkapuscinski'), ' If you have any questions, suggestions, inquiries, ideas how to improve the app or superinteresting data project, you can mail me pawel [at] databall.co.'
                                )))
  
) 
)


