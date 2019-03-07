library(shiny)
library(ggplot2)
library(dplyr)

shinyServer(function(input, output, session) {
  
  values <- reactiveValues(starting = TRUE)
  
  session$onFlushed(function() {
    values$starting <- FALSE
  })
  
  
  

  
  datRusher <- reactive({
    as.data.frame(rusherData[rusherData$posteam %in% input$team & rusherData$season %in% input$season & rusherData$Rusher %in% input$player,])
   
  })
  
  
  datRusher1 <- reactive({
    as.data.frame(rusherData1[rusherData1$posteam %in% input$team & rusherData1$season %in% input$season & rusherData1$Rusher %in% input$player,])
    
  
  })
  
  datTeam <- reactive({
   as.data.frame(teamData[teamData$posteam %in% input$team & teamData$season %in% input$season,])
  })
  
  datTeam1 <- reactive({
   as.data.frame(teamData1[teamData1$posteam %in% input$team & teamData1$season %in% input$season,])
  })

  
  
  
  output$seasonSelector <- renderUI({
    selectInput('season', 'select season',  choices = unique(teamData$season), selected = '2017') 
  })
  
  
  output$teamSelector <- renderUI({
    selectInput('team', 'select team', choices = unique(teamData1$posteam), selected = 'Broncos') 
  })
  
  
  
  conditionalPanel(
    
    condition = "input.plotType == 'player'",output$playerSelector <- renderUI({
      
      available <- rusherData1[rusherData1$posteam == input$team & rusherData1$season == input$season, 1]
      
    
      
      selectInput('player', 'select player', choices = unique(available), selected = unique(available)[1])
      
      
    }))
  
  
  
  
  
  output$teamplot1 <- renderPlot({ 
    
    
    
    ggplot(datTeam(), aes(x=direction, y=gain, fill=count)) + 
      geom_tile(color="white", size=0.1) + 
      coord_equal() + 
      scale_fill_gradient(low = "#ff1a1a",  high = "#0000cc") + geom_text(aes(label = count),color = 'white') + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = "#ffffff"), rect = element_rect(fill = '#ffffff'))
    
    
  })
  
  output$teamplot2 <- renderPlot({
    
    
    req(nrow(datTeam1())>0)
    
 
        ggplot(datTeam1(), aes(x=direction,y= sum) ) + 
      geom_col(aes(fill = 'yards'), width = .6) + 
      geom_col( aes(y=count, fill = 'rushes'), width = .4) + 
      scale_fill_manual(name='',values=c('yards'= nflColors[nflColors$team  == input$team ,2] ,'rushes'=nflColors[nflColors$team  == input$team ,3])) + 
      scale_color_manual(name = '', values = c('yards per carry' = '#020202')) + 
      geom_label( size = 4, 
                  aes(x=direction, 
                      y = -1,
                      label = round(sum/count, digits = 2), 
                      color = 'yards per carry')) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = "#ffffff"), rect = element_rect(fill = '#ffffff'))
    
  })
  
  output$playerplot1 <- renderPlot({    
    

    
    ggplot(datRusher(), aes(x=direction, y=gain, fill=count)) + 
      geom_tile(color="white", size=0.1) + 
      coord_equal() + 
      scale_fill_gradient(low = "#ff1a1a",  high = "#0000cc") + geom_text(aes(label = count),color = 'white') +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = "#ffffff"), rect = element_rect(fill = '#ffffff'))
    
    
  })
 
 
    output$playerplot2 <- renderPlot({
      req(nrow(datRusher1())>0)
      
      ggplot(datRusher1(), aes(x=direction,y= sum) ) + 
        geom_col(aes(fill = 'yards'), width = .6) + 
        geom_col( aes(y=count, fill = 'rushes'), width = .4) + 
        scale_fill_manual(name='',values=c('yards'=nflColors[nflColors$team  == input$team ,2] ,'rushes'=nflColors[nflColors$team  == input$team ,3])) + 
        scale_color_manual(name = '', values = c('yards per carry' = '#020202')) + 
        geom_label( size = 4, 
                    aes(x=direction, 
                        y = -1,
                        label = round(sum/count, digits = 2), 
                        color = 'yards per carry')) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = "#ffffff"), rect = element_rect(fill = '#ffffff'))
     
    })
  
  
})