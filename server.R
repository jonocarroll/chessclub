library(shiny)
library(shinydashboard)

source("functions.R")

server <- function(input, output, session) {
    
    reload()

    for (i in seq_len(nrow(games))) {
      calc_elo(i)
    }
    
    updateSelectizeInput(session, "player", choices = unique(players$name))
    
    output$allplayers <- plotly::renderPlotly({
        all_charts_plotly()
    })
    
    output$indivplayer_chart <- renderPlot({
        chart_elo(input$player)
    })
    
    output$indivplayer_table <- DT::renderDataTable({
        table_elo(input$player)
    }, rownames = FALSE, options = list(pageLength = 10, dom = "ti"))
    
    output$indivgames_table <- DT::renderDataTable({
        player_games(input$player)
    }, rownames = FALSE, options = list(pageLength = 100, dom = "ti"))
    
    output$playorwinmatrix <- renderPlot({
        switch(input$playorwin, 
               "Play" = plot_play_matrix(),
               "Win" = plot_win_matrix()
        )
    })
    
    output$games <- DT::renderDataTable({
        games
    }, rownames = FALSE, options = list(pageLength = 100, dom = "fti"))
    
    output$upsets <- DT::renderDataTable({
        largest_upsets()
    }, rownames = FALSE, options = list(pageLength = 20, dom = "ti"))
    
}
