library(shiny)
library(shinydashboard)

dashboardPage(
    dashboardHeader(title = paste(CLUB_NAME, "Chess Club 2023")),
    ## Sidebar content
    dashboardSidebar(
        sidebarMenu(
            menuItem("All Players", tabName = "allplayers", icon = icon("users")),
            menuItem("Individual Players", tabName = "indivplayers", icon = icon("user")),
            menuItem("Pairings", tabName = "pairings", icon = icon("random")),
            menuItem("Games", tabName = "games", icon = icon("soccer-ball-o")),
            menuItem("Highlights", tabName = "highlights", icon = icon("star")),
            menuItem("Admin", tabName = "admin", icon = icon("unlock"))
        )
    ),
    ## Body content
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "allplayers",
                    fluidRow(
                        box(plotly::plotlyOutput("allplayers", height = 500, width = 1000), width = 12),
                    )
            ),
            
            # Second tab content
            tabItem(tabName = "indivplayers",
                    fluidRow(
                        box(width = 12,
                            column(12, 
                                   selectizeInput("player", "Player", choices = "")
                            ), column(6, 
                                   plotOutput("indivplayer_chart", height = 400, width = 600)
                            ), column(4, offset = 2, 
                                   DT::dataTableOutput("indivplayer_table")
                            )
                        )
                    ),
                    fluidRow(
                        box(width = 12, 
                            DT::dataTableOutput("indivgames_table")
                        )
                    )
            ),
            tabItem(tabName = "pairings", 
                    fluidRow(
                        box(width = 12, 
                            column(12,
                                   radioButtons("playorwin", "Play or Win Matrix?", 
                                                choices = c("Play", "Win"),
                                                inline = TRUE)
                            ),
                            column(12,
                                   plotOutput("playorwinmatrix", height = 800, width = 800)
                                   )
                            )                
                    )
            ),
            tabItem(tabName = "games", 
                    fluidRow(
                        box(width = 12, 
                            column(12,
                                   h2("All Games: "),
                                   DT::dataTableOutput("games")
                            )
                        )                
                    )
            ),
            tabItem(tabName = "highlights", 
                    fluidRow(
                        box(width = 12, 
                            column(12,
                                   h2("Biggest Upsets:"),
                                   DT::dataTableOutput("upsets")
                            )
                        )                
                    )
            )
        )
    )
)
