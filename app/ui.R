library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(plotly)
library(wordcloud2)

ht <- c("#BicentenarioGye","#Guayaquil","#200IndependenciaGYE","#9deOctubre", "#bicentenariodeguayaquil")

header <- dashboardHeader(
    title = "Masapp"
    )

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Tops", tabName = "tops", icon = icon("arrow-alt-circle-up")),
        menuItem("Sentimiento", tabName = "sentimiento", icon = icon("comment")),
        # menuItem("Mapa", tabName = "widgets", icon = icon("map-marked-alt")),
        menuItem("Masapp", tabName = "widgets", icon = icon("th")),
        selectInput("Hashtag",
                    label = "Selecciones Hashtag",
                    choices = ht,
                    multiple = TRUE
                    ),
        actionButton("go",
                     "Visualizar",
                     icon = icon('chart-area'))
        #dateRangeInput("daterange_sidebar", "Fechas a Analizar:",
        #               start = "2001-01-01",
        #               end   = "2010-12-31")
    )
)

body <- dashboardBody(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
        tags$script(async = NA, src = "https://platform.twitter.com/widgets.js")
    ),
    ## 1. Dashboard Tab -------------
    tabItems(
        tabItem(tabName = "dashboard",
                ## ++ Totales -----------
                fluidRow(
                    tabBox(
                        title = "Totales",
                        id = "tabs_totales", 
                        width = 12,
                        tabPanel("Hoy", 
                                 fluidRow(
                                     valueBoxOutput("Tuits_hoy", width = 2),
                                     valueBoxOutput("Users_hoy", width = 2),
                                     valueBoxOutput("RTs_hoy", width = 2),
                                     valueBoxOutput("Likes_hoy", width = 2),
                                     valueBoxOutput("Pos_hoy", width = 2),
                                     valueBoxOutput("Neg_hoy", width = 2)
                                 ),
                        ),
                        tabPanel("Ult. 2 días", 
                                 fluidRow(
                                     valueBoxOutput("Tuits_02d", width = 2),
                                     valueBoxOutput("Users_02d", width = 2),
                                     valueBoxOutput("RTs_02d", width = 2),
                                     valueBoxOutput("Likes_02d", width = 2),
                                     valueBoxOutput("Pos_02d", width = 2),
                                     valueBoxOutput("Neg_02d", width = 2)
                                 )
                        ),
                        tabPanel("Ult. 7 días", 
                                 fluidRow(
                                     valueBoxOutput("Tuits_07d", width = 2),
                                     valueBoxOutput("Users_07d", width = 2),
                                     valueBoxOutput("RTs_07d", width = 2),
                                     valueBoxOutput("Likes_07d", width = 2),
                                     valueBoxOutput("Pos_07d", width = 2),
                                     valueBoxOutput("Neg_07d", width = 2)
                                 )
                        ),
                        tabPanel("Desde 15 Octubre", 
                                 fluidRow(
                                     valueBoxOutput("Tuits_30d", width = 2),
                                     valueBoxOutput("Users_30d", width = 2),
                                     valueBoxOutput("RTs_30d", width = 2),
                                     valueBoxOutput("Likes_30d", width = 2),
                                     valueBoxOutput("Pos_30d", width = 2),
                                     valueBoxOutput("Neg_30d", width = 2)
                                 )
                        )
                    )
                    
                ),
                ## ++ Tuits por dia ---------
                fluidRow(
                    withSpinner(plotlyOutput("plotly_tuits_diarios", height = "150px"))
                ),
                
                ## ++ usuarios por dia -----------
                fluidRow(
                    withSpinner(plotlyOutput("plotly_users_diarios", height = "150px"))
                ),
                ## ++ Sentimiento por dia -----------
                fluidRow(
                    withSpinner(plotlyOutput("plotly_tuits_diarios_sent", height = "250px"))
                )
        ),
        
        ## 2. Tops ------------
        tabItem(tabName = "tops",
                ## ++ Barras para Tops -----------
                fluidRow(
                    box(
                        title = "Top Usuarios ",
                        width = 3,
                        status = "primary",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        # withSpinner(plotlyOutput("top_users")),
                        withSpinner(plotOutput("top_users") ) ,
                        helpText("Importancia calculada con tuits + rts + likes")
                        
                    ),
                    box(
                        title = "Top Mencionados",
                        width = 3,
                        status = "primary",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        # withSpinner(plotlyOutput("top_mencionados")),
                        withSpinner(plotOutput("top_mencionados") ) ,
                        helpText("Importancia calculada con tuits + rts + likes")
                        
                    ),
                    box(
                        title = "Top Hashtags",
                        width = 3,
                        status = "primary",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        # withSpinner(plotlyOutput("top_hashtags")),
                        withSpinner(plotOutput("top_hashtags") ) ,
                        helpText("Importancia calculada con tuits + rts + likes")
                        
                    ),
                    box(
                        title = "Top Palabras",
                        width = 3,
                        status = "primary",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        # withSpinner(plotlyOutput("top_words")),
                        withSpinner(plotOutput("top_words") ) ,
                        helpText("Importancia calculada con tuits + rts + likes")
                        
                    )
                ),
                ## ++ Mostrar tuits -----------
                fluidRow(
                    box(
                        title = "Más RT últimos 7 días",
                        width = 4,
                        status = "primary",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        # withSpinner(plotlyOutput("top_users")),
                        withSpinner(uiOutput("tuit_most_rt_7d"), proxy.height = "250px")
                    ),
                    box(
                        title = "Más Likes últimos 7 días",
                        width = 4,
                        status = "primary",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        # withSpinner(plotlyOutput("top_users")),
                        withSpinner(uiOutput("tuit_most_like_7d"), proxy.height = "250px")
                    ),
                    box(
                        title = "Más interactuado desde 15 Oct",
                        width = 4,
                        status = "primary",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        # withSpinner(plotlyOutput("top_users")),
                        withSpinner(uiOutput("tuit_most_interac_30d"), proxy.height = "250px")
                    )
                    # column(
                    #     width = 4,
                    #     # class = "col-md-6 col-md-offset-0 col-lg-4",
                    #     # class = "text-center",
                    #     tags$h4(HTML( "Más RT últimos 7 días")),
                    #     withSpinner(uiOutput("tuit_most_rt_7d"), proxy.height = "250px")
                    # ),
                    # column(
                    #     width = 4,
                    #     # class = "col-md-6 col-md-offset-0 col-lg-4",
                    #     # class = "text-center",
                    #     tags$h4(HTML("Más Likes últimos 7 días")),
                    #     withSpinner(uiOutput("tuit_most_like_7d"), proxy.height = "250px")
                    #     ),
                    # 
                    # column(
                    #     width = 4,
                    #     # class = "col-md-6 col-md-offset-0 col-lg-4",
                    #     # class = "text-center",
                    #     tags$h4(HTML("Más interactuado desde 15 Oct")),
                    #     withSpinner(uiOutput("tuit_most_interac_30d"), proxy.height = "250px")
                    # )
                 
                )
        ),
        ## 3. Analizar Sentimiento ----------------
        tabItem(tabName = "sentimiento",
                ## ++ Wordcluod Completo -----------
                fluidRow(
                    # box(
                    #     title = "Top Palabras",
                    #     width = 4,
                    #     status = "primary",
                    #     solidHeader = TRUE,
                    #     collapsible = TRUE,
                    #     withSpinner(plotlyOutput("top_words")),
                    #     helpText("Importancia calculada con tuits + rts + likes")
                    #     
                    # ),
                    box(
                        title = "WordCloud",
                        width = 8,
                        status = "primary",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        wordcloud2Output("wordcloud", height = "300px")
                    ),
                ),
                
                
                # ## ++ Wordclouds por sentimiento -----------
                fluidRow(
                    box(
                        title = "WordCloud Positivas",
                        width = 6,
                        status = "primary",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        wordcloud2Output("wordcloud_posi", height = "300px")
                    ),
                    box(
                        title = "WordCloud Negativas",
                        width = 6,
                        status = "primary",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        wordcloud2Output("wordcloud_nega", height = "300px")
                    )
                )        
                )
        )
)


ui <- dashboardPage(header, sidebar, body, 
                    title = "Masapp"
                    )