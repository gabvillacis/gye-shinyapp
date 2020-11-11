library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(plotly)
library(wordcloud2)
library(shinyWidgets)


header <- dashboardHeader(
    title = "Análisis del Hashtag #Guayaquil ",
    titleWidth = 300
    )

sidebar <- dashboardSidebar(
    sidebarUserPanel("Invitado",
                     subtitle = a(href = "#", icon("circle", class = "text-success"), "Online"),
                     # Image file should be in www/ subdir
                     #image = "logo-masapp-03-300x138.png"
    ),
    # h3("Tuits sobre Guayaquil", align = "center"),
    #p("¿Qué se escribe sobre nuestra ciudad en Twitter?, 
    #  desde Masapp esta es la 3er actividad que hacemos por nuestra ciudad, 
    #  entérate de más en la pestaña [Masapp]")
    #,
    sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Tops", tabName = "tops", icon = icon("arrow-alt-circle-up")),
        menuItem("Sentimiento", tabName = "sentimiento", icon = icon("comment")),
        menuItem("Acerca de", tabName = "masapp", icon = icon("th")),

        prettyRadioButtons(
            inputId = "radio3",
            label = "Fuente",
            choiceNames = c("Muestra desde Julio"),
            choiceValues = c("muestra"),
            shape = "round",
            status = "primary",
            fill = TRUE,
            inline = TRUE)
    ),
    p("Esta muestra contiene información 
      entre 22 de Julio y 29 de Octubre"),
    p(tags$br()),
    p(
      tags$img(class = "img-responsive img-rounded center-block", src = "logo-masapp-03-300x138.png")
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
                        tabPanel("Desde 1 Octubre", 
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
                    box(
                      title = "Tuits por dia",
                      width = 12,
                      status = "primary",
                      withSpinner(plotlyOutput("plotly_tuits_diarios", height = "150px"))
                      )

                ),
                
                ## ++ usuarios por dia -----------
                fluidRow(
                    
                  box(
                    title = "Usuarios por dia", 
                    width = 12,
                    status = "warning",
                    withSpinner(plotlyOutput("plotly_users_diarios", height = "150px"))
                  )
                ),
                ## ++ Sentimiento por dia -----------
                fluidRow(
                  box(
                    title = "Total sentimiento por dia",  
                    width = 12,
                    status = "success",
                    withSpinner(plotlyOutput("plotly_tuits_diarios_sent", height = "200px"))
                  )
                    
                ),
                ## ++ % Sentimiento por dia FILL -----------
                fluidRow(
                  box(
                    title = "Porcentaje Positivo vs Negativos por dia",
                    width = 12,
                    status = "success",
                    withSpinner(plotlyOutput("plotly_tuits_diarios_sent_fill", height = "150px"))
                  )
                )
        ),
        
        ## 2. Tops ------------
        tabItem(tabName = "tops",
                ## ++ Barras para Tops -----------
                fluidRow(
                    box(
                        title = "Top Usuarios ",
                        width = 3,
                        status = "warning",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        # withSpinner(plotlyOutput("top_users")),
                        withSpinner(plotOutput("top_users") ) ,
                        helpText("Importancia calculada con tuits + rts + likes")
                        
                    ),
                    box(
                        title = "Top Mencionados",
                        width = 3,
                        status = "warning",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        # withSpinner(plotlyOutput("top_mencionados")),
                        withSpinner(plotOutput("top_mencionados") ) ,
                        helpText("Importancia calculada con tuits + rts + likes")
                        
                    ),
                    box(
                        title = "Top Hashtags",
                        width = 3,
                        status = "success",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        # withSpinner(plotlyOutput("top_hashtags")),
                        withSpinner(plotOutput("top_hashtags") ) ,
                        helpText("Importancia calculada con tuits + rts + likes")
                        
                    ),
                    box(
                        title = "Top Palabras",
                        width = 3,
                        status = "success",
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
                        title = "Más interacciones últimos 2 días",
                        width = 4,
                        status = "primary",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        # withSpinner(plotlyOutput("top_users")),
                        withSpinner(uiOutput("tw_most_interac_02d"), proxy.height = "250px")
                    ),
                    box(
                        title = "Más interacciones últimos 7 días",
                        width = 4,
                        status = "primary",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        # withSpinner(plotlyOutput("top_users")),
                        withSpinner(uiOutput("tw_most_interac_07d"), proxy.height = "250px")
                    ),
                    box(
                        title = "Más interacciones últimos 30 días",
                        width = 4,
                        status = "primary",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        # withSpinner(plotlyOutput("top_users")),
                        withSpinner(uiOutput("tw_most_interac_30d"), proxy.height = "250px")
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
                    #box(
                    #    title = "WordCloud",
                    #    width = 8,
                    #    status = "primary",
                    #    solidHeader = TRUE,
                    #    collapsible = TRUE,
                    #    wordcloud2Output("wordcloud", height = "300px")
                    #),
                    
                    box(
                        title = "Sentimientos",
                        width = 12,
                        status = "primary",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        withSpinner(plotOutput("sentimientos", height = '300px'))
                    )

                ),
                
                
                # ## ++ Wordclouds por sentimiento -----------
                fluidRow(
                    box(
                        title = "WordCloud Positivas",
                        width = 8,
                        status = "success",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        wordcloud2Output("wordcloud_posi", height = "300px")
                    ),
                    box(
                        title = "WordCloud Negativas",
                        width = 4,
                        status = "danger",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        wordcloud2Output("wordcloud_nega", height = "300px")
                    )
                )        
                ),
        ## 4. Sobre Masapp ----------------
        tabItem(tabName = "masapp",
                ## ++ Sobre Masapp -----------
                fluidRow(
                  box(
                    title = "Sobre este proyecto:",
                    width = 12,
                    status = "success",
                    solidHeader = FALSE,
                    collapsible = FALSE,
                    p(
                    "El objetivo de este proyecto es visibilizar lo que se escribe sobre la ciudad de Guayaquil, los sentimientos asociados
                    al hablar de ella, los principales generadores de contenido y sobre qué se habla."),
                    p( "En este proyecto han trabajado varios miembros del equipo Masapp --> Angel Catagua, 
                       Roberto Esteves y Néstor Montaño. Se utilizó varios paquetes de R, principalmente: Shiny, ShinyDashboard, 
                       rtweet, tidyverse, plotly entre otros."),
                    HTML(paste0(tags$img(src="gye.jpg", width="200",heigth="200",align="left")))
                   
                    
                  ),
                  
                 
                ),
                fluidRow(
                  box(
                    title = "Misión de Masapp",
                    width = 6,
                    status = "primary",
                    solidHeader = FALSE,
                    collapsible = FALSE,
                    p("La información es parte fundamental en la transformación digital de la empresa, Forbes y Harvard Business Review 
                        llamaron como",
                      strong("Data Driven Business") ,
                      "a las empresas u organizaciones que han dado el paso hacia ser manejada a través de datos"),
                    p( "El camino para convertirse en una Data Driven Business pasa por la aplicación de los conceptos de 
                          Business Intelligence, Analytics y Data Science; en MASSAP contamos con el personal capacitado para 
                          guiar a su organización, asesorarlos o capacitarlos en estos proyectos."
                    ),
                    strong("Contactos") ,
                    p(
                      "Pueden visitar nuestro ",
                      HTML(paste0(tags$a(href = "https://www.masappec.com", "sitio web", target = "_blank"))),
                      "o seguirnos en ",
                      HTML(paste0(tags$a(href = "https://www.facebook.com/MasappEC", "facebook", target = "_blank"))),
                      ", ",
                      HTML(paste0(tags$a(href = "https://twitter.com/Masappdata", "twitter", target = "_blank"))),
                      " o",
                      HTML(paste0(tags$a(href = "https://www.linkedin.com/company/masapp", "linkedIN", target = "_blank"))),
                      "o escribirnos a",
                      HTML(paste0(tags$a(href = "mailto:hola@masappec.com", "hola@masappec.com"), "."))
                    )
                    
                  ),
                  box(
                    title = "Otras iniciativas:",
                    # width = 4,
                    status = "success",
                    solidHeader = FALSE,
                    collapsible = FALSE,
                    p(
                      "¿Quieres ver cómo se verían algunos sitios turísticos e imágenes de la ciudad pintados por una", 
                      strong(" Inteligencia Artificial"),
                      "que imita a Guayasamín y a Kingman?, te invitamos a ver",
                      HTML(paste0(tags$a(href = "http://masappec.com/2020/10/la-inteligencia-artificial-convierte-fotos-de-guayaquil-en-cuadros-de-guayasamin-y-kingman/",
                                         "este post", target = "_blank"))),
                      "que fue reseñado por",
                      HTML(paste0(tags$a(href = "https://www.expreso.ec/guayaquil/inteligencia-artificial-pinta-guayasamin-kingman-91600.html",
                                         " Diario Expreso.", target = "_blank")))
                    ),
                    p("O puedes ver también lo que",
                      HTML(paste0(tags$a(href = "http://masappec.com/2020/10/una-ia-escribe-sobre-guayaquil/", 
                                         "una inteligencia artificial escribe sobre Guayaquil.", target = "_blank")))
                      ),
                    p("Para más artículos puedes vistar nuestra ",
                      HTML(paste0(tags$a(href = "http://masappec.com/#noticias", 
                                         "sección de noticias en nuestro sitio web,", target = "_blank"))),
                      " o seguir nuestro",
                      HTML(paste0(tags$a(href = "https://medium.com/@masapp",
                                         "blog en Medium", target = "_blank")))
                    )
                    
                  )
                )
        )
)
)




ui <- dashboardPage(header, sidebar, body, 
                    title = "Masapp"
                    )