library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(dplyr)
library(tibble)
library(ggplot2)
library(lubridate)
library(readr)
library(tidyr)
library(scales)
library(rtweet)
library(wordcloud2)
library(tidytext)
library(textdata)
library(plotly)
library(ggthemes)
library(magrittr)
library(stringr)
library(purrr)
library(wordcloud2)


server <- function(input, output) {
    ## Preeliminares --------------
    ## ++ Parámetros
    # tweets <- readRDS(file = "tweets.RDS") 
    # saveRDS(tweets2, file = "tweets_15_23.RDS")
    # min(tweets$created_at)
    fechas_hoy <- lubridate::ymd("2020-10-29")
    df_fechas <- tibble(
        fecha_tuit= seq.Date(
            from= lubridate::ymd("2020-07-01"),
            to= fechas_hoy,
            by= 1
    ))
    df_fechas <- df_fechas %>% 
        mutate(
            Hoy = if_else(fecha_tuit == fechas_hoy , "Si", "No"),
            T_02dias = if_else(fecha_tuit >= (fechas_hoy - 1), "Si", "No"),
            T_07dias = if_else(fecha_tuit >= (fechas_hoy - 6), "Si", "No"),
            #T_30dias = if_else(fecha_tuit >= (fechas_hoy %m-% months(1) + days(1)), "Si", "No")
            T_30dias = if_else(fecha_tuit >= ymd("2020-10-15"), "Si", "No")
        ) 
    
    textostop <- tm::stopwords(kind="es") #stopwords de la librer?a TM para tener en espa?ol
    textostop <- as_tibble(textostop) # lo convierto a tibble
    
    ## cambio de nombre a words la columna ##
    colnames(textostop)
    colnames(textostop)[1] <- "word"
    
    
    ## ++ Calculos varios -------------------
    # Aqui se calcular las funciones que se llaman luego en los outputs del server
    tweets <- tweets %>%
        mutate(
            fecha_tuit= as_date(created_at) ) %>%
        left_join( df_fechas, by = "fecha_tuit" ) 
    
    
    val_tuits_hoy <- tweets %>%
        group_by(Hoy) %>% 
        filter(Hoy== "Si") %>% 
        summarise(
            n_tuits= n(),
            n_usuarios= n_distinct( user_id ),
            n_rts= sum(retweet_count, na.rm = T),
            n_like= sum(favorite_count, na.rm = T)
            ) 
    
    val_tuits_02d <- tweets %>%
        group_by(T_02dias) %>% 
        filter(T_02dias== "Si") %>% 
        summarise(
            n_tuits= n(),
            n_usuarios= n_distinct( user_id ),
            n_rts= sum(retweet_count, na.rm = T),
            n_like= sum(favorite_count, na.rm = T)
        ) 
    
    val_tuits_07d <- tweets %>%
        group_by(T_07dias) %>% 
        filter(T_07dias== "Si") %>% 
        summarise(
            n_tuits= n(),
            n_usuarios= n_distinct( user_id ),
            n_rts= sum(retweet_count, na.rm = T),
            n_like= sum(favorite_count, na.rm = T)
        ) 
    
    val_tuits_30d <- tweets %>%
        group_by(T_30dias) %>% 
        filter(T_30dias== "Si") %>% 
        summarise(
            n_tuits= n(),
            n_usuarios= n_distinct( user_id ),
            n_rts= sum(retweet_count, na.rm = T),
            n_like= sum(favorite_count, na.rm = T)
        ) 
    
    
    top_users <- (
        tweets %>%
            mutate(
                fecha_tuit= as_date(created_at) ) %>% 
            mutate(screen_name= paste0("@",screen_name)) %>% 
            group_by(screen_name) %>% 
            summarise(
                n_tuits=n(),
                n_rts= sum(retweet_count, na.rm = T),
                n_like= sum(favorite_count, na.rm = T)) %>% 
            mutate(
                total= n_tuits + n_rts + n_like
            ) %>% 
            slice_max(order_by = total, n= 10) %>% 
            ggplot( aes(x= reorder(screen_name, total), y= total) )  +
            geom_segment( aes(xend=screen_name, y=0, yend= total), color= "gray", size= 1) +
            geom_point( size=4, color= "steelblue") +
            geom_text( aes(x= screen_name, y= 10, label= screen_name), 
                       hjust = "inward", vjust= -0.4, size= 6, color= '#1A719C') +
            coord_flip() +
            theme_minimal() +
            labs(x= "", y= "" ) +
            theme(
                axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank()
            )
        ) 
    # %>% 
    #     ggplotly()
    
    top_mencionados <- (
        tweets %>%
            select(status_id, status_url, mentions_screen_name, retweet_count, favorite_count) %>%
            filter(!is.na(mentions_screen_name)) %>%
            unnest(cols = mentions_screen_name) %>%
            mutate(hashtags= paste0("@", tolower(mentions_screen_name))) %>% 
            group_by(mentions_screen_name) %>% 
            summarise(
                n_tuits= n(),
                n_rts= sum(retweet_count, na.rm = T),
                n_like= sum(favorite_count, na.rm = T)) %>% 
            mutate(
                total= n_tuits + n_rts + n_like
            ) %>% 
            slice_max(order_by = total, n= 10) %>% 
            ggplot( aes(x= reorder(mentions_screen_name, total), y= total) )  +
            geom_segment( aes(xend=mentions_screen_name, y=0, yend= total), color= "gray", size= 1) +
            geom_point( size=4, color= "steelblue") +
            geom_text( aes(x= mentions_screen_name, y= 10, label= mentions_screen_name), 
                       hjust = "inward", vjust= -0.4, size= 6, color= '#1A719C') +
            coord_flip() +
            theme_minimal() +
            labs(x= "", y= "" ) +
            theme(
                axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank()
            )
        )
    # %>% 
    #     ggplotly()
    
    top_hashtags <- (
        tweets %>%
            select(status_id, status_url, hashtags, retweet_count, favorite_count) %>%
            filter(!is.na(hashtags)) %>%
            unnest(cols = hashtags) %>%
            mutate(hashtags= paste0("#", tolower(hashtags))) %>% 
            group_by(hashtags) %>% 
            summarise(
                n_tuits= n(),
                n_rts= sum(retweet_count, na.rm = T),
                n_like= sum(favorite_count, na.rm = T)) %>% 
            mutate(
                total= n_tuits + n_rts + n_like
            ) %>% 
            slice_max(order_by = total, n= 10) %>% 
            ggplot( aes(x= reorder(hashtags, total), y= total) )  +
            geom_segment( aes(xend=hashtags, y=0, yend= total), color= "gray", size= 1) +
            geom_point( size=4, color= "steelblue") +
            geom_text( aes(x= hashtags, y= 10, label= hashtags), 
                       hjust = "inward", vjust= -0.4, size= 6, color= '#1A719C') +
            coord_flip() +
            theme_minimal() +
            labs(x= "", y= "" ) +
            theme(
                axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank()
            )
        ) 
    # %>% 
    #     ggplotly()
    

    ############
     
    frecuencia_palabras <- tweets %>%
        select(screen_name, status_id, text, created_at, retweet_count, favorite_count) %>%
        mutate(
            text = str_remove_all(text, "@[[:alnum:]_]+\\b"),
            text = str_remove_all(text, "&\\w+;")
        ) %>%
        tidytext::unnest_tokens(word, text) %>% 
        mutate(word= tolower(word) ) %>% 
        filter(
            !word %in% c("http", "https", "t.co"),
            !word %in% textostop$word,
            !word %in% c("guayaquil"),
            nchar(word) >= 3
        ) %>% 
        group_by(word) %>% 
        summarise(
            n_tuits= n(),
            n_rts= sum(retweet_count, na.rm = T),
            n_like= sum(favorite_count, na.rm = T)) %>% 
        mutate(
            total= n_tuits + n_rts + n_like
        ) %>% 
        filter( n_tuits> 5 ) %>% 
        slice_max(order_by = n_tuits, n= 250) 
    
    
    top_words <- (frecuencia_palabras %>% 
            slice_max(order_by = n_tuits , n= 10) %>% 
            ggplot( aes(x= reorder(word, total), y= total) )  +
            geom_segment( aes(xend=word, y=0, yend= total), color= "gray", size= 1) +
            geom_point( size=4, color= "steelblue") +
            geom_text( aes(x= word, y= 10, label= word), 
                       hjust = "inward", vjust= -0.4, size= 6, color= '#1A719C') +
            coord_flip() +
            theme_minimal() +
            labs(x= "", y= "" ) +
            theme(
                axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank()
            )
        )
    # %>% 
    #     ggplotly()   

    
    tw_most_rt_7d <- tweets %>%
        filter(T_07dias== "Si") %>% 
        slice_max(order_by = retweet_count, n= 1) 
    
    tw_most_likes_7d <- tweets %>%
        filter(T_07dias== "Si") %>% 
        slice_max(order_by = favorite_count, n= 1) 
    
    
    tw_most_interac_30d <- tweets %>%
        filter(T_30dias== "Si") %>% 
        mutate(total= retweet_count + favorite_count) %>% 
        slice_max(order_by = favorite_count, n= 1) 
    
    
    ## ++ Funciones ----------
    get_tweet_blockquote <- function(screen_name, status_id, ..., null_on_error = TRUE, theme = "light") {
        oembed <- list(...)$oembed
        if (!is.null(oembed) && !is.na(oembed)) return(unlist(oembed))
        oembed_url <- glue::glue("https://publish.twitter.com/oembed?url=https://twitter.com/{screen_name}/status/{status_id}&omit_script=1&dnt=1&theme={theme}")
        bq <- possibly(httr::GET, list(status_code = 999))(URLencode(oembed_url))
        if (bq$status_code >= 400) {
            if (null_on_error) return(NULL)
            '<blockquote style="font-size: 90%">Sorry, unable to get tweet ¯\\_(ツ)_/¯</blockquote>'
        } else {
            httr::content(bq, "parsed")$html
        }
    }
    
    wordcloud2a <- function (data, size = 1, minSize = 0, gridSize = 0, fontFamily = "Segoe UI", 
                             fontWeight = "bold", color = "random-dark", backgroundColor = "white", 
                             minRotation = -pi/4, maxRotation = pi/4, shuffle = TRUE, 
                             rotateRatio = 0.4, shape = "circle", ellipticity = 0.65, 
                             widgetsize = NULL, figPath = NULL, hoverFunction = NULL) 
    {
        if ("table" %in% class(data)) {
            dataOut = data.frame(name = names(data), freq = as.vector(data))
        }
        else {
            data = as.data.frame(data)
            dataOut = data[, 1:2]
            names(dataOut) = c("name", "freq")
        }
        if (!is.null(figPath)) {
            if (!file.exists(figPath)) {
                stop("cannot find fig in the figPath")
            }
            spPath = strsplit(figPath, "\\.")[[1]]
            len = length(spPath)
            figClass = spPath[len]
            if (!figClass %in% c("jpeg", "jpg", "png", "bmp", "gif")) {
                stop("file should be a jpeg, jpg, png, bmp or gif file!")
            }
            base64 = base64enc::base64encode(figPath)
            base64 = paste0("data:image/", figClass, ";base64,", 
                            base64)
        }
        else {
            base64 = NULL
        }
        weightFactor = size * 180/max(dataOut$freq)
        settings <- list(word = dataOut$name, freq = dataOut$freq, 
                         fontFamily = fontFamily, fontWeight = fontWeight, color = color, 
                         minSize = minSize, weightFactor = weightFactor, backgroundColor = backgroundColor, 
                         gridSize = gridSize, minRotation = minRotation, maxRotation = maxRotation, 
                         shuffle = shuffle, rotateRatio = rotateRatio, shape = shape, 
                         ellipticity = ellipticity, figBase64 = base64, hover = htmlwidgets::JS(hoverFunction))
        chart = htmlwidgets::createWidget("wordcloud2", settings, 
                                          width = widgetsize[1], height = widgetsize[2], sizingPolicy = htmlwidgets::sizingPolicy(viewer.padding = 0, 
                                                                                                                                  browser.padding = 0, browser.fill = TRUE))
        chart
    }
    
    # tweets %>% slice(1:10) %>% View
    ## 1. Dashboard Tab -------------
    ## ++ Primer fila: Recuadros -------------
    # n_topic_tweets_today <- "300 \n "
    # updateBoxValue(session, "total_today", n_topic_tweets_today)

    ## ++++ Cuadros: Tab Hoy -----------
    
    output$Tuits_hoy <- renderValueBox({
        valueBox(
            val_tuits_hoy$n_tuits[1],
            "Tuits",  color = "blue",
            icon = icon("twitter"))
    })
    
    output$Users_hoy <- renderValueBox({
        valueBox(
            val_tuits_hoy$n_usuarios[1],
            "Users",  color = "light-blue",
            icon = icon("user"))
    })
    
    output$RTs_hoy <- renderValueBox({
        valueBox(
            val_tuits_hoy$n_rts[1],
            "RTs",  color = "purple",
            icon = icon("retweet"))
    })
    
    output$Likes_hoy <- renderValueBox({
        valueBox(
            val_tuits_hoy$n_like[1],
            "Likes",  color = "orange",
            icon = icon("heart"))
    })
    
    output$Pos_hoy <- renderValueBox({
        valueBox(
            paste0("57", "%"),
            "Positivo",  color = "green",
            icon = icon("plus-circle"))
    })
    
    output$Neg_hoy <- renderValueBox({
        valueBox(
            paste0("43", "%"),
            "Negativo",  color = "red",
            icon = icon("minus-circle"))
    })
    
    ## ++++ Cuadros: Tab 2 dias -----------
    output$Tuits_02d <- renderValueBox({
        valueBox(
            val_tuits_02d$n_tuits[1],
            "Tuits",  color = "blue",
            icon = icon("twitter"))
    })
    
    
    output$Users_02d <- renderValueBox({
        valueBox(
            val_tuits_02d$n_usuarios[1],
            "Users",  color = "light-blue",
            icon = icon("user"))
    })
    output$RTs_02d <- renderValueBox({
        valueBox(
            val_tuits_02d$n_rts[1],
            "RTs",  color = "purple",
            icon = icon("retweet"))
    })
    output$Likes_02d <- renderValueBox({
        valueBox(
            val_tuits_02d$n_like[1],
            "Likes",  color = "orange",
            icon = icon("heart"))
    })
    output$Pos_02d <- renderValueBox({
        valueBox(
            paste0("57", "%"),
            "Positivo",  color = "green",
            icon = icon("plus-circle"))
    })
    
    output$Neg_02d <- renderValueBox({
        valueBox(
            paste0("43", "%"),
            "Negativo",  color = "red",
            icon = icon("minus-circle"))
    })
    
    ## ++++ Cuadros: Tab 7 dias -----------
    
    output$Tuits_07d <- renderValueBox({
        valueBox(
            val_tuits_07d$n_tuits[1],
            "Tuits",  color = "blue",
            icon = icon("twitter"))
    })
    output$Users_07d <- renderValueBox({
        valueBox(
            val_tuits_07d$n_usuarios[1],
            "Users",  color = "light-blue",
            icon = icon("user"))
    })
    output$RTs_07d <- renderValueBox({
        valueBox(
            val_tuits_07d$n_rts[1],
            "RTs",  color = "purple",
            icon = icon("retweet"))
    })
    output$Likes_07d <- renderValueBox({
        valueBox(
            val_tuits_07d$n_like[1],
            "Likes",  color = "orange",
            icon = icon("heart"))
    })
    output$Pos_07d <- renderValueBox({
        valueBox(
            paste0("57", "%"),
            "Positivo",  color = "green",
            icon = icon("plus-circle"))
    })
    
    output$Neg_07d <- renderValueBox({
        valueBox(
            paste0("43", "%"),
            "Negativo",  color = "red",
            icon = icon("minus-circle"))
    })
    
    ## ++++ Cuadros: Tab 30 dias -----------------
    
    output$Tuits_30d <- renderValueBox({
        valueBox(
            val_tuits_30d$n_tuits[1],
            "Tuits",  color = "blue",
            icon = icon("twitter"))
    })
    output$Users_30d <- renderValueBox({
        valueBox(
            val_tuits_30d$n_usuarios[1],
            "Users",  color = "light-blue",
            icon = icon("user"))
    })
    output$RTs_30d <- renderValueBox({
        valueBox(
            val_tuits_30d$n_rts[1],
            "RTs",  color = "purple",
            icon = icon("retweet"))
    })
    output$Likes_30d <- renderValueBox({
        valueBox(
            val_tuits_30d$n_like[1],
            "Likes",  color = "orange",
            icon = icon("heart"))
    })
    output$Pos_30d <- renderValueBox({
        valueBox(
            paste0("57", "%"),
            "Positivo",  color = "green",
            icon = icon("plus-circle"))
    })
    
    output$Neg_30d <- renderValueBox({
        valueBox(
            paste0("43", "%"),
            "Negativo",  color = "red",
            icon = icon("minus-circle"))
    })
    
    
    
    ## ++ Segunda a Quinta fila: Gráficos de Serie -----------------
    
    
    output$plotly_tuits_diarios <- renderPlotly({
        (tweets %>%
             mutate(
                 fecha_tuit= as_date(created_at) ) %>% 
             select(created_at, fecha_tuit ) %>% 
             group_by(fecha_tuit) %>% 
             summarise(total=n()) %>% 
             ggplot( aes(x= fecha_tuit, y= total) )  +
             geom_line() +
             theme_light() +
             labs(title = "Tuits por dia",  x= "Fecha", y= "Total" )) %>% 
            ggplotly()
    })
    
    output$plotly_tuits_diarios_sent <- renderPlotly({
         
        (score %>%
             mutate(
                 fecha_tuit= as_date(created_at.y)) %>%
             group_by(fecha_tuit, sentimiento) %>% 
             summarise(total=n()) %>% 
             mutate(Y= ifelse(sentimiento== "positivo", total, -total)) %>% 
             # head(5) %>% View
             ggplot( aes(x= fecha_tuit, y= Y) )  +
             geom_area( aes(fill= sentimiento), stat = "identity") +
             theme_light() +
             theme(legend.position = "top") +
             labs(title = "Total sentimiento por dia",  x= "Fecha", y= "Total" )) %>% 
            ggplotly() %>%
            layout(
                legend = list(orientation = "h")
            )
        
    })
    
    output$plotly_users_diarios <- renderPlotly({
    (tweets %>%
            mutate(
                fecha_tuit= as_date(created_at) ) %>% 
            group_by(user_id, fecha_tuit) %>% 
            summarise(total_tuits_user=n()) %>% 
            group_by(fecha_tuit) %>% 
            summarise(total=n()) %>% 
            ggplot( aes(x= fecha_tuit, y= total) )  +
            geom_line() +
            theme_light() +
            labs(title = "users por dia",  x= "Fecha", y= "Total" )) %>% 
        ggplotly()
    })
    
    
    ## 2. Tops ------------
    ## ++ Barras de Tops -----------
    # output$top_users <- renderPlotly({
    #     top_users 
    # })
    output$top_users <- renderPlot({
        top_users
    })
    
    output$top_mencionados <- renderPlot({
        top_mencionados  
    }) 
    
    output$top_hashtags <- renderPlot({
        top_hashtags 
    }) 
    
    output$top_words <- renderPlot({
        top_words 
    }) 
    ## ++ Mostrar tuits -----------
    
    output$tuit_most_rt_7d <- renderUI({
        tagList(
            tags$blockquote(class = "twitter-tweet",
                            tags$a(href = tw_most_rt_7d$status_url[1])),
            tags$script('twttr.widgets.load(document.getElementById("tweet"));')
        )
    })
    
    output$tuit_most_like_7d <- renderUI({
        tagList(
            tags$blockquote(class = "twitter-tweet",
                            tags$a(href = tw_most_likes_7d$status_url[1])),
            tags$script('twttr.widgets.load(document.getElementById("tweet"));')
        )
    })
    
    output$tuit_most_interac_30d <- renderUI({
        tagList(
            tags$blockquote(class = "twitter-tweet",
                            tags$a(href = tw_most_interac_30d$status_url[1])),
            tags$script('twttr.widgets.load(document.getElementById("tweet"));')
        )
    })
    
    # output$tuit_most_rt_7d <- 
    #     
    #     renderUI({
    #         tw_most_rt_7d %>%
    #             purrr::pmap(get_tweet_blockquote) %>%
    #             .[[1]] %>%
    #             HTML()
    #     })
    
    # output$tuit_most_like_7d <- renderUI({
    #     tw_most_likes_7d %>%
    #         purrr::pmap(get_tweet_blockquote) %>%
    #         .[[1]] %>%
    #         HTML()
    # })

    
    # output$tuit_most_interac_30d <- renderUI({
    #     tw_most_interac_30d %>%
    #         purrr::pmap(get_tweet_blockquote) %>%
    #         .[[1]] %>%
    #         HTML()
    # })
    
    ## 3. Analizar Sentimiento ----------------
            
    ## ++ Wordcluod Completo -----------
    output$wordcloud <- renderWordcloud2({
        
        wordcloud2a(
            frecuencia_palabras[ , c("word", "n_tuits")],
            size = 0.75, shape = "circle", ellipticity = 0.65)
    })
    
    output$wordcloud_posi <- renderWordcloud2({
        
        wordcloud2a(
            frecuencia_palabras[ , c("word", "n_tuits")],
            size = 0.75, shape = "circle", ellipticity = 0.65)
    })
    output$wordcloud_nega <- renderWordcloud2({
        
        wordcloud2a(
            frecuencia_palabras[ , c("word", "n_tuits")],
            size = 0.75, shape = "circle", ellipticity = 0.65)
    })
    
}