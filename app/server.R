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
library(ggridges)
library(treemapify)

source('calculos.R', local = TRUE)
tweets <- readRDS('guayaquil.rds')

server <- function(input, output) {
    
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
            "Users",  color = "orange",
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
            "Likes",  color = "light-blue",
            icon = icon("heart"))
    })
    
    output$Pos_hoy <- renderValueBox({
        valueBox(
            paste0(round(score_tuits_hoy$porcentaje[1] *100, 0), "%"),
            "Positivo",  color = "green",
            icon = icon("plus-circle"))
    })
    
    output$Neg_hoy <- renderValueBox({
        valueBox(
            paste0( 100 - round(score_tuits_hoy$porcentaje[1] *100, 0), "%"),
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
            "Users",  color = "orange",
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
            "Likes",  color = "light-blue",
            icon = icon("heart"))
    })
    output$Pos_02d <- renderValueBox({
        valueBox(
            paste0(round(score_tuits_02d$porcentaje[1] *100, 0), "%"),
            "Positivo",  color = "green",
            icon = icon("plus-circle"))
    })
    
    output$Neg_02d <- renderValueBox({
        valueBox(
            paste0( 100 - round(score_tuits_02d$porcentaje[1] *100, 0), "%"),
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
            "Users",  color = "orange",
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
            "Likes",  color = "light-blue",
            icon = icon("heart"))
    })
    output$Pos_07d <- renderValueBox({
        valueBox(
            paste0(round(score_tuits_07d$porcentaje[1] *100, 0), "%"),
            "Positivo",  color = "green",
            icon = icon("plus-circle"))
    })
    
    output$Neg_07d <- renderValueBox({
        valueBox(
            paste0( 100 - round(score_tuits_07d$porcentaje[1] *100, 0), "%"),
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
            "Users",  color = "orange",
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
            "Likes",  color = "light-blue",
            icon = icon("heart"))
    })
    output$Pos_30d <- renderValueBox({
        valueBox(
            paste0(round(score_tuits_30d$porcentaje[1] *100, 0), "%"),
            "Positivo",  color = "green",
            icon = icon("plus-circle"))
    })
    
    output$Neg_30d <- renderValueBox({
        valueBox(
            paste0( 100 - round(score_tuits_30d$porcentaje[1] *100, 0), "%"),
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
             labs(x= "Fecha", y= "Total" )) %>% 
            ggplotly()
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
             labs( x= "Fecha", y= "Total" )) %>% 
            ggplotly()
    })
        
    output$plotly_tuits_diarios_sent <- renderPlotly({
        (score %>%
             mutate(
                 fecha_tuit= as_date(created_at)) %>%
             group_by(fecha_tuit, sentimiento) %>% 
             summarise(total=n()) %>% 
             mutate(Y= ifelse(sentimiento== "positivo", total, -total)) %>% 
             # head(5) %>% View
             ggplot( aes(x= fecha_tuit, y= Y) )  +
             geom_area( aes(fill= sentimiento), stat = "identity") +
             theme_light() +
             theme(legend.position = "top") +
             labs(x= "Fecha", y= "Total" )) %>% 
            ggplotly() %>%
            layout(
                legend = list(orientation = "h")
            )
        
    })
    
    output$plotly_tuits_diarios_sent_fill <- renderPlotly({
        (score %>%
             mutate(
                 fecha_tuit= as_date(created_at)) %>%
             group_by(fecha_tuit, sentimiento) %>% 
             summarise(total=n()) %>% 
             mutate(Y= ifelse(sentimiento== "positivo", total, total)) %>%
             # head(5) %>% View
             ggplot( aes(x= fecha_tuit, y= Y) )  +
             geom_area( aes(fill= sentimiento), stat = "identity", position= "fill") +
             theme_light() +
             theme(legend.position = "none") +
             scale_y_continuous(labels = scales::percent)+
             labs(  x= "Fecha", y= "Total" )) %>% 
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
    
    output$tw_most_interac_02d <- renderUI({
        tagList(
            tags$blockquote(class = "twitter-tweet",
                            tags$a(href = tw_most_interac_02d$status_url[1])),
            tags$script('twttr.widgets.load(document.getElementById("tweet"));')
        )
    })
    
    output$tw_most_interac_07d <- renderUI({
        tagList(
            tags$blockquote(class = "twitter-tweet",
                            tags$a(href = tw_most_interac_07d$status_url[1])),
            tags$script('twttr.widgets.load(document.getElementById("tweet"));')
        )
    })
    
    output$tw_most_interac_30d <- renderUI({
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
    
    ## ++ Sentimiento -----------
    
    output$sentimientos <- renderPlot({
        sentimientos_tiempo
        # sentimientos_frec
        # sentimientos_tree
    })
    
    # ## ++ Wordclouds por sentimiento -----------
    
    output$wordcloud_posi <- renderWordcloud2({
        
        wordcloud2a(
            frec_palabras_posit[ , c("palabra", "n_tuits")],
            size = 0.75, shape = "circle", ellipticity = 0.65)
    })
    
    output$wordcloud_nega <- renderWordcloud2({
        
        wordcloud2a(
            frec_palabras_negat[ , c("palabra", "n_tuits")],
            size = 0.75, shape = "circle", ellipticity = 0.65)
    })
    
}

