# Aqui se calcular las funciones que se llaman luego en los outputs del server

## Preeliminares --------------
## ++ Parámetros -------------

# Aqui debería ir cosas como la fecha por ejemplo

## ++ Objeto tweets -----------

tweets <- readRDS('guayaquil.rds')
tweets <- tweets %>% filter(screen_name!='MarioAspiazu')
tweets <- tweets %>% filter( !(str_detect(text, "prepago") | 
                                 str_detect(text, "scort") | 
                                 str_detect(text, "gay") | 
                                 str_detect(text, "hot") ) 
                             )
# min(tweets$created_at)

score <- readRDS('score.rds')
sentiment_afinn_tbl <- readRDS('sentiment_afinn_tbl.rds')
sentiment_nrc_tbl <- readRDS("sentiment_nrc_tbl.rds")

textostop_es <- tm::stopwords(kind="es") #stopwords de la librer?a TM para tener en espa?ol

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
    T_30dias = if_else(fecha_tuit >= ymd("2020-10-01"), "Si", "No")
  ) 

tweets <- tweets %>%
  mutate(
    fecha_tuit= as_date(created_at) ) %>%
  left_join( df_fechas, by = "fecha_tuit" ) 


score <- score %>%
  mutate(
    fecha_tuit= as_date(created_at) ) %>%
  left_join( df_fechas, by = "fecha_tuit" ) 

## ++ Objeto score ---------------------

# traduccionGlasso <- readRDS("traduccionGlasso.rds")
# traduccionGlasso <- traduccionGlasso %>% select(word, translatedContent ) %>% unique()
# 
# textostop <- tm::stopwords(kind="es") #stopwords de la librer?a TM para tener en espa?ol
# textostop <- as_tibble(textostop) # lo convierto a tibble
# 
# tweets_tokenized_tbl <- tweets %>%
#   select(screen_name, status_id, text, created_at) %>%
#   rowid_to_column() %>%
#   tidytext::unnest_tokens(word, text)
# 
# tweet3 <- inner_join(traduccionGlasso, tweets_tokenized_tbl, by="word")
# 
# tweet3 <- tweet3 %>%
#   select(screen_name, status_id, created_at, translatedContent, word)
# tweet3 <- unique(tweet3)
# 
# 
# tweet3 <- tweet3 %>% rename( "token"= "translatedContent", "palabra"= "word" )
# # colnames(tweet3)
# # colnames(tweet3)[4] = "token"
# 
# tweet3 <- tweet3 %>%
#   rowid_to_column(var = "tokenid") 
# # tweet3 %>% View('tweet3')
# 
# tweets_tokenized <- tweet3 %>%
#   # rowid_to_column() %>%
#   tidytext::unnest_tokens(word, token)
# # tweets_tokenized %>% View('tweets_tokenized')
# 
# sentiment_afinn_tbl <- tweets_tokenized %>%
#   inner_join(get_sentiments("afinn"))
# # sentiment_afinn_tbl %>% View('sentiment_afinn_tbl')
# 
# 
# score <- sentiment_afinn_tbl %>%
#   group_by(created_at, screen_name, status_id) %>%
#   summarise(score = sum(value))
# 
# score <- score %>% mutate(sentimiento = if_else(score >=0, "positivo", "negativo"))
# filas = nrow(score)
# 
# write_rds(score, "score.rds")
# write_rds(sentiment_afinn_tbl, "sentiment_afinn_tbl.rds")


## ++ Objeto sentiment_nrc_tbl (ggjoy)  ------------

# #textostop <- tm::stopwords(kind="es") #stopwords de la librer?a TM para tener en espa?ol
# textostop <- tm::stopwords(kind="en") #stopwords de la librer?a TM para tener en espa?ol
# textostop <- as_tibble(textostop) # lo convierto a tibble
# 
# ## cambio de nombre a words la columna ##
# colnames(textostop)
# colnames(textostop)[1] <- "word"
# 
# dic_nrc <- get_sentiments("nrc")
# colnames(dic_nrc)
# colnames(dic_nrc)[1] <- "word"
# 
# sentiment_nrc_tbl <- tweets_tokenized_tbl %>%
#   inner_join(dic_nrc, by = "word")
# # sentiment_nrc_tbl %>% View
# 
# sentiment_nrc_tbl <- sentiment_nrc_tbl %>%
#   anti_join(textostop, by = "word")
# 
# write_rds(sentiment_nrc_tbl, "sentiment_nrc_tbl.rds")


## 1. Dashboard Tab -------------

## ++ Primer fila: Recuadros -------------

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


score_tuits_hoy <- score %>%
  group_by(Hoy) %>% 
  filter(Hoy== "Si") %>% 
  group_by(sentimiento) %>% 
  summarise(
    n_tuits= n()
  ) %>% 
  ungroup() %>% 
  mutate(
    total= sum(n_tuits),
    porcentaje= n_tuits/total
  ) %>% 
  filter(sentimiento== 'positivo')
  

score_tuits_02d <- score %>%
  group_by(T_02dias) %>% 
  filter(T_02dias== "Si") %>% 
  group_by(sentimiento) %>% 
  summarise(
    n_tuits= n()
  ) %>% 
  ungroup() %>% 
  mutate(
    total= sum(n_tuits),
    porcentaje= n_tuits/total
  ) %>% 
  filter(sentimiento== 'positivo')

score_tuits_07d <- score %>%
  group_by(T_07dias) %>% 
  filter(T_07dias== "Si") %>% 
  group_by(sentimiento) %>% 
  summarise(
    n_tuits= n()
  ) %>% 
  ungroup() %>% 
  mutate(
    total= sum(n_tuits),
    porcentaje= n_tuits/total
  )  %>% 
  filter(sentimiento== 'positivo')

score_tuits_30d <- score %>%
  group_by(T_30dias) %>% 
  filter(T_30dias== "Si") %>% 
  group_by(sentimiento) %>% 
  summarise(
    n_tuits= n()
  ) %>% 
  ungroup() %>% 
  mutate(
    total= sum(n_tuits),
    porcentaje= n_tuits/total
  ) %>% 
  filter(sentimiento== 'positivo')



## 2. Tops ------------
## ++ Barras de Tops -----------

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
    mutate(mentions_screen_name= paste0("@", tolower(mentions_screen_name))) %>% 
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
    filter(
      !hashtags %in% c("#guayaquil", "#ecuador", "#gye")
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


frecuencia_palabras_completo <- tweets %>%
  select(screen_name, status_id, text, created_at, retweet_count, favorite_count) %>%
  mutate(
    text = str_remove_all(text, "@[[:alnum:]_]+\\b"),
    text = str_remove_all(text, "&\\w+;")
  ) %>%
  tidytext::unnest_tokens(word, text) %>% 
  mutate(word= tolower(word) ) %>% 
  filter(
    !word %in% c("http", "https", "t.co"),
    !word %in% textostop_es,
    !word %in% c("guayaquil", "ecuador", "gye"),
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
  filter( n_tuits> 5 )

frecuencia_palabras <- frecuencia_palabras_completo  %>% 
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



## ++ Mostrar tuits -----------

tw_most_interac_02d <- tweets %>%
  filter(T_02dias== "Si") %>% 
  slice_max(order_by = retweet_count, n= 1) 


tw_most_interac_07d <- tweets %>%
  filter(T_02dias== "No" & T_07dias== "Si" ) %>% 
  slice_max(order_by = favorite_count, n= 1) 


tw_most_interac_30d <- tweets %>%
  filter(T_07dias== "No" & T_30dias== "Si") %>% 
  mutate(total= retweet_count + favorite_count) %>% 
  slice_max(order_by = favorite_count, n= 1) 


## 3. Analizar Sentimiento ----------------

## ++ Grafico Sentimientos historia -----------------
sentimientos_tiempo <- sentiment_nrc_tbl %>% 
  ggplot() +
  geom_density_ridges(aes(
    x = created_at,
    y = sentiment, 
    fill = sentiment),
    rel_min_height = 0.01,
    alpha = 0.7,
    scale = 3) +
  theme_light() +
  labs(title = "Sentimiento en los posteos en el tiempo",
       x = "Fecha",
       y = "Sentimento") + 
  scale_fill_discrete(guide=FALSE) 


## ++ Grafico treemap Sentimientos -----------------
sentimientos_tree <- sentiment_nrc_tbl %>% 
  group_by(sentiment) %>% 
  summarise( Frec= n()) %>%
  arrange(-Frec) %>% 
  mutate(
    Prop= round( Frec/ sum(Frec) * 100, 2) ,
    PosLab= cumsum(Prop) - 0.6 * Prop,
    Label=  paste0(sentiment, "\n ", Prop, "%")
  ) %>% 
  ggplot( aes(area= Prop, fill= sentiment)) +
  geom_treemap() +
  geom_treemap_text(aes(label= Label), place = "centre", grow = F) +
  theme( legend.position = "None")


## ++ Grafico frecuencia Sentimientos -----------------

sentimientos_frec <- sentiment_nrc_tbl %>% 
  group_by(sentiment) %>% 
  summarise( Frec= n()) %>%
  arrange(-Frec) %>% 
  mutate(
    Prop=  Frec/ sum(Frec)  
  ) %>% 
  ggplot(aes(x= reorder(sentiment, Prop), y= Prop)) +
  geom_bar(aes(fill=sentiment), stat= "identity") + 
  coord_flip() +
  theme_light() +
  labs(title= 'Sentimientos mostrados', y= "", x= "")+
  scale_y_continuous( label= percent_format() ) +
  theme( legend.position = "None")



## ++ Wordcluod por sentimiento -----------

frec_palabras_posit <- sentiment_afinn_tbl %>% 
  select(palabra, value) %>% 
  unique() %>% 
  inner_join(frecuencia_palabras_completo, by= c("palabra"="word")) %>% 
  filter(value>0) %>%
  slice_max(order_by = n_tuits, n= 200)  

frec_palabras_negat <- sentiment_afinn_tbl %>% 
  select(palabra, value) %>% 
  unique() %>% 
  inner_join(frecuencia_palabras_completo, by= c("palabra"="word")) %>% 
  filter(value<0) %>%
  slice_max(order_by = n_tuits, n= 200)  




## Funciones ----------
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
