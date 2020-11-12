FROM rocker/shiny-verse:4.0.1 
# Imagen Base ( DEBIAN + shiny + rserver + tidyverse - dockerhub lo encuentro)

#Construyendo el plano del contenedor

# Install system libraries
# Paquetes necesarios para correr a nivel de sistema la imagen
# Documentación encontrada en internet
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libxml2-dev \
    libpq-dev \
    libssh2-1-dev \
    unixodbc-dev \
    libcurl4-openssl-dev \
    libssl-dev

# Update system libraries
RUN apt-get update && \
    apt-get clean

# Download and install libraries
RUN R -e "install.packages(c('shinydashboard', 'shinycssloaders', 'rtweet', 'wordcloud2'))"
RUN R -e "install.packages(c('tidytext', 'textdata', 'plotly', 'ggthemes', 'magrittr', 'stringr'))"
RUN R -e "install.packages(c('purrr', 'tm', 'treemapify', 'ggridges', 'shinyWidgets'))"

COPY app/ /srv/shiny-server/
#copiar directio app -> srv/shiny-server/
#el dockerfile tiene que estar al mismo nivel de la carpeta donde estén los archivos

EXPOSE 3838
#puerto que se expone para correr la aplicacion

CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/', host = '0.0.0.0', port = 3838)"]