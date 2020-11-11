FROM rocker/shiny-verse:4.0.1

#  Install system libraries
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

COPY app/* /srv/shiny-server/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/', host = '0.0.0.0', port = 3838)"]