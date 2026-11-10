FROM rocker/r-ver:4.6.2

RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    libcairo2-dev \
    libcurl4-openssl-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libfribidi-dev \
    libharfbuzz-dev \
    libjpeg-dev \
    libmagick++-dev \
    libpng-dev \
    libssl-dev \
    libtiff-dev \
    libudunits2-dev \
    libxml2-dev \
  && rm -rf /var/lib/apt/lists/*

RUN R -e 'install.packages(c(
  "shiny", "tidyverse", "shinyWidgets", "lubridate", "readxl", "ggplot2",
  "htmltools", "flextable", "htmlwidgets", "shinydashboard", "dplyr", "DT",
  "shinyjs", "reactable", "glue", "magick", "googledrive", "httr",
  "jsonlite", "cpp11", "progress"
), repos = "https://cloud.r-project.org")'

WORKDIR /app
COPY dreamleague/ /app/

EXPOSE 10000

CMD ["R", "-e", "shiny::runApp('.', host = '0.0.0.0', port = as.integer(Sys.getenv('PORT', '10000')))"]
