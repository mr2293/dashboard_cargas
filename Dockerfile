# Use official Rocker image for Shiny
FROM rocker/shiny:4.2.1

# Install system dependencies needed for R packages
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgit2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libjpeg-dev \
    libtiff5-dev \
    && rm -rf /var/lib/apt/lists/*

# Install required R packages in a single line
RUN R -e "install.packages(c('tidyverse','zoo','reshape2','gt','ggrepel','lubridate','readxl','dplyr','shiny','plotly','shinythemes','bslib','reactable','tibble','stringr','rsconnect','data.table','DT','curl','xml2'), repos='https://cloud.r-project.org')"

# Set working directory
WORKDIR /home/dashboard_cargas

# Copy app files
COPY app.R dashboard.R deploy.R dashboard_cargas.Rproj ./

# Copy folders
COPY data /home/dashboard_cargas/data
COPY micros /home/dashboard_cargas/micros
COPY www/player_images /home/dashboard_cargas/www/player_images
COPY rsconnect /home/dashboard_cargas/rsconnect

# Default command to run the deploy script
CMD ["Rscript", "deploy.R"]
