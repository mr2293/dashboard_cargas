# Use the official Rocker image for Shiny
FROM rocker/shiny:4.2.1

# Install system dependencies for R packages
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R dependencies inside Docker
RUN R -e "install.packages(c(
      'tidyverse', 'zoo', 'reshape2', 'gt', 'ggrepel',
      'lubridate', 'readxl', 'dplyr', 'shiny', 'plotly',
      'shinythemes', 'rsconnect', 'bslib', 'reactable', 'tibble', 'stringr'
    ), repos='https://cloud.r-project.org')"

# Set the working directory inside the container
WORKDIR /home/dashboard_cargas

# Copy app files
COPY app.R dashboard.R dashboard_cargas.Rproj deploy.R ./

# Copy the data and micros directories
COPY data /home/dashboard_cargas/data
COPY micros /home/dashboard_cargas/micros

# Copy the player_images folder
COPY www/player_images /home/dashboard_cargas/www/player_images

# Copy the rsconnect directory
COPY rsconnect /home/dashboard_cargas/rsconnect

# Run deploy.R when container starts
CMD ["Rscript", "deploy.R"]
