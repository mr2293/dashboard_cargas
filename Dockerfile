# Use the official Rocker image for Shiny
FROM rocker/shiny:4.2.1

# Install R dependencies for the Shiny app
RUN R -e "install.packages(c('tidyverse','zoo','reshape2','gt','ggrepel','lubridate','readxl','dplyr','shiny','plotly','shinythemes','rsconnect','bslib','reactable','tibble','stringr'), repos='https://cloud.r-project.org/')"

# Set the working directory inside the container
WORKDIR /home/dashboard_cargas

# Copy the app files into the Docker container
COPY app.R app.R
COPY dashboard.R dashboard.R
COPY dashboard_cargas.Rproj dashboard_cargas.Rproj
COPY deploy.R deploy.R

# Copy the data and micros directories
COPY data /home/dashboard_cargas/data
COPY micros /home/dashboard_cargas/micros

# Copy the player_images folder
COPY www/player_images /home/dashboard_cargas/www/player_images

# Copy the rsconnect directory
COPY rsconnect /home/dashboard_cargas/rsconnect

# Run deploy.R when container starts
CMD ["Rscript", "deploy.R"]
