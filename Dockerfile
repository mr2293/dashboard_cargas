# Use the official Rocker image for Shiny
FROM rocker/shiny:4.2.1

# Install R dependencies for the Shiny app
RUN install2.r --error \
    tidyverse zoo reshape2 gt ggrepel lubridate readxl dplyr \
    shiny plotly shinythemes rsconnect bslib reactable tibble stringr

# Set the working directory inside the container
WORKDIR /home/dashboard_cargas

# Copy the app files
COPY app.R dashboard.R deploy.R dashboard_cargas.Rproj ./

# Copy folders
COPY data /home/dashboard_cargas/data
COPY micros /home/dashboard_cargas/micros
COPY www/player_images /home/dashboard_cargas/www/player_images
COPY rsconnect /home/dashboard_cargas/rsconnect

# Run deploy.R when container starts
CMD ["Rscript", "deploy.R"]

