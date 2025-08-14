# deploy.R
library(rsconnect)

# Authenticate using environment variables set by GitHub Actions
setAccountInfo(
  name = Sys.getenv("SHINY_ACC_NAME"),
  token = Sys.getenv("TOKEN"),
  secret = Sys.getenv("SECRET")
)

# Deploy the app from the current working directory
deployApp(
  appDir = ".",  # Current working directory in Docker/GitHub Actions
  appName = "dashboard_cargas",
  account = Sys.getenv("SHINY_ACC_NAME"),
  forceUpdate = TRUE
)

