# deploy.R
library(rsconnect)

# Pull credentials from environment
shiny_acc <- Sys.getenv("SHINY_ACC_NAME")
shiny_token <- Sys.getenv("TOKEN")
shiny_secret <- Sys.getenv("SECRET")

# Make sure rsconnect is using them
rsconnect::setAccountInfo(
  name = shiny_acc,
  token = shiny_token,
  secret = shiny_secret
)

# Deploy the app
rsconnect::deployApp(
  appDir = ".", 
  appName = "dashboard_cargas",
  launch.browser = FALSE
)
