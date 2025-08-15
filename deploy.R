# deploy.R
library(rsconnect)

shiny_acc <- Sys.getenv("SHINY_ACC_NAME")
shiny_token <- Sys.getenv("TOKEN")
shiny_secret <- Sys.getenv("SECRET")

cat("Shiny account:", shiny_acc, "\n")
cat("Token length:", nchar(shiny_token), "\n")
cat("Secret length:", nchar(shiny_secret), "\n")

rsconnect::setAccountInfo(
  name = shiny_acc,
  token = shiny_token,
  secret = shiny_secret
)

rsconnect::deployApp(
  appDir = ".",
  appName = "dashboard_cargas",
  launch.browser = FALSE
)
