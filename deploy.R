# deploy.R
library(rsconnect)

# Authenticate using environment variables set by GitHub Actions
setAccountInfo(
  name = Sys.getenv("'mateo-rodriguez-23'"),
  token = Sys.getenv("'0762967B3C31FB026B0220514A94A22E'"),
  secret = Sys.getenv("'dAq9rh18omtRHu1jLxufOqJ41j0s8eZvrFxwMeVU'")
)

# Deploy the app
deployApp(appDir = "/Users/mateorodriguez/Desktop/analisis_CA/dashboard_cargas/t",
          appName = "dashboard_cargas",
          account = Sys.getenv("'mateo-rodriguez-23'"),
          forceUpdate = TRUE)
