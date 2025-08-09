# deploy_dashboard.R

rsconnect::setAccountInfo(name='mateo-rodriguez-23',
                          token='FDCF1166EA62F6DCBB2CA03403B30A8F',
                          secret='WRyIeVi5MteJfRKSV/NI4/CKewyvdxnhtbTHu0HE')


rsconnect::deployApp(
  appDir = "/Users/mateorodriguez/Desktop/analisis_CA/dashboard_cargas",
  appName = "dashboard_cargas",
  account = "mateo-rodriguez-23",
  forceUpdate = TRUE
)

