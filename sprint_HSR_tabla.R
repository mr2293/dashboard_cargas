library(dplyr)
library(lubridate)
library(readr)
library(writexl)

micros_shiny_comb <- read_csv("micros/micros_shiny_comb.csv")
micros_shiny_comb$date <- as.Date(micros_shiny_comb$date)

selected_players <- c(
  "Israel Reyes","Henry Martín","Alejandro Zendejas",
  "Isaías Violante","Alan Cervantes","Ramón Juárez","Erick Sánchez",
  "Brian Rodríguez","Kevin Álvarez","Dagoberto Espinoza","Víctor Dávila",
  "Cristian Borja","Alexis Gutiérrez","Néstor Araujo",
  "Sebastián Cáceres","Miguel Vázquez","Ralph Orquín",
  "Jonathan Dos Santos","Santiago Naveda","José Raúl Zúñiga",
  "Patricio Salas", "Rodrigo Dourado", "Aaron Mejia", "Raphael Veiga",
  "Vinícius Lima", "Thiago Espinosa"
)

hsr_dist_tabla <- micros_shiny_comb |>
  filter(player %in% selected_players) |>
  filter(month(date) %in% c(2, 3), match_day != "MD") |>
  group_by(player, date) |>
  summarize(
    match_day    = first(match_day),
    HSR_abs_dist = sum(HSR_abs_dist, na.rm = TRUE),
    distance_abs = sum(distance_abs, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(player, date) |>
  rename(
    Jugador                = player,
    Fecha                  = date,
    `Sesión`               = match_day,
    `Distancia HSR (m)`    = HSR_abs_dist,
    `Distancia Sprint (m)` = distance_abs
  )

write_xlsx(hsr_dist_tabla, path = "/Users/mateorodriguez/Desktop/hsr_dist_feb_mar_2026.xlsx")
