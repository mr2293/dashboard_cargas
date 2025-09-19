library(tidyverse)
library(zoo)
library(reshape2)
library(gt)
library(ggrepel)
library(lubridate)
library(readxl)
library(dplyr)

# Leer Cuestionario de Bienestar de Jugadores ---------
survey_path <- "data/bienestar_jugador_primer_equipo_respuestas.xlsx"

recuperacion_df <- read_xlsx(survey_path)

# Darle Puntuación a Recuperación de Jugadores -------------

compute_recovery_score <- function(fatigue, sleep_quality, sleep_hours, soreness, pain_zone) {
  score <- 0
  
  # Fatigue scoring
  score <- score + case_when(
    fatigue == "Muy fresco" ~ 2.5,
    fatigue == "Fresco" ~ 2.2,
    fatigue == "Mejor que lo normal" ~ 1.8,
    fatigue == "Normal" ~ 1.5,
    fatigue == "Peor que lo normal" ~ 1.2,
    fatigue == "Cansado" ~ 0.8,
    fatigue == "Muy cansado" ~ 0.5,
    TRUE ~ 0
  )
  
  # Sleep quality
  score <- score + case_when(
    sleep_quality == "Muy buena noche" ~ 2.5,
    sleep_quality == "Buena noche" ~ 2.2,
    sleep_quality == "Mejor que normal" ~ 1.8,
    sleep_quality == "Normal" ~ 1.5,
    sleep_quality == "Peor que normal" ~ 1.2,
    sleep_quality == "Mala noche" ~ 0.8,
    sleep_quality == "Muy mala noche" ~ 0.5,
    sleep_quality == "Me desperté mucho" ~ 0.5,
    TRUE ~ 0
  )
  
  # Sleep duration
  score <- score + case_when(
    sleep_hours == "Más de 8" ~ 2.5,
    sleep_hours == "6 a 8" ~ 2,
    sleep_hours == "Menos de 6" ~ 1,
    TRUE ~ 0
  )
  
  # Pain
  score <- score + case_when(
    soreness == "Normal" ~ 2.5,
    soreness == "No me duele nada" ~ 2,
    soreness == "Adolorido de una zona" ~ 1.5,
    soreness == "Muy adolorido en general" ~ 1,
    
    TRUE ~ 0
  )
  
  return(round(score, 1))  # Max = 10
}

compute_rest_score <- function(fatigue, sleep_quality, sleep_hours) {
  score <- 0
  
  # Fatigue scoring
  score <- score + case_when(
    fatigue == "Muy fresco" ~ 5,
    fatigue == "Fresco" ~ 4.5,
    fatigue == "Mejor que lo normal" ~ 3.5,
    fatigue == "Normal" ~ 2.5,
    fatigue == "Peor que lo normal" ~ 2,
    fatigue == "Cansado" ~ 1.5,
    fatigue == "Muy cansado" ~ 1,
    TRUE ~ 0
  )
  
  # Sleep quality
  score <- score + case_when(
    sleep_quality == "Muy buena noche" ~ 2.5,
    sleep_quality == "Buena noche" ~ 2.2,
    sleep_quality == "Mejor que normal" ~ 1.8,
    sleep_quality == "Normal" ~ 1.5,
    sleep_quality == "Peor que normal" ~ 1.2,
    sleep_quality == "Mala noche" ~ 0.8,
    sleep_quality == "Muy mala noche" ~ 0.5,
    sleep_quality == "Me desperté mucho" ~ 0.5,
    TRUE ~ 0
  )
  
  # Sleep duration
  score <- score + case_when(
    sleep_hours == "Más de 8" ~ 2.5,
    sleep_hours == "6 a 8" ~ 2,
    sleep_hours == "Menos de 6" ~ 1,
    TRUE ~ 0
  )
  
  return(round(score, 1))  # Max = 10
}

compute_pain_score <- function(soreness) {
  score <- 0
  
  # Pain
  score <- score + case_when(
    soreness == "Normal" ~ 4,
    soreness == "No me duele nada" ~ 5,
    soreness == "Adolorido de una zona" ~ 6,
    soreness == "Muy adolorido en general" ~ 8,
    
    TRUE ~ 0
  )
  
  return(round(score, 1))  # Max = 10
}

recuperacion_df <- recuperacion_df|>
  mutate(recovery_score = compute_recovery_score(
    `Nivel de Cansancio hoy (Fatiga)`,
    `Qué tal descansaste ayer?`,
    `Cuántas horas dormiste ayer?`,
    `Estás adolorido de alguna parte?`,
    `Donde te encuentras adolorido? Indica cada zona de dolor`
  )) |>
  mutate(rest_score = compute_rest_score(
    `Nivel de Cansancio hoy (Fatiga)`,
    `Qué tal descansaste ayer?`,
    `Cuántas horas dormiste ayer?`)) |>
  mutate(pain_score = compute_pain_score(
    `Estás adolorido de alguna parte?`))

# Gráficas Sueño, Fatiga, Dolor Muscular -----------

recuperacion_df <- recuperacion_df |>
  mutate(`Marca temporal` = as.Date(`Marca temporal`))

# Scoring definitions
niveles_fatiga <- c("Muy fresco" = 10, "Fresco" = 9, "Mejor que lo normal" = 8, 
                    "Normal" = 6, "Peor que lo normal" = 5, "Cansado" = 4, "Muy cansado" = 3)
niveles_sueño <- c("Muy buena noche" = 10, "Buena noche" = 9, "Mejor que normal" = 8,
                   "Normal" = 6, "Peor que normal" = 5, "Mala noche" = 4, 
                   "Muy mala noche" = 3, "Me desperté mucho" = 3)
niveles_dolor <- c("No me duele nada" = 10, "Normal" = 8,
                   "Adolorido de una zona" = 6, "Muy adolorido en general" = 4)

# Apply mappings
recuperacion_df <- recuperacion_df |> 
  mutate(
    fatiga_score = niveles_fatiga[`Nivel de Cansancio hoy (Fatiga)`],
    sueño_score = niveles_sueño[`Qué tal descansaste ayer?`],
    dolor_score = niveles_dolor[`Estás adolorido de alguna parte?`]
  )

# Function to plot by player
plot_player_recuperacion <- function(player_name) {
  
  player_df <- recuperacion_df |> 
    filter(Nombre == player_name)
  
  max_day <- max(player_df$`Marca temporal`, na.rm = TRUE)
  player_df <- player_df |> filter(`Marca temporal` >= max_day - 20)
  
  player_long <- player_df |> 
    select(`Marca temporal`, Nombre,
           `Nivel de Cansancio hoy (Fatiga)`, 
           `Qué tal descansaste ayer?`,
           `Estás adolorido de alguna parte?`,
           `Donde te encuentras adolorido? Indica cada zona de dolor`,
           fatiga_score, sueño_score, dolor_score) |> 
    rename(`Zona Adolorida` = `Donde te encuentras adolorido? Indica cada zona de dolor`) |>
    pivot_longer(
      cols = c(fatiga_score, sueño_score, dolor_score),
      names_to = "score_type", values_to = "score"
    ) |> 
    mutate(
      type = case_when(
        score_type == "fatiga_score" ~ "Fatiga",
        score_type == "sueño_score" ~ "Sueño",
        score_type == "dolor_score" ~ "Dolor Muscular"
      ),
      respuesta = case_when(
        score_type == "fatiga_score" ~ `Nivel de Cansancio hoy (Fatiga)`,
        score_type == "sueño_score" ~ `Qué tal descansaste ayer?`,
        score_type == "dolor_score" ~ `Estás adolorido de alguna parte?`
      ),
      color = case_when(
        type == "Dolor Muscular" & score >= 7 ~ "Alta",
        type == "Dolor Muscular" & score < 7 ~ "Baja",
        type != "Dolor Muscular" & score >= 6 ~ "Alta",
        type != "Dolor Muscular" & score < 6 ~ "Baja",
        TRUE ~ "Indefinido"
      )    
      )
  
  ggplot(player_long, aes(x = `Marca temporal`, y = score, group = type)) +
    geom_line(aes(color = type), linewidth = 0.6, alpha = 0.5) +
    geom_point(aes(
      fill = color,
      text = paste0(
        "Jugador: ", Nombre,
        "<br>Fecha: ", `Marca temporal`,
        "<br>Respuesta: ", respuesta,
        ifelse(type == "Dolor Muscular" & !is.na(`Zona Adolorida`),
               paste0("<br>Zona Adolorida: ", `Zona Adolorida`), ""),
        "<br>Score: ", score
      )
    ),
    shape = 21, size = 5, color = "white") +
    # geom_text(
    #   data = player_long |> filter(type == "Dolor Muscular"),
    #   aes(label = `Zona Adolorida`),
    #   vjust = -1.2, size = 3.5, color = "black"
    # ) +
    facet_wrap(~type, ncol = 1, scales = "free_y") +
    scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +
    scale_fill_manual(values = c("Alta" = "#1a9850", "Baja" = "#d7191c")) +
    scale_x_date(date_breaks = "1 day", date_labels = "%b %d") +
    labs(
      title = paste("Indicadores de Recuperación Diaria –", player_name),
      x = NULL, y = "Puntuación", subtitle = "Score Diario"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "none",
      panel.grid.major.y = element_line(color = "gray85"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      strip.text = element_text(face = "bold", size = 14),
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 14, face = "bold", hjust = 0.5)
    )
}

# Example usage:
plot_player_recuperacion("Álvaro Fidalgo")

## Plot A:C 7:21 Individual -----------

micros_shiny_comb <- read_csv("micros/micros_shiny_comb.csv")

micros_shiny_comb <- micros_shiny_comb |>
  mutate(player = case_when(
    player == "Cristian Yonathan Calderón del Real" ~ "Cristian Calderón",
    # player == "chicote Calderón del Real" ~ "Cristian Calderón",
    player == "kevin alvarez" ~ "Kevin Álvarez",
    player == "Erick Sanchez" ~ "Erick Sánchez",
    player == "Brian Rodriguez" ~ "Brian Rodríguez",
    player == "Victor Davila" ~ "Víctor Dávila",
    player == "Miguel Ramirez" ~ "Miguel Ramírez",
    player == "Miguel  Vazquez" ~ "Miguel Vázquez",
    player == "Nestor Araujo" ~ "Néstor Araujo",
    player == "Fidalgo Fidalgo" ~ "Álvaro Fidalgo",
    player == "Jona Dos Santos" ~ "Jonathan Dos Santos",
    player == "Luis Ángel Malagón Velázquez" ~ "Luis Ángel Malagón",
    player == "Alexis Gutierrez" ~ "Alexis Gutiérrez",
    player == "Sebastian Cáceres" ~ "Sebastián Cáceres",
    player == "Isaias Violante" ~ "Isaías Violante",
    player == "Jose Zuniga" ~ "José Raúl Zúñiga",
    player == "Allan Maximin" ~ "Allan Saint-Maximin",
    TRUE ~ player
  ))

selected_players <- c("Álvaro Fidalgo", 
                      "Israel Reyes",
                      "Henry Martín", "Alejandro Zendejas", "Isaías Violante", 
                      "Alan Cervantes", "Ramón Juárez", "Erick Sánchez", "Brian Rodríguez",
                      "Kevin Álvarez", "Dagoberto Espinoza", "Víctor Dávila",
                      "Rodrigo Aguirre", "Cristian Borja", "Alexis Gutiérrez", 
                      "Néstor Araujo", "Igor Lichnovsky", "Sebastián Cáceres", 
                      "Miguel Vázquez", "Ralph Orquin", "Jonathan Dos Santos", "Santiago Naveda",
                      "José Raúl Zúñiga", "Allan Saint-Maximin"
                      )

micros_individual <- micros_shiny_comb |>
  filter(player %in% selected_players)

# Parámetros de suavizado exponencial
alpha_acute <- 0.75
alpha_chronic <- 0.35

# Función para calcular media ponderada exponencial
ewma <- function(x, alpha) {
  n <- length(x)
  if (n == 0) return(NA)
  weights <- (1 - alpha)^((n - 1):0)
  weights <- weights / sum(weights)
  sum(x * weights)
}

# Función para crear aguda, crónica y ACWR
process_ewma <- function(data, col) {
  data |>
    mutate(
      !!paste0("acute_load_", col) := zoo::rollapply(
        .data[[col]],
        width = 7,
        FUN = \(x) ewma(x, alpha_acute),
        align = "right",
        fill = NA,
        partial = TRUE
      ),
      !!paste0("chronic_load_", col) := zoo::rollapply(
        .data[[paste0("acute_load_", col)]],
        width = 21,
        FUN = \(x) ewma(x, alpha_chronic),
        align = "right",
        fill = NA,
        partial = TRUE
      ),
      !!paste0("ACWR_", col) := .data[[paste0("acute_load_", col)]] / .data[[paste0("chronic_load_", col)]]
    )
}

micros_individual <- micros_individual |>
  group_by(player, date) |>
  summarize(load = sum(HSR_abs_dist, na.rm = TRUE), .groups = "drop") |>
  arrange(player, date)

micros_individual <- micros_individual |>
  group_by(player) |>
  arrange(date) |>
  group_modify(~ process_ewma(.x, "load")) |>
  ungroup()

micros_individual <- micros_individual |>
  rename(
    carga_aguda = acute_load_load,
    carga_cronica = chronic_load_load,
    ac_ratio = ACWR_load
  )

plot_individual_ac <- function(player) {
  
  micros_individual <- micros_individual |>
    mutate(date = as.Date(date))
  
  max_day <- max(micros_individual$date, na.rm = TRUE)
  filtered_data <- micros_individual |>
    filter(player == !!player & date >= max_day - 20)
  
  # --- Dynamic scaling for A:C ---
  # 1) left y-limit from loads (with headroom)
  max_load <- max(c(filtered_data$carga_aguda, filtered_data$carga_cronica), na.rm = TRUE)
  left_ylim <- if (is.finite(max_load)) max_load * 1.2 else 1
  
  # 2) right-axis range for A:C (rounded to .5, clamp sensibly)
  ac_min <- max(0.5, floor(min(filtered_data$ac_ratio, na.rm = TRUE) * 2) / 2)
  ac_max <- max(2.0, ceiling(max(filtered_data$ac_ratio, na.rm = TRUE) * 2) / 2)
  ac_max <- min(ac_max, 3.0)                      # (optional) don’t let it explode
  ac_breaks <- seq(ac_min, ac_max, by = 0.5)
  
  # 3) scaling so that ac_max sits at the top of the left axis
  scaling_factor <- left_ylim / ac_max
  
  ggplot(filtered_data, aes(x = date)) +
    # Acute Load
    geom_line(aes(y = carga_aguda, color = "Carga Aguda"), linewidth = 2) +
    geom_point(aes(y = carga_aguda, color = "Carga Aguda",
                   text = paste0("Jugador: ", player,
                                 "<br>Fecha: ", date,
                                 "<br>Carga Aguda: ", round(carga_aguda, 1))),
               size = 3.5) +
    
    # Chronic Load
    geom_line(aes(y = carga_cronica, color = "Carga Crónica"), linewidth = 2) +
    geom_point(aes(y = carga_cronica, color = "Carga Crónica",
                   text = paste0("Jugador: ", player,
                                 "<br>Fecha: ", date,
                                 "<br>Carga Crónica: ", round(carga_cronica, 1))),
               size = 3.5) +
    
    # A:C Ratio (rescaled dynamically)
    geom_line(aes(y = ac_ratio * scaling_factor, color = "Relación A:C"),
              linewidth = 2, linetype = "dashed") +
    geom_point(aes(y = ac_ratio * scaling_factor, color = "Relación A:C",
                   text = paste0("Jugador: ", player,
                                 "<br>Fecha: ", date,
                                 "<br>Relación A:C: ", round(ac_ratio, 2))),
               size = 3.5) +
    
    # Optional: show A:C guide lines at 0.8 and 1.3 on the left scale
    geom_hline(yintercept = c(0.8, 1.3) * scaling_factor,
               linetype = "dashed", color = "goldenrod", alpha = 0.35) +
    
    # Axes
    scale_y_continuous(
      limits = c(0, left_ylim),
      name   = "Nivel de Carga",
      sec.axis = sec_axis(~ . / scaling_factor,
                          name = "Relación A:C", breaks = ac_breaks)
    ) +
    scale_x_date(date_breaks = "1 day", date_labels = "%d-%b") +
    
    # (If you still want the big labels on the right edge)
    geom_text(
      data = tibble(
        x = max(filtered_data$date, na.rm = TRUE) + 0.2,
        y = ac_breaks * scaling_factor,
        label = as.character(ac_breaks)
      ),
      aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      hjust = 0, size = 5.8, fontface = "bold", color = "black"
    ) +
    
    scale_color_manual(values = c(
      "Carga Aguda"   = "#0072B2",
      "Carga Crónica" = "#4D4D4D",
      "Relación A:C"  = "goldenrod"
    )) +
    theme_minimal() +
    theme(
      axis.text.x   = element_text(angle = 45, hjust = 1, size = 14),
      axis.text.y   = element_text(size = 16),
      axis.title.y  = element_text(size = 16, margin = margin(r = 10)),
      plot.title    = element_text(hjust = 0.5, size = 22, face = "bold",
                                   margin = margin(t = 35, b = 10)),
      plot.margin   = margin(t = 25, r = 20, b = 20, l = 20),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      legend.position = "bottom",
      legend.title    = element_text(size = 14),
      legend.text     = element_text(size = 13)
    ) +
    labs(title = paste("Relación A:C 7:21 –", player), x = NULL, y = "Nivel de Carga", color = NULL)
}

plot_individual_ac("Álvaro Fidalgo")

# Plot Individual HSR -------

micros_hsr <- micros_shiny_comb |>
  filter(player %in% selected_players)

micros_hsr <- micros_hsr |>
  mutate(date = as.Date(date)) |>
  group_by(player, date) |>
  summarize(HSR_abs_dist = sum(HSR_abs_dist, na.rm = TRUE), .groups = "drop") |>
  arrange(player, date) |>
  group_by(player) |>
  group_modify(~ process_ewma(.x, "HSR_abs_dist")) |>
  ungroup() |>
  rename(
    acute_HSR = acute_load_HSR_abs_dist,
    chronic_HSR = chronic_load_HSR_abs_dist
  )

plot_individual_hsr <- function(player) {
  max_day <- max(micros_hsr$date, na.rm = TRUE)
  
  filtered_data <- micros_hsr |>
    filter(player == !!player & date >= max_day - 20)
  
  # Safeguard against empty data
  if (nrow(filtered_data) == 0) {
    return(
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = paste("No hay datos recientes para", player),
                 size = 6, color = "red") +
        theme_void()
    )
  }
  
  ggplot(filtered_data, aes(x = date)) +
    geom_line(aes(y = acute_HSR, color = "HSR Agudo"), linewidth = 2) +
    geom_point(aes(y = acute_HSR, color = "HSR Agudo",
                   text = paste0("Jugador: ", player,
                                 "<br>Fecha: ", date,
                                 "<br>HSR Agudo: ", round(acute_HSR, 1))),
               size = 3.5) +
    
    geom_line(aes(y = chronic_HSR, color = "HSR Crónico"), linewidth = 2) +
    geom_point(aes(y = chronic_HSR, color = "HSR Crónico",
                   text = paste0("Jugador: ", player,
                                 "<br>Fecha: ", date,
                                 "<br>HSR Crónico: ", round(chronic_HSR, 1))),
               size = 3.5) +
    
    scale_x_date(date_breaks = "1 day", date_labels = "%d-%b") +
    labs(
      title = paste("HSR (>21 km/h) Agudo vs. Crónico –", player),
      x = NULL, y = "HSR en metros", color = NULL,
      subtitle = "Valores de distancia de HSR en metros"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
      axis.text.y = element_text(size = 16),
      axis.title.y = element_text(size = 16, margin = margin(r = 10)),
      plot.title = element_text(hjust = 0.5, size = 22, face = "bold", margin = margin(t = 35, b = 10)),
      plot.subtitle = element_text(size = 15, face = "bold", hjust = 0.5),
      plot.margin = margin(t = 25, r = 20, b = 20, l = 20),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      legend.position = "bottom",
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 13)
    ) +
    scale_color_manual(values = c(
      "HSR Agudo" = "#0072B2",
      "HSR Crónico" = "#4D4D4D"
    ))
}

plot_individual_hsr("Álvaro Fidalgo")


# Scatter de ACWR & Recuperación -------

jugs = c("Néstor Araujo", "Brian Rodríguez", "Sebastián Cáceres", "Alan Cervantes", 
           "Rodolfo Cota", "Erick Sánchez", "Álvaro Fidalgo", "Henry Martín", "Israel Reyes",
           "Jonathan Dos Santos", "Kevin Álvarez", "Luis Ángel Malagón", "Miguel Vázquez", 
           "Ramón Juárez", "Alejandro Zendejas", "Rodrigo Aguirre", "Cristian Borja", 
           "Dagoberto Espinoza", "Víctor Dávila", "Igor Lichnovsky", "Santiago Naveda", "Ralph Orquin",
           "Alexis Gutiérrez", "Isaías Violante", "José Raúl Zúñiga", "Allan Saint-Maximin")

# Asegurarse que las fechas están en formato Date
recuperacion_df <- recuperacion_df |>
  mutate(date = as.Date(`Marca temporal`))

latest_pain <- recuperacion_df %>%
  filter(Nombre %in% jugs) %>%
  group_by(Nombre) %>%
  slice_max(order_by = date, n = 1, with_ties = FALSE) %>%
  transmute(
    player = Nombre,
    zona_adolorida = `Donde te encuentras adolorido? Indica cada zona de dolor`,
    pain_flag = !is.na(zona_adolorida) & zona_adolorida != "Nada"
  )

micros_individual <- micros_individual |>
  mutate(date = as.Date(date))

# Último ACWR por jugador
latest_acwr <- micros_individual |>
  group_by(player) |>
  filter(date == max(date, na.rm = TRUE)) |>
  select(player, ac_ratio)

# Último recovery score por jugador
latest_recovery <- recuperacion_df |>
  group_by(Nombre) |>
  filter(date == max(date, na.rm = TRUE)) |>
  select(player = Nombre, recovery_score)

latest_dates <- recuperacion_df |>
  group_by(`Nombre`) |>
  summarize(latest_date = max(date, na.rm = TRUE), .groups = "drop") |>
  rename(player = Nombre)

# Combinar datasets
scatter_df <- latest_acwr |>
  inner_join(latest_recovery, by = "player") |>
  inner_join(latest_dates,   by = "player") |>
  inner_join(latest_pain,     by = "player") |>
  mutate(
    recovery_status = if_else(recovery_score >= 6, "Recuperado", "Fatigado"),
    load_status = case_when(
      ac_ratio < 0.8 ~ "Carga Baja",
      ac_ratio > 1.3 ~ "Carga Alta",
      TRUE ~ "Carga Óptima"
    ),
    color_status = case_when(
      ac_ratio >= 0.8 & ac_ratio <= 1.3 & recovery_score >= 6 ~ "green",
      (ac_ratio >= 0.8 & ac_ratio <= 1.3 & recovery_score < 6) |
        (recovery_score >= 6 & (ac_ratio < 0.8 | ac_ratio > 1.3)) ~ "yellow",
      TRUE ~ "red"
    ),
    pain_flag = !is.na(zona_adolorida) & zona_adolorida != "Nada"
  )

acwr_scatter_plot <- ggplot(
  scatter_df,
  aes(
    x = recovery_score,
    y = ac_ratio,
    fill = color_status,
    text = paste0(
      "Jugador: ", player,
      "<br>Fecha: ", latest_date,
      "<br>Score de Recuperación: ", recovery_score,
      "<br>Índice de Carga: ", round(ac_ratio, 2),
      "<br>Estatus de Recuperación: ", recovery_status,
      "<br>Estatus de Carga: ", load_status,
      ifelse(pain_flag, paste0("<br>Zona Adolorida: ", zona_adolorida), "")
    )
  )
) +
  geom_hline(yintercept = c(0.8, 1.3), linetype = "dashed", color = "gray50") +
  # puntos base (usar shape 21 para que 'fill' funcione)
  geom_point(shape = 21, size = 6, alpha = 0.95, color = "black") +
  # anillo rojo para jugadores con dolor
  geom_point(
    data = dplyr::filter(scatter_df, pain_flag),
    aes(x = recovery_score, y = ac_ratio),
    inherit.aes = FALSE,
    shape = 21, size = 9, stroke = 1.8, fill = NA, color = "#d62728"
  ) +
  scale_fill_manual(values = c(green = "#2ca02c", yellow = "#ffbf00", red = "#d62728")) +
  labs(
    x = "Score de Recuperación",
    y = "Índice de Carga (ACWR)",
    title = "ACWR & Recuperación: Resumen del equipo de hoy"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank()
  )

# ggplotly(acwr_scatter_plot, tooltip = "text")


# ACWR x Rest Score Plot --------

recuperacion_df <- recuperacion_df |>
  mutate(
    date = as.Date(`Marca temporal`),
    rest_score = compute_rest_score(
      `Nivel de Cansancio hoy (Fatiga)`,
      `Qué tal descansaste ayer?`,
      `Cuántas horas dormiste ayer?`
    ),
    pain_score = compute_pain_score(
      `Estás adolorido de alguna parte?`
    )
  )

micros_individual <- micros_individual |>
  mutate(date = as.Date(date))

# --- Latest ACWR per player (unchanged) ---
latest_acwr <- micros_individual |>
  group_by(player) |>
  filter(date == max(date, na.rm = TRUE)) |>
  select(player, ac_ratio)

# --- Latest dates per player from wellness survey (unchanged) ---
latest_dates <- recuperacion_df |>
  group_by(Nombre) |>
  summarize(latest_date = max(date, na.rm = TRUE), .groups = "drop") |>
  rename(player = Nombre)

# =========================
#   1) ACWR x REST SCORE --------
# =========================
# Latest rest score per player
latest_rest <- recuperacion_df |>
  group_by(Nombre) |>
  filter(date == max(date, na.rm = TRUE)) |>
  select(player = Nombre, rest_score)

rest_scatter_df <- latest_acwr |>
  inner_join(latest_rest,  by = "player") |>
  inner_join(latest_dates, by = "player") |>
  mutate(
    rest_status = if_else(rest_score >= 6, "Descansado", "Cansado"),
    load_status = case_when(
      ac_ratio < 0.8 ~ "Carga Baja",
      ac_ratio > 1.3 ~ "Carga Alta",
      TRUE ~ "Carga Óptima"
    ),
    color_status_rest = case_when(
      ac_ratio >= 0.8 & ac_ratio <= 1.3 & rest_score >= 6 ~ "green",
      (ac_ratio >= 0.8 & ac_ratio <= 1.3 & rest_score < 6) |
        (rest_score >= 6 & (ac_ratio < 0.8 | ac_ratio > 1.3)) ~ "yellow",
      TRUE ~ "red"
    )
  ) |>
  filter(!is.na(rest_score), !is.na(ac_ratio))

acwr_rest_scatter_plot <- ggplot(
  rest_scatter_df,
  aes(
    x = rest_score,
    y = ac_ratio,
    fill = color_status_rest,
    text = paste0(
      "Jugador: ", player,
      "<br>Fecha: ", latest_date,
      "<br>Score de Descanso: ", rest_score,
      "<br>Índice de Carga: ", round(ac_ratio, 2),
      "<br>Estatus de Descanso: ", rest_status,
      "<br>Estatus de Carga: ", load_status
    ),
    customdata = player
  )
) +
  geom_hline(yintercept = c(0.8, 1.3), linetype = "dashed", color = "gray50") +
  # use a filled shape so 'fill' shows; keep black outline
  geom_point(shape = 21, size = 6, alpha = 0.9, color = "black", stroke = 0.7) +
  scale_fill_manual(values = c(green = "#2ca02c", yellow = "#ffbf00", red = "#d62728")) +
  labs(
    x = "Score de Descanso", 
    y = "Índice de Carga (ACWR)",
    title = "ACWR & Descanso: Resumen del equipo de hoy"
    ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank())

# =========================
#   2) ACWR x PAIN SCORE -----------
# =========================
# Latest pain score per player
latest_pain2 <- recuperacion_df |>
  filter(Nombre %in% jugs) |>
  group_by(Nombre) |>
  filter(date == max(date, na.rm = TRUE)) |>
  transmute(
    player = Nombre,
    zona_adolorida = `Donde te encuentras adolorido? Indica cada zona de dolor`,
    pain_flag = !is.na(zona_adolorida) & zona_adolorida != "Nada",
    pain_score = pain_score
  ) 

pain_scatter_df <- latest_acwr |>
  inner_join(latest_pain2,  by = "player") |>
  inner_join(latest_dates, by = "player") |>
  mutate(
    pain_status = if_else(pain_score < 6, "Sin dolor", "Con dolor"),
    load_status = case_when(
      ac_ratio < 0.8 ~ "Carga Baja",
      ac_ratio > 1.3 ~ "Carga Alta",
      TRUE ~ "Carga Óptima"
    ),
    color_status_pain = case_when(
      ac_ratio >= 0.8 & ac_ratio <= 1.3 & pain_score < 6 ~ "green",
      (ac_ratio >= 0.8 & ac_ratio <= 1.3 & pain_score >= 6) |
        (pain_score < 6 & (ac_ratio < 0.8 | ac_ratio > 1.3)) ~ "yellow",
      TRUE ~ "red"
    ),
    pain_flag = !is.na(zona_adolorida) & zona_adolorida != "Nada"
  ) |>
  filter(!is.na(pain_score), !is.na(ac_ratio))

acwr_pain_scatter_plot <- ggplot(
  pain_scatter_df,
  aes(
    x = pain_score,
    y = ac_ratio,
    fill = color_status_pain,
    text = paste0(
      "Jugador: ", player,
      "<br>Fecha: ", latest_date,
      "<br>Score de Dolor Muscular: ", pain_score,
      "<br>Índice de Carga: ", round(ac_ratio, 2),
      "<br>Estatus de Dolor Muscular: ", pain_status,
      "<br>Estatus de Carga: ", load_status,
      ifelse(pain_flag, paste0("<br>Zona Adolorida: ", zona_adolorida), "")
      
    ),
    customdata = player
  )
) +
  geom_hline(yintercept = c(0.8, 1.3), linetype = "dashed", color = "gray50") +
  geom_point(shape = 21, size = 6, alpha = 0.9, color = "black", stroke = 0.7) +
  # anillo rojo para jugadores con dolor
  geom_point(
    data = dplyr::filter(pain_scatter_df, pain_flag),
    aes(x = pain_score, y = ac_ratio),
    inherit.aes = FALSE,
    shape = 21, size = 9, stroke = 1.8, fill = NA, color = "#d62728"
  ) +
  scale_fill_manual(values = c(green = "#2ca02c", yellow = "#ffbf00", red = "#d62728")) +
  labs(
    x = "Score de Dolor Muscular", 
    y = "Índice de Carga (ACWR)",
    title = "ACWR & Dolor Muscular: Resumen del equipo de hoy"
    ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank()
)