#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# app.R
library(shiny)
library(plotly)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(tidyr)

# Load your data + helper plotting fns (recuperacion_df, micros_individual, plot_* fns)
tryCatch(
  source("dashboard.R", local = TRUE, encoding = "UTF-8"),
  error = function(e) stop("Failed to source dashboard.R: ", conditionMessage(e))
)

# Fallback if not defined in dashboard.R for any reason
if (!exists("ACWR_MISSING_Y", inherits = TRUE)) {
  ACWR_MISSING_Y <- 0.65
} else {
  ACWR_MISSING_Y <- get("ACWR_MISSING_Y", inherits = TRUE)
}

player_info <- tibble::tibble(
  player = c("Néstor Araujo", "Brian Rodríguez", "Sebastián Cáceres", "Alan Cervantes",
             "Rodolfo Cota", "Erick Sánchez", "Álvaro Fidalgo", "Henry Martín", "Israel Reyes",
             "Jonathan Dos Santos", "Kevin Álvarez", "Luis Ángel Malagón", "Miguel Vázquez",
             "Ramón Juárez", "Alejandro Zendejas", "Rodrigo Aguirre", "Cristian Borja",
             "Dagoberto Espinoza", "Víctor Dávila", "Igor Lichnovsky", "Santiago Naveda", "Ralph Orquín",
             "Alexis Gutiérrez", "Isaías Violante", "José Raúl Zúñiga", "Allan Saint-Maximin", "Patricio Salas"),
  image = c("araujo.png", "brian.png", "caceres.png", "cervantes.png", "cota.png",
            "erick_sanchez.png", "fidalgo.png", "henry.png", "israel_reyes.png",
            "jonathan.png", "kevin.png", "malagon.png", "miguel_vazquez.png", "ramon.png",
            "zendejas.png", "aguirre.jpeg", "borja.jpeg", "dagoberto.jpg", "davila.jpg",
            "igor.avif", "naveda.jpeg", "ralph.jpeg", "alexis_gtz.jpg", "violante.png",
            "zuniga.png", "maximin.png", "pato_salas.jpg"),
  age = c(33, 25, 25, 27, 37, 25, 28, 32, 25, 35, 26, 28, 21, 24, 27, 30, 32, 21, 27, 31,
          24, 22, 25, 21, 31, 28, 21),
  height = c("1.88 m", "1.75 m", "1.80 m", "1.81 m", "1.83 m", "1.67 m",
             "1.74 m", "1.77 m", "1.79 m", "1.72 m", "1.76 m",
             "1.82 m", "1.85 m", "1.82 m", "1.70 m", "1.83 m", "1.79 m", "1.80 m",
             "1.73 m", "1.86 m", "1.80 m", "1.77 m", "1.75 m", "1.73 m", "1.80 m",
             "1.73 m", "1.85m")
)

ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Dashboard de Recuperación Física – Club América"),
  
  fluidRow(
    column(
      10,
      tabsetPanel(
        id = "team_top_tabs",
        tabPanel("Recuperación",
                 plotlyOutput("acwr_scatter", height = "500px")),
        tabPanel("Descanso",
                 plotlyOutput("acwr_rest_scatter", height = "500px")),
        tabPanel("Dolor Muscular",
                 plotlyOutput("acwr_pain_scatter", height = "500px")),
        tabPanel("RPE",
                 plotlyOutput("acwr_rpe_scatter", height = "500px"))
      )
    ),
    column(
      2,
      selectInput(
        inputId = "player_select",
        label   = "Buscar jugador",
        choices = sort(player_info$player),
        selected = NULL
      ),
      uiOutput("player_info_box")
    )
  ),
  
  fluidRow(
    column(12,
           h3(NULL, align = "center"),
           plotlyOutput("survey_plot", height = "600px")
    )
  ),
  
  fluidRow(
    column(12,
           tabsetPanel(
             tabPanel("HSR (>21 km/h)", plotlyOutput("hsr_plot", height = "500px")),
             tabPanel("Carga Aguda vs Crónica (A:C)", plotlyOutput("ac_plot", height = "500px"))
           )
    )
  ),
  
  fluidRow(
    column(12,
           h3("Interpretación de Métricas", align = "center"),
           tags$div(style = "margin: 10px 25px;",
                    tags$details(
                      tags$summary("Estado del jugador", style = "font-weight:bold; font-size:16px;"),
                      "Si el jugador está en verde, tiene un score de recuperación de 6 o mayor y su índice de carga esta en rango.",
                      tags$br(),
                      "Si el jugador está en amarillo, uno de los valores, ya sea score de recuperación o índice de carga, es menor a 6 o está fuera de rango.",
                      tags$br(),
                      "Si el jugador está en rojo, tiene un score de recuperación menor a 6 y su índice de carga esta fuera rango."
                    ),
                    tags$details(
                      tags$summary("Interpretación Índice de Carga", style = "font-weight:bold; font-size:16px;"),
                      "El índice de carga es el cociente de la carga aguda dividida por la carga crónica.",
                      tags$br(),
                      "La carga aguda es la suma de la distancia absoluta de High Speed Running (HSR_abs_dist) y la carga del jugador (player_load), ambos datos capturados por WIMU.",
                      tags$br(),
                      "La carga crónica es la media rodante de los valores de carga aguda, calculada tomando en cuenta los últimos siete días de carga aguda.",
                      tags$br(),
                      "Esta carga aguda se divide por la carga crónica para así tener el A:C ratio, en este caso interpretado como Índice de Carga."
                    ),
                    tags$details(
                      tags$summary("Interpretación Estatus de Carga", style = "font-weight:bold; font-size:16px;"),
                      "El valor del índice de carga, idealmente, debe de estar en un rango de 0.8 a 1.3.",
                      tags$br(),
                      "Debajo de 0.8: 'Carga Baja'. Sobre 1.3: 'Sobrecargado'."
                    ),
                    tags$details(
                      tags$summary("Interpretación Score de Recuperación", style = "font-weight:bold; font-size:16px;"),
                      "Compuesto por fatiga, sueño, calidad de sueño y dolor muscular..."
                    ),
                    tags$details(
                      tags$summary("Interpretación Estatus de Recuperación", style = "font-weight:bold; font-size:16px;"),
                      "Si un jugador presenta un score de recuperación de 6 o mayor, su estatus será 'Recuperado'.",
                      tags$br(),
                      "Si es menor a 6, su estatus será 'Fatigado'."
                    )
           ))
  )
)

server <- function(input, output, session) {
  selected <- reactiveVal(NULL)
  
  # ---------- helpers ----------
  to_plotly <- function(p, src = NULL) {
    if (inherits(p, "ggplot")) p <- ggplotly(p, tooltip = "text")
    if (!inherits(p, "plotly")) stop("Plot must be a ggplot or plotly object.")
    if (!is.null(src)) p$x$source <- src
    p
  }
  
  # Add competition banner in Plotly (last 4 MDs ≥ 80')
  add_competition_banner <- function(p, player, up_to_date) {
    if (exists("last4_md_80_flag", inherits = TRUE) && last4_md_80_flag(player, up_to_date)) {
      p <- p |>
        layout(
          annotations = list(list(
            x = 0.5, xref = "paper", xanchor = "center",
            y = 1.08, yref = "paper", yanchor = "top",
            text = "<b>Carga Alta de Competencia</b>",
            showarrow = FALSE,
            bgcolor = "#C62828", bordercolor = "#C62828",
            font = list(color = "white", size = 16),
            opacity = 0.95
          ))
        )
    }
    p
  }
  
  # Text-only placeholder ggplot
  empty_plot <- function(msg) {
    ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = msg, size = 6) +
      theme_void()
  }
  
  # ---------- Build ACWR x Recovery data reactively (GK-safe) ----------
  scatter_df <- reactive({
    roster <- player_info$player
    
    # Pull authoritative 3-day pain flag computed in dashboard.R
    latest_pain2_obj <- get("latest_pain2", inherits = TRUE)
    
    rings_auth <- latest_pain2_obj |>
      dplyr::filter(pain_flag) |>
      dplyr::distinct(player, .keep_all = TRUE) |>
      dplyr::select(player, zona_adolorida, pain_flag)
    
    # Latest ACWR per player (may be missing for GKs)
    ac_last <- get("micros_individual", inherits = TRUE) |>
      dplyr::mutate(date = as.Date(date)) |>
      dplyr::filter(player %in% roster) |>
      dplyr::group_by(player) |>
      dplyr::slice_max(order_by = date, n = 1, with_ties = FALSE) |>
      dplyr::ungroup() |>
      dplyr::select(player, ac_ratio)
    
    # Latest recovery + date (this is the "base" so everyone with wellness appears)
    recuperacion_df <- get("recuperacion_df", inherits = TRUE)
    
    rec_last <- recuperacion_df |>
      dplyr::mutate(date = as.Date(`Marca temporal`)) |>
      dplyr::filter(Nombre %in% roster) |>
      dplyr::group_by(Nombre) |>
      dplyr::slice_max(order_by = date, n = 1, with_ties = FALSE) |>
      dplyr::ungroup() |>
      dplyr::transmute(
        player      = Nombre,
        recovery_score,
        latest_date = date
      )
    
    rec_last |>
      # LEFT JOIN so GKs stay even if ac_ratio missing
      dplyr::left_join(ac_last, by = "player") |>
      dplyr::left_join(rings_auth, by = "player") |>
      dplyr::mutate(
        pain_flag = tidyr::replace_na(pain_flag, FALSE),
        y_plot = dplyr::if_else(is.na(ac_ratio), ACWR_MISSING_Y, ac_ratio),
        recovery_status = dplyr::if_else(recovery_score >= 6, "Recuperado", "Fatigado"),
        load_status = dplyr::case_when(
          is.na(ac_ratio) ~ "Sin WIMU (sin ACWR)",
          ac_ratio < 0.8 ~ "Carga Baja",
          ac_ratio > 1.3 ~ "Carga Alta",
          TRUE ~ "Carga Óptima"
        ),
        color_status = dplyr::case_when(
          is.na(ac_ratio) & recovery_score >= 6 ~ "green",
          is.na(ac_ratio) & recovery_score < 6  ~ "red",
          
          ac_ratio >= 0.8 & ac_ratio <= 1.3 & recovery_score >= 6 ~ "green",
          (ac_ratio >= 0.8 & ac_ratio <= 1.3 & recovery_score < 6) |
            (recovery_score >= 6 & (ac_ratio < 0.8 | ac_ratio > 1.3)) ~ "yellow",
          TRUE ~ "red"
        ),
        hover_text = paste0(
          "Jugador: ", player,
          "<br>Fecha: ", latest_date,
          "<br>Score de Recuperación: ", recovery_score,
          ifelse(is.na(ac_ratio),
                 "<br>ACWR: (Sin WIMU)",
                 paste0("<br>Índice de Carga: ", round(ac_ratio, 2))),
          "<br>Estatus de Recuperación: ", recovery_status,
          "<br>Estatus de Carga: ", load_status,
          ifelse(pain_flag & !is.na(zona_adolorida),
                 paste0("<br>Zona Adolorida: ", zona_adolorida), "")
        )
      ) |>
      dplyr::filter(!is.na(recovery_score)) |>
      dplyr::distinct(player, .keep_all = TRUE)
  })
  
  # ---------- Initialize + keep selectInput in sync ----------
  observe({
    df <- scatter_df()
    if (nrow(df) == 0) return()
    
    choices <- sort(unique(df$player))
    curr <- isolate(selected())
    sel  <- if (!is.null(curr) && curr %in% choices) curr else choices[1]
    
    updateSelectInput(session, "player_select", choices = choices, selected = sel)
    if (is.null(curr)) selected(sel)
  })
  
  observeEvent(input$player_select, {
    selected(input$player_select)
  }, ignoreInit = TRUE)
  
  # --- Click sync: ACWR x Recovery
  observeEvent(event_data("plotly_click", source = "acwr_scatter"), {
    cd <- event_data("plotly_click", source = "acwr_scatter")
    if (!is.null(cd) && !is.null(cd$customdata)) {
      selected(cd$customdata)
      updateSelectInput(session, "player_select", selected = cd$customdata)
    }
  }, ignoreInit = TRUE)
  
  # --- Click sync: ACWR x Rest
  observeEvent(event_data("plotly_click", source = "acwr_rest_scatter"), {
    cd <- event_data("plotly_click", source = "acwr_rest_scatter")
    if (!is.null(cd) && !is.null(cd$customdata)) {
      selected(cd$customdata)
      updateSelectInput(session, "player_select", selected = cd$customdata)
    }
  }, ignoreInit = TRUE)
  
  # --- Click sync: ACWR x Pain
  observeEvent(event_data("plotly_click", source = "acwr_pain_scatter"), {
    cd <- event_data("plotly_click", source = "acwr_pain_scatter")
    if (!is.null(cd) && !is.null(cd$customdata)) {
      selected(cd$customdata)
      updateSelectInput(session, "player_select", selected = cd$customdata)
    }
  }, ignoreInit = TRUE)
  
  # --- Click sync: ACWR x RPE
  observeEvent(event_data("plotly_click", source = "acwr_rpe_scatter"), {
    cd <- event_data("plotly_click", source = "acwr_rpe_scatter")
    if (!is.null(cd) && !is.null(cd$customdata)) {
      selected(cd$customdata)
      updateSelectInput(session, "player_select", selected = cd$customdata)
    }
  }, ignoreInit = TRUE)
  
  # =========================
  #   TOP PLOTS (4 tabs)
  # =========================
  
  # 1) ACWR x RECOVERY (GK-safe via y_plot)
  output$acwr_scatter <- renderPlotly({
    df <- scatter_df()
    req(nrow(df) > 0, selected())
    
    df <- df |>
      dplyr::mutate(
        selected_flag = player == selected()
      )
    
    rings_df <- df |>
      dplyr::filter(pain_flag == TRUE) |>
      dplyr::distinct(player, .keep_all = TRUE)
    
    p <- ggplot(df, aes(x = recovery_score, y = y_plot)) +
      geom_hline(yintercept = c(0.8, 1.3), linetype = "dashed", color = "gray50") +
      geom_hline(yintercept = ACWR_MISSING_Y, linetype = "dotted", color = "gray60", alpha = 0.7) +
      geom_point(aes(fill = color_status, text = hover_text, customdata = player),
                 shape = 21, size = 6, alpha = 0.35, color = "black") +
      geom_point(data = rings_df,
                 aes(x = recovery_score, y = y_plot),
                 inherit.aes = FALSE,
                 shape = 21, size = 10, stroke = 1.2, fill = NA, color = "#d62728") +
      geom_point(data = dplyr::filter(df, selected_flag),
                 aes(fill = color_status, text = hover_text, customdata = player),
                 shape = 21, size = 8, stroke = 1.2, color = "black") +
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
    
    ggplotly(p, tooltip = "text", source = "acwr_scatter") |>
      layout(
        margin = list(b = 80),
        annotations = list(
          list(
            x = 0.90, y = -0.14, xref = "paper", yref = "paper",
            text = "<b>Círculo rojo = Dolor muscular</b>",
            showarrow = FALSE, xanchor = "center", yanchor = "top",
            font = list(size = 12)
          ),
          list(
            x = 0.10, y = -0.14, xref = "paper", yref = "paper",
            text = "<b>Línea punteada = Sin WIMU (sin ACWR)</b>",
            showarrow = FALSE, xanchor = "left", yanchor = "top",
            font = list(size = 12)
          )
        )
      )
  })
  
  # 2) ACWR x REST (GK-safe via y_plot from dashboard.R)
  output$acwr_rest_scatter <- renderPlotly({
    req(exists("rest_scatter_df", inherits = TRUE))
    req(selected())
    
    df <- get("rest_scatter_df", inherits = TRUE)
    
    # ensure y_plot exists (dashboard.R should provide it now)
    if (!"y_plot" %in% names(df)) {
      df <- df |>
        mutate(y_plot = if_else(is.na(ac_ratio), ACWR_MISSING_Y, ac_ratio))
    }
    
    df <- df |>
      dplyr::mutate(
        selected_flag = player == selected(),
        hover_text = paste0(
          "Jugador: ", player,
          "<br>Fecha: ", latest_date,
          "<br>Score de Descanso: ", rest_score,
          ifelse(is.na(ac_ratio),
                 "<br>ACWR: (Sin WIMU)",
                 paste0("<br>Índice de Carga: ", round(ac_ratio, 2))),
          "<br>Estatus de Descanso: ", rest_status,
          "<br>Estatus de Carga: ", load_status
        )
      )
    
    p <- ggplot(df, aes(x = rest_score, y = y_plot)) +
      geom_hline(yintercept = c(0.8, 1.3), linetype = "dashed", color = "gray50") +
      geom_hline(yintercept = ACWR_MISSING_Y, linetype = "dotted", color = "gray60", alpha = 0.7) +
      geom_point(aes(fill = color_status_rest, text = hover_text, customdata = player),
                 shape = 21, size = 6, alpha = 0.30, color = "black") +
      geom_point(data = dplyr::filter(df, selected_flag),
                 aes(fill = color_status_rest, text = hover_text, customdata = player),
                 shape = 21, size = 8, stroke = 2, color = "black") +
      scale_fill_manual(values = c(green = "#2ca02c", yellow = "#ffbf00", red = "#d62728")) +
      labs(
        x = "Score de Descanso",
        y = "Índice de Carga (ACWR)",
        title = "ACWR & Descanso: Resumen del equipo de hoy"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank()
      )
    
    ggplotly(p, tooltip = "text", source = "acwr_rest_scatter")
  })
  
  # 3) ACWR x PAIN (GK-safe via y_plot from dashboard.R)
  output$acwr_pain_scatter <- renderPlotly({
    req(exists("pain_scatter_df", inherits = TRUE))
    req(selected())
    
    df <- get("pain_scatter_df", inherits = TRUE)
    
    # ensure y_plot exists (dashboard.R should provide it now)
    if (!"y_plot" %in% names(df)) {
      df <- df |>
        mutate(y_plot = if_else(is.na(ac_ratio), ACWR_MISSING_Y, ac_ratio))
    }
    
    df <- df |>
      dplyr::mutate(
        pain_flag     = tidyr::replace_na(pain_flag, FALSE),
        selected_flag = player == selected(),
        hover_text = paste0(
          "Jugador: ", player,
          "<br>Fecha: ", latest_date,
          "<br>Score de Dolor Muscular: ", pain_score,
          ifelse(is.na(ac_ratio),
                 "<br>ACWR: (Sin WIMU)",
                 paste0("<br>Índice de Carga: ", round(ac_ratio, 2))),
          "<br>Estatus de Dolor Muscular: ", pain_status,
          "<br>Estatus de Carga: ", load_status,
          ifelse(pain_flag, paste0("<br>Zona Adolorida: ", zona_adolorida), "")
        )
      )
    
    rings_df <- df |>
      dplyr::filter(pain_flag) |>
      dplyr::distinct(player, .keep_all = TRUE)
    
    p <- ggplot(df, aes(x = pain_score, y = y_plot)) +
      geom_hline(yintercept = c(0.8, 1.3), linetype = "dashed", color = "gray50") +
      geom_hline(yintercept = ACWR_MISSING_Y, linetype = "dotted", color = "gray60", alpha = 0.7) +
      geom_point(
        aes(fill = color_status_pain, text = hover_text, customdata = player),
        shape = 21, size = 6, alpha = 0.30, color = "black", stroke = 0.7
      ) +
      geom_point(
        data = dplyr::filter(df, selected_flag),
        aes(fill = color_status_pain, text = hover_text, customdata = player),
        shape = 21, size = 8, stroke = 2, color = "black"
      ) +
      geom_point(
        data = rings_df,
        aes(x = pain_score, y = y_plot),
        inherit.aes = FALSE,
        shape = 21, size = 10, stroke = 1.2, fill = NA, color = "#d62728"
      ) +
      scale_fill_manual(values = c(green = "#2ca02c", yellow = "#ffbf00", red = "#d62728")) +
      labs(
        x = "Score de Dolor Muscular",
        y = "Índice de Carga (ACWR)",
        title = "ACWR & Dolor Muscular: Resumen del equipo de hoy"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank()
      )
    
    ggplotly(p, tooltip = "text", source = "acwr_pain_scatter")
  })
  
  # 4) ACWR x RPE (GK-safe via y_plot from dashboard.R)
  output$acwr_rpe_scatter <- renderPlotly({
    req(selected())
    req(exists("scatter_df_rpe", inherits = TRUE))
    
    df <- get("scatter_df_rpe", inherits = TRUE)
    
    # ensure y_plot exists
    if (!"y_plot" %in% names(df)) {
      df <- df |>
        mutate(y_plot = if_else(is.na(ac_ratio), ACWR_MISSING_Y, ac_ratio))
    }
    
    df <- df |>
      dplyr::mutate(
        selected_flag = player == selected(),
        hover_text = paste0(
          "Jugador: ", player,
          if ("rpe_date" %in% names(df)) paste0("<br>Fecha (RPE): ", rpe_date) else "",
          if ("acwr_date" %in% names(df) && !all(is.na(df$acwr_date))) {
            ifelse(is.na(ac_ratio), "", paste0("<br>Fecha (ACWR): ", acwr_date))
          } else "",
          "<br>RPE: ", rpe_val,
          ifelse(is.na(ac_ratio),
                 "<br>ACWR: (Sin WIMU)",
                 paste0("<br>Índice de Carga (ACWR): ", round(ac_ratio, 2))),
          if ("load_status" %in% names(df)) paste0("<br>Estatus de Carga: ", load_status) else ""
        )
      ) |>
      dplyr::filter(!is.na(rpe_val))
    
    p <- ggplot(df, aes(x = rpe_val, y = y_plot)) +
      geom_hline(yintercept = c(0.8, 1.3), linetype = "dashed", color = "gray50") +
      geom_hline(yintercept = ACWR_MISSING_Y, linetype = "dotted", color = "gray60", alpha = 0.7) +
      geom_point(
        aes(fill = color_status_rpe, text = hover_text, customdata = player),
        shape = 21, size = 6, alpha = 0.30, color = "black", stroke = 0.7
      ) +
      geom_point(
        data = dplyr::filter(df, selected_flag),
        aes(fill = color_status_rpe, text = hover_text, customdata = player),
        shape = 21, size = 8, stroke = 2, color = "black"
      ) +
      scale_x_continuous(breaks = 1:10) +
      scale_fill_manual(values = c(green = "#2ca02c", yellow = "#ffbf00", red = "#d62728")) +
      labs(
        x = "RPE de la sesión (1–10)",
        y = "Índice de Carga (ACWR)",
        title = "ACWR & RPE: Resumen del equipo (última respuesta)"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank()
      )
    
    ggplotly(p, tooltip = "text", source = "acwr_rpe_scatter")
  })
  
  # =========================
  #  Linked panels (from dashboard.R)
  # =========================
  output$survey_plot <- renderPlotly({
    req(selected())
    ggplotly(plot_player_recuperacion(selected()), tooltip = "text")
  })
  
  output$hsr_plot <- renderPlotly({
    req(selected())
    # If player has no WIMU data, plot_individual_hsr already returns a "no data" ggplot
    up_to <- if (exists("micros_hsr", inherits = TRUE)) max(get("micros_hsr", inherits = TRUE)$date, na.rm = TRUE) else Sys.Date()
    
    p <- to_plotly(plot_individual_hsr(selected()), src = "hsr_plot")
    add_competition_banner(p, selected(), up_to)
  })
  
  output$ac_plot <- renderPlotly({
    req(selected())
    
    # Guard for players without any micros_individual rows (e.g., GKs) since plot_individual_ac may assume data exists
    mi <- get("micros_individual", inherits = TRUE)
    if (!selected() %in% unique(mi$player)) {
      return(ggplotly(empty_plot(paste("Sin datos WIMU para", selected())), tooltip = "text"))
    }
    
    up_to <- max(mi$date, na.rm = TRUE)
    p <- to_plotly(plot_individual_ac(selected()), src = "ac_plot")
    add_competition_banner(p, selected(), up_to)
  })
  
  output$player_info_box <- renderUI({
    req(selected())
    player_row <- player_info |> filter(player == selected())
    if (nrow(player_row) == 0) return(NULL)
    tags$div(style = "text-align:center;",
             tags$img(
               src = file.path("player_images", player_row$image),
               width = "100%", style = "max-width:200px; border-radius:10px; margin-bottom:10px;"
             ),
             tags$h4(player_row$player),
             tags$p(paste("Edad:", player_row$age)),
             tags$p(paste("Estatura:", player_row$height))
    )
  })
}

shinyApp(ui = ui, server = server)