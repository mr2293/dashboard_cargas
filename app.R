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

# Load your data + helper plotting fns (recuperacion_df, micros_individual, plot_* fns)
tryCatch(
  source("dashboard.R", local = TRUE, encoding = "UTF-8"),
  error = function(e) stop("Failed to source dashboard.R: ", conditionMessage(e))
)

player_info <- tibble::tibble(
  player = c("Néstor Araujo", "Brian Rodríguez", "Sebastián Cáceres", "Alan Cervantes", 
             "Rodolfo Cota", "Erick Sánchez", "Álvaro Fidalgo", "Henry Martín", "Israel Reyes",
             "Jonathan Dos Santos", "Kevin Álvarez", "Luis Ángel Malagón", "Miguel Vázquez", 
             "Ramón Juárez", "Alejandro Zendejas", "Rodrigo Aguirre", "Cristian Borja", 
             "Dagoberto Espinoza", "Víctor Dávila", "Igor Lichnovsky", "Santiago Naveda", "Ralph Orquin",
             "Alexis Gutiérrez", "Isaías Violante", "José Raúl Zúñiga", "Allan Saint-Maximin"),
  image = c("araujo.png", "brian.png", "caceres.png", "cervantes.png", "cota.png", 
            "erick_sanchez.png", "fidalgo.png", "henry.png", "israel_reyes.png", 
            "jonathan.png", "kevin.png", "malagon.png", "miguel_vazquez.png", "ramon.png", 
            "zendejas.png", "aguirre.jpeg", "borja.jpeg", "dagoberto.jpg", "davila.jpg", 
            "igor.avif", "naveda.jpeg", "ralph.jpeg", "alexis_gtz.jpg", "violante.png", 
            "zuniga.png", "maximin.png"),
  age = c(33, 25, 25, 27, 37, 25, 28, 32, 25, 35, 26, 28, 21, 24, 27, 30, 32, 21, 27, 31,
          24, 22, 25, 21, 31, 28),
  height = c("1.88 m", "1.75 m", "1.80 m", "1.81 m", "1.83 m", "1.67 m", 
             "1.74 m", "1.77 m", "1.79 m", "1.72 m", "1.76 m", 
             "1.82 m", "1.85 m", "1.82 m", "1.70 m", "1.83 m", "1.79 m", "1.80 m",
             "1.73 m", "1.86 m", "1.80 m", "1.77 m", "1.75 m", "1.73 m", "1.80 m",
             "1.73 m")
)

ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Dashboard de Recuperación Física – Club América"),
  
  fluidRow(
    column(10,
           h3(NULL, align = "center"),
           plotlyOutput("acwr_scatter", height = "500px")
    ),
    column(2,
           selectInput(
             inputId = "player_select",
             label = "Buscar jugador",
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
  
  # ---------- Build scatter data reactively ----------
  scatter_df <- reactive({
    roster <- player_info$player
    
    # Latest ACWR per player
    ac_last <- micros_individual %>%
      mutate(date = as.Date(date)) %>%
      filter(player %in% roster) %>%
      group_by(player) %>%
      slice_max(order_by = date, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      select(player, ac_ratio)
    
    # Latest recovery + date + pain; handle either "Zona Adolorida" or long question name
    rec_cols <- names(recuperacion_df)
    pain_col <- if ("Zona Adolorida" %in% rec_cols) "Zona Adolorida"
    else "Donde te encuentras adolorido? Indica cada zona de dolor"
    
    rec_last <- recuperacion_df %>%
      mutate(date = as.Date(`Marca temporal`)) %>%
      filter(Nombre %in% roster) %>%
      group_by(Nombre) %>%
      slice_max(order_by = date, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      transmute(
        player         = Nombre,
        recovery_score,
        latest_date    = date,
        zona_adolorida = .data[[pain_col]]
      )
    
    # Combine + flags + hover text
    ac_last %>%
      inner_join(rec_last, by = "player") %>%
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
        pain_flag = !is.na(zona_adolorida) & zona_adolorida != "Nada",
        hover_text = paste0(
          "Jugador: ", player,
          "<br>Fecha: ", latest_date,
          "<br>Score de Recuperación: ", recovery_score,
          "<br>Índice de Carga: ", round(ac_ratio, 2),
          "<br>Estatus de Recuperación: ", recovery_status,
          "<br>Estatus de Carga: ", load_status
          # ifelse(pain_flag, paste0("<br>Zona Adolorida: ", zona_adolorida), "")
        )
      )
  })
  
  # ---------- Initialize + keep selectInput in sync (without resetting selection) ----------
  observe({
    df <- scatter_df()
    if (nrow(df) == 0) return()
    
    choices <- sort(unique(df$player))
    curr <- isolate(selected())
    sel  <- if (!is.null(curr) && curr %in% choices) curr else choices[1]
    
    updateSelectInput(session, "player_select", choices = choices, selected = sel)
    if (is.null(curr)) selected(sel)  # only set default once
  })
  
  # 1) selection via dropdown
  observeEvent(input$player_select, {
    selected(input$player_select)
  }, ignoreInit = TRUE)
  
  # 2) selection via plot click and sync dropdown (ignore initial empty event)
  observeEvent(event_data("plotly_click", source = "acwr_scatter"), {
    cd <- event_data("plotly_click", source = "acwr_scatter")
    if (!is.null(cd) && !is.null(cd$customdata)) {
      selected(cd$customdata)
      updateSelectInput(session, "player_select", selected = cd$customdata)
    }
  }, ignoreInit = TRUE)
  
  # ---------- ACWR scatter ----------
  output$acwr_scatter <- renderPlotly({
    df <- scatter_df()
    req(nrow(df) > 0)
    
    df <- df %>% mutate(selected_flag = player == selected())
    
    p <- ggplot(df, aes(x = recovery_score, y = ac_ratio)) +
      geom_hline(yintercept = c(0.8, 1.3), linetype = "dashed", color = "gray50") +
      geom_point(aes(fill = color_status, text = hover_text, customdata = player),
                 shape = 21, size = 6, alpha = 0.35, color = "black") +
      geom_point(data = dplyr::filter(df, pain_flag),
                 aes(x = recovery_score, y = ac_ratio),
                 inherit.aes = FALSE, shape = 21, size = 10, stroke = 1.2,
                 fill = NA, color = "#d62728") +
      geom_point(data = dplyr::filter(df, selected_flag),
                 aes(fill = color_status, text = hover_text, customdata = player),
                 shape = 21, size = 8, stroke = 1.2, color = "black") +
      scale_fill_manual(values = c(green = "#2ca02c", yellow = "#ffbf00", red = "#d62728")) +
      labs(
        x = "Score de Recuperación",
        y = "Índice de Carga (ACWR)",
        title = "ACWR & Recuperación: Resumen del equipo de hoy"
        # caption won't show in plotly
      ) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank()
      )
    
    ggplotly(p, tooltip = "text", source = "acwr_scatter") %>%
      layout(
        margin = list(b = 70),  # make room for caption
        annotations = list(
          list(
            x = 0.90, y = -0.12, xref = "paper", yref = "paper",
            text = "<b>Círculo rojo = Dolor muscular</b>",
            showarrow = FALSE, xanchor = "center", yanchor = "top",
            font = list(size = 12)
          )
        )
      )
  })
  
  
  # ---------- Linked panels (from dashboard.R) ----------
  output$survey_plot <- renderPlotly({
    req(selected())
    ggplotly(plot_player_recuperacion(selected()), tooltip = "text")
  })
  
  output$hsr_plot <- renderPlotly({
    req(selected())
    ggplotly(plot_individual_hsr(selected()), tooltip = "text")
  })
  
  output$ac_plot <- renderPlotly({
    req(selected())
    ggplotly(plot_individual_ac(selected()), tooltip = "text")
  })
  
  output$player_info_box <- renderUI({
    req(selected())
    player_row <- player_info %>% filter(player == selected())
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
