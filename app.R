#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(plotly)
library(shinythemes)
source("dashboard.R")  # Ensure this file returns all needed plot functions and data

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
                      "Si está dentro de este rango, se considera 'Punto Ideal'.",
                      tags$br(),
                      "Debajo de 0.8: 'Carga Baja'. Sobre 1.3: 'Sobrecargado'."
                    ),
                    tags$details(
                      tags$summary("Interpretación Score de Recuperación", style = "font-weight:bold; font-size:16px;"),
                      "Compuesto por fatiga, sueño, calidad de sueño y dolor muscular. Cada categoría tiene un puntaje entre 0.5 y 2.5, dependiendo de la respuesta del jugador en la encuesta de bienestar.",
                      tags$br(),
                      "A la mejor respuesta posible se le asigna un valor de 2.5, y la peor respuesta posible de 0.5 o 1. Si elige alguna opción entre la mejor y la peor opción, se le asigna un valor intermedio a su respuesta.",
                      tags$br(),
                      "Este proceso se lleva a cabo para cada una de las cuatro categorías. La suma total genera el score de recuperación sobre 10."
                    ),
                    tags$details(
                      tags$summary("Interpretación Estatus de Recuperación", style = "font-weight:bold; font-size:16px;"),
                      "El estatus de recuperación se calcula directamente del score de recuperación.",
                      tags$br(),
                      "Si un jugador presenta un score de recuperación de 6 o mayor, su estatus será 'Recuperado'.",
                      tags$br(),
                      "Si es menor a 6, su estatus será 'Fatigado'."
                    ),
                    tags$details(
                      tags$summary("Scores de Indicadores de Recuperación Diaria", style = "font-weight:bold; font-size:16px;"),
                      "Son basados en las respuestas diarias de los jugadores en la encuesta de bienestar."
                    )
           )
    )
  )
)

server <- function(input, output, session) {
  selected <- reactiveVal(NULL)
  
  # 1. Set selection via dropdown
  observeEvent(input$player_select, {
    selected(input$player_select)
  })
  
  # 2. Update selection via plot click and update dropdown
  observeEvent(event_data("plotly_click", source = "acwr_scatter"), {
    click_data <- event_data("plotly_click", source = "acwr_scatter")
    if (!is.null(click_data)) {
      selected(click_data$customdata)
      updateSelectInput(session, "player_select", selected = click_data$customdata)
    }
  })
  
  output$acwr_scatter <- renderPlotly({
    req(selected())
    
    scatter_df <- scatter_df |>
      mutate(
        selected_flag = player == selected(),
        hover_text = paste0(
          "Jugador: ", player,
          "<br>Fecha: ", latest_date,
          "<br>Score de Recuperación: ", recovery_score,
          "<br>Índice de Carga: ", round(ac_ratio, 2),
          "<br>Estatus de Recuperación: ", recovery_status,
          "<br>Estatus de Carga: ", load_status
        )
      )
    
    p <- ggplot(scatter_df, aes(x = recovery_score, y = ac_ratio)) +
      geom_hline(yintercept = c(0.8, 1.3), linetype = "dashed", color = "gray50") +
      
      # Base layer (transparent dots)
      geom_point(
        aes(fill = color_status, text = hover_text, customdata = player),
        size = 6, alpha = 0.3, color = "black"
      ) +
      
      # Highlighted point with same text/customdata
      geom_point(
        data = scatter_df |> filter(selected_flag),
        aes(fill = color_status, text = hover_text, customdata = player),
        size = 8, shape = 21, stroke = 2, color = "black"
      ) +
      
      scale_fill_manual(
        values = c("green" = "#2ca02c", "yellow" = "#ffbf00", "red" = "#d62728")
      ) +
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
    
    ggplotly(p, tooltip = "text", source = "acwr_scatter")
  })
  
  
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
