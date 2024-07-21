import("shiny")
import("ggplot2")
import("glue")
import("dplyr")
import("lubridate")

export("ui")
export("init_server")

# Datos estáticos para el gráfico de líneas con varias series
static_line_data <- data.frame(
  date = rep(seq.Date(as.Date("2023-01-01"), as.Date("2023-12-01"), by = "month"), 3),
  series = rep(c("X", "Y", "Z"), each = 12),
  value = c(
    cumsum(c(-5, 3, 8, -2, 7, -4, 10, -6, 5, 0, -3, 4)),
    cumsum(c(10, -8, 7, -10, 6, -4, 12, -3, 2, 9, -1, 5)),
    cumsum(c(3, -2, 5, -3, 7, -4, 6, -2, 8, -5, 9, -6))
  )
)

ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$div(
      class = "panel-header",
      selectInput(
        ns("chart_type"), "Tipo de gráfico",
        choices = c("Línea" = "line"),
        selected = "line"
      )
    ),
    tags$div(
      class = "chart-time-container",
      plotOutput(ns("summary_plot"), height = "240px", width = "100%")
    )
  )
}

init_server <- function(id) {
  callModule(server, id)
}

server <- function(input, output, session) {

  selected_data <- reactive({
    static_line_data
  })
  
  output$summary_plot <- renderPlot({
    data <- selected_data()
    
    ggplot(data, aes(x = date, y = value, color = series, group = series)) +
      #geom_line(size = 1.2, alpha = 0.8) +
      geom_point(size = 2) +
      geom_smooth(method = "loess", se = TRUE, span = 0.2, alpha = 0.2, linewidth = 1.1) +
      theme_minimal() +
      labs(title = "Gráfico de Líneas", x = "Fecha", y = "Valor") +
      theme(
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_line(color = "#f0f0f008"),
        panel.grid.minor = element_line(color = "#f0f0f0")
      ) 
  })
}