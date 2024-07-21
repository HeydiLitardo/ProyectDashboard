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

# Datos estáticos para el gráfico de barras verticales
static_bar_data <- data.frame(
  category = c("A", "B", "C", "D", "E"),
  value = c(30, 50, 20, 60, 40)
)

ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$div(
      class = "panel-header",
      selectInput(
        ns("chart_type"), "Tipo de gráfico",
        choices = c("Barra" = "bar"),
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
    if (input$chart_type == "line") {
      static_line_data
    } else {
      static_bar_data
    }
  })
  
  output$summary_plot <- renderPlot({
    data <- selected_data()
    
    if (input$chart_type == "line") {
      ggplot(data, aes(x = date, y = value, color = series, group = series)) +
        geom_line(size = 1.2, alpha = 0.8) +
        geom_point(size = 2) +
        geom_smooth(method = "loess", se = TRUE, span = 0.2, alpha = 0.2) +
        theme_minimal() +
        labs(title = "Gráfico de Líneas Suaves", x = "Fecha", y = "Valor") +
        theme(
          plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_line(color = "#f0f0f0"),
          panel.grid.minor = element_line(color = "#f0f0f0")
        )
      
    } else if (input$chart_type == "bar") {
      ggplot(data, aes(x = category, y = value, fill = category)) +
        geom_bar(stat = "identity") +
        theme_minimal() +
        labs(title = "Gráfico de Barras Verticales", x = "Categoría", y = "Valor") +
        theme(
          plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_line(color = "#f0f0f0"),
          panel.grid.minor = element_line(color = "#f0f0f0")
        )
    }
  })
}
