import("shiny")
import("ggplot2")
import("glue")
import("dplyr")
import("lubridate")

export("ui")
export("init_server")

# Datos estáticos
static_data <- data.frame(
  date = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01", "2023-04-01", "2023-05-01")),
  category = c("A", "B", "C", "D", "E"),
  value = c(10, 20, 30, 25, 15)
)

ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$div(
      class = "panel-header",
      selectInput(
        ns("chart_type"), "Tipo de gráfico",
        choices = c("Barra" = "bar", "Pastel" = "pie"),
        selected = "bar"
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
    static_data
  })
  
  output$summary_plot <- renderPlot({
    data <- selected_data()
    
    if (input$chart_type == "bar") {
      ggplot(data, aes(x = category, y = value, fill = category)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        theme_minimal() +
        labs(title = "Gráfico de Barras", x = NULL, y = "Valor")
      
    } else if (input$chart_type == "pie") {
      ggplot(data, aes(x = "", y = value, fill = category)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar(theta = "y") +
        theme_void() +
        labs(title = "Gráfico de Pastel")
    }
  })
}