import("shiny")
import("dygraphs")
import("glue")
import("dplyr")
import("lubridate")
import("xts")

export("ui")
export("init_server")

ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$div(
      class = "panel-header",
      selectInput(
        ns("group_by"), "Seleccionar columna de agrupación",
        choices = NULL, # Choices will be set dynamically
        width = NULL,
        selectize = TRUE
      )
    ),
    tags$div(
      class = "chart-time-container",
      dygraphOutput(ns("dygraph"), height = "350px")
    )
  )
}

init_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    data <- reactiveVal(NULL)
    group_choices <- reactiveVal(NULL)

    observe({
      req(session$userData$uploaded_data)
      df <- session$userData$uploaded_data()

      # Set available group choices
      if (ncol(df) > 1) {
        group_choices <- names(df)[!(names(df) %in% c("Fecha.y.hora", "Cantidad"))]
        updateSelectInput(session, "group_by", choices = group_choices)
        data(df)
      }
    })

    dy_bar_chart <- function(dygraph) {
      dyPlotter(
        dygraph = dygraph,
        name = "BarChart",
        path = system.file("plotters/barchart.js", package = "dygraphs")
      )
    }

    output$dygraph <- renderDygraph({
      req(data(), input$group_by)

      df <- data()
      group_by_column <- input$group_by

      if (nrow(df) == 0) {
        showNotification("No data available for the selected grouping column.", type = "error")
        return(NULL)
      }

      # Convertir los datos a objeto xts agrupado por semana
      df <- df %>%
        mutate(
          Fecha.y.hora = floor_date(as.POSIXct(Fecha.y.hora, format = "%d/%m/%Y, %I:%M:%S %p", tz = "UTC"), "week"),
          Cantidad = as.numeric(gsub(",", ".", Cantidad))
        ) %>%
        filter(!is.na(Fecha.y.hora), !is.na(!!sym(group_by_column))) %>%
        group_by(Fecha.y.hora, !!sym(group_by_column)) %>%
        summarize(Cantidad = sum(Cantidad, na.rm = TRUE)) %>%
        ungroup()

      # Convertir los datos a objeto xts para cada grupo
      xts_data_list <- df %>%
        split(.[[group_by_column]]) %>%
        lapply(function(sub_df) {
          if (nrow(sub_df) > 0) {
            xts(sub_df$Cantidad, order.by = sub_df$Fecha.y.hora)
          } else {
            return(NULL)
          }
        })

      # Filtrar los NULL de la lista
      xts_data_list <- Filter(Negate(is.null), xts_data_list)

      # Combinar todas las series xts
      if (length(xts_data_list) == 0) {
        showNotification("No data available after processing.", type = "error")
        return(NULL)
      }

      combined_xts_data <- do.call(cbind, xts_data_list)

      # Asignar nombres a las series
      colnames(combined_xts_data) <- names(xts_data_list)

      dygraph(combined_xts_data) %>%
        dy_bar_chart() %>%
        dyOptions(
          drawPoints = TRUE,
          pointSize = 2,
          strokeWidth = 1.5,
          includeZero = TRUE,
          axisLineColor = "#585858",
          gridLineColor = "#bdc2c6",
          axisLabelFontSize = 12,
          axisLabelColor = "#585858",
          disableZoom = TRUE
        )
    })
  })
}
