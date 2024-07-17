library(shiny)

warning_modal <- function(id, title, description) {
  ns <- NS(id)
  tagList(
    tags$div(
      class = "modal fade", id = ns("modal"), tabindex = "-1", `aria-labelledby` = ns("modalLabel"), `aria-hidden` = "true",
      tags$div(
        class = "modal-dialog",
        tags$div(
          class = "modal-content",
          tags$div(
            class = "modal-header",
            tags$h5(class = "modal-title", id = ns("modalLabel"), title),
            tags$button(type = "button", class = "btn-close", `data-bs-dismiss` = "modal", `aria-label` = "Close")
          ),
          tags$div(
            class = "modal-body",
            tags$p(description)
          ),
          tags$div(
            class = "modal-footer",
            tags$button(type = "button", class = "btn btn-secondary", `data-bs-dismiss` = "modal", "Cerrar"),
            tags$button(type = "button", class = "btn btn-warning", `data-bs-dismiss` = "modal", "Advertencia")
          )
        )
      )
    )
  )
}

success_modal <- function(id, title, description) {
    ns <- NS(id)
    tagList(
        tags$div(
            class = "modal fade", id = ns("modal"), tabindex = "-1", `aria-labelledby` = ns("modalLabel"), `aria-hidden` = "true",
            tags$div(
                class = "modal-dialog",
                tags$div(
                    class = "modal-content",
                    tags$div(
                        class = "modal-header",
                        tags$h5(class = "modal-title", id = ns("modalLabel"), title),
                        tags$button(type = "button", class = "btn-close", `data-bs-dismiss` = "modal", `aria-label` = "Close")
                    ),
                    tags$div(
                        class = "modal-body",
                        tags$p(description)
                    ),
                    tags$div(
                        class = "modal-footer",
                        tags$button(type = "button", class = "btn btn-secondary", `data-bs-dismiss` = "modal", "Cerrar"),
                        tags$button(type = "button", class = "btn btn-primary", "Guardar cambios")
                    )
                )
            )
        )
    )
}
