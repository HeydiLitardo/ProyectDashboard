library(shiny)
library(shiny.router)

# Load utility functions
source("utilities/getTimeFilterChoices.R")
source("utilities/getMetricsChoices.R")
source("utilities/getExternalLink.R")

# Load constant variables
consts <- use("constants.R")

# Definir la UI para las páginas de login y registro con enlaces de rutas
login_page <- htmlTemplate(
  "www/login.html",
  login_link = route_link("login"),
  register_link = route_link("register")
)

register_page <- htmlTemplate(
  "www/register.html",
  login_link = route_link("login"),
  register_link = route_link("register")
)

index_page <- htmlTemplate(
  "www/index.html",
  login_link = route_link("login"),
  register_link = route_link("register"),
  appTitle = "Dashboard App",
  appVersion = "1.0.0",
  selectYear = selectInput(
    "selected_year", NULL,
    choices = getYearChoices(consts$data_first_day, consts$data_last_day),
    selectize = TRUE,
    width = "100px",
  ),
  selectMonth = selectInput(
    "selected_month", NULL,
    choices = getMonthsChoices(year = NULL, consts$data_last_day),
    selected = month(consts$data_last_day),
    selectize = TRUE,
    width = "180px",
  ),
  previousTimeRange = selectInput(
    "previous_time_range", NULL,
    choices = consts$prev_time_range_choices,
    selected = "prev_year",
    selectize = TRUE,
    width = "180px",
  ),
  uploadButton = actionButton("upload_button", "Cargar Archivo",
    icon = icon("upload"),
    style = "color: white; background-color: #5b0e91; border-color: #5b0e91;",
    onclick = "document.getElementById('file').click();"
  ),
  vertical_bar_chart = vertical_bar_chart$ui("time_chart"),
  contents = tableOutput("contents")
)

# Definir el enrutador
router <- router_ui(
  route("/", login_page),
  route("register", register_page),
  route("login", login_page),
  route("index", index_page)
)

# Definir la UI principal
ui <- fluidPage(
  id = "test",
  tags$style("#test {
                width: 100%;
                height: 100vh;
                background-color: #5b0e91;
                padding: 0;
                margin: 0;
              }
              .modal {
                display: block;
                opacity: 1;
                }
              "),
  tags$head(
    tags$link(
      rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css",
      integrity = "sha384-QWTKZyjpPEjISv5WaRU9OFeRpok6YctnYmDr5pNlyT2bRjXh0JMhjY6hW+ALEwIH", crossorigin = "anonymous"
    ),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  router,
  tags$script(
    src = "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/js/bootstrap.bundle.min.js",
    integrity = "sha384-YvpcrYf0tY3lHB60NNkmXc5s9fDVZLESaAA55NDzOxhy9GkcIdslK1eN7N6jIeHz",
    crossorigin = "anonymous"
  ),
)
