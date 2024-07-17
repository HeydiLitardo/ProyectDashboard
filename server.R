library(shiny)
library(RSQLite)
library(shiny.router)

source("utilities/clean_data.R")

daily_stats <-
  read.csv("data/daily_stats.csv", header = TRUE, stringsAsFactors = TRUE) %>%
  mutate(date = ymd(date))

monthly_stats <- read.csv("data/monthly_stats.csv", header = TRUE) %>%
  mutate(date = ymd(date))

yearly_stats <- read.csv("data/yearly_stats.csv", header = TRUE) %>%
  mutate(date = ymd(date))

countries_stats <- read.csv("data/countries_stats.csv", header = TRUE) %>%
  mutate(date = ymd(date))

# Función para definir el servidor
server <- function(input, output, session) {
  # Conectar a la base de datos
  con <- dbConnect(SQLite(), dbname = "dbb_test.sqlite")

  # Crear la tabla de usuarios si no existe
  dbExecute(con, "CREATE TABLE IF NOT EXISTS users (first_name TEXT, last_name TEXT, company TEXT, email TEXT, password TEXT)")

  # Función para autenticar usuarios
  authenticate_user <- function(email, password) {
    res <- dbGetQuery(con, sprintf("SELECT * FROM users WHERE email = '%s' AND password = '%s'", email, password))
    print(res)
    return(nrow(res) > 0)
  }

  # Función para registrar usuarios
  register_user <- function(first_name, last_name, company, email, password, confirm_password) {
    if (password != confirm_password) {
      return(FALSE)
    }
    if (nchar(first_name) == 0 || nchar(last_name) == 0 || nchar(company) == 0 || nchar(email) == 0 || nchar(password) == 0) {
      return(FALSE)
    }
    if (nrow(dbGetQuery(con, sprintf("SELECT * FROM users WHERE email = '%s'", email))) > 0) {
      return(FALSE)
    }
    dbExecute(con, sprintf(
      "INSERT INTO users (first_name, last_name, company, email, password) VALUES ('%s', '%s', '%s', '%s', '%s')",
      first_name, last_name, company, email, password
    ))
    return(TRUE)
  }

  # Manejar el evento de login
  observeEvent(input$login_button, {
    if (nchar(input$email) == 0 || nchar(input$password) == 0) {
      showModal(modalDialog(
        title = "Error de Login",
        "Por favor, ingrese su usuario y contraseña.",
        easyClose = TRUE,
        footer = NULL
      ))
      return()
    }
    if (authenticate_user(input$email, input$password)) {
      session$sendCustomMessage(type = "redirect", message = list(url = "#!/index"))
    } else {
      showModal(modalDialog(
        title = "Error de Login",
        "Usuario o contraseña incorrectos.",
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })

  # Manejar el evento de registro
  observeEvent(input$register_button, {
    if (nchar(input$first_name) == 0 || nchar(input$last_name) == 0 || nchar(input$company) == 0 || nchar(input$email) == 0 || nchar(input$confirm_password) == 0 || nchar(input$password) == 0) {
      showModal(modalDialog(
        title = "Error de Registro",
        "Por favor, complete todos los campos.",
        easyClose = TRUE,
        footer = NULL
      ))
      return()
    }
    if (register_user(input$first_name, input$last_name, input$company, input$email, input$confirm_password, input$password)) {
      showModal(modalDialog(
        title = "Registro Exitoso",
        "Tu cuenta ha sido creada.",
        easyClose = TRUE,
        footer = NULL
      ))
    } else {
      showModal(modalDialog(
        title = "Error de Registro",
        "El usuario ya existe o los datos son inválidos.",
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })

  observeEvent(c(input$selected_year), {
    months_choices <-
      getMonthsChoices(input$selected_year, consts$data_last_day)
    selected_month <-
      ifelse(input$selected_month %in% months_choices,
        input$selected_month,
        "0"
      )
    updateSelectInput(session,
      "selected_month",
      selected = selected_month,
      choices = months_choices
    )
  })

  observeEvent(c(input$selected_month), {
    if (input$selected_month == "0") {
      updateSelectInput(
        session,
        "previous_time_range",
        choices = consts$prev_time_range_choices["Previous Year"],
        selected = consts$prev_time_range_choices[["Previous Year"]]
      )
    } else {
      updateSelectInput(
        session,
        "previous_time_range",
        choices = consts$prev_time_range_choices,
        selected = input$previous_time_range
      )
    }
  })

  selected_year <- reactive({
    input$selected_year
  })
  selected_month <- reactive({
    input$selected_month
  })
  previous_time_range <- reactive({
    input$previous_time_range
  })

  # Manejar la carga de archivos
  observeEvent(input$file, {
    shinyjs::show("loading")
    req(input$file)
    ext <- tools::file_ext(input$file$datapath)

    if (ext == "csv") {
      data <- clean_data(input$file$datapath)
    } else if (ext == "xlsx") {
      data <- read_excel(input$file$datapath, col_names = input$header)
    } else {
      showModal(modalDialog(
        title = "Error de archivo",
        "Archivo no soportado. Por favor suba un archivo CSV o Excel.",
        easyClose = TRUE,
        footer = NULL
      ))
      shinyjs::hide("loading")
      return(NULL)
    }

    # Verificar si las columnas existen antes de inicializar el gráfico
    required_columns <- c("Fecha.y.hora", "Cantidad")
    if (all(required_columns %in% colnames(data))) {
      session$userData$uploaded_data <- reactive({
        data
      })
      vertical_bar_chart$init_server("time_chart")
    } else {
      showModal(modalDialog(
        title = "Error en los Datos",
        "Las columnas requeridas no están presentes en el archivo cargado.",
        easyClose = TRUE,
        footer = NULL
      ))
    }
    shinyjs::hide("loading")
  })


  router_server()
}
