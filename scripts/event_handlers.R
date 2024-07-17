handle_auth_events <- function(input, output, session, con) {
  observeEvent(input$login_button, {
    email <- input$email
    password <- input$password

    res <- dbGetQuery(con, sprintf("SELECT * FROM users WHERE email = '%s' AND password = '%s'", email, password))

    if (nrow(res) > 0) {
      login_user()
      session$sendCustomMessage(type = "redirect", message = list(url = "#!/dashboard/index"))
    } else {
      showModal(modalDialog(
        title = "Error de Login",
        "Usuario o contraseña incorrectos.",
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })

  observeEvent(input$register_button, {
    first_name <- input$first_name
    last_name <- input$last_name
    company <- input$company
    email <- input$email
    password <- input$password
    confirm_password <- input$confirm_password

    if (password != confirm_password) {
      showModal(modalDialog(
        title = "Error de Registro",
        "Las contraseñas no coinciden.",
        easyClose = TRUE,
        footer = NULL
      ))
      return()
    }

    if (nchar(first_name) == 0 || nchar(last_name) == 0 || nchar(company) == 0 || nchar(email) == 0 || nchar(password) == 0) {
      showModal(modalDialog(
        title = "Error de Registro",
        "Por favor, complete todos los campos.",
        easyClose = TRUE,
        footer = NULL
      ))
      return()
    }

    res <- dbGetQuery(con, sprintf("SELECT * FROM users WHERE email = '%s'", email))

    if (nrow(res) > 0) {
      showModal(modalDialog(
        title = "Error de Registro",
        "El usuario ya existe.",
        easyClose = TRUE,
        footer = NULL
      ))
      return()
    }

    dbExecute(con, sprintf(
      "INSERT INTO users (first_name, last_name, company, email, password) VALUES ('%s', '%s', '%s', '%s', '%s')",
      first_name, last_name, company, email, password
    ))

    showModal(modalDialog(
      title = "Registro Exitoso",
      "Tu cuenta ha sido creada.",
      easyClose = TRUE,
      footer = NULL
    ))

    session$sendCustomMessage(type = "redirect", message = list(url = "#!/login"))
  })
}
