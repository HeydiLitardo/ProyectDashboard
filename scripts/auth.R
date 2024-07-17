# Función para autenticar usuarios
authenticate_user <- function(con, email, password) {
  res <- dbGetQuery(con, sprintf("SELECT * FROM users WHERE email = '%s' AND password = '%s'", email, password))
  print(res)
  return(nrow(res) > 0)
}

# Función para registrar usuarios
register_user <- function(con, first_name, last_name, company, email, password, confirm_password) {
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
