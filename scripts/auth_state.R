# Estado reactivo para manejar la autenticación del usuario
is_authenticated <- reactiveVal(FALSE)

# Función para iniciar sesión
login_user <- function() {
  is_authenticated(TRUE)
}

# Función para cerrar sesión
logout_user <- function() {
  is_authenticated(FALSE)
}
