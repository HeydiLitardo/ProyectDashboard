library(shiny)
library(DBI)

server <- function(input, output, session) {
    # Conectar a la base de datos
    con <- dbConnect(SQLite(), dbname = "./dbb_test.sqlite")

    # Crear la tabla de usuarios si no existe
    dbExecute(con, "CREATE TABLE IF NOT EXISTS users (first_name TEXT, last_name TEXT, company TEXT, email TEXT, password TEXT)")

    handle_auth_events(input, output, session, con)
    handle_date_selection_events(input, output, session)
    handle_file_upload_event(input, output, session)

    router_server()
}
