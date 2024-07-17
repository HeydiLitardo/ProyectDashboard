library(shiny.router)

# Definir la UI del sidebar y el contenido principal
sidebar_ui <- div(
    id = "sidebarMenu",
    class = "col-md-3 col-lg-2 d-md-block bg-light sidebar collapse",
    div(
        class = "position-sticky pt-3",
        tags$ul(
            class = "nav flex-column",
            tags$li(class = "nav-item", tags$a(class = "nav-link active", href = "#!/dashboard/index", "Inicio")),
            tags$li(class = "nav-item", tags$a(class = "nav-link", href = "#!/dashboard/dashboard", "Dashboard")),
            tags$li(class = "nav-item", tags$a(class = "nav-link", href = "#!/dashboard/orders", "Pedidos")),
            tags$li(class = "nav-item", tags$a(class = "nav-link", href = "#!/dashboard/products", "Productos")),
            tags$li(class = "nav-item", tags$a(class = "nav-link", href = "#!/dashboard/customers", "Clientes"))
        )
    )
)

index_page <- htmlTemplate(
  "www/index.html",
  login_link = route_link("login"),
  register_link = route_link("register"),
  appTitle = "Dashboard App",
  appVersion = "1.0.0",
  selectYear = selectInput(
    "selected_year", "Year",
    choices = getYearChoices(consts$data_first_day, consts$data_last_day),
    selectize = TRUE
  ),
  selectMonth = selectInput(
    "selected_month", "Month",
    choices = getMonthsChoices(year = NULL, consts$data_last_day),
    selected = month(consts$data_last_day),
    selectize = TRUE
  ),
  previousTimeRange = selectInput(
    "previous_time_range", "Compare to",
    choices = consts$prev_time_range_choices,
    selected = "prev_year",
    selectize = TRUE
  ),
  vertical_bar_chart = vertical_bar_chart$ui("time_chart"),
  contents = tableOutput("contents")
)

dashboard_page <- div(
    class = "container-fluid",
    div(
        class = "row",
        sidebar_ui,
        main_content_ui(
            router_ui(
                route("dashboard/index", index_page),
                route("dashboard/dashboard", div(h2("Dashboard"), p("Contenido del Dashboard..."))),
                route("dashboard/orders", div(h2("Pedidos"), p("Contenido de Pedidos..."))),
                route("dashboard/products", div(h2("Productos"), p("Contenido de Productos..."))),
                route("dashboard/customers", div(h2("Clientes"), p("Contenido de Clientes...")))
            )
        )
    )
)

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

# Definir el enrutador
router <- router_ui(
    route("/", login_page),
    route("register", register_page),
    route("login", login_page),
)
