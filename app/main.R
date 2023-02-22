box::use(
  bs4Dash[dashboardPage],
  shiny[moduleServer, NS, stopApp, reactiveValues, observeEvent, onStop, HTML],
  htmlwidgets[JS],
)
box::use(
  app/view/dashboard_header,
  app/view/dashboard_body,
  app/view/dashboard_sidebar,
  app/view/dashboard_controlbar,
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  dashboardPage(
    title = "ECG Analysis",
    header = dashboard_header$ui(ns("header")),
    body = dashboard_body$ui(ns("body")),
    sidebar = dashboard_sidebar$ui(ns("sidebar")),
    controlbar = dashboard_controlbar$ui(ns("controlbar"))
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    options(shiny.maxRequestSize = 1000*1024^2)

    re_plots_data <- reactiveValues(raw = NULL, filtered = NULL)
    re_plots <- reactiveValues(raw = NULL, filtered = NULL, template = NULL)

    dashboard_header$server("header")
    dashboard_sidebar$server("sidebar")
    dashboard_body$server("body")

    # Stop R process when closing browser window
    session$onSessionEnded(stopApp)
  })
}
