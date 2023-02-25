box::use(
  bs4Dash[dashboardBody, tabItems, tabItem, box, tabBox, actionButton, infoBox],
  DT[DTOutput, renderDT, datatable],
  magrittr[`%>%`],
  plotly[plotlyOutput, plot_ly],
  shiny[moduleServer, NS, tagList, tags, tabPanel, req, observeEvent, reactiveVal, reactive, observe, bindCache, isolate, renderTable, tableOutput, icon, fileInput],
  tools,
  fst[read_fst],
  readr[read_csv, read_rds, read_delim],
  tidytable,
  rlang,
)
box::use(
  app/view/charts,
  app/view/mod_import,
  app/view/mod_transform,
)

#' @export
ui <- function(id){
  ns <- NS(id)

  dashboardBody(
    tags$head(
      tags$link(
        href = "https://cdnjs.cloudflare.com/ajax/libs/MaterialDesign-Webfont/7.1.96/css/materialdesignicons.min.css",
        rel = "stylesheet",
        integrity = "sha512-NaaXI5f4rdmlThv3ZAVS44U9yNWJaUYWzPhvlg5SC7nMRvQYV9suauRK3gVbxh7qjE33ApTPD+hkOW78VSHyeg==",
        crossorigin = "anonymous",
        referrerpolicy = "no-referrer"
      )
    ),
    tabItems(
      tabItem(
        tabName = "intro",
        infoBox(
          title = "Introduction",
          value = "Lorem ipsum",
          icon = icon(
            name = NULL,
            class = NULL,
            lib = NULL,
            tags$span(
              class = c("mdi", "mdi-information")
            )
          ),
          width = 8
        )
      ),
      mod_import$ui(ns("mod_import")),
      tabItem(
        tabName = "transform",
        box(
          width = 6,
          title = "TransformationStation"
        )
      ),
      charts$ui(ns("visualisation")),
      tabItem(
        tabName = "analysis",
        "Coming soon"
      )
    )
  )
}

#' @export
server <- function(id){
  moduleServer(id, function(input, output, session) {
    raw_data <- mod_import$server("mod_import")
    selected_filter <- mod_transform$server("mod_transform")
    charts$server("visualisation", data = raw_data)
  })
}
