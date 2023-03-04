box::use(
  bs4Dash[dashboardBody, tabItems, tabItem, box, tabBox, actionButton, infoBox],
  DT[DTOutput, renderDT, datatable],
  magrittr[`%>%`],
  plotly[plotlyOutput, plot_ly],
  shiny[moduleServer, NS, tagList, tags, tabPanel, req, observeEvent, reactiveVal, reactive, observe, bindCache, isolate, renderTable, tableOutput, icon, fileInput, reactiveValues],
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
    # Allows the use of material design icons
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
      mod_transform$ui(ns("mod_transform")),
      charts$ui(ns("visualisation")),
      tabItem(
        tabName = "analysis",
        "Coming soon"
      )
    )
  )
}

#' @export
server <- function(id, data){
  moduleServer(id, function(input, output, session) {
    # Initialize reactive value that will store the data that is currently being
    # worked on
    re_data <-
      reactiveValues(
        df = NULL,
        filter_type = NULL,
        temp = NULL,
        extra = NULL
      )

    # Initial file upload gets stored in the `temp` value, will get deleted (?)
    # once the signal (and optionally index) column have been selected in the
    # transform tab to save memory
    re_data$temp <- mod_import$server("mod_import")

    # Pass data in temp to transform module for relevant column selection and
    # application of smoothing filters
    processed_data <- mod_transform$server("mod_transform", data = re_data)

    charts$server("visualisation", data = raw_data)
  })
}
