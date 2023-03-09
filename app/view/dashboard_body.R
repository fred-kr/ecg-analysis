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
    re_data <- reactiveValues(
        df = NULL,
        filter_type = NULL,
        extra = NULL
    )

    # `import_data` stores the reactiveVal() returned by the import module (a
    # tidytable object). The data inside can be accessed via `import_data()`
    import_data <- mod_import$server("mod_import")

    # The imported data gets passed along to the transformation module. It
    # returns a list of variable length, depending on what methods and
    # selections were made inside the transformation module. However, the first
    # two components will always be a dataframe containing the raw and
    # transformed signal values (+ index column), as well as a `character()`
    # indicating the type of filter that was selected.
    processed_data <- mod_transform$server("mod_transform", data = import_data)

    observe({
      updated_data <- processed_data()
      re_data$df <- updated_data$df
      re_data$filter_type <- updated_data$filt_type
      re_data$extra <- updated_data$filt_info
      print("Body observer just fired")
    })

    charts$server("visualisation", data = raw_data)
  })
}
