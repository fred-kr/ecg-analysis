box::use(
  base[paste],
  bs4Dash[
    dashboardHeader,
    dashboardBrand,
    actionButton,
    infoBox,
  ],
  shinyjs,
  magrittr[`%>%`],
  shiny[...],
  tools,
  fst[read_fst],
  readr[read_csv, read_delim],
  pryr,
  utils,
  shiny.info,
)
box::use(
  app/logic/utils[md_icon],
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  dashboardHeader(
    shinyjs$useShinyjs(),
    title = dashboardBrand(
      title = "ECG Analysis",
      image = "static/images/heart-pulse.svg"
    ),
    border = TRUE,
    controlbarIcon = md_icon("filter-settings"),
    sidebarIcon = md_icon("menu"),
    fixed = FALSE,
    span(
      class = "header-components-container",
      actionButton(
        class = c("dev-btn", "header-btn"),
        inputId = ns("refresh"),
        label = "Refresh",
        icon = md_icon("refresh")
      ),
      actionButton(
        class = c("dev-btn", "header-btn"),
        inputId = ns("in_vars"),
        label = "Input Variables",
        icon = md_icon("console")
      ),
      actionButton(
        class = c("dev-btn", "header-btn"),
        inputId = ns("mem_usage"),
        label = "Show memory usage",
        icon = md_icon("memory")
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$refresh, {
      session$reload()
    })

    observeEvent(input$in_vars, {
      li_inputs <- reactiveValuesToList(input)
      print(li_inputs)
    })

    # Prints memory usage to console
    observeEvent(input$mem_usage, {
      print(pryr$mem_used())
    })

  })
}
