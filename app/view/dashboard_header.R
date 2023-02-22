box::use(
  base[paste],
  bs4Dash[
    dashboardHeader,
    dashboardBrand,
    actionButton,
    infoBox,
  ],
  shinyjs[useShinyjs],
  magrittr[`%>%`],
  shiny[
    moduleServer,
    NS,
    tagList,
    tags,
    span,
    div,
    icon,
    observeEvent,
    reactiveValuesToList,
    reactiveValues,
    eventReactive,
    modalDialog,
    showModal,
    removeModal,
    fileInput,
    selectInput,
    modalButton,
    HTML,
    reactive,
    observe,
    isolate,
    req,
  ],
  tools,
  fst[read_fst],
  readr[read_csv, read_delim],
)
# box::use(
#
# )

#' @export
ui <- function(id) {
  ns <- NS(id)

  dashboardHeader(
    useShinyjs(),
    title = dashboardBrand(
      title = "ECG Analysis",
      image = "static/images/heart-pulse.svg"
    ),
    border = TRUE,
    controlbarIcon = icon(
      name = NULL,
      class = NULL,
      lib = NULL,
      tags$span(
        class = c("mdi", "mdi-filter-settings")
      )
    ),
    sidebarIcon = icon(
      name = NULL,
      class = NULL,
      lib = NULL,
      tags$span(
        class = c("mdi", "mdi-menu")
      )
    ),
    fixed = FALSE,
    span(
      class = "header-components-container",
      actionButton(
        class = c("dev-btn", "header-btn"),
        inputId = ns("refresh"),
        label = "Refresh",
        icon = icon(
          name = NULL,
          class = NULL,
          lib = NULL,
          tags$span(class = c("mdi", "mdi-refresh"))
        )
      ),
      actionButton(
        class = c("dev-btn", "header-btn"),
        inputId = ns("in_vars"),
        label = "Input Variables",
        icon = icon(
          name = NULL,
          class = NULL,
          lib = NULL,
          tags$span(class = c("mdi", "mdi-console"))
        )
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
  })
}
