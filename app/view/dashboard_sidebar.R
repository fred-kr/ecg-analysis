box::use(
  bs4Dash[
    dashboardSidebar,
    sidebarMenu,
    menuItem,
    updateControlbar,
    updateControlbarMenu
  ],
  shiny[moduleServer, NS, tagList, tags, observeEvent, icon, reactive],
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  dashboardSidebar(
    id = "sidebar",
    collapsed = FALSE,
    sidebarMenu(
      id = ns("sidebar_menu"),
      menuItem(
        text = "Intro",
        tabName = "intro",
        icon = icon(
          name = NULL,
          class = NULL,
          lib = NULL,
          tags$span(
            class = c("mdi", "mdi-help-circle")
          )
        )
      ),
      menuItem(
        text = "Import",
        tabName = "import",
        icon = icon(
          name = NULL,
          class = NULL,
          lib = NULL,
          tags$span(
            class = c("mdi", "mdi-file-import")
          )
        )
      ),
      menuItem(
        text = "Transformation",
        tabName = "transform",
        icon = icon(
          name = NULL,
          class = NULL,
          lib = NULL,
          tags$span(
            class = c("mdi", "mdi-file-document-edit")
          )
        )
      ),
      menuItem(
        text = "Visualisation",
        tabName = "visualisation",
        icon = icon(
          name = NULL,
          class = NULL,
          lib = NULL,
          tags$span(
            class = c("mdi", "mdi-sine-wave")
          )
        )
      ),
      menuItem(
        text = "Analysis",
        tabName = "analysis",
        icon = icon(
          name = NULL,
          class = NULL,
          lib = NULL,
          tags$span(
            class = c("mdi", "mdi-magnify-scan")
          )
        )
      ),
      menuItem(
        text = "Report",
        tabName = "report",
        icon = icon(
          name = NULL,
          class = NULL,
          lib = NULL,
          tags$span(
            class = c("mdi", "mdi-file-export")
          )
        )
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # return(active_tab <- reactive({ input$sidebar_menu }))
  })
}
