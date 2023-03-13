box::use(
  bs4Dash[
    dashboardSidebar,
    sidebarMenu,
    menuItem,
  ],
  shiny[moduleServer, NS, tagList, tags, observeEvent, icon, reactive],
)
box::use(
  app/logic/utils[md_icon],
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
        icon = md_icon("help-circle")
      ),
      menuItem(
        text = "Import",
        tabName = "import",
        icon = md_icon("file-import")
      ),
      menuItem(
        text = "Transformation",
        tabName = "transform",
        icon = md_icon("file-document-edit")
      ),
      menuItem(
        text = "Visualisation",
        tabName = "visualisation",
        icon = md_icon("sine-wave")
      ),
      menuItem(
        text = "Analysis",
        tabName = "analysis",
        icon = md_icon("magnify-scan")
      ),
      menuItem(
        text = "Report",
        tabName = "report",
        icon = md_icon("file-export")
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
