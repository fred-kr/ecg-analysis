box::use(
  bs4Dash[
    dashboardSidebar,
    sidebarMenu,
    menuItem,
    updateControlbar,
    updateControlbarMenu
  ],
  shiny[moduleServer, NS, tagList, tags, observeEvent, icon],
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
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$sidebar_menu, {
      current_tab_id <- input$sidebar_menu

      if (current_tab_id == "visualisation") {
        updateControlbar("controlbar")
      }

      updateControlbarMenu(
        inputId = "controlbar_menu",
        selected = current_tab_id
      )
    })
  })
}
