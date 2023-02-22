box::use(
  bs4Dash[dashboardControlbar],
  magrittr[`%>%`],
  shiny[moduleServer, NS, tagList, tags],
)

#' @export
ui <- function(id){
  ns <- NS(id)

  dashboardControlbar()
}

#' @export
server <- function(id){
  moduleServer(id, function(input, output, session) {

  })
}
