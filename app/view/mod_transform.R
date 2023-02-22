box::use(
  magrittr[`%>%`],
  shiny[moduleServer, NS, tagList],
)

#' @export
ui <- function(id){
  ns <- NS(id)

  tagList(

  )
}

#' @export
server <- function(id){
  moduleServer(id, function(input, output, session) {

  })
}
