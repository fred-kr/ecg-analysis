box::use(
  magrittr[`%>%`],
  shiny[moduleServer, NS, tagList, tags,],
)

#' @export
ui <- function(id){
  ns <- NS(id)

  tagList(
    # "Haar" = "wt_h",
    # "Beylkin" = "wt_b",
    # "Coiflet" = "wt_c",
    # "Daubechies" = "wt_d",
    # "Symmlet" = "wt_s"
  )
}

#' @export
server <- function(id, filter_type, raw_data){
  moduleServer(id, function(input, output, session) {

  })
}
