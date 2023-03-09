box::use(
  magrittr[`%>%`],
  shiny[moduleServer, NS, tagList, tags,],
)

#' @export
ui <- function(id){
  ns <- NS(id)

  tagList(
    # "Moving Average" = "m_mean",
    # "Moving Median" = "m_median",
    # "Savitzky-Golay" = "sgolay"
  )
}

#' @export
server <- function(id, filter_type, raw_data){
  moduleServer(id, function(input, output, session) {

  })
}
