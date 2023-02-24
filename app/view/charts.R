box::use(
  bs4Dash[dashboardBody, tabItem, tabBox],
  magrittr[`%>%`],
  plotly[plotlyOutput, plot_ly, layout, add_trace, renderPlotly],
  shiny[moduleServer, NS, tags, tabPanel, req],
  tidytable
)

#' @export
ui <- function(id){
  ns <- NS(id)

  tabItem(
    tabName = "visualisation",
    tabBox(
      id = ns("plots"),
      width = 12,
      maximizable = TRUE,
      collapsible = FALSE,
      type = "tabs",
      tabPanel(
        title = "Raw vs Filtered",
        value = "plots_raw_filtered",
        plotlyOutput(
          outputId = ns("raw"),
          height = "500px"
        ),
        tags$hr(),
        plotlyOutput(
          outputId = ns("filtered"),
          height = "500px"
        )
      ),
      tabPanel(
        title = "Linked",
        value = "plots_linked",
        plotlyOutput(
          outputId = ns("linked")
        )
      ),
      tabPanel(
        title = "Info",
        value = "plots_info",
        plotlyOutput(
          outputId = ns("info")
        )
      )
    )
  )
}

#' @export
server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    # filtered_data <- data$filtered

    # from <- range$from
    # to <- range$to

    output$raw <- renderPlotly({
      raw_data <- data$data$m5_Z_norm
      if (!is.null(raw_data)) {
        data_plot_raw <- tidytable$tidytable(
          x = seq_along(raw_data),
          y = raw_data
        )

        plot_ly(
          data = data_plot_raw,
          type = "scatter",
          mode = "lines",
          line = list(width = "1px")
        ) %>%
          add_trace(x = ~x, y = ~y) %>%
          layout(
            showlegend = FALSE,
            title = "Raw ECG data (Z-score normed)",
            xaxis = list(rangeslider = list(visible = TRUE))
          )
      }

      # filtered_data <-
      # if (!is.null(filtered_data)) {
      #   data_plot_filtered <- tidytable$tidytable(
      #     x = seq_along(raw_data),
      #     y = raw_data
      #   )
      #
      #   plot_ly(
      #     data = data_plot_filtered,
      #     type = "scatter",
      #     mode = "lines",
      #     line = list(width = "1px")
      #   ) %>%
      #     add_trace(x = ~x, y = ~y) %>%
      #     layout(
      #       showlegend = FALSE,
      #       title = "Raw ECG data (Z-score normed)",
      #       xaxis = list(rangeslider = list(visible = TRUE))
      #     )
      # }

    })
  })
}
