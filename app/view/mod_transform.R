box::use(
  magrittr[`%>%`],
  bs4Dash[tabItem, box, actionButton],
  DT[renderDT, DTOutput, datatable],
  shiny[...],
  rlang,
  tidytable,
  tools,
  utils,
  sW = shinyWidgets,
  shinyjs,
)
box::use(
  app/logic/utils[
    create_FIR_filter,
    create_WT_filter,
    reactive_vals_as_list,
  ]
)
#' @export
ui <- function(id, data){
  ns <- NS(id)

  box(
    width = 12,
    title = "Modify your data",
    varSelectInput(
      inputId = ns("col_signal"),
      label = "Specify which column contains the signal values",
      data = data
    ),
    sW$materialSwitch(
      inputId = ns("toggle_index_col"),
      label = "Specify column containing corresponding time values?"
    ),
    conditionalPanel(
      condition = "input.toggle_index_col == true",
      varSelectInput(
        inputId = ns("col_index"),
        label = "Optional indexing column",
        data = data
      )
    ),
    selectInput(
      inputId = ns("norm_method"),
      label = "Select what method should be used to normalize the data",
      choices = list(
        "None" = "",
        "Z-Score" = "z_score",
        "Min-Max" = "min_max"
      )
    ),
    selectInput(
      inputId = ns("filter_type"),
      label = "Select what filtering method to use for smoothing the data",
      choices = list(
        "None" = "",
        "FIR-Filters" = list(
          "Low-Pass" = "low",
          "High-Pass" = "high",
          "Pass-Band" = "pass",
          "Stop-Band" = "stop"
        ),
        "Wavelet-Transform" = list(
          "Haar" = "wt_h",
          "Beylkin" = "wt_b",
          "Coiflet" = "wt_c",
          "Daubechies" = "wt_d",
          "Symmlet" = "wt_s"
        ),
        "Sliding Window" = list(
          "Moving Average" = "m_mean",
          "Moving Median" = "m_median",
          "Savitzky-Golay" = "sgolay"
        )
      )
    ),
    uiOutput()
  )

  box(
    width = 12,
    title = "Preview",
    tags$div(
      class = "modified-preview",
      DTOutput(ns("data_preview"), width = "fit-content")
    )
  )
}

      # Specify which column contains the signal values. Optionally allow
      #   selection of a indexing/time/etc column, same length as value column
      # Option(s) for normalizing the data (z-score,...)
      # Filter data (smoothing) either here or in controlbar in the visualisation tab?
      # Change column types
      # Subset data by factor column(s) if they exist/Allow interactive grouping
      # Provide summary/descriptive statistics (per group/factor)
      # Ability to save multiple different subsets of the original data
      # Provide aceEditor window to allow editing via code
#' @export
server <- function(id, raw_data){
  moduleServer(id, function(input, output, session) {
    filter_type <- reactive({ input$filter_type })

    updated_data <- reactive({
      if (input$filter_type %in% c("low", "high", "pass", "stop")) {
        filter <- mod_FIR_filter$server("fir")
      } else if (input$filter_type %in% c("wt_h", "wt_b", "wt_c", "wt_d", "wt_s")) {
        filter <- mod_WT_filter$server("wt")
      } else if (input$filter_type %in% c("m_mean", "m_median", "sgolay")) {
        filter <- mod_SW_filter$server("sw")
      } else {
        filter <- raw_data
      }

      # TODO: if the list of slots inside the reactive value is longer than 1 entry, display all slots and let user pick which to keep using
      if () {

      }
    })

    output$data_preview <- renderDT({
      datatable(
        data = utils$head(updated_data(), n = 10),
        style = "bootstrap4"
      )
    })




  })
}
