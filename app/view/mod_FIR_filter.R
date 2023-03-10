box::use(
  magrittr[`%>%`],
  shiny[tagList, numericInput, conditionalPanel, moduleServer, NS, observeEvent,
        updateNumericInput, reactiveValues, reactive, uiOutput, renderUI, tags, observe, req, selectInput],
  gsignal[filter, freqz],
  tidytable[transmute],
  sW = shinyWidgets,
  bs4Dash[actionButton],
  shinyjs,
)
box::use(
  app/logic/utils[
    create_FIR_filter,
  ],
)

#' @export
ui <- function(id){
  ns <- NS(id)

  # shinyjs$useShinyjs()
  tagList(
    numericInput(
      inputId = ns("f_s"),
      label = "Enter the sampling frequency of your data in Hz",
      value = 400,
      min = 1
    ),
    selectInput(
      inputId = ns("filter_type"),
      label = "Specify the type of filter you want to use",
      choices = list(
        "Low-Pass" = "low",
        "High-Pass" = "high",
        "Pass-Band" = "pass",
        "Stop-Band" = "stop"
      )
    ),
    conditionalPanel(
      condition = paste0("input.", ns("filter_type"), "== 'low' || input.", ns("filter_type"), "== 'high'"),
      numericInput(
        inputId = ns("low_high"),
        label = "Desired cutoff frequency in Hz",
        min = 0,
        max = 400,
        value = 2,
        step = 0.1
      )
    ),
    conditionalPanel(
      condition = paste0(
        "input.",
        ns("filter_type"),
        "== 'stop' || input.",
        ns("filter_type"),
        "== 'pass'"
      ),
      sW$numericRangeInput(
        inputId = ns("stop_pass"),
        label = "Desired band edges in Hz. The first value has to be smaller than the last",
        min = 0,
        max = 400,
        value = c(0.5, 3),
        step = 0.1
      )
    ),
    numericInput(
      inputId = ns("filter_order"),
      label = "Filter order (1 less than filter length)",
      min = 2,
      value = 30,
      step = 1
    ),
    actionButton(inputId = ns("apply_filter"), label = "Apply filter settings"),
    actionButton(inputId = ns("view_in"), label = "View Inputs")
  )

}

#' @export
server <- function(id, data){
  moduleServer(id, function(input, output, session) {
    fir_filtered <- reactiveValues(data = NULL, filt_info = NULL, filt_type = NULL)

    # Change maximum allowed value for cutoff frequency depending on sampling
    # frequency
    observeEvent(input$f_s, {
      if (input$filter_type %in% c("low", "high")) {
        updateNumericInput(
          inputId = "low_high",
          max = input$f_s
        )
      } else if (input$filter_type %in% c("stop", "pass")) {
        updateNumericInput(
          inputId = "stop_pass",
          max = input$f_s
        )
      }
    })

    # When the `Apply filter settings` button is pressed, a FIR-filter is
    # created with the current settings and then applied to the signal data
    observeEvent(input$apply_filter, {

      # Cutoff frequency, given in Hz
      fir_w <- switch(
        input$filter_type,
        "low" = input$low_high,
        "high" = input$low_high,
        "stop" = c(input$stop_pass[1], input$stop_pass[2]),
        "pass" = c(input$stop_pass[1], input$stop_pass[2])
      )

      # Actual filter creation, using own wrapper function around
      # gsignal::fir1()
      fir_filter <- create_FIR_filter(
        n = input$filter_order,
        w = fir_w,
        f_s = input$f_s,
        type = input$filter_type
      )

      # Apply the filter and write the new values into the reactiveValues().
      # `gsignal::freqz` gives visual information about the created filter
      fir_filtered$data <- filter(fir_filter, data)
      fir_filtered$filt_info <- freqz(fir_filter)
      fir_filtered$filt_type <- input$filter_type
    })

    # Reactive expression returning `fir_filtered` once all the keys are no
    # longer NULL
    re_out_fir <- reactive({
      if (!is.null(fir_filtered$data) & !is.null(fir_filtered$filt_info) & !is.null(fir_filtered$filt_type)) {
        return(fir_filtered)
      }
    })

    return(re_out_fir)
  })
}
