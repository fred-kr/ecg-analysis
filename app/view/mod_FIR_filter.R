box::use(
  magrittr[`%>%`],
  shiny[tagList, numericInput, conditionalPanel, moduleServer, NS, observeEvent,
        updateNumericInput, reactiveValues],
  gsignal[filter, freqz],
  tidytable[transmute],
  sW = shinyWidgets,
  bs4Dash[actionButton]
)
box::use(
  app/logic/utils[
    create_FIR_filter,
    reactive_storage,
  ],
)

#' @export
ui <- function(id){
  ns <- NS(id)

  tagList(
    numericInput(
      inputId = ns("f_s"),
      label = "Enter the sampling frequency of your data in Hz",
      value = 400,
      min = 1
    ),
    # FIXME: Possibly won't work cause filter_type is an input from the transform module. Maybe ns param of conditionalPanel is a workaround?
    conditionalPanel(
      condition = "input.filter_type == 'low' || input.filter_type == 'high'",
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
      condition = "input.filter_type == 'stop' || input.filter_type == 'pass'",
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
    actionButton(
      inputId = ns("apply_filter"),
      label = "Apply filter settings"
    )
  )

}

#' @export
server <- function(id, filter_type, raw_data){
  moduleServer(id, function(input, output, session) {
    re_data_fir <- reactiveValues(slot_1 = NULL)
    re_fir_prop <- reactiveValues(slot_1 = NULL)

    observeEvent(input$f_s, {
      if (filter_type() %in% c("low", "high")) {
        updateNumericInput(
          inputId = "low_high",
          max = input$f_s
        )
      } else if (filter_type() %in% c("stop", "pass")) {
        updateNumericInput(
          inputId = "stop_pass",
          max = input$f_s
        )
      }
    })

    observeEvent(input$apply_filter, {
      fir_w <- switch(
        filter_type(),
        'low' = input$low_high,
        'high' = input$low_high,
        'stop' = c(input$stop_pass[1], input$stop_pass[2]),
        'pass' = c(input$stop_pass[1], input$stop_pass[2])
      )

      fir_filter <- create_FIR_filter(
        n = input$filter_order,
        w = fir_w,
        f_s = input$f_s,
        type = filter_type()
      )

      fir_properties_plot <- freqz(fir_filter)
      fir_data <- filter(fir_filter, raw_data())

      store_fir_prop <- reactive_storage(max_slots = 3)
      store_fir_prop(storage = re_fir_prop, data = fir_properties_plot)

      store_fir_data <- reactive_storage(max_slots = 3)
      store_fir_data(storage = re_data_fir, data = fir_data)
    })

    return(list(data = re_data_fir, props = re_fir_prop))
  })
}
