box::use(
  magrittr[`%>%`],
  bs4Dash[tabItem, box, actionButton],
  DT[renderDT, DTOutput, datatable],
  shiny[...],
  rlang,
  tt = tidytable,
  tools,
  utils,
  sW = shinyWidgets,
  shinyjs,
  stats[sd],
  tidyselect[ends_with],
)
box::use(
  app/view/mod_FIR_filter,
  app/view/mod_WT_filter,
  app/view/mod_SW_filter,
)
#' @export
ui <- function(id){
  ns <- NS(id)

  shinyjs$useShinyjs()
  tabItem(
    tabName = "transform",
    fluidRow(
      column(
        width = 4,
        box(
          width = NULL,
          title = "Modify your data",
          uiOutput(ns("col_selection")),
          selectInput(
            inputId = ns("norm_method"),
            label = "Select what method should be used for normalization",
            choices = list(
              "None" = "",
              "Z-Score" = "z_score",
              "Min-Max" = "min_max"
            )
          ),
          selectInput(
            inputId = ns("filter_family"),
            label = "Select what filtering method to use for smoothing the data",
            choices = list(
              "None" = "",
              "FIR-Filters" = "fir",
              "Wavelet-Transform" = "wt",
              "Sliding Window" = "sw"
            )
          )
        )
      ),
      column(
        width = 8,
        box(
          width = NULL,
          uiOutput(ns("filter_ui"))
        )
      )
    ),
    box(
      width = 12,
      title = "Preview",
      tags$div(
        class = "modified-preview",
        DTOutput(ns("data_preview"), width = "fit-content")
      )
    )
  )

}


#' @export
server <- function(id, data){
  moduleServer(id, function(input, output, session) {
    # Helper functions ----
    min_max_norm <- function(x, ...) {
      return((x - min(x, ...)) / (max(x, ...) - min(x, ...)))
    }

    z_score_norm <- function(x, ...) {
      return((x - mean(x, ...)) / sd(x, ...))
    }

    # Dynamic UI elements for selection of signal/index column ----
    output$col_selection <- renderUI({
      req(data())
      ns <- session$ns
      tagList(
        varSelectInput(
          inputId = ns("col_signal"),
          label = "Specify which column contains the signal values",
          data = data()
        ),
        sW$awesomeCheckbox(
          inputId = ns("toggle_col_index"),
          label = "Specify column containing corresponding time values?"
        ),
        shinyjs$hidden(
          varSelectInput(
            inputId = ns("col_index"),
            label =  "Optional indexing column",
            data = data()
          )
        )
      )
    })

    # Logic to show/hide `selectInput` for index column
    observeEvent(input$toggle_col_index, {
      if (input$toggle_col_index) {
        shinyjs$show("col_index")
      } else {
        shinyjs$hide("col_index")
      }
    })


    # Display UI elements for the chosen filter family ----
    output$filter_ui <- renderUI({
      switch(
        input$filter_family,
        "fir" = mod_FIR_filter$ui("fir"),
        "wt" = mod_WT_filter$ui("wt"),
        "sw" = mod_SW_filter$ui("sw"),
        "No smoothing filter selected"
      )
    })


    # Create a tidytable from the index and signal columns ----
    # core_tidy_df <- reactive({
    #   # req(data())
    #   # Assign data to a non-reactive variable to limit re-evaluation
    #   d_inst <- data()
    #
    #   # Return NULL if no data exists
    #   if (is.null(d_inst)) {
    #     return(NULL)
    #   }
    #
    #   # Get index toggle value
    #   index_toggle <- input$toggle_col_index
    #
    #   # Get signal column
    #   raw_sig <- d_inst[[input$col_signal]]
    #
    #   # Get index column based on toggle value
    #   if (!index_toggle) {
    #     index <- seq(from = 0, by = 1, length.out = length(raw_sig))
    #   } else {
    #     index <- d_inst[[input$col_index]]
    #   }
    #
    #   # Create tidytable with index and signal columns
    #   tidy_df <- tt$tidytable(index = index, raw_sig = raw_sig)
    #
    #   return(tidy_df)
    # })


    # Add column with normalized values (if selected by user)
    # normalized_data <- reactive({
    #   # Get core data
    #   core_df <- core_tidy_df()
    #
    #   # Return NULL if no data exists
    #   if (is.null(core_df)) {
    #     return(NULL)
    #   }
    #
    #   # Add normalized column based on selected method
    #   if (input$norm_method == "z_score") {
    #     core_df <- core_df %>% tt$mutate(z_score_normed = z_score_norm(raw_sig))
    #   } else if (input$norm_method == "min_max") {
    #     core_df <- core_df %>% tt$mutate(min_max_normed = min_max_norm(raw_sig))
    #   } else {
    #     core_df
    #   }
    # })
    #
    #
    # # Add column with filtered values ----
    # # TODO: Make it so selecting a different filtering method just adds a new
    # # column to the existing core data table
    # smoothed_data <- eventReactive(input$filter_family, {
    #   norm_dat <- normalized_data()
    #   # Depends on normalized data to exist before it can evaluate
    #   if (is.null(norm_dat)) {
    #     return(NULL)
    #   }
    #
    #   # Normalized values get sent to filter-specific module for smoothing
    #   smoothed_col <- switch(
    #     input$filter_family,
    #     "fir" = mod_FIR_filter$server("fir", norm_dat),
    #     "wt" = mod_WT_filter$server("wt", norm_dat),
    #     "sw" = mod_SW_filter$server("sw", norm_dat),
    #     "No smoothing filter selected"
    #   )
    #
    #   # The returned object is a reactive expression containing a
    #   # `reactiveValues()`. The keys inside the `reactiveValues()` can differ
    #   # slightly depending on the used filtering method
    #   # FIR: `data` and `filt_info`
    #   re_smooth <- smoothed_col()
    #
    #   if (!is.null(re_smooth)) {
    #     return(re_smooth)
    #   }
    # })
    #
    # # Combine raw, normed and filtered (and index) into one tidytable ----
    # main_data <- reactive({
    #   normalized <- normalized_data()
    #   smoothed <- smoothed_data()
    #
    #   if (!is.null(smoothed$data)) {
    #     combined <- normalized %>%
    #       tt$mutate(smoothed = smoothed$data)
    #
    #     return(combined)
    #   }
    # })

    # TODO: Wrap your head around how inputs are allowed to work while
    # transforming, either calculate on button press or add a new column
    # whenever a new combination of values is selected?
    # Display a preview of the the three columns (index, raw, filtered)

    # output$data_preview <- renderDT({
    #   main <- main_data()
    #   validate(need(!is.null(main), "Select a column and specify how it should be filtered"))
    #
    #   datatable(
    #     data = utils$head(main, n = 10),
    #     style = "bootstrap4"
    #   )
    # })
    #
    # # Creates a list that returns the newly filtered data, as well as info about
    # # used filter type and certain attributes
    # re_out_transform <- reactive({
    #   main <- main_data()
    #   filt_props <- smoothed_data()
    #   if (!is.null(main)) {
    #     return(
    #       list(
    #         df = main,
    #         filt_type = filt_props$filt_type,
    #         filt_info = filt_props$filt_info
    #       )
    #     )
    #   }
    # })
    #
    # return(re_out_transform)
  })
}


