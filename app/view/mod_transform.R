box::use(
  magrittr[`%>%`],
  bs4Dash[tabItem, box, actionButton],
  DT[renderDT, DTOutput, datatable],
  shiny[...],
  rl = rlang,
  tt = tidytable,
  tools,
  utils,
  sW = shinyWidgets,
  shinyjs,
  stats[sd],
  tidyselect[ends_with],
)
box::use(
  app/logic/utils[
    create_FIR_filter,
    create_WT_filter,
    rv_as_list,
  ],
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

      # Change column types
      # Subset data by factor column(s) if they exist/Allow interactive grouping
      # Provide summary/descriptive statistics (per group/factor)
      # Ability to save multiple different subsets of the original data
      # Provide aceEditor window to allow editing via code
      # Provide a way to save/reload settings/selections in future sessions
#' @export
server <- function(id, data){
  moduleServer(id, function(input, output, session) {
    ## Helper functions #####
    # Norming data
    min_max_norm <- function(x, ...) {
      return((x - min(x, ...)) / (max(x, ...) - min(x, ...)))
    }

    z_score_norm <- function(x, ...) {
      return((x - mean(x, ...)) / sd(x, ...))
    }

    # Save `input$filter_family` as a reactive expression ----
    filt_fam <- reactive({ input$filter_family })

    # Dynamic UI elements for selection of signal/index column ----
    output$col_selection <- renderUI({
      req(data$temp)
      ns <- session$ns
      tagList(
        varSelectInput(
          inputId = ns("col_signal"),
          label = "Specify which column contains the signal values",
          data = data$temp()
        ),
        sW$awesomeCheckbox(
          inputId = ns("toggle_col_index"),
          label = "Specify column containing corresponding time values?"
        ),
        shinyjs$hidden(
          varSelectInput(
            inputId = ns("col_index"),
            label =  "Optional indexing column",
            data = data$temp()
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
    core_tidy_df <- reactive({
      # Signal column
      raw_sig <- data$temp()[[!!input$col_signal]]

      # If no index column is provided, create one by creating a sequence along
      # the raw signal column, from 1 to length(raw_sig)
      index <-
        tt$if_else(input$toggle_col_index, data$temp()[[!!input$col_index]], seq_along(raw_sig))

      tidy_df <- tt$tidytable(index = index, raw_sig = raw_sig)
      return(tidy_df)
    })

    # Add column with normalized values (if selected by user) ----
    normalized_data <- eventReactive(input$norm_method, {
      if (is.null(core_tidy_df)) {
        return(NULL)
      }

      core_df <- core_tidy_df()

      if (input$norm_method == "z_score") {
        core_df %>% tt$mutate(z_score_normed = z_score_norm(raw_sig))
      } else if (input$norm_method == "min_max") {
        core_df %>% tt$mutate(min_max_normed = min_max_norm(raw_sig))
      } else {
        core_df
      }
    })

    # Add column with filtered values ----
    # TODO: Make it so selecting a different filtering method just adds a new
    # column to the existing core data table
    smoothed_data <- eventReactive(input$filter_family, {
      if (is.null(normalized_data)) {
        return(NULL)
      }

      smoothed_col <- switch(
        input$filter_family,
        "fir" = mod_FIR_filter$server("fir", normalized_data),
        "wt" = mod_WT_filter$server("wt", normalized_data),
        "sw" = mod_SW_filter$server("sw", normalized_data),
        "No smoothing filter selected"
      )

      # For FIR filters, this is a list with objects `data` and `filt_info`.
      # `filt_info` contains the output of gsignal::freqz(fir_filter) (a plot)
      # For WT filters, ...
      # For SW filers, ...
      return(
        list(
          data = smoothed_col$data,
          filt_info = smoothed_col$filt_info
        )
      )
    })

    # Combine raw, normed and filtered (and index) into one tidytable ----
    main_data <- reactive({
      # normalized_data() is a tidytable with columns `index`, `raw_sig` and, if
      # a normalization method was selected, either `z_score_normed` or
      # `min_max_normed`
      # smoothed_data$data is a numeric vector
      validate(
        need(normalized_data, "No normalized/raw values found"),
        need(smoothed_data$data, "No smoothed values found")
      )

      combined <- normalized_data() %>%
        tt$mutate(smoothed = smoothed_data$data)

      return(combined)
    })

    # Display a preview of the the three columns (index, raw, filtered)
    output$data_preview <- renderDT({
      req(main_data)
      datatable(
        data = utils$head(main_data(), n = 10),
        style = "bootstrap4"
      )
    })

    return(
      list(
        main_df = reactive({ main_data }),
        filter_family = reactive({ filt_fam }),
        extra = reactive({ smoothed_data()$filt_info })
      )
    )
  })
}


