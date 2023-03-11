box::use(
  magrittr[`%>%`],
  bs4Dash[bs4AB = actionButton, tabItem, box, infoBox, boxDropdown, boxDropdownItem, boxSidebar],
  DT[renderDT, DTOutput, datatable],
  shiny[shAB = actionButton, ...],
  rlang,
  fst[read_fst],
  readr[read_rds, read_csv2, read_delim, locale],
  tt = tidytable,
  tools,
  utils,
  sW = shinyWidgets,
  shinyjs,
)
box::use(
  app/logic/utils[md_icon]
)


#' @export
ui <- function(id) {
  ns <- NS(id)
  shinyjs$useShinyjs()

  tabItem(
    tabName = "import",
    fluidRow(
      column(
        width = 2,
        box(
          width = NULL,
          title = "Data Import Settings",
          id = "data-import-settings-box",
          height = "680px",
          fillCol(
            flex = c(1, 0.3, 1, 0.3, 3.5, 0.3, 1, 0.3, 0.8),
            fillRow(
              selectizeInput(
                inputId = ns("loaded_data"),
                label = "Loaded datasets:",
                choices = list("Male 5 Data" = "m5_filter"),
                options = list(create = TRUE)
              )
            ),
            tags$hr(),
            fillRow(
              selectInput(
                inputId = ns("file_type"),
                label = "Select file type",
                choices = list(
                  "CSV" = "csv",
                  "RDS" = "rds",
                  "FST" = "fst"
                )
              )
            ),
            tags$hr(),
            uiOutput(ns("import_options")),
            tags$hr(),
            fillRow(
              fileInput(
                inputId = ns("file_upload"),
                label = "Choose file",
                accept = c(".fst", ".rds", ".txt", ".csv")
              )
            ),
            tags$hr(),
            fillRow(
              tagList(
                tags$span(
                  style = "display: flex; justify-content: space-evenly; margin-top: 10px;",
                  bs4AB(
                    inputId = ns("confirm_import_opts"),
                    label = "Confirm",
                    status = "success"
                  ),
                  bs4AB(
                    inputId = ns("reset_import"),
                    label = "Reset inputs",
                    status = "info"
                  ),
                  bs4AB(
                    inputId = ns("clear_cache"),
                    label = "Clear Memory",
                    status = "danger"
                  )
                )
              )
            )
          )
        )
      ),
      column(
        width = 10,
        box(
          width = NULL,
          title = "Data preview",
          id = "data-preview-box",
          tags$div(
            class = "import-preview",
            DTOutput(ns("data_preview"))
          )
        )
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    demo_data <- read_fst("app/static/data/crab_demo_fst.fst") %>%
      tt$as_tidytable()

    dataset <- reactiveVal(demo_data)

    selected_file_type <- reactive({
      input$file_type
    })

    output$import_options <- renderUI({
      ns <- session$ns

      # UI elements for FST file upload ----
      fst_options <- tagList(
        tags$div(
          class = "fst-options",
          sW$numericRangeInput(
            inputId = ns("fst_opts_range"),
            label = "Specify rows to load:",
            value = c(1, 100),
            min = 1
          )
        )
      )

      # UI elements for CSV file upload ----
      csv_options <- fillRow(
        fillCol(
          width = "80%",
          tagList(
            tags$div(
              class = "csv-opts-delim",
              sW$awesomeRadio(
                inputId = ns("csv_opts_delim"),
                label = "Column separator:",
                choices = c(
                  "Tab" = "\t",
                  "Comma" = ",",
                  "Semicolon" = ";"
                ),
                selected = "\t"
              )
            ),
            tags$div(
              class = "csv-opts-skip",
              numericInput(
                inputId = ns("csv_opts_skip"),
                label = "Skip rows:",
                value = 0,
                min = 0
              )
            )
          )
        ),
        fillCol(
          tagList(
            tags$div(
              class = "csv-opts-quote",
              sW$awesomeRadio(
                inputId = ns("csv_opts_quote"),
                label = "Quotation character:",
                choices = c(
                  "None" = "",
                  "Double Quote" = '"',
                  "Single Quote" = "'"
                ),
                selected = '"'
              )
            ),
            tags$div(
              class = "csv-opts-decimal-mark",
              sW$awesomeRadio(
                inputId = ns("csv_opts_decimal_mark"),
                label = "Decimal separator:",
                choices = c(
                  "Dot" = ".",
                  "Comma" = ","
                ),
                selected = "."
              )
            ),
            tags$div(
              class = "csv-opts-col-names",
              sW$awesomeCheckbox(
                inputId = ns("csv_opts_col_names"),
                label = "First row as column names?",
                value = TRUE
              )
            )
          )
        )
      )

      # Return correct UI elements depending on file type ----
      switch(selected_file_type(),
        "csv" = csv_options,
        "txt" = csv_options,
        "fst" = fst_options,
        "rds" = "No input options exist for this file type"
      )
    })

    # Reset to initial value (empty)
    observeEvent(input$reset_import, {
      shinyjs$reset(id = "file_upload")
      shinyjs$reset(id = "loaded_data")
    })

    # Clear/delete the currently loaded data
    observeEvent(input$clear_cache, {
      dataset(NULL)
    })

    # Loads selected data set in tidytable format
    observeEvent(input$confirm_import_opts, {
      req(input$file_upload)
      dataset(NULL)

      file <- input$file_upload
      file_name <- file$name
      path <- file$datapath
      ext <- tools$file_ext(path)
      content <- switch(ext,
        "fst" = read_fst(
          path,
          from = input$fst_opts_range[1],
          to = input$fst_opts_range[2]
        ),
        "rds" = read_rds(path),
        "csv" = read_delim(
          file = path,
          delim = input$csv_opts_delim,
          skip = input$csv_opts_skip,
          quote = input$csv_opts_quote,
          locale = locale(decimal_mark = input$csv_opts_decimal_mark),
          col_names = input$csv_opts_col_names
        ),
        "txt" = read_delim(
          file = path,
          delim = input$csv_opts_delim,
          skip = input$csv_opts_skip,
          quote = input$csv_opts_quote,
          locale = locale(decimal_mark = input$csv_opts_decimal_mark),
          col_names = input$csv_opts_col_names
        )
      )

      updateSelectizeInput(
        inputId = "loaded_data",
        choices = c(isolate(input$loaded_data), file_name),
        selected = isolate(input$loaded_data)
      )

      dataset(tt$as_tidytable(content))
    })

    observeEvent(input$loaded_data, {
      if (input$loaded_data == "m5_filter") {
        dataset(demo_data)
      }
    })


    # Display the first 10 rows of the loaded data as a preview
    output$data_preview <- renderDT({
      datatable(
        data = utils$head(dataset(), n = 10),
        style = "bootstrap4"
      )
    })

    # `dataset` is a reactiveVal() containing the data to be analysed in form of a
    # tidytable. The reactive component gets returned to the `dashboard_body`
    # module, where the data it holds can be accessed by adding a pair of
    # parentheses after the return value (e.g. `raw_input <-
    # mod_import$server("imp"); raw_input()`)
    return(req(dataset))
  })
}
