box::use(
  magrittr[`%>%`],
  bs4Dash[tabItem, box, actionButton],
  DT[renderDT, DTOutput, datatable],
  shiny[moduleServer, NS, tagList, modalDialog, tags, fileInput, selectInput, modalButton, observeEvent, reactive, req, HTML, reactiveVal, reactiveValues, observe, textInput, uiOutput, renderUI, conditionalPanel, numericInput, icon, reactiveValuesToList, isolate, isTruthy],
  rlang,
  fst[read_fst],
  readr[read_rds, read_csv2, read_delim, locale],
  tidytable,
  tools,
  utils,
  sW = shinyWidgets,
  shinyjs,
)
box::use(
)


#' @export
ui <- function(id){
  ns <- NS(id)
  shinyjs$useShinyjs()

  tabItem(
    tabName = "import",
    box(
      width = 12,
      title = "Import your data",
      tags$span(
        class = "import-components-container",
        # style = "display: flex;flex-direction: row;align-items: center;justify-content: flex-start;column-gap: 50px;",
        tags$div(
          class = "data-import-inputs",
          tags$div(
            title = "Accepted file types: fst, rds, csv, txt",
            class = "file_input",
            fileInput(
              inputId = ns("file_upload"),
              label = "Select a file from your computer:",
              accept = c(".fst", ".rds", ".txt", ".csv")
            )
          ),
          tags$div(
            class = "crab-data",
            selectInput(
              inputId = ns("crab_data"),
              label = "Select one of the built-in datasets:",
              choices = list(
                `Select one` = "",
                `Crabs` = list(
                  "Male 5 - Raw" = "m5_raw",
                  "Male 5 - Raw + WT Filter" = "m5_filter",
                  "Male 6 - Raw" = "m6_raw",
                  "Male 6 - Raw + WT Filter" = "m6_filter"
                )
              )
            )
          )
        ),
        tags$div(
          class = "allowed-file-types",
          tags$p(tags$b("Accepted file types:")),
          tags$table(
            id = "file-type-table",
            tags$tbody(
              tags$tr(
                tags$td(".fst"),
                tags$td(HTML("From <code>'fst'</code> R package"))
              ),
              tags$tr(
                tags$td(".rds"),
                tags$td("Standard R file format")
              ),
              tags$tr(
                tags$td(".csv"),
                tags$td("Comma-separated values")
              ),
              tags$tr(
                tags$td(".txt"),
                tags$td("Text file")
              )
            )
          )
        ),
        tags$div(
          class = "import-options",
          uiOutput(ns("import_options"))
        )
      ),
      actionButton(
        inputId = ns("confirm_import_opts"),
        label = "Confirm"
      ),
      actionButton(
        inputId = ns("reset_import"),
        label = "Reset import field"
      ),
      actionButton(
        inputId = ns("clear_cache"),
        label = "Clear loaded data"
      )
    ),
    box(
      width = 12,
      title = "Preview",
      tags$div(
        class = "import-preview",
        DTOutput(ns("data_preview"), width = "fit-content")
      )
    )
  )

}

#' @export
server <- function(id){
  moduleServer(id, function(input, output, session) {

    re_data <- reactiveValues(data = NULL)

    output$import_options <- renderUI({
      req(isTruthy(!is.null(input$file_upload) | input$crab_data != ""))
      ns <- session$ns
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

      csv_options <- tagList(
        tags$div(
          class = "csv-options",
          tags$div(
            class = "csv-1",
            numericInput(
              inputId = ns("csv_opts_skip"),
              label = "Amount of rows to skip:",
              value = 0,
              min = 0
            ),
            sW$awesomeRadio(
              inputId = ns("csv_opts_quote"),
              label = "Character used for quoting strings:",
              choices = c(
                `None` = "",
                `Double Quote` = '"',
                `Single Quote` = "'"
              ),
              selected = '"'
            )
          ),
          tags$div(
            class = "csv-2",
            sW$awesomeRadio(
              inputId = ns("csv_opts_decimal_mark"),
              label = "Decimal separator:",
              choices = c(
                `Dot` = ".",
                `Comma` = ","
              ),
              selected = "."
            ),
            sW$awesomeCheckbox(
              inputId = ns("csv_opts_col_names"),
              label = "Use first row as column names?",
              value = TRUE
            )
          )
        )
      )

      txt_options <- tagList(
        tags$div(
          class = "txt-options",
          sW$awesomeRadio(
            inputId = ns("txt_opts_delim"),
            label = "Column separator:",
            choices = c(
              `Tab` = "\t",
              `Comma` = ",",
              `Semicolon` = ";"
            ),
            selected = "\t"
          ),
          numericInput(
            inputId = ns("txt_opts_skip"),
            label = "Amount of rows to skip:",
            value = 0,
            min = 0
          ),
          sW$awesomeRadio(
            inputId = ns("txt_opts_quote"),
            label = "Character used for quoting strings:",
            choices = c(
              `None` = "",
              `Double Quote` = '"',
              `Single Quote` = "'"
            ),
            selected = '"'
          ),
          sW$awesomeRadio(
            inputId = ns("txt_opts_decimal_mark"),
            label = "Decimal separator:",
            choices = c(
              `Dot` = ".",
              `Comma` = ","
            ),
            selected = "."
          ),
          sW$awesomeCheckbox(
            inputId = ns("txt_opts_col_names"),
            label = "Use first row as column names?",
            value = TRUE
          )
        )
      )

      up <- input$file_upload
      ext <- tools$file_ext(up$datapath)
      switch(
        ext,
        'fst' = fst_options,
        'csv' = csv_options,
        'txt' = txt_options,
        'rds' = "No input options exist for this filetype"
      )
    })

    observeEvent(input$reset_import, {
      shinyjs$reset(id = "file_upload")
      shinyjs$reset(id = "crab_data")
    })

    observeEvent(input$clear_cache, {
      re_data$data <- NULL
    })

    observeEvent(input$confirm_import_opts, {
      re_data$data <- NULL
      if (!is.null(input$file_upload)) {
        file <- input$file_upload
        path <- file$datapath
        ext <- tools$file_ext(path)
        data <- switch(
          ext,
          'fst' = read_fst(path, from = input$fst_opts_range[1], to = input$fst_opts_range[2]),
          'rds' = read_rds(path),
          'csv' = read_csv2(path, skip = input$csv_opts_skip, quote = input$csv_opts_quote, locale = locale(decimal_mark = input$csv_opts_decimal_mark), col_names = input$csv_opts_col_names),
          'txt' = read_delim(path, skip = input$txt_opts_skip, quote = input$txt_opts_quote, locale = locale(decimal_mark = input$txt_opts_decimal_mark), col_names = input$txt_opts_col_names)
        )
        re_data$data <- tidytable$as_tidytable(data)
      } else if (input$crab_data != "") {
        file <- input$crab_data
        data <- switch(
          file,
          'm5_raw' = read_fst("app/static/data/crab_demo_fst.fst"),
          'm5_filter' = read_fst("app/static/data/crab_demo_fst.fst"),
          NULL
        )
        re_data$data <- tidytable$as_tidytable(data)
      }

    })

    output$data_preview <- renderDT({
      datatable(
        data = utils$head(re_data$data, n = 10),
        style = "bootstrap4"
      )
    })

    return(re_data)
  })
}

