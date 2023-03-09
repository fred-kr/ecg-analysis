box::use(
  magrittr[`%>%`],
  bs4Dash[tabItem, box, actionButton, infoBox, boxDropdown, boxDropdownItem],
  DT[renderDT, DTOutput, datatable],
  shiny[moduleServer, NS, tagList, modalDialog, tags, fileInput, selectInput, modalButton, observeEvent, reactive, req, HTML, reactiveVal, reactiveValues, observe, textInput, uiOutput, renderUI, conditionalPanel, numericInput, icon, reactiveValuesToList, isolate, isTruthy, fluidRow, column],
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
  app/logic/utils[md_icon]
)


#' @export
ui <- function(id){
  ns <- NS(id)
  shinyjs$useShinyjs()

  tabItem(
    tabName = "import",
    fluidRow(
      column(
        width = 6,
        box(
          width = NULL,
          title = "Import your data",
          tags$div(
            class = "select-file-type",
            selectInput(
              inputId = ns("file_type"),
              label = "Select file type",
              choices = list(
                "CSV" = "csv",
                "RDS" = "rds",
                "FST" = "fst",
                "TXT" = "txt"
              )
            )
          ),
          tags$hr(),
          tags$span(
            class = "import-components-container",
            tags$div(
              class = "import-options",
              uiOutput(ns("import_options"))
            )
          ),
          tags$hr(),
          tags$div(
            class = "file-input",
            fileInput(
              inputId = ns("file_upload"),
              label = "Choose file",
              accept = c(".fst", ".rds", ".txt", ".csv")
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
        )
      ),
      column(
        width = 6,
        box(
          width = NULL,
          title = "Use one of the integrated data sets",
          tags$div(
            class = "crab-data",
            selectInput(
              inputId = ns("crab_data"),
              label = "Select one of the built-in datasets:",
              choices = list(
                "Select one" = "",
                "Crabs" = list(
                  # "Male 5 - Raw" = "m5_raw",
                  "Male 5 - Raw + WT Filter" = "m5_filter"
                )
              )
            )
          )
        )
      )
    ),
    box(
      id = "data-preview-box",
      width = NULL,
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

    temp <- reactiveVal()

    selected_file_type <- reactive({ input$file_type })

    output$import_options <- renderUI({
      ns <- session$ns

      # UI elements for FST file upload
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

      # UI elements for CSV file upload
      csv_options <- tagList(
        tags$div(
          class = "csv-options",
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
              "None" = "",
              "Double Quote" = '"',
              "Single Quote" = "'"
            ),
            selected = '"'
          ),
          sW$awesomeRadio(
            inputId = ns("csv_opts_decimal_mark"),
            label = "Decimal separator:",
            choices = c("Dot" = ".",
                        "Comma" = ","),
            selected = "."
          ),
          sW$awesomeCheckbox(
            inputId = ns("csv_opts_col_names"),
            label = "Use first row as column names?",
            value = TRUE
          )
        )
      )

      # UI elements for text file upload
      txt_options <- tagList(
        tags$div(
          class = "txt-options",
          sW$awesomeRadio(
            inputId = ns("txt_opts_delim"),
            label = "Column separator:",
            choices = c(
              "Tab" = "\t",
              "Comma" = ",",
              "Semicolon" = ";"
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
              "None" = "",
              "Double Quote" = '"',
              "Single Quote" = "'"
            ),
            selected = '"'
          ),
          sW$awesomeRadio(
            inputId = ns("txt_opts_decimal_mark"),
            label = "Decimal separator:",
            choices = c(
              "Dot" = ".",
              "Comma" = ","
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

      # Render UI elements for the chosen file type
      switch(
        selected_file_type(),
        "csv" = csv_options,
        "fst" = fst_options,
        "txt" = txt_options,
        "rds" = "No input options exist for this file type"
      )
    })

    # Reset the "fileInput" and "selectInput" back to their initial values (NULL
    # and "", respectively)
    observeEvent(input$reset_import, {
      shinyjs$reset(id = "file_upload")
      shinyjs$reset(id = "crab_data")
    })

    # Clear/delete the currently loaded data
    observeEvent(input$clear_cache, {
      temp(NULL)
    })

    # When the "Confirm" button is pressed, either load the user selected file
    # using the correct function or load the selected pre-existing demo data set
    # into the app. In both cases the loaded data will be converted into a
    # tidytable
    observeEvent(input$confirm_import_opts, {
      temp(NULL)
      if (!is.null(input$file_upload)) {
        file <- input$file_upload
        path <- file$datapath
        ext <- tools$file_ext(path)
        content <- switch(
          ext,
          "fst" = read_fst(path, from = input$fst_opts_range[1], to = input$fst_opts_range[2]),
          "rds" = read_rds(path),
          "csv" = read_csv2(path, skip = input$csv_opts_skip, quote = input$csv_opts_quote, locale = locale(decimal_mark = input$csv_opts_decimal_mark), col_names = input$csv_opts_col_names),
          "txt" = read_delim(path, skip = input$txt_opts_skip, quote = input$txt_opts_quote, locale = locale(decimal_mark = input$txt_opts_decimal_mark), col_names = input$txt_opts_col_names)
        )
        temp(tidytable$as_tidytable(content))
      } else if (input$crab_data != "") {
        file <- input$crab_data
        # TODO: Fix demo data
        content <- switch(
          file,
          "m5_raw" = read_fst("app/static/data/crab_demo_fst.fst"),
          "m5_filter" = read_fst("app/static/data/crab_demo_fst.fst"),
          NULL
        )
        temp(tidytable$as_tidytable(content))
      }

    })

    # Display the first 10 rows of the loaded data as a preview
    output$data_preview <- renderDT({
      datatable(
        data = utils$head(temp(), n = 10),
        style = "bootstrap4"
      )
    })

    # `temp` is a reactiveVal() containing the data to be analysed in form of a
    # tidytable. The reactive component gets returned to the `dasboard_body`
    # module, where the data it holds can be accessed by adding a pair of
    # parentheses after the return value (e.g. `raw_input <-
    # mod_import$server("imp"); raw_input()`)
    return(req(temp))
  })
}

