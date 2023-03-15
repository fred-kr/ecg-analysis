box::use(
  magrittr[`%>%`],
  bs4Dash[
    bs4AB = actionButton, tabItem, box, infoBox, tabBox, boxDropdown, boxDropdownItem, boxSidebar, createAlert
  ],
  DT[renderDT, DTOutput, datatable],
  shiny[shAB = actionButton, ...],
  shinyalert[shinyalert],
  rlang,
  fst[read_fst],
  readr[read_rds, read_csv2, read_delim, locale],
  tt = tidytable,
  tools,
  utils,
  sW = shinyWidgets,
  shinyjs,
  htmlwidgets[JS],
)
box::use(
  app/logic/utils[md_icon, rv_remove_all_but_first, rv_remove_key, `%!in%`]
)


#' @export
ui <- function(id) {

  ns <- NS(id)

  tabItem(
    shinyjs$useShinyjs(),
    tabName = "import",
    tags$span(
      style = "display: flex; flex-direction: row; justify-content: flex-start; column-gap: 15px; width: 100%;",
      tags$div(
        style = "flex: 0 0 25%; max-width: 410px !important; min-width: 390px !important;",
        box(
          id = ns("data_import_settings"),
          title = "Data Import Settings",
          width = NULL,
          height = "675px", # to align with table
          fillCol(
            flex = c(1, 0.3, 1, 0.3, 3.5, 0.3, 1, 0.3, 0.8),
            fillRow(
              selectizeInput(
                inputId = ns("loaded_data"),
                label = "Loaded datasets:",
                choices = c("Male 5"),
                options = list(create = TRUE),
                multiple = FALSE
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
                multiple = FALSE,
                accept = c(".fst", ".csv", ".txt", ".rds")
              )
            ),
            tags$hr(),
            fillRow(
              tagList(
                tags$span(
                  style = "display: flex; justify-content: space-evenly; margin-top: 10px;",
                  bs4AB(
                    inputId = ns("confirm"),
                    label = "Confirm",
                    status = "success"
                  ),
                  bs4AB(
                    inputId = ns("clear_active"),
                    label = "Clear active data",
                    status = "warning"
                  ),
                  bs4AB(
                    inputId = ns("clear_all"),
                    label = "Clear Memory",
                    status = "danger"
                  )
                )
              )
            )
          )
        )
      ),
      tags$div(
        style = "flex: 1 1 75%; max-width: 100%; min-width: fit-content;",
        box(
          id = ns("data_preview"),
          title = "Data preview",
          height = "675px",
          width = NULL,
          tags$div(
            class = "import-preview",
            DTOutput(ns("data_preview"))
          )
        )
      )
    ),
    tags$span(
      style = "display: flex; flex-direction: row; justify-content: flex-start; column-gap: 15px; width: 100%;",
      column(
        width = 6,
        offset = 3,
        # Access with `input$data_info_box` to get state of box / update from server with
        # updateBox
        tabBox(
          id = ns("data_info"),
          width = NULL,
          type = "tabs",
          tabPanel(
            title = "Str",
            value = "str",
            icon = md_icon("table-question"),
            verbatimTextOutput(ns("data_str"))
          ),
          tabPanel(
            title = "Summary",
            value = "summ",
            icon = md_icon("table-eye"),
            verbatimTextOutput(ns("data_summ"))
          )
        )
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Loads demo data on app start up
    demo_data <- read_fst("app/static/data/crab_demo_fst.fst") %>%
      tt$as_tidytable()

    active_dataset <- reactiveVal(demo_data)

    # Store all file uploads as tidytables
    # FIXME: Probably not all that efficient
    loaded_datasets <- reactiveValues("Male 5" = demo_data)

    # Stores names of uploaded files as character vector to be used as choices in a selectInput()
    dataset_names <- reactiveVal("Male 5")

    selected_file_type <- reactive({ input$file_type })

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
      tags$span(
        style = "display: flex; justify-content: space-between; margin-top: 10px;",
        fillCol(
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
                min = 0,
                width = "60%"
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
                  "Double Quote [\"]" = "\"",
                  "Single Quote [']" = "'"
                ),
                selected = "\""
              )
            ),
            tags$div(
              class = "csv-opts-decimal-mark",
              sW$awesomeRadio(
                inputId = ns("csv_opts_decimal_mark"),
                label = "Decimal separator:",
                choices = c(
                  "Dot [.]" = ".",
                  "Comma [,]" = ","
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
    )

    # Return correct UI elements depending on file type
    output$import_options <- renderUI({
      switch(selected_file_type(),
        "csv" = csv_options,
        "txt" = csv_options,
        "fst" = fst_options,
        "rds" = "No input options exist for this file type"
      )
    })

    # Logic ----

    # Delete currently active data from memory
    observeEvent(input$clear_active, {
      name <- input$loaded_data
      # Disallow deletion of demo data set
      if (name == "Male 5" && length(dataset_names()) == 1L) {
        shinyalert(
          title = "Not Possible",
          text = "The demo dataset cannot be deleted",
          type = "error",
          closeOnEsc = TRUE,
          closeOnClickOutside = TRUE,
          showConfirmButton = TRUE
        )

        return()
      }

      name_index <- which(dataset_names() == name)
      data <- loaded_datasets$name

      # Removes name of data from dataset_names
      dataset_names(dataset_names()[-name_index])

      updateSelectizeInput(
        inputId = "loaded_data",
        choices = dataset_names(),
        selected = dataset_names()[1]
      )

      # Removes data from loaded_datasets
      rv_remove_key(loaded_datasets, name)

    })

    # Clear all in-memory data sets
    observeEvent(input$clear_all, {
      name <- input$loaded_data
      # Disallow deletion of demo data set
      if (name == "Male 5" && length(dataset_names()) == 1L) {
        shinyalert(
          title = "Not Possible",
          text = "The demo dataset cannot be deleted",
          type = "error",
          closeOnEsc = TRUE,
          closeOnClickOutside = TRUE,
          showConfirmButton = TRUE
        )

        return()
      }

      # Removes every data set but the demo one
      rv_remove_all_but_first(loaded_datasets)

      # Resets stored data set names
      dataset_names("Male 5")

      # Resets selectInput
      updateSelectizeInput(
        inputId = "loaded_data",
        choices = dataset_names(),
        selected = "Male 5"
      )

      # Resets fileInput selection
      shinyjs$reset(id = "file_upload")

      # Resets import options
      shinyjs$reset(id = "import_options")
    })

    # Loads selected data set in tidytable format
    observeEvent(input$confirm, {
      req(input$file_upload)
      # active_dataset(NULL)

      file <- input$file_upload
      file_name <- file$name
      path <- file$datapath
      ext <- tools$file_ext(path)

      # If data isn't already loaded, add it to the selection of data sets
      if (file_name %!in% dataset_names()) {
        content <- switch(
          ext,
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

        table_data <- tt$as_tidytable(content)

        # Add name of uploaded file to dataset_names()
        dataset_names(c(dataset_names(), file_name))

        # Add it to the list of data sets in the selectInput()
        updateSelectizeInput(
          inputId = "loaded_data",
          choices = dataset_names(),
          selected = file_name
        )

        # Add it to the loaded_datasets reactiveValues()
        loaded_datasets[[paste0(file_name)]] <- table_data
      }

      active_dataset(loaded_datasets[[paste0(file_name)]])
    })

    observeEvent(input$loaded_data, {
      selection <- loaded_datasets[[paste0(input$loaded_data)]]
      active_dataset(selection)
    })


    # Display the first 10 rows of the loaded data as a preview
    output$data_preview <- renderDT({
      datatable(
        data = utils$head(active_dataset(), n = 10),
        # height = "675px",
        # class = "",
        rownames = FALSE,
        # fillContainer = FALSE,
        # autoHideNavigation = TRUE,
        style = "bootstrap4",
        # options = list(pageLength = 10)
      )
    })

    # `dataset` is a reactiveVal() containing the data to be analysed in form of a
    # tidytable. The reactive component gets returned to the `dashboard_body`
    # module, where the data it holds can be accessed by adding a pair of
    # parentheses after the return value (e.g. `raw_input <-
    # mod_import$server("imp"); raw_input()`)
    return(req(active_dataset))
  })
}
