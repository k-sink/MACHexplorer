###################################
### DATASET IMPORT MODULE ###
###################################

dataset_import_ui <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    fluidRow(
      column(width = 12,
        wellPanel(
          h6(strong("WELCOME TO MACH EXPLORER")),
          p("This app allows users to navigate and manipulate the MACH dataset, which contains daily 
            climate and streamflow data along with catchment attributes for 1,014 watersheds within the 
            United States. All tabs retrieve data based on the sites selected on the 'Site Selection' tab.")
        )
      )
    ),
    br(),
    fluidRow(
      column(width = 12,
        wellPanel(
          h6(strong("DATA IMPORT")),
          p(strong("This application uses data from the MACH dataset, which is consolidated into a single DuckDB file. 
                   DuckDB is a relational database management system that offers a client API for R.")),
          tags$ul(
            tags$li(HTML("Prior to utilizing any of these tabs, download the full_dataset.duckdb file from <a href='https://zenodo.org/records/15311986' target='_blank'>https://zenodo.org/records/15311986</a>.")),
            tags$li("Save the database file to your local machine, making sure to retain the 'full_dataset.duckdb' name and DUCKDB format.")
          )
        )
      )
    ),
    br(),
    fluidRow(
      column(width = 12,
        wellPanel(
          h6(strong("LOCATE DATABASE FILE")),
          p(strong("Use the browse button to locate the full_dataset database file.")),
          shinyFiles::shinyFilesButton(ns("duck_database_btn"),
           label = HTML('<i class="fa-solid fa-folder-open"></i> Browse for database file'),
           title = "Please select the full_dataset DuckDB database file", multiple = FALSE),
          textOutput(ns("database_folderpath"))
        )
      )
    )
  )
}

#' dataset_import Server Functions
#'
#' @noRd
mod_dataset_import_server <- function(id, shared_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

# setup shinyFiles for selecting a .duckdb file
    roots <-  c(Home = normalizePath("~"), Computer = "/")

    shinyFiles::shinyFileChoose(
      input, "duck_database_btn", roots = roots, filetypes = "duckdb")

   # ReactiveVal to store selected file path
    selected_db <- reactiveVal("")

    # Update selected_db when a file is chosen
    observeEvent(input$duck_database_btn, {
      file_info <- parseFilePaths(roots, input$duck_database_btn)
      message("file_info: ", toString(file_info))
      if (is.null(file_info) || nrow(file_info) == 0 || is.null(file_info$datapath) || !nzchar(file_info$datapath)) {
        selected_db("")
        message("No valid file selected")
        return()
      }
      path <- as.character(file_info$datapath[1])
      selected_db(path)
      message("Selected file path: ", path)
    })

    # Render selected file path
    output$database_folderpath <- renderText({
      db <- selected_db()
      if (db == "") return("No file selected")
      db
    })

    # Handle file selection and connection
    observeEvent(input$duck_database_btn, {
      req(selected_db() != "")
      path <- selected_db()
      if (!file.exists(path)) {
        shinyalert("Invalid file", "Selected DuckDB file not found.", type = "error")
        selected_db("")
        return()
      }
      if (basename(path) != "full_dataset.duckdb") {
        shinyalert("Invalid file name", "Please select a file named 'full_dataset.duckdb'.", type = "error")
        selected_db("")
        return()
      }

      withProgress(message = "Connecting to DuckDB...", value = 0, {
        # Close previous connection
        if (!is.null(isolate(shared_data$duckdb_con)) && DBI::dbIsValid(isolate(shared_data$duckdb_con))) {
          tryCatch({
            DBI::dbDisconnect(isolate(shared_data$duckdb_con), shutdown = TRUE)
            message("Closed previous DuckDB connection")
          }, error = function(e) {
            message("Error closing previous DuckDB connection: ", e$message)
          })
        }

        tryCatch({
          con <- get_duckdb_con(NULL, path)
          expected_tables <- c(
            "mach_daily", "mopex_daily", "annual_climate", "anthropogenic",
            "discharge_mach", "geology", "hydrology", "land_cover",
            "monthly_climate", "overall_climate", "regional", "site_info", "soil"
          )

          if (!validate_duckdb_tables(con, expected_tables)) {
            DBI::dbDisconnect(con, shutdown = TRUE)
            shinyalert::shinyalert("Invalid database", "Missing one or more expected tables.", type = "error")
            return()
          }

          shared_data$duckdb_path <- path
          shared_data$duckdb_con <- con
          shared_data$tables <- lapply(expected_tables, function(tbl) dplyr::tbl(con, tbl))
          names(shared_data$tables) <- expected_tables
          shared_data$mach_ids <- get_site_ids(con, "mach_daily")
          shared_data$mopex_ids <- get_site_ids(con, "mopex_daily")

          incProgress(1)
          shinyalert::shinyalert(
            title = "Success",
            text = sprintf("Database connected. %d MACH sites and %d MOPEX sites loaded.",
                           length(shared_data$mach_ids), length(shared_data$mopex_ids)),
            type = "success"
          )
        }, error = function(e) {
          message("Connection error: ", e$message)
          shinyalert::shinyalert("Failed to connect", paste("Error:", e$message), type = "error")
          shared_data$duckdb_con <- NULL
          shared_data$duckdb_path <- NULL
          shared_data$tables <- list()
          shared_data$mach_ids <- character(0)
          shared_data$mopex_ids <- character(0)
        })

        gc()
      })
    })

  })
}
