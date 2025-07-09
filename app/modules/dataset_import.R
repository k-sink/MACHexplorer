###################################
### DATASET IMPORT MODULE ###
###################################
datasetImportUI <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    fluidRow(
      column(
        width = 12,
        wellPanel(
          h6(strong("WELCOME TO MACH EXPLORER")),
          p("This app allows users to navigate and manipulate the MACH dataset, 
            which contains daily climate and streamflow data along with catchment 
            attributes for 1,014 watersheds within the United States. 
            All tabs retrieve data based on the sites selected on the 'Site Selection' tab.")
        )
      )
    ),
    br(),
    fluidRow(
      column(
        width = 12,
        wellPanel(
          h6(strong("DATA IMPORT")),
          p("Select the 'full_dataset.duckdb' file to connect to the database. 
            Download it from the GitHub repository if you haven't already: ",
            tags$a(href="https://github.com/k-sink/MACHexplorer/releases", target="_blank", "GitHub Releases")),
          fileInput(
            ns("db_file"),
            label = "Choose MACH Dataset (full_dataset.duckdb)",
            accept = ".duckdb",
            buttonLabel = HTML('<i class="fa-solid fa-folder-open"></i> Browse')
          ),
          uiOutput(ns("connection_status"))
        )
      )
    )
  )
}

datasetImportServer <- function(id, shared_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive value to track connection status
    status <- reactiveValues(
      connected = FALSE,
      message = ""
    )
    
    # Render connection status
    output$connection_status <- renderUI({
      if (status$connected) {
        div(
          style = "background-color: #d4edda; border: 1px solid #c3e6cb; padding: 10px; border-radius: 5px;",
          icon("check-circle", style = "color: #155724; margin-right: 8px;"),
          span("Connected to database!", style = "color: #155724; font-weight: bold;")
        )
      } else if (status$message != "") {
        div(
          style = "background-color: #f8d7da; border: 1px solid #f5c6cb; padding: 10px; border-radius: 5px;",
          icon("exclamation-circle", style = "color: #721c24; margin-right: 8px;"),
          span(status$message, style = "color: #721c24; font-weight: bold;")
        )
      } else {
        div(
          style = "background-color: #e2e3e5; border: 1px solid #d3d3d3; padding: 10px; border-radius: 5px;",
          icon("info-circle", style = "color: #6c757d; margin-right: 8px;"),
          span("Select a DuckDB file to connect.", style = "color: #6c757d; font-weight: bold;")
        )
      }
    })
    
    # Handle file selection and database connection
    observeEvent(input$db_file, {
      if (is.null(input$db_file)) {
        status$message <- "No file selected."
        showNotification(status$message, type = "error")
        return()
      }
      
      # Check file extension
      if (!grepl("\\.duckdb$", input$db_file$datapath, ignore.case = TRUE)) {
        status$message <- "Invalid file format. Please select a .duckdb file."
        showNotification(status$message, type = "error")
        return()
      }
      
      # Close existing connection if valid
      if (!is.null(shared_data$duckdb_connection) && DBI::dbIsValid(shared_data$duckdb_connection)) {
        DBI::dbDisconnect(shared_data$duckdb_connection, shutdown = TRUE)
        message("Closed previous DuckDB connection")
      }
      
      # Connect to new database
      tryCatch({
        con <- DBI::dbConnect(duckdb::duckdb(), dbdir = input$db_file$datapath, read_only = TRUE)
        
        # Check for required tables
        required_tables <- c("mach_daily", "mopex_daily", "land_cover", "site_info")
        actual_tables <- DBI::dbListTables(con)
        missing_tables <- setdiff(required_tables, actual_tables)
        
        if (length(missing_tables) > 0) {
          DBI::dbDisconnect(con, shutdown = TRUE)
          status$message <- paste("Missing required tables:", paste(missing_tables, collapse = ", "))
          showNotification(status$message, type = "error")
          return()
        }
        
        # Extract site IDs from mach_daily
        site_ids <- dplyr::tbl(con, "mach_daily") %>%
          dplyr::select(SITENO) %>%
          dplyr::distinct() %>%
          dplyr::pull(SITENO)
        
        if (length(site_ids) == 0) {
          DBI::dbDisconnect(con, shutdown = TRUE)
          status$message <- "No sites found in mach_daily table."
          showNotification(status$message, type = "warning")
          return()
        }
        
        # Successful connection
        shared_data$duckdb_connection <- con
        shared_data$mach_ids <- site_ids
        shared_data$database_ready <- TRUE
        status$connected <- TRUE
        status$message <- ""
        showNotification(sprintf("Connected to database with %d sites.", length(site_ids)), type = "message")
        
      }, error = function(e) {
        status$message <- paste("Failed to connect to database:", e$message)
        showNotification(status$message, type = "error")
        shared_data$duckdb_connection <- NULL
        shared_data$mach_ids <- character(0)
        shared_data$database_ready <- FALSE
      })
      
      gc()
    })
  })
}