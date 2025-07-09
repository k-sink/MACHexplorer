# Source global configuration
source("global.R")

# Source all modules
source("modules/dataset_import.R")
# Add other module sources here as needed (e.g., source("modules/site_selection.R"))

# Increase file upload size limit to 1GB
options(shiny.maxRequestSize = 1000 * 1024^2)

ui <- fluidPage(
  # Add custom CSS for resizing
  tags$head(tags$style(HTML("
    .well { 
      min-height: 100px; 
      height: auto !important; 
      overflow: hidden; 
    }
    .dataTables_scroll {
      max-height: 70vh !important;
      overflow-y: auto !important;
    }
    .dataTables_paginate { 
      margin-top: 10px !important; 
    }
    .dataTables_info { 
      margin-top: 10px !important; 
    }
  "))),
  
  # Bootstrap CSS framework
  theme = bslib::bs_theme(bootswatch = "lumen"),
  
  # Implement shinyjs features
  shinyjs::useShinyjs(),
  
  titlePanel("MACH Explorer"),
  
  # Create tabs
  tabsetPanel(
    tabPanel("Data Import", datasetImportUI("dataset_import")),
    tabPanel("Site Selection", "Site selection content here"),
    tabPanel("Daily Data", "Daily data content here"),
    tabPanel("Monthly Data", "Monthly data content here"),
    tabPanel("Annual Data", "Annual data content here")
  )
)

server <- function(input, output, session) {
  # Initialize shared data
  shared_data <- reactiveValues(
    database_ready = FALSE,
    mach_ids = character(0),
    duckdb_connection = NULL
  )
  
  # Call module servers
  datasetImportServer("dataset_import", shared_data)
  # Add other module servers here as needed
  
  # Ensure database connection is closed on session end
  onSessionEnded(function() {
    if (!is.null(isolate(shared_data$duckdb_connection)) && DBI::dbIsValid(isolate(shared_data$duckdb_connection))) {
      DBI::dbDisconnect(isolate(shared_data$duckdb_connection), shutdown = TRUE)
      message("DuckDB connection closed on session end")
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)