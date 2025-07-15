###################################
### MACH EXPLORER APP ###
###################################
# source global configuration file
source("global.R")

# source all modules
source("modules/dataset_import.R")
source("modules/site_selection.R")
source("modules/daily_data.R")
source("modules/monthly_data.R")


# increase file upload size limit to 1GB to allow for 900MB duckdb
options(shiny.maxRequestSize = 1000 * 1024^2)

# ui setup
ui <- fluidPage(
  
  # add custom CSS for universal wellPanel and DataTables sizing
  tags$head(tags$style(HTML("
    .well { 
      height: auto !important; 
      min-height: 100px;
      overflow: hidden; 
    }
    .dataTables_scroll {
      max-height: 70vh !important;
      overflow-y: auto !important;
    }
    .dataTables_scroll:empty {
      max-height: 0px !important;
      overflow: hidden !important;
    }
    .dataTables_paginate { 
      margin-top: 10px !important; 
    }
    .dataTables_info { 
      margin-top: 10px !important; 
    }
    .nav-tabs > li > a.disabled {
      pointer-events: none;
      color: #aaa;
      cursor: not-allowed;
    }
  "))),
  
  # bootstrap CSS framework
  theme = bslib::bs_theme(bootswatch = "lumen"),
  
  # implement shinyjs features
  shinyjs::useShinyjs(),
  
  titlePanel("MACH Explorer"),
  
  # create tabs
  tabsetPanel(
    id = "main_tabs",
    tabPanel("Data Import", dataset_import_ui("dataset_import")),
    tabPanel("Site Selection", site_selection_ui("site_selection")),
    tabPanel("Daily Data", daily_data_ui("daily_data")),
    tabPanel("Monthly Data", monthly_data_ui("monthly_data")),
    tabPanel("Annual Data", "Annual data content here")
  )
)

# server setup 
server <- function(input, output, session) {
 
   # initialize shared data for all tabs
  shared_data <- reactiveValues(
    database_ready = FALSE,
    mach_ids = character(0),
    duckdb_connection = NULL, 
    selected_sites = character(0)
   
  )
  
  # call module servers
  dataset_import_server("dataset_import", shared_data)
  site_selection_server("site_selection", shared_data)
  daily_data_server("daily_data", shared_data)
  monthly_data_server("monthly_data", shared_data)
  
  # disable tabs until database is connected
  observe({
    if (!shared_data$database_ready) {
      shinyjs::disable(selector = "#main_tabs li a[data-value='site_selection']")
      shinyjs::disable(selector = "#main_tabs li a[data-value='daily_data']")
      shinyjs::disable(selector = "#main_tabs li a[data-value='monthly_data']")
      shinyjs::disable(selector = "#main_tabs li a[data-value='annual_data']")
    } else {
      shinyjs::enable(selector = "#main_tabs li a[data-value='site_selection']")
      shinyjs::enable(selector = "#main_tabs li a[data-value='daily_data']")
      shinyjs::enable(selector = "#main_tabs li a[data-value='monthly_data']")
      shinyjs::enable(selector = "#main_tabs li a[data-value='annual_data']")
    }
  })

  # show popup if user tries to switch tabs without connection
  observeEvent(input$main_tabs, {
    if (!shared_data$database_ready && input$main_tabs != "Data Import") {
      shinyalert("I feel disconnected.", "Please connect to the database file before using other tabs.", type = "error")
      updateTabsetPanel(session, "main_tabs", selected = "Data Import")
    }
  })
  
  
  # ensure database connection is closed on session end
  onSessionEnded(function() {
    if (!is.null(isolate(shared_data$duckdb_connection)) && DBI::dbIsValid(isolate(shared_data$duckdb_connection))) {
      DBI::dbDisconnect(isolate(shared_data$duckdb_connection), shutdown = TRUE)
      message("DuckDB connection closed on session end")
    }
  })
  
  # close app when session completes
  if (!interactive()) {
    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
  }
}

# Run the app
shinyApp(ui = ui, server = server)