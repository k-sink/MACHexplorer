source("global.R")

# Main Shiny App
ui <- page_navbar(
  title = "DuckDB USGS Data Explorer",
  id = "main_navbar",
  
  # Add required JS libraries
  tags$head(
    tags$script(src = "https://cdn.jsdelivr.net/npm/localforage@1.10.0/dist/localforage.min.js"),
    tags$script(src = "https://cdn.jsdelivr.net/npm/@duckdb/duckdb-wasm@1.28.0/dist/duckdb-browser-eh.js")
  ),
  
  # Initialize shinyjs
  useShinyjs(),
  
  # Dataset Import Tab
  nav_panel(
    title = "Dataset Import",
    icon = icon("database"),
    dataset_import_ui("dataset_import")  # Fixed function name
  ),
  
  nav_spacer(),
  
  nav_menu(
    title = "Settings",
    icon = icon("gear"),
    nav_item(
      actionButton("refresh_connection", "Refresh Connection", class = "btn-outline-secondary")
    )
  )
)

server <- function(input, output, session) {
  
  # Initialize dataset import module (fixed function name)
  dataset_import_server("dataset_import")
  
  # Global refresh connection handler
  observeEvent(input$refresh_connection, {
    showNotification("Refreshing connection...", type = "message")
    session$sendCustomMessage("refresh_db_connection", TRUE)
  })
  
  onSessionEnded(function() {
    runjs("if (typeof disconnectDuckDB === 'function') { disconnectDuckDB(); }")
  })
}

shinyApp(ui = ui, server = server)
