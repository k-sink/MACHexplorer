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
source("modules/annual_data.R")
source("modules/mopex_data.R")
source("modules/attributes_data.R")
source("modules/landcover_data.R")

# increase file upload size limit to 1GB to allow for 900MB duckdb
options(shiny.maxRequestSize = 1000 * 1024^2)

### ui setup ###
ui <- bslib::page_navbar(
  title = "MACH Explorer",
 
  theme = bslib::bs_theme(
    version = 5,
    bg = "#FFFFFF", # White background
    fg = "#000000", # Black text
    primary = "#593196", # Purple for buttons and headers
    base_font = bslib::font_google("Roboto"), 
    heading_font = bslib::font_google("Lora") 
  ),
  id = "main_tabs",

  header = tagList(
    shinyjs::useShinyjs(),
    tags$head(
      tags$style(HTML("
        /* Ensure navbar is black with white text */
        .navbar {
          background-color: #000000 !important;
          color: #FFFFFF !important;
        }
        .navbar-nav .nav-link {
          color: #FFFFFF !important;
          font-family: 'Lora', serif !important;
          font-size: 1.2rem;
        }
        .navbar-nav .nav-link:hover {
          color: #D3D3D3 !important; /* Light gray hover for contrast */
        }
        .navbar-brand {
          color: #FFFFFF !important;
          font-family: 'Lora', serif !important;
          font-size: 1.5rem;
        }

        /* Buttons: purple with white text, Roboto font */
        .btn-dark {
          background-color: #593196;
          color: #FFFFFF;
          font-family: 'Roboto', sans-serif;
          font-size: 1rem;
          border: none;
          padding: 0.5rem 0.75rem;
          width: auto;
          display: block;
          margin: 0;
        }
        .btn-dark:hover {
          background-color: #43246e;
          color: #FFFFFF;
        }

        /* Overall app background color */
        body {
          background-color: #FFFFFF;
        }
        
        .card > .card-header {
        background-color: #000000;
        color: #FFFFFF !important; 
        font-family: 'Lora', serif !important;
        font-size: 1.1rem; 
        font-weight: normal !important;
        padding: 0.5rem 0.5rem; 
        border: none;
        }
        
        .card-body {
        background-color: #f0f0f0; 
        font-family: 'Roboto', sans-serif;
        font-size: 1rem;
        color: #000000;
        padding: 1rem;
        }

  
        /* Disable tabs when database not loaded */
        .nav-tabs > li > a.disabled {
          pointer-events: none;
          color: #aaa;
          cursor: not-allowed;
        }
        
        
      "))
    )
  ),


  nav_panel("Data Import", dataset_import_ui("dataset_import")),
  nav_panel("Site Selection", site_selection_ui("site_selection")),
  nav_panel("Daily Data", daily_data_ui("daily_data")),
  nav_panel("Monthly Data", monthly_data_ui("monthly_data")),
  nav_panel("Annual Data", annual_data_ui("annual_data")),
  nav_panel("MOPEX Data", mopex_data_ui("mopex_data")),
  nav_panel("Attributes", attributes_data_ui("attributes_data")),
  nav_panel("Land Cover", landcover_data_ui("landcover_data")),


  nav_spacer(),

  nav_menu(
    title = "Help",
    align = "right",
    nav_item("Coming soon…")
  ) 
  
)


### server setup ###
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
  annual_data_server("annual_data", shared_data)
  mopex_data_server("mopex_data", shared_data)
  attributes_data_server("attributes_data", shared_data)
  landcover_data_server("landcover_data", shared_data)

  observe({
    if (!shared_data$database_ready) {
      shinyjs::disable(selector = "#main_tabs li a[data-value!='Data Import']")
    } else {
      shinyjs::enable(selector = "#main_tabs li a[data-value!='Data Import']")
    }
  })

  observeEvent(input$main_tabs, {
    if (!shared_data$database_ready && input$main_tabs != "Data Import") {
      shinyalert("I feel disconnected.", "Please connect to the database file.", type = "error")
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
    session$onSessionEnded(function() {
      stopApp()
    })
  
}

# Run the app
shinyApp(ui = ui, server = server)