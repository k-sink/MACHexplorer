#######################
### DAILY DATA UI ###
#######################
daily_data_ui = function(id) {
  ns = NS(id)
  tagList(
    br(),
    fluidRow(
      column(width = 3,
        wellPanel(
          style = "overflow: visible; height: auto;",
          h6(strong("Select Variable(s)")),
          checkboxInput(ns("select_prcp"), "Precipitation, PRCP (mm)", FALSE),
          conditionalPanel(
            condition = "input.select_prcp == true", ns = ns,
            fluidRow(
              column(width = 6, numericInput(ns("prcp_min"), "Min PRCP", value = 0, min = 0, max = 300, step = 1)),
              column(width = 6, numericInput(ns("prcp_max"), "Max PRCP", value = 300, min = 0, max = 300, step = 1))
            )
          ),

          checkboxInput(ns("select_tair"), HTML("Mean Temperature, TAIR (°C)"), FALSE),
          conditionalPanel(
            condition = "input.select_tair == true", ns = ns,
            fluidRow(
              column(width = 6, numericInput(ns("tair_min"), "Min TAIR", value = -50, min = -50, max = 50, step = 1)),
              column(width = 6, numericInput(ns("tair_max"), "Max TAIR", value = 50, min = -50, max = 50, step = 1))
            )
          ),

          checkboxInput(ns("select_tmin"), HTML("Minimum Temperature, TMIN (°C)"), FALSE),
          conditionalPanel(
            condition = "input.select_tmin == true", ns = ns,
            fluidRow(
              column(width = 6, numericInput(ns("tmin_min"), "Min TMIN", value = -50, min = -50, max = 50, step = 1)),
              column(width = 6, numericInput(ns("tmin_max"), "Max TMIN", value = 50, min = -50, max = 50, step = 1))
            )
          ),

          checkboxInput(ns("select_tmax"), HTML("Maximum Temperature, TMAX (°C)"), FALSE),
          conditionalPanel(
            condition = "input.select_tmax == true", ns = ns,
            fluidRow(
              column(width = 6, numericInput(ns("tmax_min"), "Min TMAX", value = -50, min = -50, max = 50, step = 1)),
              column(width = 6, numericInput(ns("tmax_max"), "Max TMAX", value = 50, min = -50, max = 50, step = 1))
            )
          ),

          checkboxInput(ns("select_pet"), "Potential Evapotranspiration, PET (mm)", FALSE),
          conditionalPanel(
            condition = "input.select_pet == true", ns = ns,
            fluidRow(
              column(width = 6, numericInput(ns("pet_min"), "Min PET", value = -1, min = -1, max = 40, step = 1)),
              column(width = 6, numericInput(ns("pet_max"), "Max PET", value = 40, min = -1, max = 40, step = 1))
            )
          ),

          checkboxInput(ns("select_aet"), "Actual Evapotranspiration, AET (mm)", FALSE),
          conditionalPanel(
            condition = "input.select_aet == true", ns = ns,
            fluidRow(
              column(width = 6, numericInput(ns("aet_min"), "Min AET", value = -1, min = -1, max = 40, step = 1)),
              column(width = 6, numericInput(ns("aet_max"), "Max AET", value = 40, min = -1, max = 40, step = 1))
            )
          ),

          checkboxInput(ns("select_disch"), "Observed Discharge, OBSQ (mm)", FALSE),
          conditionalPanel(
            condition = "input.select_disch == true", ns = ns,
            fluidRow(
              column(width = 6, numericInput(ns("disch_min"), "Min OBSQ", value = -1, min = -1, max = 400, step = 1)),
              column(width = 6, numericInput(ns("disch_max"), "Max OBSQ", value = 400, min = -1, max = 400, step = 1))
            )
          ),

          checkboxInput(ns("select_swe"), "Snow Water Equivalent, SWE (mm)", FALSE),
          conditionalPanel(
            condition = "input.select_swe == true", ns = ns,
            fluidRow(
              column(width = 6, numericInput(ns("swe_min"), "Min SWE", value = 0, min = 0, max = 1850, step = 1)),
              column(width = 6, numericInput(ns("swe_max"), "Max SWE", value = 1850, min = 0, max = 1850, step = 1))
            )
          ),

          checkboxInput(ns("select_srad"), HTML("Shortwave Radiation, SRAD (W/m<sup>2</sup>)"), FALSE),
          conditionalPanel(
            condition = "input.select_srad == true", ns = ns,
            fluidRow(
              column(width = 6, numericInput(ns("srad_min"), "Min SRAD", value = 10, min = 10, max = 900, step = 1)),
              column(width = 6, numericInput(ns("srad_max"), "Max SRAD", value = 900, min = 10, max = 900, step = 1))
            )
          ),

          checkboxInput(ns("select_vp"), "Water Vapor Pressure, VP (Pa)", FALSE),
          conditionalPanel(
            condition = "input.select_vp == true", ns = ns,
            fluidRow(
              column(width = 6, numericInput(ns("vp_min"), "Min VP", value = 5, min = 5, max = 4000, step = 1)),
              column(width = 6, numericInput(ns("vp_max"), "Max VP", value = 4000, min = 5, max = 4000, step = 1))
            )
          ),

          checkboxInput(ns("select_dayl"), "Day Length, DAYL (sec)", FALSE),
          conditionalPanel(
            condition = "input.select_dayl == true", ns = ns,
            fluidRow(
              column(width = 6, numericInput(ns("dayl_min"), "Min DAYL", value = 30000, min = 30000, max = 60000, step = 1)),
              column(width = 6, numericInput(ns("dayl_max"), "Max DAYL", value = 60000, min = 30000, max = 60000, step = 1))
            )
          ),

          br(),
          h6(strong("Select Time Period(s)")),
          checkboxInput(ns("select_date"), "Date Range", FALSE),
          conditionalPanel(
            condition = "input.select_date == true", ns = ns,
            dateRangeInput(ns("date_range1"), label = NULL,
              start = "1980-01-01", end = "2023-12-31",
              min = "1980-01-01", max = "2023-12-31",
              format = "mm/dd/yyyy", separator = "to")
          ),

          checkboxInput(ns("select_year"), "Calendar Year", FALSE),
          conditionalPanel(
            condition = "input.select_year == true", ns = ns,
            selectizeInput(ns("year1"), label = NULL,
              choices = years, multiple = TRUE,
              options = list(placeholder = "Select one or more"))
          ),

          checkboxInput(ns("select_month"), "Month", FALSE),
          conditionalPanel(
            condition = "input.select_month == true", ns = ns,
            selectizeInput(ns("month1"), label = NULL,
              choices = c("JAN"= 1, "FEB"= 2, "MAR"= 3, "APR"= 4, "MAY"= 5, "JUN"= 6,
                          "JUL"= 7, "AUG"= 8, "SEP"= 9, "OCT"= 10, "NOV"= 11, "DEC"= 12),
              multiple = TRUE, options = list(placeholder = "Select one or more"))
          ),

          br(),
          actionButton(ns("retrieve_data"), "Retrieve and View Data"), 
          br(), br(), 
            actionButton(ns("reset_filters_daily"), "Reset all Filters")
        ),
        br(),
        wellPanel(
          h6(strong("Download Daily Data")),
          br(),
          downloadButton(ns("download_csv"), "Export as csv"),
          br(), br(),
          downloadButton(ns("download_separate"), "Export as separate csv files")
        )
      ),
      column(width = 6,
        style = "overflow: visible; height: auto;",
          wellPanel(
          h6(strong(textOutput(ns("daily_data_heading")))),
          br(), 
          div(style = "height: auto;",
          shinycssloaders::withSpinner(DT::DTOutput(ns("merged_data_table")), type = 5)
        ))
      ), 
      column(width = 3, 
             style = "overflow: visible; height: auto;",
             wellPanel(
               h6(strong("Data summary")), 
               br(), 
               div(style = "height: auto;",
               shinycssloaders::withSpinner(DT::DTOutput(ns("daily_summary")), type = 5)
             )))
    )
  )
}

#########################
### DAILY DATA SERVER ###
#########################
# initialize module server 
daily_data_server <- function(id, shared_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # get site numbers for filtered sites from site selection tab
    filtered_sites <- reactive({
      sitenos <- shared_data$selected_sites
      if (is.null(sitenos) || length(sitenos) == 0) {
        character(0)
      } else {
        sitenos
      }
    })

    # get total number of sites selected
    last_site_count <- reactiveVal(NULL)
    query_triggered <- reactiveVal(FALSE)
    

    # reset filters if button clicked
    observeEvent(input$reset_filters_daily, {
      updateCheckboxInput(session, "select_prcp", value = FALSE)
      updateNumericInput(session, "prcp_min", value = 0)
      updateNumericInput(session, "prcp_max", value = 300)

      updateCheckboxInput(session, "select_tair", value = FALSE)
      updateNumericInput(session, "tair_min", value = -50)
      updateNumericInput(session, "tair_max", value = 50)

      updateCheckboxInput(session, "select_tmin", value = FALSE)
      updateNumericInput(session, "tmin_min", value = -50)
      updateNumericInput(session, "tmin_max", value = 50)

      updateCheckboxInput(session, "select_tmax", value = FALSE)
      updateNumericInput(session, "tmax_min", value = -50)
      updateNumericInput(session, "tmax_max", value = 50)

      updateCheckboxInput(session, "select_pet", value = FALSE)
      updateNumericInput(session, "pet_min", value = -1)
      updateNumericInput(session, "pet_max", value = 40)

      updateCheckboxInput(session, "select_aet", value = FALSE)
      updateNumericInput(session, "aet_min", value = -1)
      updateNumericInput(session, "aet_max", value = 40)

      updateCheckboxInput(session, "select_disch", value = FALSE)
      updateNumericInput(session, "disch_min", value = -1)
      updateNumericInput(session, "disch_max", value = 400)

      updateCheckboxInput(session, "select_swe", value = FALSE)
      updateNumericInput(session, "swe_min", value = 0)
      updateNumericInput(session, "swe_max", value = 1850)

      updateCheckboxInput(session, "select_srad", value = FALSE)
      updateNumericInput(session, "srad_min", value = 10)
      updateNumericInput(session, "srad_max", value = 900)

      updateCheckboxInput(session, "select_vp", value = FALSE)
      updateNumericInput(session, "vp_min", value = 5)
      updateNumericInput(session, "vp_max", value = 4000)

      updateCheckboxInput(session, "select_dayl", value = FALSE)
      updateNumericInput(session, "dayl_min", value = 30000)
      updateNumericInput(session, "dayl_max", value = 60000)

      updateCheckboxInput(session, "select_date", value = FALSE)
      updateDateRangeInput(session, "date_range1", start = "1980-01-01", end = "2023-12-31")

      updateCheckboxInput(session, "select_year", value = FALSE)
      updateSelectizeInput(session, "year1", selected = character(0))

      updateCheckboxInput(session, "select_month", value = FALSE)
      updateSelectizeInput(session, "month1", selected = character(0))

      # clear reactive values to reset tables
      last_site_count(NULL)
      query_triggered(FALSE)

      showNotification("All filters have been reset.", type = "message")
    })

    # helper function to apply filters
    filter_if_needed <- function(tbl, input, select_var, col_name, min_input, max_input, default_min, default_max) {
      if (isTRUE(input[[select_var]]) && !is.null(input[[min_input]]) && !is.null(input[[max_input]]) &&
          is.numeric(input[[min_input]]) && is.numeric(input[[max_input]]) &&
          !is.na(input[[min_input]]) && !is.na(input[[max_input]])) {
        min_val <- as.numeric(input[[min_input]])
        max_val <- as.numeric(input[[max_input]])
        message(sprintf("Filtering %s between %s and %s", col_name, min_val, max_val))
        if (min_val > default_min || max_val < default_max) {
          if (col_name == "OBSQ") {
            tbl <- tbl %>%
              dplyr::filter(dplyr::sql(sprintf("is_null(%s) OR (%s BETWEEN %f AND %f)", col_name, col_name, min_val, max_val)))
          } else {
            tbl <- tbl %>%
              dplyr::filter(dplyr::sql(sprintf("%s BETWEEN %f AND %f", col_name, min_val, max_val)))
          }
        }
      }
      tbl
    }

    # function to build filtered query (shared logic)
    build_filtered_query <- function(conn, sites, vars = c("SITENO", "DATE")) {
      tbl <- dplyr::tbl(conn, "mach_daily") %>%
        dplyr::filter(SITENO %in% sites) %>%
        dplyr::select(all_of(vars))

      # apply filters based on user input
      tbl <- filter_if_needed(tbl, input, "select_prcp", "PRCP", "prcp_min", "prcp_max", 0, 300)
      tbl <- filter_if_needed(tbl, input, "select_tair", "TAIR", "tair_min", "tair_max", -50, 50)
      tbl <- filter_if_needed(tbl, input, "select_tmin", "TMIN", "tmin_min", "tmin_max", -50, 50)
      tbl <- filter_if_needed(tbl, input, "select_tmax", "TMAX", "tmax_min", "tmax_max", -50, 50)
      tbl <- filter_if_needed(tbl, input, "select_pet", "PET", "pet_min", "pet_max", -1, 40)
      tbl <- filter_if_needed(tbl, input, "select_aet", "AET", "aet_min", "aet_max", -1, 40)
      tbl <- filter_if_needed(tbl, input, "select_disch", "OBSQ", "disch_min", "disch_max", -1, 400)
      tbl <- filter_if_needed(tbl, input, "select_swe", "SWE", "swe_min", "swe_max", 0, 1850)
      tbl <- filter_if_needed(tbl, input, "select_srad", "SRAD", "srad_min", "srad_max", 10, 900)
      tbl <- filter_if_needed(tbl, input, "select_vp", "VP", "vp_min", "vp_max", 5, 4000)
      tbl <- filter_if_needed(tbl, input, "select_dayl", "DAYL", "dayl_min", "dayl_max", 30000, 60000)

      if (isTRUE(input$select_date) && !is.null(input$date_range1) && length(input$date_range1) == 2 &&
          !is.na(input$date_range1[1]) && !is.na(input$date_range1[2])) {
        start_date <- as.Date(input$date_range1[1])
        end_date <- as.Date(input$date_range1[2])
        if (!is.na(start_date) && !is.na(end_date)) {
          tbl <- tbl %>% dplyr::filter(DATE >= start_date, DATE <= end_date)
        }
      }

      if (isTRUE(input$select_year) && !is.null(input$year1) && length(input$year1) > 0 &&
          all(!is.na(as.numeric(input$year1)))) {
        selected_years <- as.numeric(input$year1)
        tbl <- tbl %>% dplyr::filter(lubridate::year(DATE) %in% selected_years)
      }

      if (isTRUE(input$select_month) && !is.null(input$month1) && length(input$month1) > 0 &&
          all(!is.na(as.numeric(input$month1)))) {
        selected_months <- as.numeric(input$month1)
        tbl <- tbl %>% dplyr::filter(lubridate::month(DATE) %in% selected_months)
      }

      tbl
    }

    # validate date range and calendar year
    observeEvent(input$retrieve_data, {
      if (isTRUE(input$select_date) && isTRUE(input$select_year) &&
          !is.null(input$date_range1) && length(input$date_range1) == 2 &&
          !is.null(input$year1) && length(input$year1) > 0 &&
          !is.na(input$date_range1[1]) && !is.na(input$date_range1[2]) &&
          all(!is.na(as.numeric(input$year1)))) {
        start_year <- lubridate::year(as.Date(input$date_range1[1]))
        end_year <- lubridate::year(as.Date(input$date_range1[2]))
        selected_years <- as.numeric(input$year1)
        if (any(selected_years < start_year | selected_years > end_year)) {
          shinyalert("Warning: Selected calendar year(s) outside of date range.", type = "warning")
        }
      }
    })

    # reactive filtered query (lazy, for table display with 1000 row limit)
    filtered_query <- eventReactive(input$retrieve_data, {
      req(shared_data$database_ready, shared_data$duckdb_connection)
      conn <- shared_data$duckdb_connection
      if (!DBI::dbIsValid(conn)) {
        shinyalert("Database connection is invalid. Please re-import.", type = "error")
        return(NULL)
      }

      sites <- filtered_sites()
      if (length(sites) == 0) {
        shinyalert("Please select at least one site.", type = "error")
        return(NULL)
      }

      valid_site_ids <- intersect(shared_data$mach_ids, sites)
      if (length(valid_site_ids) == 0) {
        shinyalert("No valid site data found.", type = "error")
        return(NULL)
      }

      last_site_count(length(valid_site_ids))
      query_triggered(TRUE)

      vars <- c("SITENO", "DATE")
      if (isTRUE(input$select_prcp)) vars <- c(vars, "PRCP")
      if (isTRUE(input$select_tair)) vars <- c(vars, "TAIR")
      if (isTRUE(input$select_tmin)) vars <- c(vars, "TMIN")
      if (isTRUE(input$select_tmax)) vars <- c(vars, "TMAX")
      if (isTRUE(input$select_pet)) vars <- c(vars, "PET")
      if (isTRUE(input$select_aet)) vars <- c(vars, "AET")
      if (isTRUE(input$select_disch)) vars <- c(vars, "OBSQ")
      if (isTRUE(input$select_swe)) vars <- c(vars, "SWE")
      if (isTRUE(input$select_srad)) vars <- c(vars, "SRAD")
      if (isTRUE(input$select_vp)) vars <- c(vars, "VP")
      if (isTRUE(input$select_dayl)) vars <- c(vars, "DAYL")

      if (length(vars) <= 2) {
        shinyalert("Please select at least one variable.", type = "error")
        query_triggered(FALSE) # prevent summary table from triggering alert
        return(NULL)
      }

      tbl <- build_filtered_query(conn, valid_site_ids, vars)
      sql_query <- paste0("SELECT * FROM (", dbplyr::sql_render(tbl), ") AS subquery ORDER BY SITENO, DATE LIMIT 1000")
      tbl <- dplyr::tbl(conn, dbplyr::sql(sql_query))

      tbl
    })

    # reactive for full filtered query (no LIMIT, for summary and downloads)
    full_filtered_query <- eventReactive(input$retrieve_data, {
      req(shared_data$database_ready, shared_data$duckdb_connection)
      conn <- shared_data$duckdb_connection
      if (!DBI::dbIsValid(conn)) {
        shinyalert("Database connection is invalid. Please re-import.", type = "error")
        return(NULL)
      }

      sites <- filtered_sites()
      valid_site_ids <- intersect(shared_data$mach_ids, sites)
      if (length(valid_site_ids) == 0) {
        shinyalert("No valid site data found.", type = "error")
        return(NULL)
      }

      vars <- c("SITENO", "DATE")
      if (isTRUE(input$select_prcp)) vars <- c(vars, "PRCP")
      if (isTRUE(input$select_tair)) vars <- c(vars, "TAIR")
      if (isTRUE(input$select_tmin)) vars <- c(vars, "TMIN")
      if (isTRUE(input$select_tmax)) vars <- c(vars, "TMAX")
      if (isTRUE(input$select_pet)) vars <- c(vars, "PET")
      if (isTRUE(input$select_aet)) vars <- c(vars, "AET")
      if (isTRUE(input$select_disch)) vars <- c(vars, "OBSQ")
      if (isTRUE(input$select_swe)) vars <- c(vars, "SWE")
      if (isTRUE(input$select_srad)) vars <- c(vars, "SRAD")
      if (isTRUE(input$select_vp)) vars <- c(vars, "VP")
      if (isTRUE(input$select_dayl)) vars <- c(vars, "DAYL")

      if (length(vars) <= 2) {
        return(NULL) # Avoid duplicate alert, handled in filtered_query
      }

      tbl <- build_filtered_query(conn, valid_site_ids, vars)
      tbl
    })

    # reactive data frame for displayed table (collect only 1000 rows)
    table_data <- reactive({
      req(filtered_query())
      withProgress(message = "Fetching data...", value = 0, {
        df <- tryCatch({
          incProgress(0.5)
          df <- filtered_query() %>% dplyr::collect()
          incProgress(0.8)
          if ("DATE" %in% colnames(df)) {
            df$DATE <- format(as.Date(df$DATE), "%Y-%m-%d")
          }
          if ("SITENO" %in% colnames(df)) {
            df$SITENO <- as.character(df$SITENO)
          }
          df
        }, error = function(e) {
          shinyalert("Error fetching data: ", e$message, type = "error")
          NULL
        })
        incProgress(1)
        if (!is.null(df) && nrow(df) > 0) {
          showNotification("Table displays first 1,000 rows for preview. Downloads include all data.", type = "message")
        }
        df
      })
    })

    # reactive for summary table (SITENO and TOTAL rows, using full query)
    summary_data <- eventReactive(input$retrieve_data, {
      req(shared_data$database_ready, shared_data$duckdb_connection, full_filtered_query())
      conn <- shared_data$duckdb_connection
      if (!DBI::dbIsValid(conn)) {
        message("Summary Data: Invalid database connection")
        return(data.frame(SITENO = character(), TOTAL = integer()))
      }

      sites <- filtered_sites()
      valid_site_ids <- intersect(shared_data$mach_ids, sites)
      if (length(valid_site_ids) == 0) {
        message("Summary Data: No valid site IDs")
        return(data.frame(SITENO = character(), TOTAL = integer()))
      }

      summary_df <- tryCatch({
        tbl <- full_filtered_query() %>%
          dplyr::select(SITENO) %>%
          dplyr::group_by(SITENO) %>%
          dplyr::summarise(TOTAL = dplyr::n(), .groups = "drop") %>%
          dplyr::arrange(SITENO) # Order by SITENO smallest to largest
        message("Summary Query: ", dbplyr::sql_render(tbl))
        df <- tbl %>% dplyr::collect()
        df$SITENO <- as.character(df$SITENO)
        df$TOTAL <- as.integer(df$TOTAL)
        message("Summary Data Rows: ", nrow(df))
        df
      }, error = function(e) {
        message("Summary Data Error: ", e$message)
        shinyalert("Error fetching summary data.", type = "error")
        data.frame(SITENO = character(), TOTAL = integer())
      })

      summary_df
    })

    # render DT with client-side processing
    output$merged_data_table <- DT::renderDT({
      req(table_data(), query_triggered())
      df <- table_data()
      if (is.null(df) || nrow(df) == 0 || !query_triggered()) {
        return(DT::datatable(
          data.frame(SITENO = character(), DATE = character()),
          options = list(pageLength = 25, searching = FALSE, ordering = TRUE, autoWidth = FALSE, dom = 'lfrtip', scrollX = TRUE),
          rownames = FALSE
        ))
      }

      col_defs <- lapply(seq_along(colnames(df)), function(i) {
        col_name <- colnames(df)[i]
        if (col_name == "DATE") {
          list(targets = i - 1, title = col_name, width = 100, render = DT::JS("function(data) { return data || ''; }"))
        } else if (col_name == "SITENO") {
          list(targets = i - 1, title = col_name, width = 100, render = DT::JS("function(data) { return data || ''; }"))
        } else if (col_name %in% c("PRCP", "TAIR", "TMIN", "TMAX", "SWE", "SRAD", "OBSQ", "PET", "AET", "VP", "DAYL")) {
          list(targets = i - 1, title = col_name, render = DT::JS("function(data) { return data !== null && data !== undefined ? Number(data).toFixed(2) : ''; }"))
        }
      })

      DT::datatable(
        df,
        options = list(
          pageLength = 25,
          lengthMenu = c(10, 25, 50, 100),
          searching = TRUE,
          ordering = TRUE,
          autoWidth = FALSE,
          dom = 'lfrtip',
          scrollX = TRUE,
          columnDefs = col_defs
        ),
        rownames = FALSE,
        class = 'cell-border stripe',
        filter = 'none',
        callback = DT::JS(
          "table.on('draw.dt', function() {
             table.rows().every(function() {
               var row = this.data();
               for (var i = 0; i < row.length; i++) {
                 if (row[i] === null || row[i] === undefined) row[i] = '';
               }
               this.data(row);
             });
           });"
        )
      )
    }, server = FALSE)

    # Render summary table (SITENO, TOTAL)
    output$daily_summary <- DT::renderDT({
      req(summary_data(), query_triggered())
      df <- summary_data()
      if (nrow(df) == 0 || !query_triggered()) {
        return(DT::datatable(
          data.frame(SITENO = character(), TOTAL = integer()),
          options = list(pageLength = 10, searching = FALSE, ordering = TRUE, autoWidth = FALSE, dom = 'lfrtip', scrollX = TRUE),
          rownames = FALSE
        ))
      }

      DT::datatable(
        df,
        options = list(
          pageLength = 10,
          lengthMenu = c(10, 25, 50),
          searching = TRUE,
          ordering = TRUE,
          autoWidth = FALSE,
          dom = 'lfrtip',
          scrollX = TRUE,
          columnDefs = list(
            list(targets = "_all", render = DT::JS("function(data) { return data !== null && data !== undefined ? data : ''; }"))
          )
        ),
        rownames = FALSE,
        class = 'cell-border stripe',
        filter = 'none'
      )
    }, server = FALSE)

    # Update heading
    output$daily_data_heading <- renderText({
      req(last_site_count())
      sprintf("Filtered Daily Data for %d sites (first 1,000 rows displayed)", last_site_count())
    })

output$download_csv <- downloadHandler(
  filename = function() {
    paste0("MACH_daily_", Sys.Date(), ".csv")
  },
  content = function(file) {
    req(shared_data$database_ready, shared_data$duckdb_connection)
    conn <- shared_data$duckdb_connection

    sites <- filtered_sites()
    valid_site_ids <- intersect(shared_data$mach_ids, sites)
    if (query_triggered() && length(valid_site_ids) != last_site_count()) {
      message("download_csv: Site selection mismatch, current sites: ", length(valid_site_ids), ", last count: ", last_site_count())
      shinyalert(
        "Site selection has changed. Please click 'Retrieve and View Data' to update the data before downloading.",
        type = "warning"
      )
    }

    req(full_filtered_query())
    withProgress(message = "Creating CSV file...", value = 0, {
      incProgress(0.5)
      df <- full_filtered_query() %>%
        dplyr::mutate(DATE = as.character(DATE)) %>%
        dplyr::arrange(SITENO, DATE) %>%
        dplyr::collect()
      df$SITENO <- as.character(df$SITENO)
      message("download_csv: Data fetched, rows: ", nrow(df))
      incProgress(0.8)
      readr::write_csv(df, file)
      incProgress(1)
      message("download_csv: CSV written successfully")
    })
  },
  contentType = "text/csv"
)

# Download separate CSV files as ZIP
output$download_separate <- downloadHandler(
  filename = function() {
    paste0("MACH_daily_", Sys.Date(), ".zip")
  },
  content = function(file) {
    req(shared_data$database_ready, shared_data$duckdb_connection)
    conn <- shared_data$duckdb_connection

    sites <- filtered_sites()
    valid_site_ids <- intersect(shared_data$mach_ids, sites)
    if (query_triggered() && length(valid_site_ids) != last_site_count()) {
      message("download_separate: Site selection mismatch, current sites: ", length(valid_site_ids), ", last count: ", last_site_count())
      shinyalert(
        "Site selection has changed. Please click 'Retrieve and View Data' to update the data before downloading.",
        type = "warning"
      )
    }

    req(full_filtered_query())
    full_data <- full_filtered_query() %>%
      dplyr::mutate(DATE = as.character(DATE)) %>%
      dplyr::arrange(SITENO, DATE) %>%
      dplyr::collect() %>%
      dplyr::mutate(SITENO = as.character(SITENO))

    sites <- unique(full_data$SITENO)
    temp_download_dir <- file.path(tempdir(), paste0("download_", format(Sys.time(), "%Y%m%d_%H%M%S")))
    dir.create(temp_download_dir, showWarnings = FALSE, recursive = TRUE)
    on.exit(unlink(temp_download_dir, recursive = TRUE, force = TRUE), add = TRUE)

    withProgress(message = "Creating ZIP file...", value = 0, {
      csv_files <- character(length(sites))
      for (i in seq_along(sites)) {
        site <- sites[i]
        safe_site <- gsub("[^[:alnum:]_-]", "_", site)
        csv_file <- file.path(temp_download_dir, paste0("MACH_daily_", safe_site, ".csv"))
        csv_files[i] <- csv_file
        data <- full_data %>% dplyr::filter(SITENO == site)
        if (nrow(data) > 0) {
          readr::write_csv(data, csv_file)
          message("download_separate: CSV written for site ", site)
        }
        incProgress(1 / length(sites), detail = paste("Processed site", site))
      }

      valid_csv_files <- csv_files[file.exists(csv_files)]
      if (length(valid_csv_files) > 0) {
        zip::zip(zipfile = file, files = basename(valid_csv_files), root = temp_download_dir)
        message("download_separate: ZIP file created successfully")
      }
    })
  },
  contentType = "application/zip"
)
  })
}