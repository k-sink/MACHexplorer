###################################
### MONTHLY DATA MODULE ###
###################################
monthly_data_ui = function(id) {
  ns = NS(id)
  tagList(
    br(),
    fluidRow(
      column(width = 4,
        wellPanel(
          style = "overflow: visible; height: auto;",
          h6(strong("Select Statistic")),
          selectInput(inputId = ns("month_agg"), label = NULL,
            multiple = FALSE, choices = c("Minimum", "Maximum", "Median", "Mean", "Total"),
            selected = "Mean")),

        br(),
        wellPanel(
          style = "overflow: visible; height: auto;",
          h6(strong("Select Variable(s)")),
          checkboxInput(ns("select_prcp_m"), "Precipitation, PRCP (mm)", FALSE),
          checkboxInput(ns("select_tair_m"), HTML("Mean Temperature, TAIR (°C)"), FALSE),
          checkboxInput(ns("select_tmin_m"), HTML("Minimum Temperature, TMIN (°C)"), FALSE),
          checkboxInput(ns("select_tmax_m"), HTML("Maximum Temperature, TMAX (°C)"), FALSE),
          checkboxInput(ns("select_pet_m"), "Potential Evapotranspiration, PET (mm)", FALSE),
          checkboxInput(ns("select_aet_m"), "Actual Evapotranspiration, AET (mm)", FALSE),
          checkboxInput(ns("select_disch_m"), "Observed Discharge, OBSQ (mm)", FALSE),
          checkboxInput(ns("select_swe_m"), "Snow Water Equivalent, SWE (mm)", FALSE),
          checkboxInput(ns("select_srad_m"), HTML("Shortwave Radiation, SRAD (W/m<sup>2</sup>)"), FALSE),
          checkboxInput(ns("select_vp_m"), "Water Vapor Pressure, VP (Pa)", FALSE),
          checkboxInput(ns("select_dayl_m"), "Day Length, DAYL (sec)", FALSE),
          br(),
          h6(strong("Select Time Period(s)")),
          checkboxInput(ns("select_year_m"), "Calendar Year", FALSE),
          conditionalPanel(
            condition = paste0("input['", ns("select_year_m"), "'] == true"),
            selectizeInput(inputId = ns("year2"), label = NULL,
              choices = years, multiple = TRUE,
              options = list(placeholder = "Select one or more"))),

          checkboxInput(ns("select_month_m"), "Month", FALSE),
          conditionalPanel(
            condition = paste0("input['", ns("select_month_m"), "'] == true"),
            selectizeInput(inputId = ns("month2"), label = NULL,
              choices = c("JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4, "MAY" = 5, "JUN" = 6,
                          "JUL" = 7, "AUG" = 8, "SEP" = 9, "OCT" = 10, "NOV" = 11, "DEC" = 12),
              multiple = TRUE,
              options = list(placeholder = "Select one or more"))),

          br(),
          actionButton(ns("retrieve_month"), "Retrieve and View Data")),

        br(),
        wellPanel(
          h6(strong("Download Monthly Data")),
          br(),
          downloadButton(ns("download_csv_m"), "Export as csv"),
          br(), br(),
          downloadButton(ns("download_separate_m"), "Export as separate csv files")),

      ),
    column(
        width = 8,
        wellPanel(
          style = "overflow: visible; height: auto;",
          h6(strong(textOutput(ns("monthly_data_heading")))),
          br(),
          withSpinner(DTOutput(ns("merged_data_table_m")), type = 5)
        )
      )
    )
  )
}

### SERVER ###
monthly_data_server <- function(id, shared_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Get site numbers for filtered sites from site selection tab
    filtered_sites <- reactive({
      sitenos <- shared_data$selected_sites
      if (is.null(sitenos) || length(sitenos) == 0) {
        character(0)
      } else {
        sitenos
      }
    })

    # Get total number of sites selected
    last_site_count <- reactiveVal(NULL)
    query_triggered <- reactiveVal(FALSE)


# Aggregate monthly data based on selected statistic
    aggregate_monthly_data <- function(tbl, var_name, agg_type) {
      agg_func <- switch(
        agg_type,
        "Minimum" = "MIN",
        "Maximum" = "MAX",
        "Median" = "MEDIAN",
        "Mean" = "AVG",
        "Total" = "SUM"
      )
      # For TAIR, TMIN, TMAX, use AVG for Total aggregation
      if (agg_type == "Total" && var_name %in% c("TAIR", "TMIN", "TMAX")) {
        agg_func <- "AVG"
      }
      # Use dbplyr::sql to construct the SQL aggregation expression
      agg_expr <- dbplyr::sql(sprintf("%s(%s)", agg_func, var_name))
      tbl %>%
        dplyr::mutate(YEAR = lubridate::year(DATE), MONTH = lubridate::month(DATE)) %>%
        dplyr::group_by(SITENO, YEAR, MONTH) %>%
        dplyr::summarise(!!var_name := !!agg_expr, .groups = "drop") %>%
        dplyr::mutate(!!var_name := round(!!rlang::sym(var_name), 2))
    }

    # Build filtered query
    build_filtered_query <- function(conn, sites, vars = c("SITENO", "DATE")) {
      tbl <- dplyr::tbl(conn, "mach_daily") %>%
        dplyr::filter(SITENO %in% sites) %>%
        dplyr::select(all_of(vars))

      if (isTRUE(input$select_year_m) && !is.null(input$year2) && length(input$year2) > 0 &&
          all(!is.na(as.numeric(input$year2)))) {
        selected_years <- as.numeric(input$year2)
        tbl <- tbl %>% dplyr::filter(lubridate::year(DATE) %in% selected_years)
      }

      if (isTRUE(input$select_month_m) && !is.null(input$month2) && length(input$month2) > 0 &&
          all(!is.na(as.numeric(input$month2)))) {
        selected_months <- as.numeric(input$month2)
        tbl <- tbl %>% dplyr::filter(lubridate::month(DATE) %in% selected_months)
      }

      tbl
    }

    # Reactive for monthly data query
    monthly_table <- eventReactive(input$retrieve_month, {
      req(shared_data$database_ready, shared_data$duckdb_connection)
      conn <- shared_data$duckdb_connection
      if (!DBI::dbIsValid(conn)) {
        shinyalert("Database connection is invalid. Please re-import.", type = "error")
        return(data.frame(Message = "Database connection invalid"))
      }

      sites <- filtered_sites()
      if (length(sites) == 0) {
        shinyalert("Please select at least one site.", type = "error")
        return(data.frame(Message = "No sites selected"))
      }

      valid_site_ids <- intersect(shared_data$mach_ids, sites)
      if (length(valid_site_ids) == 0) {
        shinyalert("No valid site data found.", type = "error")
        return(data.frame(Message = "No valid site data"))
      }

      last_site_count(length(valid_site_ids))
      query_triggered(TRUE)

      vars <- c("SITENO", "DATE")
      if (isTRUE(input$select_prcp_m)) vars <- c(vars, "PRCP")
      if (isTRUE(input$select_tair_m)) vars <- c(vars, "TAIR")
      if (isTRUE(input$select_tmin_m)) vars <- c(vars, "TMIN")
      if (isTRUE(input$select_tmax_m)) vars <- c(vars, "TMAX")
      if (isTRUE(input$select_pet_m)) vars <- c(vars, "PET")
      if (isTRUE(input$select_aet_m)) vars <- c(vars, "AET")
      if (isTRUE(input$select_disch_m)) vars <- c(vars, "OBSQ")
      if (isTRUE(input$select_swe_m)) vars <- c(vars, "SWE")
      if (isTRUE(input$select_srad_m)) vars <- c(vars, "SRAD")
      if (isTRUE(input$select_vp_m)) vars <- c(vars, "VP")
      if (isTRUE(input$select_dayl_m)) vars <- c(vars, "DAYL")

      if (length(vars) <= 2) {
        shinyalert("Please select at least one variable.", type = "error")
        query_triggered(FALSE)
        return(data.frame(Message = "No variables selected"))
      }

      withProgress(message = "Fetching monthly data...", value = 0, {
        incProgress(0.3)
        tbl <- build_filtered_query(conn, valid_site_ids, vars)
        combined_df <- NULL
        for (var in vars[vars != "SITENO" & vars != "DATE"]) {
          agg_tbl <- aggregate_monthly_data(tbl, var, input$month_agg)
          if (is.null(combined_df)) {
            combined_df <- agg_tbl
          } else {
            combined_df <- combined_df %>%
              dplyr::full_join(agg_tbl, by = c("SITENO", "YEAR", "MONTH"))
          }
        }
        incProgress(0.6)
        df <- tryCatch({
          df <- combined_df %>%
            dplyr::arrange(SITENO, YEAR, MONTH) %>%
            dplyr::collect()
          df$YEAR <- as.integer(df$YEAR)
          df$MONTH <- as.integer(df$MONTH)
          df$SITENO <- as.character(df$SITENO)
          df
        }, error = function(e) {
          shinyalert("Error fetching data: ", e$message, type = "error")
          return(data.frame(Message = "Error fetching data"))
        })
        incProgress(1)
        df
      })
    })

    # Update heading
    output$monthly_data_heading <- renderText({
      req(last_site_count())
      sprintf("Filtered Monthly Data for %d sites", last_site_count())
    })

    # Render data table (server-side)
    output$merged_data_table_m <- DT::renderDT({
      req(monthly_table(), query_triggered())
      data <- monthly_table()
      if ("Message" %in% colnames(data)) {
        DT::datatable(
          data,
          options = list(pageLength = 10, scrollX = TRUE, dom = "t"),
          class = "display responsive nowrap",
          rownames = FALSE
        )
      } else {
        col_defs <- lapply(seq_along(colnames(data)), function(i) {
          col_name <- colnames(data)[i]
          if (col_name %in% c("SITENO", "YEAR", "MONTH")) {
            list(targets = i - 1, title = col_name, width = 100, render = DT::JS("function(data) { return data || ''; }"))
          } else {
            list(targets = i - 1, title = col_name, render = DT::JS("function(data) { return data !== null && data !== undefined ? Number(data).toFixed(2) : ''; }"))
          }
        })

        DT::datatable(
          as.data.frame(data), # Ensure data is a data frame to avoid jsonlite warning
          options = list(
            pageLength = 10,
            lengthMenu = c(5, 10, 20, 50),
            scrollX = TRUE,
            dom = "Blfrtip",
            paging = TRUE,
            serverSide = TRUE,
            processing = TRUE,
            columnDefs = col_defs
          ),
          class = "display responsive nowrap",
          rownames = FALSE
        )
      }
    }, server = TRUE)

output$download_csv_m <- downloadHandler(
  filename = function() {
    paste0("MACH_monthly_", Sys.Date(), ".csv")
  },
  content = function(file) {
    req(shared_data$database_ready, shared_data$duckdb_connection)
    conn <- shared_data$duckdb_connection

    sites <- filtered_sites()
    valid_site_ids <- intersect(shared_data$mach_ids, sites)
    if (query_triggered() && length(valid_site_ids) != last_site_count()) {
      message("download_csv_m: Site selection mismatch, current sites: ", length(valid_site_ids), ", last count: ", last_site_count())
      shinyalert(
        "Site selection has changed. Please click 'Retrieve and View Data' to update the data before downloading.",
        type = "warning"
      )
    }

    req(monthly_table())
    data <- monthly_table()
    withProgress(message = "Creating CSV file...", value = 0, {
      incProgress(0.8)
      readr::write_csv(data, file)
      incProgress(1)
      message("download_csv_m: CSV written successfully")
    })
  },
  contentType = "text/csv"
)

# Download separate CSV files as ZIP
output$download_separate_m <- downloadHandler(
  filename = function() {
    paste0("MACH_monthly_", Sys.Date(), ".zip")
  },
  content = function(file) {
    req(shared_data$database_ready, shared_data$duckdb_connection)
    conn <- shared_data$duckdb_connection

    sites <- filtered_sites()
    valid_site_ids <- intersect(shared_data$mach_ids, sites)
    if (query_triggered() && length(valid_site_ids) != last_site_count()) {
      message("download_separate_m: Site selection mismatch, current sites: ", length(valid_site_ids), ", last count: ", last_site_count())
      shinyalert(
        "Site selection has changed. Please click 'Retrieve and View Data' to update the data before downloading.",
        type = "warning"
      )
    }

    req(monthly_table())
    full_data <- monthly_table()
    sites <- unique(full_data$SITENO)
    temp_download_dir <- file.path(tempdir(), paste0("download_", format(Sys.time(), "%Y%m%d_%H%M%S")))
    dir.create(temp_download_dir, showWarnings = FALSE, recursive = TRUE)
    on.exit(unlink(temp_download_dir, recursive = TRUE, force = TRUE), add = TRUE)

    withProgress(message = "Creating ZIP file...", value = 0, {
      csv_files <- character(length(sites))
      for (i in seq_along(sites)) {
        site <- sites[i]
        safe_site <- gsub("[^[:alnum:]_-]", "_", site)
        csv_file <- file.path(temp_download_dir, paste0("MACH_monthly_", safe_site, ".csv"))
        csv_files[i] <- csv_file
        data <- full_data %>% dplyr::filter(SITENO == site)
        if (nrow(data) > 0) {
          readr::write_csv(data, csv_file)
          message("download_separate_m: CSV written for site ", site)
        }
        incProgress(1 / length(sites), detail = paste("Processed site", site))
      }

      valid_csv_files <- csv_files[file.exists(csv_files)]
      if (length(valid_csv_files) > 0) {
        zip::zip(zipfile = file, files = basename(valid_csv_files), root = temp_download_dir)
        message("download_separate_m: ZIP file created successfully")
      }
    })
  },
  contentType = "application/zip"

)
  })
}