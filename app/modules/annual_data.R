#######################
### ANNUAL DATA UI ###
#######################
annual_data_ui <- function(id) {
  ns <- NS(id)
  tagList(
   
    fluidRow(
      column(width = 4,
        card(style = "overflow: visible; height: auto;",
          card_header("Annual Aggregation"), 
          card_body(style = "overflow: visible; height: auto;",
          radioButtons(
            inputId = ns("year_type"),
            label = NULL,
            inline = FALSE,
            choices = c("Water Year" = "water", "Calendar Year" = "calendar"),
            selected = "water"
          ),
          h6(strong("Select Statistic")),
          selectizeInput(
            inputId = ns("year_agg"),
            label = NULL,
            multiple = FALSE,
            choices = c("Minimum", "Maximum", "Median", "Mean", "Total"),
            selected = "Mean"
          ))
        ),
    
        card(style = "overflow: visible; height: auto;",
          card_header("Filter Annual Data"), 
          card_body(style = "overflow: visible; height: auto;",
          h6(strong("Select Variable(s)")),
          checkboxInput(ns("select_prcp_y"), "Precipitation, PRCP (mm)", FALSE),
          checkboxInput(ns("select_tair_y"), HTML("Mean Temperature, TAIR (°C)"), FALSE),
          checkboxInput(ns("select_tmin_y"), HTML("Minimum Temperature, TMIN (°C)"), FALSE),
          checkboxInput(ns("select_tmax_y"), HTML("Maximum Temperature, TMAX (°C)"), FALSE),
          checkboxInput(ns("select_pet_y"), "Potential Evapotranspiration, PET (mm)", FALSE),
          checkboxInput(ns("select_aet_y"), "Actual Evapotranspiration, AET (mm)", FALSE),
          checkboxInput(ns("select_disch_y"), "Observed Discharge, OBSQ (mm)", FALSE),
          checkboxInput(ns("select_swe_y"), "Snow Water Equivalent, SWE (mm)", FALSE),
          checkboxInput(ns("select_srad_y"), HTML("Shortwave Radiation, SRAD (W/m<sup>2</sup>)"), FALSE),
          checkboxInput(ns("select_vp_y"), "Water Vapor Pressure, VP (Pa)", FALSE),
          checkboxInput(ns("select_dayl_y"), "Day Length, DAYL (sec)", FALSE),
          br(),
          h6(strong("Select Time Period(s)")),
          conditionalPanel(
            condition = "input.year_type == 'water'", ns = ns, 
            checkboxInput(ns("select_year_wy"), "Water Year", FALSE),
            conditionalPanel(
              condition = "input.select_year_wy == true", ns = ns, 
              selectizeInput(
                inputId = ns("wateryear1"),
                label = NULL,
                choices = seq(1981, 2023, 1),
                multiple = TRUE,
                options = list(placeholder = "Select one or more")
              )
            )
          ),
          conditionalPanel(
            condition = "input.year_type == 'calendar'", ns = ns, 
            checkboxInput(ns("select_year_cal"), "Calendar Year", FALSE),
            conditionalPanel(
              condition = "input.select_year_cal == true", ns = ns, 
              selectizeInput(
                inputId = ns("calyear1"),
                label = NULL,
                choices = seq(1980, 2023, 1),
                multiple = TRUE,
                options = list(placeholder = "Select one or more")
              )
            )
          ),
  
          actionButton(ns("retrieve_year"), "Retrieve and View Data", 
                       class = "btn-dark", style = "width: 75%; margin-top: 5px;")
        )),
        

        card(
          card_header("Download Annual Data"), 
          card_body(
          downloadButton(ns("download_csv_y"), "Export as csv", 
                         class = "btn-dark", style = "width: 50%; margin-top: 5px;"),
       
          downloadButton(ns("download_separate_y"), "Export as separate csv files", 
                         class = "btn-dark", style = "width: 75%; margin-top: 5px;")
        )),
 
      ),
      
      column(width = 8,
        card(
          card_header(textOutput(ns("annual_data_heading"))),
          card_body(style = "overflow-y: auto;",
           withSpinner(DT::DTOutput(ns("merged_data_table_y")), type = 5)))
        )
      )
     )
}

##########################
### ANNUAL DATA SERVER ###
##########################
annual_data_server <- function(id, shared_data) {
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

    # Aggregate annual data based on selected statistic and year type
    aggregate_annual_data <- function(tbl, var_name, agg_type) {
      agg_func <- switch(
        agg_type,
        "Minimum" = min,
        "Maximum" = max,
        "Median" = median,
        "Mean" = mean,
        "Total" = sum
      )
      # For TAIR, TMIN, TMAX, use mean for Total aggregation
      if (agg_type == "Total" && var_name %in% c("TAIR", "TMIN", "TMAX")) {
        agg_func <- mean
      }
      if (input$year_type == "water") {
        tbl %>%
          dplyr::group_by(SITENO, WATERYR) %>%
          dplyr::summarise(!!var_name := agg_func(!!rlang::sym(var_name), na.rm = TRUE), .groups = "drop") %>%
          dplyr::mutate(!!var_name := round(!!rlang::sym(var_name), 2))
      } else {
        tbl %>%
          dplyr::group_by(SITENO, YEAR) %>%
          dplyr::summarise(!!var_name := agg_func(!!rlang::sym(var_name), na.rm = TRUE), .groups = "drop") %>%
          dplyr::mutate(!!var_name := round(!!rlang::sym(var_name), 2))
      }
    }

    # Reactive for annual data query with debugging
    annual_table <- eventReactive(input$retrieve_year, {
      req(shared_data$database_ready, shared_data$duckdb_connection)
      conn <- shared_data$duckdb_connection

      if (!DBI::dbIsValid(conn)) {
        shinyalert("Error", "Database connection is invalid. Please re-import.", type = "error")
        return(data.frame(Message = "Database connection invalid"))
      }

      sites <- filtered_sites()
      if (length(sites) == 0) {
        shinyalert("Error", "Please select at least one site.", type = "error")
        return(data.frame(Message = "No sites selected"))
      }

      valid_site_ids <- intersect(shared_data$mach_ids, sites)
      if (length(valid_site_ids) == 0) {
        shinyalert("Error", "No valid site data found.", type = "error")
        return(data.frame(Message = "No valid site data"))
      }

      last_site_count(length(valid_site_ids))
      query_triggered(TRUE)

      vars <- c("SITENO", "DATE")
      if (isTRUE(input$select_prcp_y)) vars <- c(vars, "PRCP")
      if (isTRUE(input$select_tair_y)) vars <- c(vars, "TAIR")
      if (isTRUE(input$select_tmin_y)) vars <- c(vars, "TMIN")
      if (isTRUE(input$select_tmax_y)) vars <- c(vars, "TMAX")
      if (isTRUE(input$select_pet_y)) vars <- c(vars, "PET")
      if (isTRUE(input$select_aet_y)) vars <- c(vars, "AET")
      if (isTRUE(input$select_disch_y)) vars <- c(vars, "OBSQ")
      if (isTRUE(input$select_swe_y)) vars <- c(vars, "SWE")
      if (isTRUE(input$select_srad_y)) vars <- c(vars, "SRAD")
      if (isTRUE(input$select_vp_y)) vars <- c(vars, "VP")
      if (isTRUE(input$select_dayl_y)) vars <- c(vars, "DAYL")

      if (length(vars) <= 2) {
        shinyalert("Error", "Please select at least one variable.", type = "error")
        query_triggered(FALSE)
        return(data.frame(Message = "No variables selected"))
      }

      withProgress(message="Fetching annual data...", value=0, {
        incProgress(0.3)
        # Initial query to fetch data
        tbl <- dplyr::tbl(conn, "mach_daily") %>%
          dplyr::filter(SITENO %in% valid_site_ids) %>%
          dplyr::select(all_of(vars))

        # Collect data and compute years locally
        initial_data <- tbl %>% dplyr::collect()
        # Ensure DATE is a Date object
        initial_data <- initial_data %>%
          dplyr::mutate(DATE = as.Date(DATE))
        if (input$year_type == "water" && isTRUE(input$select_year_wy) && !is.null(input$wateryear1) && length(input$wateryear1) > 0 &&
            all(!is.na(as.numeric(input$wateryear1)))) {
          selected_years <- as.numeric(input$wateryear1)
          tbl <- initial_data %>%
            dplyr::mutate(WATERYR = wYear(DATE)) %>%
             dplyr::filter(WATERYR %in% selected_years) 
        } else if (input$year_type == "calendar" && isTRUE(input$select_year_cal) && !is.null(input$calyear1) && length(input$calyear1) > 0 &&
                   all(!is.na(as.numeric(input$calyear1)))) {
          selected_years <- as.numeric(input$calyear1)
          tbl <- initial_data %>%
            dplyr::mutate(YEAR = lubridate::year(DATE)) %>%
            dplyr::filter(YEAR %in% selected_years)
        } else {
          if (input$year_type == "water") {
            tbl <- initial_data %>%
              dplyr::mutate(WATERYR = wYear(DATE)) %>% 
              dplyr::filter(WATERYR > 1980)
          } else {
            tbl <- initial_data %>%
              dplyr::mutate(YEAR = lubridate::year(DATE))
          }
        }

        combined_df <- NULL
        year_col <- if (input$year_type == "water") "WATERYR" else "YEAR"
        for (var in vars[vars != "SITENO" & vars != "DATE"]) {
          agg_tbl <- aggregate_annual_data(tbl, var, input$year_agg)
          if (is.null(combined_df)) {
            combined_df <- agg_tbl
          } else {
            combined_df <- combined_df %>%
              dplyr::full_join(agg_tbl, by = c("SITENO", year_col))
          }
          incProgress(0.3 / length(vars[vars != "SITENO" & vars != "DATE"]))
        }
        incProgress(0.3)

        df <- tryCatch({
          df <- combined_df %>%
            dplyr::arrange(SITENO, !!rlang::sym(year_col)) %>%
            dplyr::collect()
          df$SITENO <- as.character(df$SITENO)
          df[[year_col]] <- as.integer(df[[year_col]])
          # Ensure numeric variables, handling NA for OBSQ
          for (var in vars[vars != "SITENO" & vars != "DATE" & vars != year_col]) {
            df[[var]] <- suppressWarnings(as.numeric(df[[var]]))
          }
          df
        }, error = function(e) {
          shinyalert("Error", paste("Error fetching data:", e$message), type = "error")
          return(data.frame(Message = "Error fetching data"))
        })
        incProgress(1)
        df
      })
    })

    # Update heading
    output$annual_data_heading <- renderText({
      req(last_site_count())
      sprintf("Filtered Annual Data for %d sites", last_site_count())
    })

    # Render data table
    output$merged_data_table_y <- renderDT({
      req(annual_table(), query_triggered())
      data <- annual_table()
      if ("Message" %in% colnames(data)) {
        DT::datatable(
          data,
          options = list(pageLength = 10, scrollX = TRUE, dom = "Blfrtip"),
          class = "display responsive nowrap",
          rownames = FALSE
        )
      } else {
        col_defs <- lapply(seq_along(colnames(data)), function(i) {
          col_name <- colnames(data)[i]
          if (col_name %in% c("SITENO", "YEAR", "WATERYR")) {
            list(targets = i - 1, title = col_name, width = 100, render = DT::JS("function(data) { return data || ''; }"))
          } else {
            list(targets = i - 1, title = col_name, render = DT::JS("function(data) { return data !== null && data !== undefined ? Number(data).toFixed(2) : ''; }"))
          }
        })

        DT::datatable(
          as.data.frame(data),
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

    # Download single CSV
    output$download_csv_y <- downloadHandler(
      filename = function() {
        paste0("MACH_annual_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(shared_data$database_ready, shared_data$duckdb_connection)
        conn <- shared_data$duckdb_connection

        sites <- filtered_sites()
        valid_site_ids <- intersect(shared_data$mach_ids, sites)
        if (query_triggered() && length(valid_site_ids) != last_site_count()) {
          message("download_csv_y: Site selection mismatch, current sites: ", length(valid_site_ids), ", last count: ", last_site_count())
          shinyalert("We have our differences",
            "Site selection has changed. Please click 'Retrieve and View Data' to update the data before downloading.",
            type = "warning"
          )
        }

        req(annual_table())
        data <- annual_table()
        if ("Message" %in% colnames(data)) {
          return()
        }

        withProgress(message = "Creating CSV file...", value = 0, {
          incProgress(0.8)
          readr::write_csv(data, file)
          incProgress(1)
          message("download_csv_y: CSV written successfully")
        })
      },
      contentType = "text/csv"
    )

    # Download separate CSV files as ZIP
    output$download_separate_y <- downloadHandler(
      filename = function() {
        paste0("MACH_annual_", Sys.Date(), ".zip")
      },
      content = function(file) {
        req(shared_data$database_ready, shared_data$duckdb_connection)
        conn <- shared_data$duckdb_connection

        sites <- filtered_sites()
        valid_site_ids <- intersect(shared_data$mach_ids, sites)
        if (query_triggered() && length(valid_site_ids) != last_site_count()) {
          message("download_separate_y: Site selection mismatch, current sites: ", length(valid_site_ids), ", last count: ", last_site_count())
          shinyalert("We have our differences.",
            "Site selection has changed. Please click 'Retrieve and View Data' to update the data before downloading.",
            type = "warning"
          )
        }

        req(annual_table())
        full_data <- annual_table()
        if ("Message" %in% colnames(full_data)) {
          return()
        }

        sites <- unique(full_data$SITENO)
        temp_download_dir <- file.path(tempdir(), paste0("download_", format(Sys.time(), "%Y%m%d_%H%M%S")))
        dir.create(temp_download_dir, showWarnings = FALSE, recursive = TRUE)
        on.exit(unlink(temp_download_dir, recursive = TRUE, force = TRUE), add = TRUE)

        withProgress(message = "Creating ZIP file...", value = 0, {
          csv_files <- character(length(sites))
          for (i in seq_along(sites)) {
            site <- sites[i]
            safe_site <- gsub("[^[:alnum:]_-]", "_", site)
            csv_file <- file.path(temp_download_dir, paste0("MACH_annual_", safe_site, ".csv"))
            csv_files[i] <- csv_file
            data <- full_data %>% dplyr::filter(SITENO == site)
            if (nrow(data) > 0) {
              readr::write_csv(data, csv_file)
              message("download_separate_y: CSV written for site ", site)
            }
            incProgress(1 / length(sites), detail = paste("Processed site", site))
          }

          valid_csv_files <- csv_files[file.exists(csv_files)]
          if (length(valid_csv_files) > 0) {
            zip::zip(zipfile = file, files = basename(valid_csv_files), root = temp_download_dir)
            message("download_separate_y: ZIP file created successfully")
          }
        })
      },
      contentType = "application/zip"
    )
  })
}