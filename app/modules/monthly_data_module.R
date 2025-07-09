###################################
### MONTHLY DATA MODULE ###
###################################
monthlyDataUI = function(id) {
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
              choices = seq(1980, 2023, 1), multiple = TRUE,
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
          h6(strong("Filtered Monthly Data")),
          br(),
          withSpinner(DTOutput(ns("merged_data_table_m")), type = 5)
        )
      )
    )
  )
}

### SERVER ###
monthlyDataServer <- function(id, selected_data, shared_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
# Get filtered site numbers from gauges table
    filtered_sites <- reactive({
      req(shared_data$site$SITENO)
      shared_data$site$SITENO
    })

    # Aggregate monthly data based on selected statistic
    aggregate_monthly_data <- function(df, var_name, agg_type) {
      agg_func <- if (agg_type == "Total" && var_name %in% c("TAIR", "TMIN", "TMAX")) {
        function(x) round(mean(x, na.rm = TRUE), 2)
      } else {
        switch(
          agg_type,
          "Minimum" = min,
          "Maximum" = max,
          "Median" = median,
          "Mean" = function(x) round(mean(x, na.rm = TRUE), 2),
          "Total" = function(x) round(sum(x, na.rm = TRUE), 2)
        )
      }

      df %>%
        dplyr::mutate(YEAR = lubridate::year(DATE), MONTH = lubridate::month(DATE)) %>%
        dplyr::group_by(SITENO, YEAR, MONTH) %>%
        dplyr::summarise(!!var_name := agg_func(.data[[var_name]]), .groups = "drop")
    }

    # Retrieve and process data
    monthly_table <- eventReactive(input$retrieve_month, {
      show_modal_progress_line(text = "Retrieving data...", session = session)
      on.exit(remove_modal_progress(session = session))

      if (is.null(selected_data()$cached_data) || length(selected_data()$cached_data) == 0) {
        shinyalert::shinyalert(
          title = "Error",
          text = "Click the 'Confirm Sites' button on the Site Selection tab to proceed.",
          type = "error"
        )
        return(data.frame(Message = "No data available"))
      }

      if (length(filtered_sites()) == 0) {
        shinyalert::shinyalert(
          title = "Error",
          text = "No sites selected",
          type = "error"
        )
        return(data.frame(Message = "No sites selected"))
      }

      # Collect selected variables
      selected_vars_m <- isolate({
        vars <- c()
        if (input$select_prcp_m) vars <- c(vars, "PRCP")
        if (input$select_tair_m) vars <- c(vars, "TAIR")
        if (input$select_tmin_m) vars <- c(vars, "TMIN")
        if (input$select_tmax_m) vars <- c(vars, "TMAX")
        if (input$select_pet_m) vars <- c(vars, "PET")
        if (input$select_aet_m) vars <- c(vars, "AET")
        if (input$select_disch_m) vars <- c(vars, "OBSQ")
        if (input$select_swe_m) vars <- c(vars, "SWE")
        if (input$select_srad_m) vars <- c(vars, "SRAD")
        if (input$select_vp_m) vars <- c(vars, "VP")
        if (input$select_dayl_m) vars <- c(vars, "DAYL")
        vars
      })

      if (length(selected_vars_m) == 0) {
        shinyalert::shinyalert(
          title = "Error",
          text = "No variables selected",
          type = "error"
        )
        return(data.frame(Message = "No data available"))
      }

      # Process cached data for filtered sites
      combined_df <- tryCatch({
        withProgress(message = "Processing sites", value = 0, {
          purrr::map_dfr(filtered_sites(), function(gauge_id) {
            incProgress(1 / length(filtered_sites()), detail = paste("Site", gauge_id))
            data <- selected_data()$cached_data[[gauge_id]]
            if (is.null(data)) {
              return(NULL)
            }
            gauge_df <- create_complete_dates(gauge_id, frequency = "monthly") %>%
              mutate(YEAR = lubridate::year(DATE), MONTH = lubridate::month(DATE))
            
            for (varname in selected_vars_m) {
              if (varname %in% colnames(data)) {
                var_agg <- aggregate_monthly_data(data, varname, isolate(input$month_agg))
                gauge_df <- left_join(gauge_df, var_agg, by = c("SITENO", "YEAR", "MONTH"))
              } else {
                gauge_df[[varname]] <- NA
              }
            }
            gauge_df
          })
        })
      }, error = function(e) {
        shinyalert::shinyalert(
          title = "Error",
          text = paste("Error processing data:", e$message),
          type = "error"
        )
        return(data.frame(Message = "Error processing data"))
      })

      if (is.null(combined_df) || nrow(combined_df) == 0) {
        shinyalert::shinyalert(
          title = "Error",
          text = "No data available after processing",
          type = "error"
        )
        return(data.frame(Message = "No data available"))
      }

      # Apply year and month filters
      if (isolate(input$select_year_m)) {
        combined_df <- combined_df %>%
          dplyr::filter(YEAR %in% isolate(input$year2))
      }
      if (isolate(input$select_month_m)) {
        combined_df <- combined_df %>%
          dplyr::filter(MONTH %in% isolate(input$month2))
      }

      if (nrow(combined_df) == 0) {
        shinyalert::shinyalert(
          title = "Error",
          text = "No data matches the selected filters",
          type = "error"
        )
        return(data.frame(Message = "No data available"))
      }

      combined_df %>% dplyr::select(-DATE)
    })

    # Render table container
    output$table_container_m <- renderUI({
      req(monthly_table())
      DT::dataTableOutput(ns("merged_data_table_m")) %>% shinycssloaders::withSpinner(type = 5)
    })

    # Render data table
    output$merged_data_table_m <- DT::renderDataTable({
      data <- monthly_table()
      if ("Message" %in% colnames(data)) {
        DT::datatable(
          data,
          options = list(pageLength = 10, scrollX = TRUE, dom = "t"),
          class = "display responsive nowrap"
        )
      } else {
        DT::datatable(
          data,
          options = list(
            pageLength = 10,
            scrollX = TRUE,
            lengthMenu = c(5, 10, 20, 50),
            dom = "Blfrtip",
            paging = TRUE,
            server = TRUE
          ),
          class = "display responsive nowrap"
        )
      }
    })

    # Download single CSV
    output$download_csv_m <- downloadHandler(
      filename = function() {
        paste0("MACH_monthly_", Sys.Date(), ".csv")
      },
      content = function(file) {
        withProgress(message = "Creating CSV file", value = 0, {
          for (i in 1:5) {
            Sys.sleep(0.2)
            incProgress(1/5)
          }
          write.csv(monthly_table(), file, row.names = FALSE)
        })
      }
    )

    # Download separate CSV files as ZIP
    output$download_separate_m <- downloadHandler(
      filename = function() {
        paste0("MACH_monthly_", Sys.Date(), ".zip")
      },
      content = function(file) {
        temp_dir <- "basin_files/"
        if (dir.exists(temp_dir)) {
          unlink(temp_dir, recursive = TRUE, force = TRUE)
        }
        dir.create(temp_dir, showWarnings = FALSE)
        full_data <- monthly_table()
        sites <- unique(full_data$SITENO)
        n_sites <- length(sites)
        withProgress(message = "Creating ZIP file", value = 0, {
          for (i in seq_along(sites)) {
            siteno <- sites[i]
            data <- full_data[full_data$SITENO == siteno, ]
            write.csv(data, file = paste0(temp_dir, "MACH_monthly_", siteno, ".csv"), row.names = FALSE)
            incProgress(1 / n_sites)
          }
          zip::zip(
            zipfile = file,
            files = list.files(temp_dir, full.names = TRUE)
          )
        })
      }
    )
  })
}