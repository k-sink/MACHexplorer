annualDataUI <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    fluidRow(
      column(
        width = 4,
        wellPanel(
          style = "overflow: visible; height: auto;",
          h6(strong("Annual Aggregation")),
          radioButtons(
            inputId = ns("year_type"),
            label = NULL,
            inline = FALSE,
            choices = c("Water Year" = "water", "Calendar Year" = "calendar"),
            selected = "water"
          ),
          h6(strong("Select Statistic")),
          selectInput(
            inputId = ns("year_agg"),
            label = NULL,
            multiple = FALSE,
            choices = c("Minimum", "Maximum", "Median", "Mean", "Total"),
            selected = "Mean"
          )
        ),
        br(),
        wellPanel(
          style = "overflow: visible; height: auto;",
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
            condition = paste0("input['", ns("year_type"), "'] == 'water'"),
            checkboxInput(ns("select_year_wy"), "Water Year", FALSE),
            conditionalPanel(
              condition = paste0("input['", ns("select_year_wy"), "'] == true"),
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
            condition = paste0("input['", ns("year_type"), "'] == 'calendar'"),
            checkboxInput(ns("select_year_cal"), "Calendar Year", FALSE),
            conditionalPanel(
              condition = paste0("input['", ns("select_year_cal"), "'] == true"),
              selectizeInput(
                inputId = ns("calyear1"),
                label = NULL,
                choices = seq(1980, 2023, 1),
                multiple = TRUE,
                options = list(placeholder = "Select one or more")
              )
            )
          ),
          br(),
          actionButton(ns("retrieve_year"), "Retrieve and View Data")
        ),
        br(),
        wellPanel(
          h6(strong("Download Annual Data")),
          br(),
          downloadButton(ns("download_csv_y"), "Export as csv"),
          br(),
          br(),
          downloadButton(ns("download_separate_y"), "Export as separate csv files")
        ),
 
      ),
      column(
        width = 8,
        wellPanel(
          h6(strong("Filtered Annual Data")),
          br(),
          DTOutput(ns("merged_data_table_y"))
        )
      )
    )
  )
}

annualDataServer <- function(id, shared_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Get filtered site numbers reactively
    filtered_sites <- reactive({
      req(shared_data$site$SITENO)
      shared_data$site$SITENO
    })

    # Aggregate annual data based on selected statistic and year type
    aggregate_annual_data <- function(df, var_name, agg_type) {
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

      if (input$year_type == "water") {
        df %>%
          dplyr::mutate(WATERYR = wYear(DATE)) %>%
          dplyr::group_by(SITENO, WATERYR) %>%
          dplyr::summarise(!!var_name := agg_func(.data[[var_name]]), .groups = "drop")
      } else {
        df %>%
          dplyr::mutate(YEAR = lubridate::year(DATE)) %>%
          dplyr::group_by(SITENO, YEAR) %>%
          dplyr::summarise(!!var_name := agg_func(.data[[var_name]]), .groups = "drop")
      }
    }

    # Retrieve and process data
    annual_table <- eventReactive(input$retrieve_year, {
      # Show progress modal
      show_modal_progress_line(text = "Retrieving data...", session = session)
      on.exit(remove_modal_progress(session = session))

      # Validate inputs
      if (is.null(shared_data$mach_folder) || !dir.exists(shared_data$mach_folder)) {
       shinyalert::shinyalert(
          title = "Error",
          text = "Please specify MACH data location",
          type = "error"
        )
        return(data.frame(Message = "No data available"))
      }
      
    if (is.null(shared_data$qs_cache_dir) || !dir.exists(shared_data$qs_cache_dir)) {
        shinyalert::shinyalert(
          title = "Error",
          text = "Click the 'Confirm Sites' button on Site Selection tab to proceed.",
          type = "error"
        )
        return(data.frame(Message = "No data available"))
      }
      
      if (length(filtered_sites()) == 0) {
          shinyalert::shinyalert(
           title = "Error",
           text = "No sites selected",
           type = "error")
        return(data.frame(Message = "No sites selected"))
      }

      # Collect selected variables
      selected_vars_y <- isolate({
        vars <- c()
        if (input$select_prcp_y) vars <- c(vars, "PRCP")
        if (input$select_tair_y) vars <- c(vars, "TAIR")
        if (input$select_tmin_y) vars <- c(vars, "TMIN")
        if (input$select_tmax_y) vars <- c(vars, "TMAX")
        if (input$select_pet_y) vars <- c(vars, "PET")
        if (input$select_aet_y) vars <- c(vars, "AET")
        if (input$select_disch_y) vars <- c(vars, "OBSQ")
        if (input$select_swe_y) vars <- c(vars, "SWE")
        if (input$select_srad_y) vars <- c(vars, "SRAD")
        if (input$select_vp_y) vars <- c(vars, "VP")
        if (input$select_dayl_y) vars <- c(vars, "DAYL")
        vars
      })

      if (length(selected_vars_y) == 0) {
        shinyalert::shinyalert(
          title = "Error", 
          text = "No variables selected", 
          type = "error"
        )
        return(data.frame(Message = "No data available"))
      }

      # Get matching files and IDs
      selected_site_ids <- shared_data$mach_ids[shared_data$mach_ids %in% filtered_sites()]

      if (length(selected_site_ids) == 0) {
        showNotification("No matching data files found", type = "error")
        return(data.frame(Message = "No data available"))
      }

    # Check for missing QS files
      missing_qs <- sapply(selected_site_ids, function(gauge_id) {
        qs_file <- file.path(shared_data$qs_cache_dir, paste0("basin_", gauge_id, "_MACH.qs"))
        !file.exists(qs_file)
      })
      if (any(missing_qs)) {

        shinyalert::shinyalert(
          title = "Error",
          text = "Click the 'Confirm Sites' button on Site Selection tab to proceed.",
          type = "error"
        )
        return(data.frame(Message = "Missing QS files"))
      }

   # Process data with progress
      all_gauges_data <- list()
      withProgress(message = "Processing sites", value = 0, {
        for (gauge_id in selected_site_ids) {
          qs_file <- file.path(shared_data$qs_cache_dir, paste0("basin_", gauge_id, "_MACH.qs"))
          incProgress(1 / length(selected_site_ids), detail = paste("Site", gauge_id))

          if (input$year_type == "water") {
            gauge_df <- create_complete_dates(gauge_id, frequency = "wyearly") %>%
              dplyr::mutate(WATERYR = wYear(DATE))
          } else {
            gauge_df <- create_complete_dates(gauge_id, frequency = "yearly") %>%
              dplyr::mutate(YEAR = lubridate::year(DATE))
          }

          for (var_name in selected_vars_y) {
            var_data <- read_qs(qs_file, var_name, gauge_id, frequency = "daily")
          
            if (!is.null(var_data) && nrow(var_data) > 0 && var_name %in% colnames(var_data)) {
              var_agg <- aggregate_annual_data(var_data, var_name, input$year_agg)

              if (input$year_type == "water") {
                gauge_df <- dplyr::left_join(gauge_df, var_agg, by = c("SITENO", "WATERYR"))
              } else {
                gauge_df <- dplyr::left_join(gauge_df, var_agg, by = c("SITENO", "YEAR"))
              }
            } else {
            
              gauge_df[[var_name]] <- NA
            }
          }
          all_gauges_data[[gauge_id]] <- gauge_df
        }
      })

      # Combine all gauge data frames
      combined_df <- dplyr::bind_rows(all_gauges_data)


      if (input$year_type == "water") {
        if (length(input$wateryear1) > 0) {

          combined_df <- combined_df %>%
            dplyr::filter(WATERYR %in% input$wateryear1)

        }
      } else {
        if (length(input$calyear1) > 0) {
 
          combined_df <- combined_df %>%
            dplyr::filter(YEAR %in% input$calyear1)

        }
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


    # Render data table with client-side processing
    output$merged_data_table_y <- DT::renderDataTable({
      data <- annual_table()
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
    output$download_csv_y <- downloadHandler(
      filename = function() {
        paste0("MACH_annual_", Sys.Date(), ".csv")
      },
      content = function(file) {
        show_modal_progress_line(text = "Creating CSV file...", session = session)
        on.exit(remove_modal_progress(session = session))
        withProgress(message = "Creating CSV file", value = 0, {
          for (i in 1:5) {
            Sys.sleep(0.2)
            incProgress(1/5)
          }
          write.csv(annual_table(), file, row.names = FALSE)
        })
      }
    )

    # Download separate CSV files as ZIP
    output$download_separate_y <- downloadHandler(
      filename = function() {
        paste0("MACH_annual_", Sys.Date(), ".zip")
      },
      content = function(file) {
        show_modal_progress_line(text = "Creating ZIP file...", session = session)
        on.exit(remove_modal_progress(session = session))
        temp_dir <- "basin_files/"
        dir.create(temp_dir, showWarnings = FALSE)
        unlink(paste0(temp_dir, "*"), recursive = TRUE)
        full_data <- annual_table()
        sites <- unique(full_data$SITENO)
        n_sites <- length(sites)
        withProgress(message = "Creating ZIP file", value = 0, {
          for (i in seq_along(sites)) {
            siteno <- sites[i]
            data <- full_data[full_data$SITENO == siteno, ]
            write.csv(data, file = paste0(temp_dir, "MACH_annual_", siteno, ".csv"), row.names = FALSE)
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
