###################################
### DAILY DATA MODULE ###
###################################
dailyDataUI = function(id) {
  ns = NS(id)
  tagList(
    br(),
    fluidRow(
      column(width = 4,
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
      column(width = 8,
        wellPanel(
          h6(strong(textOutput(ns("daily_data_heading")))),
          br(),
          withSpinner(reactable_extras_ui(ns("merged_data_table")), type = 5)
        )
      )
    )
  )
}

### SERVER ###
dailyDataServer = function(id, shared_data) {
  moduleServer(id, function(input, output, session) {
    ns = session$ns

    filtered_sites <- reactive({
      sitenos <- shared_data$site$SITENO
      if (is.null(sitenos) || length(sitenos) == 0) character(0) else sitenos
    })

    last_site_count <- reactiveVal(NULL)

    # Reset filters
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

      showNotification("All filters have been reset.", type = "message")
    })

    filter_if_needed <- function(tbl, input, select_var, col_name, min_input, max_input, default_min, default_max) {
      if (isTRUE(input[[select_var]]) && !is.null(input[[min_input]]) && !is.null(input[[max_input]])) {
        min_val <- as.numeric(input[[min_input]])
        max_val <- as.numeric(input[[max_input]])
        if (min_val > default_min || max_val < default_max) {
          col_sym <- rlang::sym(col_name)
          if (col_name == "OBSQ") {
            tbl <- tbl %>% dplyr::filter(is.na(!!col_sym) | dplyr::between(!!col_sym, min_val, max_val))
          } else {
            tbl <- tbl %>% dplyr::filter(dplyr::between(!!col_sym, min_val, max_val))
          }
        }
      }
      tbl
    }

    filtered_query <- eventReactive(input$retrieve_data, {
      req(input$retrieve_data)

      if (is.null(shared_data$mach_db) || !file.exists(shared_data$mach_db)) {
        shinyalert(
          title = HTML('<i class="fa-solid fa-folder-open"></i> MACH Database Missing'),
          text = "Please select the MACH database file on the Dataset Import tab.",
          type = "error",
          html = TRUE
        )
        return(NULL)
      }

      req(shared_data$duckdb_con)
      sites <- filtered_sites()
      if (length(sites) == 0) {
        shinyalert("Please select sites on site selection tab.", type = "error")
        return(NULL)
      }

      valid_site_ids <- shared_data$mach_ids[shared_data$mach_ids %in% sites]
      if (length(valid_site_ids) == 0) {
        shinyalert("No valid site data found in MACH database.", type = "error")
        return(NULL)
      }

      last_site_count(length(valid_site_ids))

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
        return(NULL)
      }

      withProgress(message = "Building query...", value = 0.4, {
        combined <- dplyr::tbl(shared_data$duckdb_con, "mach_daily") %>%
          dplyr::filter(SITENO %in% valid_site_ids) %>%
          dplyr::select(all_of(vars))

        combined <- filter_if_needed(combined, input, "select_prcp", "PRCP", "prcp_min", "prcp_max", 0, 300)
        combined <- filter_if_needed(combined, input, "select_tair", "TAIR", "tair_min", "tair_max", -50, 50)
        combined <- filter_if_needed(combined, input, "select_tmin", "TMIN", "tmin_min", "tmin_max", -50, 50)
        combined <- filter_if_needed(combined, input, "select_tmax", "TMAX", "tmax_min", "tmax_max", -50, 50)
        combined <- filter_if_needed(combined, input, "select_pet", "PET", "pet_min", "pet_max", -1, 40)
        combined <- filter_if_needed(combined, input, "select_aet", "AET", "aet_min", "aet_max", -1, 40)
        combined <- filter_if_needed(combined, input, "select_disch", "OBSQ", "disch_min", "disch_max", -1, 400)
        combined <- filter_if_needed(combined, input, "select_swe", "SWE", "swe_min", "swe_max", 0, 1850)
        combined <- filter_if_needed(combined, input, "select_srad", "SRAD", "srad_min", "srad_max", 10, 900)
        combined <- filter_if_needed(combined, input, "select_vp", "VP", "vp_min", "vp_max", 5, 4000)
        combined <- filter_if_needed(combined, input, "select_dayl", "DAYL", "dayl_min", "dayl_max", 30000, 60000)

        if (isTRUE(input$select_date) && !is.null(input$date_range1) && length(input$date_range1) == 2) {
          start_date <- as.Date(input$date_range1[1])
          end_date <- as.Date(input$date_range1[2])
          combined <- combined %>% dplyr::filter(DATE >= start_date, DATE <= end_date)

          if (isTRUE(input$select_year) && !is.null(input$year1) && length(input$year1) > 0) {
            selected_years <- as.numeric(input$year1)
            years_in_range <- selected_years >= lubridate::year(start_date) & selected_years <= lubridate::year(end_date)
            if (!all(years_in_range)) {
              shinyalert("Selected year(s) are outside the specified date range.", type = "error")
              req(FALSE)
            }
            combined <- combined %>% dplyr::filter(lubridate::year(DATE) %in% selected_years)
          }
        } else if (isTRUE(input$select_year) && !is.null(input$year1) && length(input$year1) > 0) {
          selected_years <- as.numeric(input$year1)
          combined <- combined %>% dplyr::filter(lubridate::year(DATE) %in% selected_years)
        }

        if (isTRUE(input$select_month) && !is.null(input$month1) && length(input$month1) > 0) {
          selected_months <- as.numeric(input$month1)
          combined <- combined %>% dplyr::filter(lubridate::month(DATE) %in% selected_months)
        }

        incProgress(1)
        combined
      })
    })



output$merged_data_table <- renderReactable({
  req(filtered_query())

  preview <- filtered_query() %>% head(1000) %>% collect()

  if (nrow(preview) == 0) {
    return(reactable(data.frame(Message = "No data available.")))
  }

  reactable(
    preview,
    searchable = TRUE,
    filterable = TRUE,
    sortable = TRUE,
    pagination = TRUE,
    defaultPageSize = 25,
    striped = TRUE,
    highlight = TRUE,
    resizable = TRUE,
    columns = list(
      DATE = colDef(format = colFormat(date = TRUE)),
      SITENO = colDef(name = "SITENO")
    )
  )
})

    output$daily_data_heading <- renderText({
      req(last_site_count())
      sprintf("Filtered Daily Data for %d sites", last_site_count())
    })

    output$download_csv <- downloadHandler(
      filename = function() paste0("MACH_daily_", Sys.Date(), ".csv"),
      content = function(file) {
        withProgress(message = "Preparing full download...", value = 0, {
          query <- filtered_query()
          req(query)
          df <- query %>% collect()
          if (nrow(df) == 0) {
            shinyalert("No data available to download.", type = "error")
            return()
          }
          incProgress(0.8)
          readr::write_csv(df, file)
          incProgress(1)
        })
      }
    )

    output$download_separate <- downloadHandler(
      filename = function() paste0("MACH_daily_", Sys.Date(), ".zip"),
      content = function(file) {
        temp_download_dir <- file.path(shared_data$session_temp_dir, paste0("download_", format(Sys.time(), "%Y%m%d_%H%M%S")))
        dir.create(temp_download_dir, showWarnings = FALSE)
        on.exit(unlink(temp_download_dir, recursive = TRUE, force = TRUE), add = TRUE)

        query <- filtered_query()
        req(query)
        df <- query %>% collect()

        if (nrow(df) == 0) {
          shinyalert("No data available to download.", type = "error")
          return()
        }

        withProgress(message = "Creating ZIP file...", value = 0, {
          site_groups <- split(df, df$SITENO)
          total <- length(site_groups)
          for (i in seq_along(site_groups)) {
            site <- names(site_groups)[i]
            safe_site <- gsub("[^[:alnum:]]", "_", site)
            file_path <- file.path(temp_download_dir, paste0("MACH_daily_", safe_site, ".csv"))
            readr::write_csv(site_groups[[i]], file_path)
            incProgress(1 / total)
          }
          zip::zip(zipfile = file, files = list.files(temp_download_dir, full.names = TRUE))
        })
      },
      contentType = "application/zip"
    )
  })
}
