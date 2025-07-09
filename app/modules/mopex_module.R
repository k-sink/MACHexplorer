### UI ###
mopexDataUI <- function(id) {
  ns <- NS(id)
  tabPanel(
    title = "MOPEX",
    br(),
    fluidRow(
      column(
        width = 4,
        wellPanel(
          style = "overflow: visible; height: auto;",
          h6(strong("Select Export Option")),
          br(),
          selectInput(
            inputId = ns("mopex_data"),
            label = NULL,
            multiple = FALSE,
            choices = c("MOPEX only" = "mopex", "MOPEX & MACH" = "combined")
          ),
          br(),
          actionButton(inputId = ns("retrieve_mopex"), label = "Retrieve and View Data")
        ),
        br(),
        wellPanel(
          h6(strong("Download MOPEX")),
          br(),
          downloadButton(outputId = ns("download_mopex"), label = "Export as csv"),
          br(),
          br(),
          downloadButton(outputId = ns("download_separate_mopex"), label = "Export as separate csv files")
        )
      ),
      column(
        width = 8,
        wellPanel(
          style = "overflow: visible; height: auto;",
          h6(strong("MOPEX Basins Data")),
          br(),
          DTOutput(outputId = ns("mopex_table"))
        )
      )
    )
  )
}

### SERVER ###
mopexDataServer <- function(id, shared_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Get filtered site numbers reactively
    filtered_sites <- reactive({
      req(shared_data$site$SITENO)
      shared_data$site$SITENO
    })

    # Retrieve and process data
    mopex_table <- eventReactive(input$retrieve_mopex, {

      # Show progress modal
      show_modal_progress_line(text = "Retrieving data...", session = session)
      on.exit(remove_modal_progress(session = session))

      # Validate inputs
      if (is.null(shared_data$mopex_folder) || !dir.exists(shared_data$mopex_folder)) {
        shinyalert::shinyalert(
          title = "Error",
          text = "Please specify MOPEX data location",
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

      # Get matching MOPEX files and IDs
      gauge_ids <- filtered_sites()
      matching_mopex_files <- shared_data$mopex_files[shared_data$mopex_ids %in% gauge_ids]
      if (length(matching_mopex_files) == 0) {
        shinyalert::shinyalert(
          title = "Error",
          text = "No MOPEX sites in current selection",
          type = "error"
        )
        return(data.frame(Message = "No MOPEX sites in current selection"))
      }

      # Ensure QS cache directory exists
      if (is.null(shared_data$qs_cache_dir) || !dir.exists(shared_data$qs_cache_dir)) {
        shared_data$qs_cache_dir <- file.path(dirname(shared_data$mach_folder), "qs_cache_temp")
        dir.create(shared_data$qs_cache_dir, showWarnings = FALSE)
      }

      # Process MOPEX data with progress
      mopex_data <- NULL
      withProgress(message = "Processing MOPEX sites", value = 0, {
        for (file in matching_mopex_files) {
          gauge_id <- stringr::str_extract(basename(file), "(?<=basin_)\\d{8}(?=_MOPEX\\.csv)")
          incProgress(1 / length(matching_mopex_files), detail = paste("Site", gauge_id))

          # Read MOPEX CSV
          df <- read_csv(
            file,
            col_types = cols(
              SITENO = col_character(),
              DATE = col_date(),
              OBSQ = col_double(),
              PRCP = col_double(),
              TMIN = col_double(),
              TMAX = col_double()
            )
          )

          # Convert to QS
          qs_file <- file.path(shared_data$qs_cache_dir, paste0("basin_", gauge_id, "_MOPEX.qs"))
          qs::qsave(df, qs_file)

          # Combine data
          mopex_data <- dplyr::bind_rows(mopex_data, df)
        }
      })

      # Handle MOPEX & MACH option
      if (input$mopex_data == "combined") {
        matching_mach_ids <- shared_data$mach_ids[shared_data$mach_ids %in% gauge_ids]
        if (length(matching_mach_ids) == 0) {
          return(mopex_data)
        }

        # Check for missing MACH QS files
        missing_mach_qs <- sapply(matching_mach_ids, function(gauge_id) {
          qs_file <- file.path(shared_data$qs_cache_dir, paste0("basin_", gauge_id, "_MACH.qs"))
          !file.exists(qs_file)
        })
        if (any(missing_mach_qs)) {
       
          shinyalert::shinyalert(
            title = "Error",
            text = "Click the 'Confirm Sites' button on Site Selection tab to proceed.",
            type = "error"
          )
          return(mopex_data)
        }

        # Process MACH data
        mach_data <- NULL
        withProgress(message = "Processing MACH sites", value = 0, {
          for (gauge_id in matching_mach_ids) {
            qs_file <- file.path(shared_data$qs_cache_dir, paste0("basin_", gauge_id, "_MACH.qs"))
            incProgress(1 / length(matching_mach_ids), detail = paste("Site", gauge_id))
         

            # Read MACH QS
            df <- qs::qread(qs_file) %>%
              dplyr::select(SITENO, DATE, OBSQ, PRCP, TMIN, TMAX)
            

            # Combine data
            mach_data <- dplyr::bind_rows(mach_data, df)
          }
        })
       
        # Combine MOPEX and MACH data
        combined_data <- dplyr::bind_rows(mopex_data, mach_data)
      
        return(combined_data)
      }

      # Return MOPEX only data
      return(mopex_data)
    })

    output$mopex_table <- renderDT({
      data <- mopex_table()
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

    output$download_mopex <- downloadHandler(
      filename = function() {
        paste0("MOPEX_", Sys.Date(), ".csv")
      },
      content = function(file) {
        withProgress(message = "Creating CSV file", value = 0, {
          for (i in 1:5) {
            Sys.sleep(0.2)
            incProgress(1 / 5)
          }
          write.csv(mopex_table(), file, row.names = FALSE)
        })
      }
    )

    output$download_separate_mopex <- downloadHandler(
      filename = function() {
        paste0("MOPEX_", Sys.Date(), ".zip")
      },
      content = function(file) {
        temp_dir <- "basin_files/"
        dir.create(temp_dir, showWarnings = FALSE)
        unlink(paste0(temp_dir, "*"), recursive = TRUE)
        full_data <- mopex_table()
        sites <- unique(full_data$SITENO)
        n_sites <- length(sites)
        withProgress(message = "Creating ZIP file", value = 0, {
          for (i in seq_along(sites)) {
            siteno <- sites[i]
            data <- full_data[full_data$SITENO == siteno, ]
            write.csv(data, file = paste0(temp_dir, "MOPEX_", siteno, ".csv"), row.names = FALSE)
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
