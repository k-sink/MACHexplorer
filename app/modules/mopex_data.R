mopex_data_ui <- function(id) {
  ns <- NS(id)
  tagList(
   
    fluidRow(
      column(width = 4,
        card(style = "overflow: visible; height: auto;",
          card_header("Select Export Option"),
          card_body(style = "overflow: visible; height: auto;",
          selectInput(
            inputId = ns("mopex_data"),
            label = NULL,
            multiple = FALSE,
            choices = c("MOPEX only" = "mopex", "MOPEX & MACH" = "combined")
          ),
          actionButton(inputId = ns("retrieve_mopex"), label = "Retrieve and Confirm Data", 
                        class = "btn-dark", style = "width: 75%; margin-top: 5px;"))
        ),
      
        card(
          card_header("Download MOPEX"),
          card_body(
          downloadButton(outputId = ns("download_mopex"), label = "Export as csv", 
                          class = "btn-dark", style = "width: 50%; margin-top: 5px;"),
          downloadButton(outputId = ns("download_separate_mopex"), label = "Export as separate csv files", 
                          class = "btn-dark", style = "width: 75%; margin-top: 5px;"))
        )),
      
      column(width = 8,
        card(
          card_header(textOutput(ns("mopex_data_heading"))),
          card_body(style = "overflow-y: auto;",
          withSpinner(DT::DTOutput(ns("mopex_count")), type = 6)
        )
      )
    )
  )
 )
}

### SERVER ###
mopex_data_server <- function(id, shared_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    discharge_mopex_count <- reactive({
      tryCatch({
        qs::qread("data/discharge_mopex.qs")
      }, error = function(e) {
        showNotification(paste("Error loading discharge_mopex.qs:", e$message), type = "error")
        NULL
      })
    })
    
    filtered_sites <- reactive({
      req(shared_data$selected_sites)
      shared_data$selected_sites
    })

    discharge_mopex <- reactive({
      req(discharge_mopex_count(), filtered_sites())
      tryCatch({
        discharge_mopex_count() %>%
          dplyr::filter(SITENO %in% filtered_sites())
      }, error = function(e) {
        showNotification(paste("Error processing discharge_mopex_count:", e$message), type = "error")
        data.frame(SITENO = character())
      })
    })

    # Use reactiveVal to store data and trigger alerts only on retrieve
    mopex_data <- reactiveVal()
    observeEvent(input$retrieve_mopex, {
      req(shared_data$database_ready, shared_data$duckdb_connection)
      conn <- shared_data$duckdb_connection
      if (!DBI::dbIsValid(conn)) {
        shinyalert("Error", "Database connection is invalid. Please re-import.", type = "error")
        mopex_data(data.frame(Message = "Database connection invalid"))
        return()
      }

      sites <- filtered_sites()
      if (length(sites) == 0) {
        shinyalert("Error", "No sites selected", type = "error")
        mopex_data(data.frame(Message = "No sites selected"))
        return()
      }

      mopex_sites <- DBI::dbGetQuery(conn, "SELECT DISTINCT SITENO FROM mopex_daily")$SITENO
      valid_mopex_ids <- intersect(sites, mopex_sites)
      if (length(valid_mopex_ids) == 0) {
        shinyalert("Error", "No MOPEX sites in current selection", type = "error")
        mopex_data(data.frame(Message = "No MOPEX sites in current selection"))
        return()
      }

      withProgress(message = "Fetching MOPEX data...", value = 0, {
        incProgress(0.3)
        if (input$mopex_data == "mopex") {
          mopex_data <- dplyr::tbl(conn, "mopex_daily") %>%
            dplyr::filter(SITENO %in% valid_mopex_ids) %>%
            dplyr::filter(DATE >= '1948-01-01' & DATE <= '1979-12-31') %>%
            dplyr::collect()
          shinyalert("Confirmation", "MOPEX only data retrieved (1948-1979).", type = "success")
          mopex_data(mopex_data)
        } else if (input$mopex_data == "combined") {
          mopex_data <- dplyr::tbl(conn, "mopex_daily") %>%
            dplyr::filter(SITENO %in% valid_mopex_ids) %>%
            dplyr::filter(DATE >= '1948-01-01' & DATE <= '1979-12-31') %>%
            dplyr::collect()
          incProgress(0.3)
          mach_data <- dplyr::tbl(conn, "mach_daily") %>%
            dplyr::filter(SITENO %in% valid_mopex_ids) %>%
            dplyr::filter(DATE >= '1980-01-01' & DATE <= '2023-12-31') %>%
            dplyr::select(SITENO, DATE, OBSQ, PRCP, TMIN, TMAX) %>%
            dplyr::collect()
          combined_data <- dplyr::bind_rows(mopex_data, mach_data)
          shinyalert("Confirmation", "MOPEX & MACH data retrieved. MACH data appended to MOPEX (1948-2023).", type = "success")
          mopex_data(combined_data)
        }
      })
    }, ignoreInit = TRUE)

    output$mopex_count <- renderDT({
      req(discharge_mopex())
      data <- discharge_mopex()
      if (nrow(data) == 0) {
        DT::datatable(
          data.frame(Message = "No MOPEX sites with discharge data"),
          options = list(pageLength = 10, dom = "t", scrollX = TRUE),
          class = "display responsive nowrap",
          rownames = FALSE
        )
      } else {
        mopex_sites_count <- length(unique(data$SITENO))
        output$mopex_data_heading <- renderText({
          sprintf("Stream Discharge Record for %d sites", mopex_sites_count)
        })
        DT::datatable(
          data,
          options = list(
            pageLength = 10,
            lengthMenu = c(5, 10, 20, 50),
            dom = "Blfrtip",
            paging = TRUE,
            server = FALSE,
            scrollX = TRUE,
            columnDefs = list(list(width = 'auto', targets = "_all"))
          ),
          class = "display responsive nowrap",
          rownames = FALSE
        )
      }
    })

    output$download_mopex <- downloadHandler(
      filename = function() { paste0("MOPEX_", Sys.Date(), ".csv") },
      content = function(file) {
        data <- mopex_data()
        if ("Message" %in% colnames(data)) {
          return()
        }
        withProgress(message = "Creating CSV file...", value = 0, {
          incProgress(0.8)
          readr::write_csv(data, file)
          incProgress(1)
        })
      },
      contentType = "text/csv"
    )

    output$download_separate_mopex <- downloadHandler(
      filename = function() { paste0("MOPEX_", Sys.Date(), ".zip") },
      content = function(file) {
        data <- mopex_data()
        if ("Message" %in% colnames(data)) {
          return()
        }
        sites <- unique(data$SITENO)
        temp_download_dir <- file.path(tempdir(), paste0("download_", format(Sys.time(), "%Y%m%d_%H%M%S")))
        dir.create(temp_download_dir, showWarnings = FALSE, recursive = TRUE)
        on.exit(unlink(temp_download_dir, recursive = TRUE, force = TRUE), add = TRUE)

        withProgress(message = "Creating ZIP file...", value = 0, {
          csv_files <- character(length(sites))
          for (i in seq_along(sites)) {
            site <- sites[i]
            safe_site <- gsub("[^[:alnum:]_-]", "_", site)
            csv_file <- file.path(temp_download_dir, paste0("MOPEX_", safe_site, ".csv"))
            csv_files[i] <- csv_file
            data_subset <- data %>% dplyr::filter(SITENO == site)
            if (nrow(data_subset) > 0) {
              readr::write_csv(data_subset, csv_file)
            }
            incProgress(1 / length(sites), detail = paste("Processed site", site))
          }
          valid_csv_files <- csv_files[file.exists(csv_files)]
          if (length(valid_csv_files) > 0) {
            zip::zip(zipfile = file, files = basename(valid_csv_files), root = temp_download_dir)
          }
        })
      },
      contentType = "application/zip"
    )
  })
}