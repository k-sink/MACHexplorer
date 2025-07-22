##########################
### SITE ATTRIBUTES UI ###
##########################
attributes_data_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 4,
        card(style = "overflow: visible; height: auto;",
          card_header("Select Attribute Type"), 
          card_body(style = "overflow: visible; height: auto;",
          radioButtons(
            inputId = ns("att_data_type"),
            label = NULL,
            choices = c(
              "Single Value per Site" = "single",
              "Monthly Value per Site" = "monthly",
              "Annual Value per Site" = "annual"
            ),
            selected = "single",
            inline = FALSE
          ),
          br(),
          conditionalPanel(
            condition = paste0("input['", ns("att_data_type"), "'] == 'single'"),
            h6(strong("Select Overall Site Attribute(s)")),
            selectizeInput(
              inputId = ns("site_att"),
              label = "Catchment",
              choices = NULL,
              multiple = TRUE,
              options = list(placeholder = "Select one or more")
            ),
            selectizeInput(
              inputId = ns("overall_climate_att"),
              label = "Climate",
              choices = NULL,
              multiple = TRUE,
              options = list(placeholder = "Select one or more")
            ),
            selectizeInput(
              inputId = ns("hydro_att"),
              label = "Hydrology",
              choices = NULL,
              multiple = TRUE,
              options = list(placeholder = "Select one or more")
            ),
            selectizeInput(
              inputId = ns("soil_att"),
              label = "Soil",
              choices = NULL,
              multiple = TRUE,
              options = list(placeholder = "Select one or more")
            ),
            selectizeInput(
              inputId = ns("geology_att"),
              label = "Geology",
              choices = NULL,
              multiple = TRUE,
              options = list(placeholder = "Select one or more")
            ),
            selectizeInput(
              inputId = ns("regional_att"),
              label = "Regional",
              choices = NULL,
              multiple = TRUE,
              options = list(placeholder = "Select one or more")
            ),
            selectizeInput(
              inputId = ns("anthro_att"),
              label = "Anthropogenic",
              choices = NULL,
              multiple = TRUE,
              options = list(placeholder = "Select one or more")
            )
          ),
          conditionalPanel(
            condition = paste0("input['", ns("att_data_type"), "'] == 'monthly'"),
            h6(strong("Select Monthly Site Attribute(s)")),
            selectizeInput(
              inputId = ns("monthly_climate_att"),
              label = "Monthly Climate",
              choices = NULL,
              multiple = TRUE,
              options = list(placeholder = "Select one or more")
            )
          ),
          conditionalPanel(
            condition = paste0("input['", ns("att_data_type"), "'] == 'annual'"),
            h6(strong("Select Annual Site Attribute(s)")),
            selectizeInput(
              inputId = ns("annual_climate_att"),
              label = "Annual Climate",
              choices = NULL,
              multiple = TRUE,
              options = list(placeholder = "Select one or more")
            )
          ),

          actionButton(inputId = ns("get_attributes"), label = "Retrieve Attributes", 
                       class = "btn-dark", style = "width: 75%; margin-top: 5px;"), 
          
          actionButton(inputId = ns("clear_all"), label = "Clear all Filters", icon("refresh"), 
                       class = "btn-dark", style = "width: 75%; margin-top: 10px; ")
        )),
        
       card(
        card_header("Download Attributes"), 
         card_body(
          conditionalPanel(
            condition = paste0("input['", ns("att_data_type"), "'] == 'single'"),
            downloadButton(outputId = ns("download_single_att"), label = "Export Overall Data", 
                           class = "btn-dark", style = "width: 75%; margin-top: 5px;")
          ),
          conditionalPanel(
            condition = paste0("input['", ns("att_data_type"), "'] == 'monthly'"),
            downloadButton(outputId = ns("download_monthly_att"), label = "Export Monthly Data", 
                            class = "btn-dark", style = "width: 75%; margin-top: 5px;")
          ),
          conditionalPanel(
            condition = paste0("input['", ns("att_data_type"), "'] == 'annual'"),
            downloadButton(outputId = ns("download_annual_att"), label = "Export Annual Data", 
                            class = "btn-dark", style = "width: 75%; margin-top: 5px;")
          )
        )
      )),
      
      column(width = 8,
        card(
          card_header(textOutput(ns("att_table_heading"))),
          card_body(style = "overflow-y: auto;", 
             conditionalPanel(
              condition = paste0("input['", ns("att_data_type"), "'] == 'single'"),
              withSpinner(DTOutput(outputId = ns("single_attributes")), type = 5)
            ),
            conditionalPanel(
              condition = paste0("input['", ns("att_data_type"), "'] == 'monthly'"),
              withSpinner(DTOutput(outputId = ns("monthly_attributes")), type = 5)
            ),
            conditionalPanel(
              condition = paste0("input['", ns("att_data_type"), "'] == 'annual'"),
              withSpinner(DTOutput(outputId = ns("annual_attributes")), type = 5)
            )
          )
        )
      )
    )
  )
}

##############################
### ATTRIBUTES DATA SERVER ###
##############################
attributes_data_server <- function(id, shared_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      req(shared_data$duckdb_connection)
      conn <- shared_data$duckdb_connection

      site_cols <- DBI::dbGetQuery(conn, "SELECT * FROM site_info LIMIT 1") %>% colnames() %>% setdiff(c("SITENO", "station_name"))
      updateSelectizeInput(session, "site_att", choices = site_cols)
      climate_cols <- DBI::dbGetQuery(conn, "SELECT * FROM overall_climate LIMIT 1") %>% colnames() %>% setdiff("SITENO")
      updateSelectizeInput(session, "overall_climate_att", choices = climate_cols)
      hydro_cols <- DBI::dbGetQuery(conn, "SELECT * FROM hydrology LIMIT 1") %>% colnames() %>% setdiff("SITENO")
      updateSelectizeInput(session, "hydro_att", choices = hydro_cols)
      soil_cols <- DBI::dbGetQuery(conn, "SELECT * FROM soil LIMIT 1") %>% colnames() %>% setdiff("SITENO")
      updateSelectizeInput(session, "soil_att", choices = soil_cols)
      geology_cols <- DBI::dbGetQuery(conn, "SELECT * FROM geology LIMIT 1") %>% colnames() %>% setdiff("SITENO")
      updateSelectizeInput(session, "geology_att", choices = geology_cols)
      regional_cols <- DBI::dbGetQuery(conn, "SELECT * FROM regional LIMIT 1") %>% colnames() %>% setdiff("SITENO")
      updateSelectizeInput(session, "regional_att", choices = regional_cols)
      anthro_cols <- DBI::dbGetQuery(conn, "SELECT * FROM anthropogenic LIMIT 1") %>% colnames() %>% setdiff("SITENO")
      updateSelectizeInput(session, "anthro_att", choices = anthro_cols)
      monthly_cols <- DBI::dbGetQuery(conn, "SELECT * FROM monthly_climate LIMIT 1") %>% colnames() %>% setdiff(c("SITENO", "MNTH"))
      updateSelectizeInput(session, "monthly_climate_att", choices = monthly_cols)
      annual_cols <- DBI::dbGetQuery(conn, "SELECT * FROM annual_climate LIMIT 1") %>% colnames() %>% setdiff(c("SITENO", "YR"))
      updateSelectizeInput(session, "annual_climate_att", choices = annual_cols)
    })

    observeEvent(input$clear_all, {
      updateSelectizeInput(session, "site_att", selected = character(0))
      updateSelectizeInput(session, "overall_climate_att", selected = character(0))
      updateSelectizeInput(session, "hydro_att", selected = character(0))
      updateSelectizeInput(session, "soil_att", selected = character(0))
      updateSelectizeInput(session, "geology_att", selected = character(0))
      updateSelectizeInput(session, "regional_att", selected = character(0))
      updateSelectizeInput(session, "anthro_att", selected = character(0))
      updateSelectizeInput(session, "monthly_climate_att", selected = character(0))
      updateSelectizeInput(session, "annual_climate_att", selected = character(0))
    })

    filtered_sites <- reactive({
      req(shared_data$selected_sites)
      shared_data$selected_sites
    })

    # separate reactives for each attribute type
    single_att_table <- reactiveVal()
    monthly_att_table <- reactiveVal()
    annual_att_table <- reactiveVal()

    observeEvent(input$get_attributes, {
      req(shared_data$duckdb_connection)
      conn <- shared_data$duckdb_connection
      gauge_numbers <- filtered_sites()
      if (length(gauge_numbers) == 0) {
        shinyalert::shinyalert(
          title = "Error", text = "No sites selected", type = "error")
        
        single_att_table(data.frame(Message = "No sites selected"))
        monthly_att_table(data.frame(Message = "No sites selected"))
        annual_att_table(data.frame(Message = "No sites selected"))
        return()
      }

      if (input$att_data_type == "single") {
        att_data <- data.frame(SITENO = gauge_numbers)
        if (length(c(input$site_att, input$overall_climate_att, input$hydro_att, input$soil_att, 
                    input$geology_att, input$regional_att, input$anthro_att)) == 0) {
          single_att_table(data.frame(Message = "No attributes selected"))
          return()
        }
        if (length(input$site_att) > 0) {
          site_att_data <- dplyr::tbl(conn, "site_info") %>%
            dplyr::select(SITENO, dplyr::all_of(input$site_att)) %>%
            dplyr::filter(SITENO %in% gauge_numbers) %>%
            dplyr::collect()
          att_data <- dplyr::left_join(att_data, site_att_data, by = "SITENO")
        }
        if (length(input$overall_climate_att) > 0) {
          climate_att_data <- dplyr::tbl(conn, "overall_climate") %>%
            dplyr::select(SITENO, dplyr::all_of(input$overall_climate_att)) %>%
            dplyr::filter(SITENO %in% gauge_numbers) %>%
            dplyr::collect()
          att_data <- dplyr::left_join(att_data, climate_att_data, by = "SITENO")
        }
        if (length(input$hydro_att) > 0) {
          hydro_att_data <- dplyr::tbl(conn, "hydrology") %>%
            dplyr::select(SITENO, dplyr::all_of(input$hydro_att)) %>%
            dplyr::filter(SITENO %in% gauge_numbers) %>%
            dplyr::collect()
          att_data <- dplyr::left_join(att_data, hydro_att_data, by = "SITENO")
        }
        if (length(input$soil_att) > 0) {
          soil_att_data <- dplyr::tbl(conn, "soil") %>%
            dplyr::select(SITENO, dplyr::all_of(input$soil_att)) %>%
            dplyr::filter(SITENO %in% gauge_numbers) %>%
            dplyr::collect()
          att_data <- dplyr::left_join(att_data, soil_att_data, by = "SITENO")
        }
        if (length(input$geology_att) > 0) {
          geology_att_data <- dplyr::tbl(conn, "geology") %>%
            dplyr::select(SITENO, dplyr::all_of(input$geology_att)) %>%
            dplyr::filter(SITENO %in% gauge_numbers) %>%
            dplyr::collect()
          att_data <- dplyr::left_join(att_data, geology_att_data, by = "SITENO")
        }
        if (length(input$regional_att) > 0) {
          regional_att_data <- dplyr::tbl(conn, "regional") %>%
            dplyr::select(SITENO, dplyr::all_of(input$regional_att)) %>%
            dplyr::filter(SITENO %in% gauge_numbers) %>%
            dplyr::collect()
          att_data <- dplyr::left_join(att_data, regional_att_data, by = "SITENO")
        }
        if (length(input$anthro_att) > 0) {
          anthro_att_data <- dplyr::tbl(conn, "anthropogenic") %>%
            dplyr::select(SITENO, dplyr::all_of(input$anthro_att)) %>%
            dplyr::filter(SITENO %in% gauge_numbers) %>%
            dplyr::collect()
          att_data <- dplyr::left_join(att_data, anthro_att_data, by = "SITENO")
        }
        single_att_table(att_data)
      } else if (input$att_data_type == "monthly") {
        att_data <- data.frame(SITENO = gauge_numbers)
        if (length(input$monthly_climate_att) == 0) {
          monthly_att_table(data.frame(Message = "No attributes selected"))
          return()
        }
        if (length(input$monthly_climate_att) > 0) {
          monthly_att_data <- dplyr::tbl(conn, "monthly_climate") %>%
            dplyr::select(SITENO, MNTH, dplyr::all_of(input$monthly_climate_att)) %>%
            dplyr::filter(SITENO %in% gauge_numbers) %>%
            dplyr::collect()
          att_data <- dplyr::left_join(att_data, monthly_att_data, by = "SITENO")
        }
        monthly_att_table(att_data)
      } else if (input$att_data_type == "annual") {
        att_data <- data.frame(SITENO = gauge_numbers)
        if (length(input$annual_climate_att) == 0) {
          annual_att_table(data.frame(Message = "No attributes selected"))
          return()
        }
        if (length(input$annual_climate_att) > 0) {
          annual_att_data <- dplyr::tbl(conn, "annual_climate") %>%
            dplyr::select(SITENO, YR, dplyr::all_of(input$annual_climate_att)) %>%
            dplyr::filter(SITENO %in% gauge_numbers) %>%
            dplyr::collect()
          att_data <- dplyr::left_join(att_data, annual_att_data, by = "SITENO")
        }
        annual_att_table(att_data)
      }
    }, ignoreInit = TRUE)

    output$att_table_heading <- renderText({
      req(input$att_data_type, filtered_sites())
      site_count <- length(filtered_sites())
      switch(input$att_data_type,
             "single" = sprintf("Overall Attributes for %d sites", site_count),
             "monthly" = sprintf("Monthly Attributes for %d sites", site_count),
             "annual" = sprintf("Annual Attributes for %d sites", site_count))
    })

    output$single_attributes <- renderDT({
      req(input$att_data_type == "single", input$get_attributes)
      data <- single_att_table()
      if ("Message" %in% colnames(data)) {
        DT::datatable(
          data,
          options = list(pageLength = 10, dom = "t", scrollX = TRUE, columnDefs = list(list(width = 'auto', targets = "_all"))),
          class = "display responsive nowrap"
        )
      } else {
        format_cols <- setdiff(colnames(data), "SITENO")
        for (col in format_cols) {
          if (col %in% c("COMID", "huc_cd")) {
            data[[col]] <- format(data[[col]], nsmall = 0, big.mark = "")
          } else if (col == "dam_count") {
            data[[col]] <- as.integer(data[[col]])
          } else if (col %in% c("GAGESII_class", "ADR_citation", "Screening_comments")) {
            data[[col]] <- as.character(data[[col]])
          } else if (col %in% c("dataset", "site_NWIS")) {
            data[[col]] <- as.character(data[[col]])
          } else if (is.numeric(data[[col]])) {
            data[[col]] <- format(round(data[[col]], 2), nsmall = 2, big.mark = "")
          }
        }
        DT::datatable(
          data,
          options = list(
            pageLength = 10,
            lengthMenu = c(5, 10, 20, 50),
            dom = "Blfrtip",
            paging = TRUE,
            server = TRUE,
            scrollX = TRUE,
            columnDefs = list(list(width = 'auto', targets = "_all"))
          ),
          class = "display responsive nowrap",
          rownames = FALSE
        )
      }
    })

    output$monthly_attributes <- renderDT({
      req(input$att_data_type == "monthly", input$get_attributes)
      data <- monthly_att_table()
      if ("Message" %in% colnames(data)) {
        DT::datatable(
          data,
          options = list(pageLength = 10, dom = "Blfrtip", scrollX = TRUE, columnDefs = list(list(width = 'auto', targets = "_all"))),
          class = "display responsive nowrap"
        )
      } else {
        format_cols <- setdiff(colnames(data), c("SITENO", "MNTH"))
        for (col in format_cols) {
          if (is.numeric(data[[col]])) {
            data[[col]] <- format(round(data[[col]], 2), nsmall = 2, big.mark = "")
          }
        }
        DT::datatable(
          data,
          options = list(
            pageLength = 10,
            lengthMenu = c(5, 10, 20, 50),
            dom = "Blfrtip",
            paging = TRUE,
            server = TRUE,
            scrollX = TRUE,
            columnDefs = list(list(width = 'auto', targets = "_all"))
          ),
          class = "display responsive nowrap",
          rownames = FALSE
        )
      }
    })

    output$annual_attributes <- renderDT({
      req(input$att_data_type == "annual", input$get_attributes)
      data <- annual_att_table()
      if ("Message" %in% colnames(data)) {
        DT::datatable(
          data,
          options = list(pageLength = 10, dom = "t", scrollX = TRUE, columnDefs = list(list(width = 'auto', targets = "_all"))),
          class = "display responsive nowrap"
        )
      } else {
        format_cols <- setdiff(colnames(data), c("SITENO", "YR"))
        for (col in format_cols) {
          if (is.numeric(data[[col]])) {
            data[[col]] <- format(round(data[[col]], 2), nsmall = 2, big.mark = "")
          }
        }
        DT::datatable(
          data,
          options = list(
            pageLength = 10,
            lengthMenu = c(5, 10, 20, 50),
            dom = "Blfrtip",
            paging = TRUE,
            server = TRUE,
            scrollX = TRUE,
            columnDefs = list(list(width = 'auto', targets = "_all"))
          ),
          class = "display responsive nowrap",
          rownames = FALSE
        )
      }
    })

    output$download_single_att <- downloadHandler(
      filename = function() { paste0("MACH_att_", Sys.Date(), ".csv") },
      content = function(file) {
        req(input$att_data_type == "single", single_att_table())
        data <- single_att_table()
        if ("Message" %in% colnames(data)) {
          return()
        }
        format_cols <- setdiff(colnames(data), "SITENO")
        for (col in format_cols) {
          if (col %in% c("COMID", "huc_cd")) {
            data[[col]] <- format(data[[col]], nsmall = 0, big.mark = "")
          } else if (col == "dam_count") {
            data[[col]] <- as.integer(data[[col]])
          } else if (col %in% c("GAGESII_class", "ADR_citation", "Screening_comments")) {
            data[[col]] <- as.character(data[[col]])
          } else if (col %in% c("dataset", "site_NWIS")) {
            data[[col]] <- as.character(data[[col]])
          } else if (is.numeric(data[[col]])) {
            data[[col]] <- format(round(data[[col]], 2), nsmall = 2, big.mark = "")
          }
        }
        withProgress(message = "Creating CSV file", value = 0, {
          incProgress(0.8)
          readr::write_csv(data, file)
          incProgress(1)
        })
      }
    )

    output$download_monthly_att <- downloadHandler(
      filename = function() { paste0("MACH_monthly_att_", Sys.Date(), ".csv") },
      content = function(file) {
        req(input$att_data_type == "monthly", monthly_att_table())
        data <- monthly_att_table()
        if ("Message" %in% colnames(data)) {
          return()
        }
        format_cols <- setdiff(colnames(data), c("SITENO", "MNTH"))
        for (col in format_cols) {
          if (is.numeric(data[[col]])) {
            data[[col]] <- format(round(data[[col]], 2), nsmall = 2, big.mark = "")
          }
        }
        withProgress(message = "Creating CSV file", value = 0, {
          incProgress(0.8)
          readr::write_csv(data, file)
          incProgress(1)
        })
      }
    )

    output$download_annual_att <- downloadHandler(
      filename = function() { paste0("MACH_annual_att_", Sys.Date(), ".csv") },
      content = function(file) {
        req(input$att_data_type == "annual", annual_att_table())
        data <- annual_att_table()
        if ("Message" %in% colnames(data)) {
          return()
        }
        format_cols <- setdiff(colnames(data), c("SITENO", "YR"))
        for (col in format_cols) {
          if (is.numeric(data[[col]])) {
            data[[col]] <- format(round(data[[col]], 2), nsmall = 2, big.mark = "")
          }
        }
        withProgress(message = "Creating CSV file", value = 0, {
          incProgress(0.8)
          readr::write_csv(data, file)
          incProgress(1)
        })
      }
    )
  })
}