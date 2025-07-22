### UI ###
landcover_data_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 4,
        card(
          card_header("Filter Land Cover Data"),
          card_body(
            h6(strong("Select Calendar Year(s)")),
            selectizeInput(
              inputId = ns("lc_year"),
              label = NULL,
              choices = 1985:2023,
              multiple = TRUE,
              options = list(placeholder = "Select one or more")
            ),
            br(),
            h6(strong("Select Land Cover Class(es)")),
            selectizeInput(
              inputId = ns("lc_class"),
              label = NULL,
              choices = c(
                "Water" = "Water",
                "Perennial Ice/Snow" = "Ice_snow",
                "Developed, Open Space" = "Open_space",
                "Developed, Low Intensity" = "Low_intensity",
                "Developed, Medium Intensity" = "Med_intensity",
                "Developed, High Intensity" = "High_intensity",
                "Barren Land" = "Barren",
                "Deciduous Forest" = "Deciduous",
                "Evergreen Forest" = "Evergreen",
                "Mixed Forest" = "Mixed_forest",
                "Shrub" = "Shrub",
                "Grassland/Herbaceous" = "Grassland",
                "Pasture/Hay" = "Pasture",
                "Cultivated Crops" = "Crops",
                "Woody Wetlands" = "Woody_wet",
                "Herbaceous Wetlands" = "Herbaceous_wet"
              ),
              multiple = TRUE,
              options = list(placeholder = "Select one or more")
            ),
            br(),
            actionButton(inputId = ns("get_landcover"), "Retrieve Land Cover Data",
                         class = "btn-dark", style = "width: 75%; margin-top: 5px;"),
            actionButton(inputId = ns("clear_filters"), "Reset all Filters", icon("refresh"),
                         class = "btn-dark", style = "width: 75%; margin-top: 10px;")
          )
        ),
        card(
          card_header("Download Land Cover Data"),
          card_body(
            downloadButton(outputId = ns("download_lc_att"), "Export as CSV",
                           class = "btn-dark", style = "width: 75%; margin-top: 5px;")
          )
        )
      ),
      column(width = 8,
        card(
          card_header(textOutput(ns("lc_table_heading"))),
          card_body(
            shinycssloaders::withSpinner(DTOutput(outputId = ns("lc_attributes")), type = 5)
          )
        )
      )
    )
  )
}

### SERVER ###
landcover_data_server <- function(id, shared_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    # Reactive for filtered sites from site_selection
    filtered_sites <- reactive({
      req(shared_data$selected_sites)
      shared_data$selected_sites
    })

    # Reset filters
    observeEvent(input$clear_filters, {
      updateSelectizeInput(session, "lc_year", selected = character(0))
      updateSelectizeInput(session, "lc_class", selected = character(0))
      showNotification("All filters have been reset.", type = "message")
    })

    lc_data_table <- reactiveVal()
    
    # Reactive for land cover data
    observeEvent(input$get_landcover, {
      req(shared_data$duckdb_connection)
      conn <- shared_data$duckdb_connection
      gauge_numbers <- filtered_sites()
      if (length(gauge_numbers) == 0) {
        shinyalert::shinyalert(
          title = "Error", text = "Please select at least one site in the Site Selection tab.", type = "error")
        
        lc_data_table(data.frame(Message = "No sites selected"))
        return()
      }
      

      selected_years <- input$lc_year
      if (is.null(selected_years) || length(selected_years) == 0) {
        shinyalert("Please select at least one year.", type = "error")
        return()
      }

      selected_classes <- input$lc_class
      if (is.null(selected_classes) || length(selected_classes) == 0) {
        shinyalert("Please select at least one land cover class.", type = "error")
        return()
      }

      
        lc_data <- dplyr::tbl(conn, "land_cover") %>% 
          dplyr::filter(SITENO %in% gauge_numbers, YEAR %in% selected_years) %>% 
          dplyr::select(SITENO, YEAR, dplyr::all_of(selected_classes)) %>% 
          dplyr::collect()
        
        lc_data_table(lc_data)
    })
        
    
    # Render land cover table
    output$lc_attributes <- DT::renderDT({
      req(lc_data_table())
      data <- lc_data_table()
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
            lengthMenu = c(5, 10, 20, 50),
            scrollX = TRUE,
            dom = "Blfrtip",
            paging = TRUE,
            server = FALSE
          ),
          class = "display responsive nowrap",
          rownames = FALSE
        ) %>% DT::formatRound(columns = setdiff(colnames(data), c("SITENO", "YEAR")), digits = 2)
      }
    })

    # Update heading (remove year count)
    output$lc_table_heading <- renderText({
      req(filtered_sites())
      site_count <- length(filtered_sites())
      sprintf("Land Cover Data for %d sites", site_count)
    })

    # Download handler
    output$download_lc_att <- downloadHandler(
      filename = function() {
        paste0("MACH_land_cover_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(lc_data_table())
        data <- lc_data_table()
        if ("Message" %in% colnames(data)) {
          return()
        }
        withProgress(message = "Creating CSV file...", value = 0, {
          incProgress(0.8)
          readr::write_csv(data, file)
          incProgress(1)
        })
      }
    )
  })
}