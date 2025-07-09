### UI ###
landCoverDataUI <- function(id) {
  ns <- NS(id)
  tabPanel(
    title = "Land Cover",
    br(),
    fluidRow(
      column(
        5,
        wellPanel(
          style = "overflow: visible; height: auto; width: 100%;",
          h6(strong("Select Calendar Year(s)")),
          selectizeInput(
            inputId = ns("lc_year"),
            label = NULL,
            multiple = TRUE,
            choices = NULL, # Populated dynamically
            options = list(placeholder = "Select one or more")
          ),
          br(),
          h6(strong("Select Land Cover Class(es)")),
          selectizeInput(
            inputId = ns("lc_class"),
            label = NULL,
            multiple = TRUE,
            choices = NULL, # Populated dynamically
            options = list(placeholder = "Select one or more")
          ),
          br(),
          actionButton(inputId = ns("get_landcover"), label = "Retrieve Attributes")
        ),
        br(),
        wellPanel(
          h6(strong("Download Attributes")),
          br(),
          downloadButton(outputId = ns("download_lc_att"), label = "Export LC Data")
        )
      ),
      column(
        7,
        wellPanel(
          h6(strong("Selected Land Cover Data")),
          br(),
          withSpinner(DTOutput(outputId = ns("lc_attributes")), type = 5)
        )
      )
    )
  )
}

### SERVER ###
landCoverDataServer <- function(id, shared_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

        
data_dir = "data"
lc_dir = file.path(data_dir, "land_cover")


    # Load QS file metadata
    lc_files <- list.files(lc_dir, pattern = "^ANLCD\\d{4}\\.qs$", full.names = TRUE)
    lc_years_static <- gsub(".*ANLCD(\\d{4})\\.qs$", "\\1", basename(lc_files))
    names(lc_files) <- lc_years_static

    # Reactive for years
    lc_years <- reactive({
      sort(lc_years_static)
    })

    # Define land cover class names
    lc_class_names <- c(
      "Water" = "11",
      "Perennial Ice/Snow" = "12",
      "Developed, Open Space" = "21",
      "Developed, Low Intensity" = "22",
      "Developed, Medium Intensity" = "23",
      "Developed, High Intensity" = "24",
      "Barren Land" = "31",
      "Deciduous Forest" = "41",
      "Evergreen Forest" = "42",
      "Mixed Forest" = "43",
      "Shrub" = "52",
      "Grassland/Herbaceous" = "71",
      "Pasture/Hay" = "81",
      "Cultivated Crops" = "82",
      "Woody Wetlands" = "90",
      "Herbaceous Wetlands" = "95"
    )

# Update selectizeInput
    observe({
      updateSelectizeInput(session, "site_no", choices = isolate(shared_data$site$SITENO))
      updateSelectizeInput(session, "lc_year", choices = lc_years())
      updateSelectizeInput(session, "lc_class", choices = lc_class_names)
    })

    # Get filtered site numbers
    filtered_sites <- reactive({
      req(shared_data$sites_confirmed, "Please confirm sites in the Site Selection tab.")
      req(shared_data$site$SITENO, "No sites selected.")
      if (is.null(input$site_no) || length(input$site_no) == 0) {
        shared_data$site$SITENO
      } else {
        input$site_no
      }
    })

    # Process land cover data
    lc_table <- eventReactive(input$get_landcover, {
      gauge_numbers <- filtered_sites()
      selected_years <- input$lc_year
      selected_classes <- input$lc_class

      if (length(gauge_numbers) == 0) {
        shinyalert::shinyalert(
          title = "Error",
          text = "No sites selected",
          type = "error"
        )
        return(data.frame(Message = "No sites selected"))
      }
      if (length(selected_years) == 0 || length(selected_classes) == 0) {
        shinyalert::shinyalert(
          title = "Error",
          text = "No years or classes selected",
          type = "error"
        )
        return(data.frame(Message = "No data available"))
      }
      if (length(lc_files) == 0) {
        return(data.frame(Message = "No land cover files found"))
      }

      selected_files <- lc_files[names(lc_files) %in% selected_years]
      if (length(selected_files) == 0) {
        return(data.frame(Message = "No data available"))
      }

      lc_data <- NULL
      for (file in selected_files) {
        df <- qs::qread(file)
        df <- dplyr::filter(df, SITENO %in% gauge_numbers)
        df <- dplyr::select(df, SITENO, YR, dplyr::all_of(selected_classes))
        lc_data <- dplyr::bind_rows(lc_data, df)
      }

      if (is.null(lc_data) || nrow(lc_data) == 0) {
        return(data.frame(Message = "No data available"))
      }

      lc_data
    })

    output$lc_attributes <- renderDT({
      data <- lc_table()
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
            server = TRUE,
            select = list(mode = "multiple", selected = isolate(which(data$SITENO %in% shared_data$site$SITENO)))
          ),
          class = "display responsive nowrap",
          selection = "none"
        ) %>% DT::formatRound(columns = setdiff(colnames(data), c("SITENO", "YR")), digits = 2)
      }
    })

    output$download_lc_att <- downloadHandler(
      filename = function() {
        paste0("LC_data_", Sys.Date(), ".csv")
      },
      content = function(file) {
        withProgress(message = "Creating CSV file", value = 0, {
          for (i in 1:5) {
            Sys.sleep(0.2)
            incProgress(1 / 5)
          }
          write.csv(lc_table(), file, row.names = FALSE)
        })
      }
    )
  })
}