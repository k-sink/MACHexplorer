###############################
### SITE ATTRIBUTES MODULE ###
################################ 

attributesUI <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    fluidRow(
      column(
        5,
        wellPanel(
          style = "overflow: visible; height: auto; width: 100%;",
          h6(strong("Select Attribute Type")),
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
          br(),
          actionButton(inputId = ns("get_attributes"), label = "Retrieve Attributes")
        ),
        br(),
        wellPanel(
          h6(strong("Download Attributes")),
          br(),
          conditionalPanel(
            condition = paste0("input['", ns("att_data_type"), "'] == 'single'"),
            downloadButton(outputId = ns("download_single_att"), label = "Export Overall Data")
          ),
          conditionalPanel(
            condition = paste0("input['", ns("att_data_type"), "'] == 'monthly'"),
            downloadButton(outputId = ns("download_monthly_att"), label = "Export Monthly Data")
          ),
          conditionalPanel(
            condition = paste0("input['", ns("att_data_type"), "'] == 'annual'"),
            downloadButton(outputId = ns("download_annual_att"), label = "Export Annual Data")
          )
        )
      ),
      column(
        7,
        wellPanel(
          h6(strong("Selected Attributes")),
          br(),
          withSpinner(DTOutput(outputId = ns("catch_attributes")), type = 5)
        )
      )
    )
  )
}

# Attributes Server Module
attributesServer <- function(id, shared_data) {
  moduleServer(id, function(input, output, session) {
    ns = session$ns
    
data_dir = "data"
attributes_dir = file.path(data_dir, "attributes")

site_attributes = qs::qread(file.path(attributes_dir, "site_info.qs")) 
                  
site = site_attributes
site_names = colnames(site)[-(1:2)]

# Load data 
annual_climate = qs::qread(file.path(attributes_dir, "annual_climate.qs")) 
annual_climate = annual_climate[, colSums(is.na(annual_climate) | annual_climate == "") != nrow(annual_climate)]
annual_clim_names = colnames(annual_climate)[-(1:2)]

anthropogenic = qs::qread(file.path(attributes_dir, "anthropogenic.qs")) 
anthropogenic = anthropogenic[, colSums(is.na(anthropogenic) | anthropogenic == "") != nrow(anthropogenic)]
anthropogenic_names = colnames(anthropogenic)[-1]

geology = qs::qread(file.path(attributes_dir, "geology.qs")) 
geology = geology[, colSums(is.na(geology) | geology == "") != nrow(geology)]
geology_names = colnames(geology)[-1]

hydrology = qs::qread(file.path(attributes_dir, "hydrology.qs")) 
hydrology = hydrology[, colSums(is.na(hydrology) | hydrology == "") != nrow(hydrology)]
hydro_names = colnames(hydrology)[-1]

monthly_climate = qs::qread(file.path(attributes_dir, "monthly_climate.qs"))
monthly_climate = monthly_climate[, colSums(is.na(monthly_climate) | monthly_climate == "") != nrow(monthly_climate)]
monthly_clim_names = colnames(monthly_climate)[-(1:2)]

overall_climate = qs::qread(file.path(attributes_dir, "overall_climate.qs"))
overall_climate = overall_climate[, colSums(is.na(overall_climate) | overall_climate == "") != nrow(overall_climate)]
overall_clim_names = colnames(overall_climate)[-1]

regional = qs::qread(file.path(attributes_dir, "regional.qs")) 
regional = regional[, colSums(is.na(regional) | regional == "") != nrow(regional)]
regional_names = colnames(regional)[-1]


soil = qs::qread(file.path(attributes_dir, "soil.qs"))
soil = soil[, colSums(is.na(soil) | soil == "") != nrow(soil)]
soil_names = colnames(soil)[-1]
    
 # Update selectizeInput choices
    observe({
      updateSelectizeInput(session, "site_no", choices = isolate(shared_data$site$SITENO))
      updateSelectizeInput(session, "site_att", choices = site_names)
      updateSelectizeInput(session, "overall_climate_att", choices = overall_clim_names)
      updateSelectizeInput(session, "hydro_att", choices = hydro_names)
      updateSelectizeInput(session, "soil_att", choices = soil_names)
      updateSelectizeInput(session, "geology_att", choices = geology_names)
      updateSelectizeInput(session, "regional_att", choices = regional_names)
      updateSelectizeInput(session, "anthro_att", choices = anthropogenic_names)
      updateSelectizeInput(session, "monthly_climate_att", choices = monthly_clim_names)
      updateSelectizeInput(session, "annual_climate_att", choices = annual_clim_names)
    })


 # Get filtered site numbers reactively
    filtered_sites <- reactive({
      req(shared_data$sites_confirmed, "Please confirm sites in the Site Selection tab.")
      req(shared_data$site$SITENO, "No sites selected.")
      if (is.null(input$site_no) || length(input$site_no) == 0) {
        shared_data$site$SITENO
      } else {
        input$site_no
      }
    })

    # Retrieve and process attributes
    att_table <- eventReactive(input$get_attributes, {
      gauge_numbers <- filtered_sites()
      if (length(gauge_numbers) == 0) {
        shinyalert::shinyalert(
          title = "Error",
          text = "No sites selected",
          type = "error"
        )
        return(data.frame(Message = "No sites selected"))
      }

      att_data <- data.frame(SITENO = gauge_numbers)

      # Process attributes based on type
      if (input$att_data_type == "single") {
        if (length(input$site_att) > 0) {
          site_att_data <- site_attributes %>%
            dplyr::select(SITENO, dplyr::all_of(input$site_att)) %>%
            dplyr::filter(SITENO %in% gauge_numbers)
          att_data <- dplyr::left_join(att_data, site_att_data, by = "SITENO")
        }
        if (length(input$overall_climate_att) > 0) {
          climate_att_data <- overall_climate %>%
            dplyr::select(SITENO, dplyr::all_of(input$overall_climate_att)) %>%
            dplyr::filter(SITENO %in% gauge_numbers)
          att_data <- dplyr::left_join(att_data, climate_att_data, by = "SITENO")
        }
        if (length(input$hydro_att) > 0) {
          hydro_att_data <- hydrology %>%
            dplyr::select(SITENO, dplyr::all_of(input$hydro_att)) %>%
            dplyr::filter(SITENO %in% gauge_numbers)
          att_data <- dplyr::left_join(att_data, hydro_att_data, by = "SITENO")
        }
        if (length(input$soil_att) > 0) {
          soil_att_data <- soil %>%
            dplyr::select(SITENO, dplyr::all_of(input$soil_att)) %>%
            dplyr::filter(SITENO %in% gauge_numbers)
          att_data <- dplyr::left_join(att_data, soil_att_data, by = "SITENO")
        }
        if (length(input$geology_att) > 0) {
          geology_att_data <- geology %>%
            dplyr::select(SITENO, dplyr::all_of(input$geology_att)) %>%
            dplyr::filter(SITENO %in% gauge_numbers)
          att_data <- dplyr::left_join(att_data, geology_att_data, by = "SITENO")
        }
        if (length(input$regional_att) > 0) {
          regional_att_data <- regional %>%
            dplyr::select(SITENO, dplyr::all_of(input$regional_att)) %>%
            dplyr::filter(SITENO %in% gauge_numbers)
          att_data <- dplyr::left_join(att_data, regional_att_data, by = "SITENO")
        }
        if (length(input$anthro_att) > 0) {
          anthro_att_data <- anthropogenic %>%
            dplyr::select(SITENO, dplyr::all_of(input$anthro_att)) %>%
            dplyr::filter(SITENO %in% gauge_numbers)
          att_data <- dplyr::left_join(att_data, anthro_att_data, by = "SITENO")
        }
      } else if (input$att_data_type == "monthly") {
        if (length(input$monthly_climate_att) > 0) {
          monthly_att_data <- monthly_climate %>%
            dplyr::select(SITENO, MNTH, dplyr::all_of(input$monthly_climate_att)) %>%
            dplyr::filter(SITENO %in% gauge_numbers)
          att_data <- dplyr::left_join(att_data, monthly_att_data, by = "SITENO")
        }
      } else if (input$att_data_type == "annual") {
        if (length(input$annual_climate_att) > 0) {
          annual_att_data <- annual_climate %>%
            dplyr::select(SITENO, YR, dplyr::all_of(input$annual_climate_att)) %>%
            dplyr::filter(SITENO %in% gauge_numbers)
          att_data <- dplyr::left_join(att_data, annual_att_data, by = "SITENO")
        }
      }

      if (nrow(att_data) == 0 || ncol(att_data) == 1) {
        shinyalert::shinyalert(
          title = "Error",
          text = "No attributes selected",
          type = "error"
        )
        return(data.frame(Message = "No attributes selected"))
      }
      att_data
    })

    output$catch_attributes <- renderDT({
      data <- att_table()
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
        ) %>% DT::formatRound(columns = setdiff(colnames(data), c("SITENO", "MNTH", "YR")), digits = 2)
      }
    })

    output$download_single_att <- downloadHandler(
      filename = function() { paste0("MACH_att_", Sys.Date(), ".csv") },
      content = function(file) {
        withProgress(message = "Creating CSV file", value = 0, {
          for (i in 1:5) {
            Sys.sleep(0.2)
            incProgress(1 / 5)
          }
          write.csv(att_table(), file, row.names = FALSE)
        })
      }
    )

    output$download_monthly_att <- downloadHandler(
      filename = function() { paste0("MACH_monthly_att_", Sys.Date(), ".csv") },
      content = function(file) {
        withProgress(message = "Creating CSV file", value = 0, {
          for (i in 1:5) {
            Sys.sleep(0.2)
            incProgress(1 / 5)
          }
          write.csv(att_table(), file, row.names = FALSE)
        })
      }
    )

    output$download_annual_att <- downloadHandler(
      filename = function() { paste0("MACH_annual_att_", Sys.Date(), ".csv") },
      content = function(file) {
        withProgress(message = "Creating CSV file", value = 0, {
          for (i in 1:5) {
            Sys.sleep(0.2)
            incProgress(1 / 5)
          }
          write.csv(att_table(), file, row.names = FALSE)
        })
      }
    )
  })  
}