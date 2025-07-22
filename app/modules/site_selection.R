###################################
### SITE SELECTION MODULE ###
###################################
site_selection_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(width = 3, 
        card(
          card_header("Filter Sites"),
          card_body(
            selectizeInput(ns("state1"), "State", choices = NULL, multiple = TRUE, 
                           options = list(placeholder = "Select one or more states")),
            checkboxInput(ns("latitude"), HTML("Latitude (°N)"), FALSE),
            conditionalPanel(condition = "input.latitude == true", ns = ns,
              sliderInput(ns("latitude1"), NULL, min = 25, max = 50, value = c(25, 50), ticks = FALSE)),
            checkboxInput(ns("longitude"), HTML("Longitude (°W)"), FALSE),
            conditionalPanel(condition = "input.longitude == true", ns = ns,
              sliderInput(ns("longitude1"), NULL, min = -125, max = -65, value = c(-125, -65), ticks = FALSE)),
            checkboxInput(ns("elevation"), "Mean Elevation (m)", FALSE),
            conditionalPanel(condition = "input.elevation == true", ns = ns,
              sliderInput(ns("elevation1"), NULL, min = 5, max = 3605, value = c(5, 3605), ticks = FALSE)),
            checkboxInput(ns("area"), HTML("Drainage Area (km<sup>2</sup>)"), FALSE),
            conditionalPanel(condition = "input.area == true", ns = ns,
              sliderInput(ns("area1"), NULL, min = 2, max = 26000, value = c(2, 26000), ticks = FALSE)),
            checkboxInput(ns("slope"), "Mean Slope (percent)", FALSE),
            conditionalPanel(condition = "input.slope == true", ns = ns,
              sliderInput(ns("slope1"), NULL, min = 0, max = 70, value = c(0, 70), ticks = FALSE)),
            actionButton(ns("reset"), "Reset Filters", icon = icon("refresh"), class = "btn-dark", style = "width: 75%; margin-top: 10px;")
          )
        )
      ),

      column(width = 3,   
        card(
          card_header("Edit Individual Sites"),
          card_body(
            textInput(ns("add_site_no"), "Manually Add Site", placeholder = "Enter 8-digit SITENO"),
            actionButton(ns("add_site_btn"), "Add Site", icon = icon("plus"), class = "btn-dark", style = "width: 75%; margin-top: 5px;"),
            textInput(ns("remove_site_no"), "Manually Remove Site", placeholder = "Enter 8-digit SITENO"),
            actionButton(ns("remove_site_btn"), "Remove Site", icon = icon("minus"), class = "btn-dark", style = "width: 75%; margin-top: 5px;"),
            actionButton(ns("remove_site_all"), "Remove All Sites", icon = icon("trash"), class = "btn-dark", style = "width: 75%; margin-top: 20px;")
          )
        )
      ),

      column(width = 6,   
        card(
          card_header("USGS Station Locations"),
          card_body(
            withSpinner(leafletOutput(ns("my_leaflet")), type = 5)
          )
        )
      )
    ),
      
    fluidRow(
      column(width = 6,  
        card(
          card_header("Discharge Record"),
          card_body(
            withSpinner(DTOutput(ns("discharge_days")), type = 5)
          )
        )
      ),

      column(width = 6, 
        card(
          card_header("Selected Sites"),
          card_body(
            withSpinner(DTOutput(ns("gauges")), type = 5)
          )
        )
      )
    )
  )
}

################
### SERVER ###
################
site_selection_server <- function(id, shared_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # load the basin shapefile from .qs format
    basins_shp <- reactive({
      tryCatch({
        qs::qread("data/MACH_basins.qs")
      }, error = function(e) {
        showNotification(paste("Error loading discharge_mach.qs:", e$message), type = "error")
        NULL
      })
    })

 # load the discharge count file from .qs format
    discharge_count <- reactive({
      tryCatch({
        qs::qread("data/discharge_mach.qs")
      }, error = function(e) {
        showNotification(paste("Error loading discharge_mach.qs:", e$message), type = "error")
        NULL
      })
    })

    # reactive to fetch site_info table from DuckDB
    site_info <- reactive({
      req(shared_data$database_ready, shared_data$duckdb_connection)
      tryCatch({
        dplyr::tbl(shared_data$duckdb_connection, "site_info") %>%
          dplyr::filter(SITENO %in% shared_data$mach_ids) %>%
          dplyr::select(
            SITENO, station_name, state, dec_lat_va, dec_long_va,
            elev_mean, NHD_drain_area_sqkm, basin_slope
          ) %>%
          collect()
      }, error = function(e) {
        showNotification(paste("Error loading site_info:", e$message), type = "error")
        data.frame()
      })
    })

    # update state choices
    observe({
      req(site_info())
      updateSelectizeInput(session, "state1", choices = sort(unique(site_info()$state)))
    })

    # reactive for filtered sites
    filtered_sites <- reactive({
      req(site_info())
      siteinfo <- site_info()
      if (!is.null(input$state1) && length(input$state1) > 0) {
        siteinfo <- siteinfo %>% dplyr::filter(state %in% input$state1)
      }
      if (!is.null(input$latitude1) && length(input$latitude1) == 2 && input$latitude) {
        siteinfo <- siteinfo %>% dplyr::filter(dec_lat_va >= input$latitude1[1], dec_lat_va <= input$latitude1[2])
      }
      if (!is.null(input$longitude1) && length(input$longitude1) == 2 && input$longitude) {
        siteinfo <- siteinfo %>% dplyr::filter(dec_long_va >= input$longitude1[1], dec_long_va <= input$longitude1[2])
      }
      if (!is.null(input$elevation1) && length(input$elevation1) == 2 && input$elevation) {
        siteinfo <- siteinfo %>% dplyr::filter(elev_mean >= input$elevation1[1], elev_mean <= input$elevation1[2])
      }
      if (!is.null(input$area1) && length(input$area1) == 2 && input$area) {
        siteinfo <- siteinfo %>% dplyr::filter(NHD_drain_area_sqkm >= input$area1[1], NHD_drain_area_sqkm <= input$area1[2])
      }
      if (!is.null(input$slope1) && length(input$slope1) == 2 && input$slope) {
        siteinfo <- siteinfo %>% dplyr::filter(basin_slope >= input$slope1[1], basin_slope <= input$slope1[2])
      }
      siteinfo %>% dplyr::select(
        SITENO,
        NAME = station_name,
        STATE = state,
        LAT = dec_lat_va,
        LONG = dec_long_va,
        ELEV = elev_mean,
        AREA = NHD_drain_area_sqkm,
        SLOPE = basin_slope
      )
    }) %>% debounce(500)

    # initialize selected sites
    selected_sites <- reactiveVal()
    observe({
      req(filtered_sites())
      selected_sites(filtered_sites())
      # Update shared_data$selected_sites
      shared_data$selected_sites <- filtered_sites()$SITENO
    })

    # manual site addition
    observeEvent(input$add_site_btn, {
      req(input$add_site_no, shared_data$mach_ids, site_info())
      new_site_no <- stringr::str_pad(input$add_site_no, 8, pad = "0")
      if (!new_site_no %in% shared_data$mach_ids) {
        showNotification("Invalid site number. Site not found in MACH database.", type = "error")
        updateTextInput(session, "add_site_no", value = "")
        return()
      }
      new_site <- site_info() %>%
        dplyr::filter(SITENO == new_site_no) %>%
        dplyr::select(
          SITENO,
          NAME = station_name,
          STATE = state,
          LAT = dec_lat_va,
          LONG = dec_long_va,
          ELEV = elev_mean,
          AREA = NHD_drain_area_sqkm,
          SLOPE = basin_slope
        )
      if (nrow(new_site) > 0) {
        current <- selected_sites()
        if (!new_site_no %in% current$SITENO) {
          selected_sites(rbind(current, new_site))
          shared_data$selected_sites <- c(shared_data$selected_sites, new_site_no)
          showNotification(paste("Site", input$add_site_no, "added successfully!"), type = "message")
        } else {
          showNotification("Site already selected.", type = "warning")
        }
      } else {
        showNotification("Invalid site number. Site not found.", type = "error")
      }
      updateTextInput(session, "add_site_no", value = "")
    })

    # manual site removal
    observeEvent(input$remove_site_btn, {
      req(input$remove_site_no)
      remove_site_no <- stringr::str_pad(input$remove_site_no, 8, pad = "0")
      current <- selected_sites()
      if (remove_site_no %in% current$SITENO) {
        selected_sites(current %>% dplyr::filter(SITENO != remove_site_no))
        shared_data$selected_sites <- current$SITENO[current$SITENO != remove_site_no]
        showNotification(paste("Site", input$remove_site_no, "removed successfully!"), type = "message")
      } else {
        showNotification("Site number not found in the current table.", type = "error")
      }
      updateTextInput(session, "remove_site_no", value = "")
    })

    # remove all sites
    observeEvent(input$remove_site_all, {
      selected_sites(data.frame(
        SITENO = character(), NAME = character(), STATE = character(),
        LAT = numeric(), LONG = numeric(), ELEV = numeric(),
        AREA = numeric(), SLOPE = numeric()
      ))
      shared_data$selected_sites <- character(0)
      showNotification("All sites removed successfully!", type = "message")
    })

    # reset filters
    observeEvent(input$reset, {
      updateSelectizeInput(session, "state1", selected = "")
      updateCheckboxInput(session, "latitude", value = FALSE)
      updateSliderInput(session, "latitude1", value = c(25, 50))
      updateCheckboxInput(session, "longitude", value = FALSE)
      updateSliderInput(session, "longitude1", value = c(-125, -65))
      updateCheckboxInput(session, "elevation", value = FALSE)
      updateSliderInput(session, "elevation1", value = c(5, 3605))
      updateCheckboxInput(session, "area", value = FALSE)
      updateSliderInput(session, "area1", value = c(2, 26000))
      updateCheckboxInput(session, "slope", value = FALSE)
      updateSliderInput(session, "slope1", value = c(0, 70))
      selected_sites(site_info() %>%
        dplyr::filter(SITENO %in% shared_data$mach_ids) %>%
        dplyr::select(
          SITENO,
          NAME = station_name,
          STATE = state,
          LAT = dec_lat_va,
          LONG = dec_long_va,
          ELEV = elev_mean,
          AREA = NHD_drain_area_sqkm,
          SLOPE = basin_slope
        ))
      shared_data$selected_sites <- site_info()$SITENO[site_info()$SITENO %in% shared_data$mach_ids]
    })

    # render gauges table
    output$gauges <- DT::renderDT({
      req(selected_sites())
      data <- selected_sites()
      if (nrow(data) == 0) {
        DT::datatable(
          data.frame(Message = "No sites selected"),
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
            server = FALSE
          ),
          class = "display responsive nowrap"
        ) %>% DT::formatRound(columns = c("LAT", "LONG", "ELEV", "AREA", "SLOPE"), digits = 2)
      }
    }, server = FALSE)

    
    # reactive to fetch discharge_count data
    discharge_data <- reactive({
      req(discharge_count(), selected_sites())
      site_ids <- selected_sites()$SITENO
      if (length(site_ids) == 0) {
        return(data.frame(SITENO = character()))
      }
      tryCatch({
        discharge_count() %>%
          dplyr::filter(SITENO %in% site_ids)
      }, error = function(e) {
        showNotification(paste("Error processing discharge_count:", e$message), type = "error")
        data.frame(SITENO = character())
      })
    })

    # render discharge_days table
    output$discharge_days <- DT::renderDT({
      req(discharge_data())
      data <- discharge_data()
      if (nrow(data) == 0) {
        DT::datatable(
          data.frame(Message = "No sites selected"),
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
            server = FALSE
          ),
          class = "display responsive nowrap"
        )
      }
    }, server = FALSE)
    


    # render Leaflet map
    output$my_leaflet <- renderLeaflet({
      leaflet() %>%
        setView(lng = -99, lat = 40, zoom = 4) %>%
        addTiles(group = "OpenStreetMap", options = tileOptions(maxZoom = 18)) %>%
        addProviderTiles(providers$Esri.WorldTopoMap, group = "EsriTopo", options = tileOptions(maxZoom = 18)) %>%
        addLayersControl(
          baseGroups = c("OpenStreetMap", "EsriTopo"),
          overlayGroups = c("Basin Delineations"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        setMaxBounds(lng1 = -125, lat1 = 25, lng2 = -65, lat2 = 50) %>%
        { if (!is.null(basins_shp())) {
            addPolygons(
              .,
              data = basins_shp(),
              color = "black",
              fillColor = "white",
              weight = 1,
              opacity = 0.7,
              fillOpacity = 0.2,
              group = "Basin Delineations"
            ) %>% hideGroup("Basin Delineations")
          } else . } %>%
        { if (nrow(selected_sites()) > 0) {
            addCircleMarkers(
              .,
              data = selected_sites(),
              lng = ~LONG,
              lat = ~LAT,
              radius = 3,
              color = "blue",
              popup = ~paste0(
                "Gauge ID: ", SITENO, "<br>",
                "Gauge Name: ", NAME, "<br>",
                "Latitude: ", LAT, "<br>",
                "Longitude: ", LONG
              )
            )
          } else . }
    })
  })
}