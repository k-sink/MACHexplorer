###################################
### SITE SELECTION MODULE ###
###################################
siteSelectionUI = function(id) {
  ns = NS(id)
  tagList(
    
    fluidRow(
      
      column(width = 7,
        wellPanel(h6(strong("USGS Stream Gauging Site Locations")), br(), 
          withSpinner(leafletOutput(ns("my_leaflet"), height = 500), type = 6))),
      
      column(width = 3,
        wellPanel(style = "overflow: visible; height: auto;",
          h6(strong("Filter Sites")), br(), 
          selectizeInput(ns("state1"), "State",
            choices = NULL, multiple = TRUE,
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
          br(),
          actionButton(ns("reset"), "Reset Filters"))),
      
      column(width = 2,
        wellPanel(h6(strong("Edit Site Selections")), br(), 
          textInput(ns("add_site_no"), "Manually Add Site", placeholder = "Enter 8-digit SITENO"),
          actionButton(ns("add_site_btn"), "Add Site"),
          br(), br(),
          textInput(ns("remove_site_no"), "Manually Remove Site", placeholder = "Enter 8-digit SITENO"),
          actionButton(ns("remove_site_btn"), "Remove Site"),
          br(), br(),
          actionButton(ns("remove_site_all"), "Remove All Sites")))
    ),
    
    fluidRow(
      column(width = 5,
        br(),
        wellPanel(h6(strong("Stream Discharge Record")), br(),
          withSpinner(DTOutput(ns("discharge_days")), type = 6))),
     
       column(width = 7,
        br(),
        wellPanel(h6(strong("Selected Stream Gauging Sites")), br(),
          withSpinner(DTOutput(ns("gauges")), type = 6)))
    )
  )
}

###################################################
### SERVER ###
###################################################
### SERVER ###
### SERVER ###
siteSelectionServer <- function(id, shared_data) {
  moduleServer(id, function(input, output, session) {
    ns = session$ns

    observe({
      req(shared_data$site_attributes)
      updateSelectizeInput(session, "state1", choices = sort(unique(shared_data$site_attributes$state)))
    })

    manual_edit <- reactive({
      req(shared_data$site_attributes)
      siteinfo <- shared_data$site_attributes
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
        SITENO = SITENO,
        NAME = station_name,
        STATE = state,
        LAT = dec_lat_va,
        LONG = dec_long_va,
        ELEV = elev_mean,
        AREA = NHD_drain_area_sqkm,
        SLOPE = basin_slope
      )
    }) %>% debounce(500)

    observe({
      shared_data$site <- manual_edit()
    })

    observeEvent(input$add_site_btn, {
      req(input$add_site_no, shared_data$mach_ids)
      new_site_no <- stringr::str_pad(input$add_site_no, 8, pad = "0")
      if (!new_site_no %in% shared_data$mach_ids) {
        showNotification("Invalid site number. Site not found in MACH database.", type = "error")
        updateTextInput(session, "add_site_no", value = "")
        return()
      }
      new_site <- shared_data$site_attributes %>% 
        dplyr::filter(SITENO == new_site_no) %>%
        dplyr::select(
          SITENO = SITENO,
          NAME = station_name,
          STATE = state,
          LAT = dec_lat_va,
          LONG = dec_long_va,
          ELEV = elev_mean,
          AREA = NHD_drain_area_sqkm,
          SLOPE = basin_slope
        )
      if (nrow(new_site) > 0) {
        if (!new_site_no %in% shared_data$site$SITENO) {
          shared_data$site <- rbind(shared_data$site, new_site)
          showNotification(paste("Site", input$add_site_no, "added successfully!"), type = "message")
        } else {
          showNotification("Site already selected.", type = "warning")
        }
      } else {
        showNotification("Invalid site number. Site not found.", type = "error")
      }
      updateTextInput(session, "add_site_no", value = "")
    })

    observeEvent(input$remove_site_btn, {
      req(input$remove_site_no)
      remove_site_no <- stringr::str_pad(input$remove_site_no, 8, pad = "0")
      if (remove_site_no %in% shared_data$site$SITENO) {
        shared_data$site <- shared_data$site %>% dplyr::filter(SITENO != remove_site_no)
        showNotification(paste("Site", input$remove_site_no, "removed successfully!"), type = "message")
      } else {
        showNotification("Site number not found in the current table.", type = "error")
      }
      updateTextInput(session, "remove_site_no", value = "")
    })

    observeEvent(input$remove_site_all, {
      shared_data$site <- shared_data$site_attributes[0, ] %>% dplyr::select(
        SITENO, NAME = station_name, STATE = state, LAT = dec_lat_va, LONG = dec_long_va,
        ELEV = elev_mean, AREA = NHD_drain_area_sqkm, SLOPE = basin_slope
      )
      showNotification("All sites removed successfully!", type = "message")
    })

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
      shared_data$site <- shared_data$site_attributes %>% 
        dplyr::filter(SITENO %in% shared_data$mach_ids) %>% 
        dplyr::select(
          SITENO, NAME = station_name, STATE = state, LAT = dec_lat_va, LONG = dec_long_va,
          ELEV = elev_mean, AREA = NHD_drain_area_sqkm, SLOPE = basin_slope
        )
    })

    output$gauges <- renderDT({
      data <- shared_data$site
      if (nrow(data) == 0) {
        return(DT::datatable(
          data.frame(Message = "No sites selected"),
          options = list(pageLength = 10, scrollX = TRUE, dom = "t"),
          class = "display responsive nowrap"
        ))
      }
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
      ) %>% DT::formatRound(columns = c("LAT", "LONG", "ELEV", "AREA", "SLOPE"), digits = 2)
    })

    output$discharge_days <- renderDT({
      data <- shared_data$discharge_count %>% dplyr::filter(SITENO %in% shared_data$site$SITENO)
      if (nrow(data) == 0) {
        return(DT::datatable(
          data.frame(Message = "No sites selected"),
          options = list(pageLength = 10, scrollX = TRUE, dom = "t"),
          class = "display responsive nowrap"
        ))
      }
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
    })

    output$my_leaflet <- renderLeaflet({
      req(shared_data$basins_shp, shared_data$site)
      leaflet() %>%
        setView(lng = -99, lat = 40, zoom = 4) %>%
        addTiles(group = "OpenStreetMap", options = tileOptions(maxZoom = 18)) %>%
        addProviderTiles(providers$Esri.WorldTopoMap, group = "EsriTopo", options = tileOptions(maxZoom = 18)) %>%
        addCircleMarkers(
          data = shared_data$site,
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
        ) %>%
        addPolygons(
          data = shared_data$basins_shp,
          color = "black",
          fillColor = "white",
          weight = 1,
          opacity = 0.7,
          fillOpacity = 0.2,
          group = "Basin Delineations"
        ) %>%
        addLayersControl(
          baseGroups = c("OpenStreetMap", "EsriTopo"),
          overlayGroups = c("Basin Delineations"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        hideGroup("Basin Delineations") %>%
        setMaxBounds(lng1 = -125, lat1 = 25, lng2 = -65, lat2 = 50)
    })
  })
}