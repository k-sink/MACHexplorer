###################################
### GLOBAL FILE ###
###################################

# load required libraries
library(shiny)
library(bslib)
library(leaflet)
library(sf)
library(shinyjs)
library(shinybusy)
library(shinyalert)
library(shinycssloaders)
library(shinyWidgets)
library(DT)
library(duckdb)
library(dplyr)
library(dbplyr)
library(DBI)
library(qs)
library(rlang)
library(readr)
library(lubridate)
library(zip)


# numeric vector for calendar years to filter daily data
years = seq(from = 1980, to = 2023, by = 1)
# numeric vector for water years 
wateryears = seq(from = 1981, to = 2023, by = 1)



# create water year using date 
  wYear = function(date) {
  ifelse(lubridate::month(date) < 10, lubridate::year(date), lubridate::year(date)+1)}

  
   