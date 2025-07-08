###################################
### GLOBAL FUNCTIONS ###
###################################

# numeric vector for calendar years to filter daily data
years = seq(from = 1980, to = 2023, by = 1)
# numeric vector for water years 
wateryears = seq(from = 1981, to = 2023, by = 1)



# create water year using date 
  wYear = function(date) {
  ifelse(month(date) < 10, year(date), year(date)+1)}

  
# create complete date to include missing OBSQ values as NA
  create_complete_dates = function(gauge_id, frequency = "daily") {
    complete_dates = switch(
      frequency,
      "daily" = seq.Date(from = as.Date("1980-01-01"), to = as.Date("2023-12-31"), by = "day"),
      "monthly" = seq.Date(from = as.Date("1980-01-01"), to = as.Date("2023-12-31"), by = "month"),
      "yearly" = seq.Date(from = as.Date("1980-01-01"), to = as.Date("2023-12-31"), by = "year"), 
      "wyearly" = seq.Date(from = as.Date("1980-10-01"), to = as.Date("2023-09-30"), by = "year")
    )
    data.frame(SITENO = gauge_id, DATE = complete_dates)
  }  

  

# create function to conditionally apply filters based on the input values in the sliders    
apply_filters = function(df, var_name, input_check, range_input) {
  if(input_check) {
    df = df %>% 
      dplyr::filter(is.na(.data[[var_name]]) |
          (.data[[var_name]] >= range_input[1] & .data[[var_name]] <= range_input[2])
      )
  }
  df
}    