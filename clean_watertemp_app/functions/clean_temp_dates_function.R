### clean_dates_function
clean_dates_function <- function(temp_data) {
  
  temp_cleaned <- temp_data %>%
    mutate(datetime1 = case_when(
      #if if has AM and  "12" in it, subtrct 12 hours and parse
      str_detect(`Date Time, GMT-08:00`, "AM") & str_detect(`Date Time, GMT-08:00`, "12:") ~ parse_date_time(`Date Time, GMT-08:00`, "mdy_HMS") -hours(12),
      str_detect(`Date Time, GMT-08:00`, "PM") & str_detect(`Date Time, GMT-08:00`, "12:") ~ parse_date_time(`Date Time, GMT-08:00`, "mdy_HMS"),
      #if if has AM and not "12" in it, parse
      
      str_detect(`Date Time, GMT-08:00`, "AM") & str_detect(`Date Time, GMT-08:00`, "12:", negate = TRUE) ~ parse_date_time(`Date Time, GMT-08:00`, "mdy_HMS"),
      str_detect(`Date Time, GMT-08:00`, "PM") & str_detect(`Date Time, GMT-08:00`, "^12:", negate = TRUE) ~ parse_date_time(`Date Time, GMT-08:00`, "mdy_HMS") + hours(12),
      
    ) #end of case_when
    ) %>%
    filter(!is.na(temp_data[3]))
    #rename(Temp_Celsius = `Temp, °C (LGR S/N: 20338010, SEN S/N: 20338010)`)
  #na.omit(temp)
  #colnames(temp_cleaned[3]) <- c("Temp_Celsius")
  
  return(temp_cleaned)
}

