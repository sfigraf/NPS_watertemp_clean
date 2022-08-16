### clean dates function pre-2017
# pre_2017_data <- read_csv("lacl_newhr_rm22_001_20050808.csv",locale=locale(encoding="latin1"),
#               col_names = c("Date", "Time_GMT_0800", "Temp_Celsius"),
#               skip = 1)

clean_dates_pre_2018_function <- function(pre_2018_data) {
  temp_data <- pre_2018_data %>%
    mutate(datetime1 = parse_date_time(paste(Date, Time_GMT_0800), "mdy_HMS")) %>%
    filter(!is.na(Temp_Celsius))
  return(temp_data)
}


