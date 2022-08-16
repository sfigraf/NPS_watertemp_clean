library(tidyverse)
library(lubridate)
library(plotly)
# rm22_001_20200610_a <- read_csv("lacl_newhr_rm22_001_20200610_a.csv", 
#                                            col_types = cols(`Date Time, GMT-08:00` = col_character(), 
#                                                             `Temp, °C (LGR S/N: 20338010, SEN S/N: 20338010)` = col_number()), 
#                                            skip = 1)

lacl_newhr_rm22_001_20200610_a <- read_csv("lacl_newhr_rm22_001_20200610_a.csv", 
                                           col_names = c("row_number1", "Datetime_GMT_0800", "Temp_Celsius", 
                                                         "Coupler_Detached", "Coupler_Attached", "Host_Connected","End_of_File"),
                                           col_types = cols(row_number1 = col_number(),
                                                            Datetime_GMT_0800 = col_character(),
                                                            Temp_Celsius = col_number(),
                                                            Coupler_Attached = col_character(), 
                                                            Host_Connected = col_character(), 
                                                            End_of_File = col_character()), 
                                                skip = 1
                                           )
x <- lacl_newhr_rm22_001_20200610_a %>%  
  filter(!row_number() %in% c(1))

#getting dates and times in order
lacl_001_2020_11 <- lacl_newhr_rm22_001_20200610_a %>%
  mutate(datetime1 = case_when(
    #if if has AM and  "12" in it, subtrct 12 hours and parse
    str_detect(`Date Time, GMT-08:00`, "AM") & str_detect(`Date Time, GMT-08:00`, "12:") ~ parse_date_time(`Date Time, GMT-08:00`, "mdy_HMS") -hours(12),
    str_detect(`Date Time, GMT-08:00`, "PM") & str_detect(`Date Time, GMT-08:00`, "12:") ~ parse_date_time(`Date Time, GMT-08:00`, "mdy_HMS"),
    #if if has AM and not "12" in it, parse
    
    str_detect(`Date Time, GMT-08:00`, "AM") & str_detect(`Date Time, GMT-08:00`, "12:", negate = TRUE) ~ parse_date_time(`Date Time, GMT-08:00`, "mdy_HMS"),
    str_detect(`Date Time, GMT-08:00`, "PM") & str_detect(`Date Time, GMT-08:00`, "^12:", negate = TRUE) ~ parse_date_time(`Date Time, GMT-08:00`, "mdy_HMS") + hours(12),
    
  ), #end of case_when
  temp_celsius = lacl_newhr_rm22_001_20200610_a[3]
           ) 
  #rename(Temp_Celsius = `Temp, °C (LGR S/N: 20338010, SEN S/N: 20338010)`)
  
data1 <- clean_dates_function(x)

problem_rows <- data1 %>%
  
  filter(
    #`Temp, °C (LGR S/N: 20338010, SEN S/N: 20338010)` >= 4 & `Temp, °C (LGR S/N: 20338010, SEN S/N: 20338010)` <= 10,
    datetime1 >= parse_date_time(paste0("2019-05-19"," ", "22", ":00:00"),"ymd_HMS") &
    datetime1 <= parse_date_time(paste0("2020-08-10"," ", "13", ":00:00"),"ymd_HMS")
  )

#x <- na.omit(data1$Temp_Celsius)
min(data1$`Temp, °C (LGR S/N: 20338010, SEN S/N: 20338010)`) 


plot <- data1 %>%
  ggplot(aes(x = datetime1, y = data1[3])) +
  geom_line() +
  theme_classic()

ggplotly(plot)

df <- dplyr::tibble(Height = sample(10), Weight = sample(10))
