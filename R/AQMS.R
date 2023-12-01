# Load Packages -----------------------------------------------------------

library(tidyverse)
library(novaAQMeng)
library(openair)
library(insight)

# Load Data ---------------------------------------------------------------

fls <- fs::dir_ls("Data/AQMS/")

dfMS <- map_dfr(fls, ~{
  x <- readSAAQIS(.)
  data <- x$df_data
  data %>% mutate(station = x$file %>% str_extract("/.+$") %>% str_remove_all("/|.xlsx|AQMS"))
  })%>%
  mutate(month = lubridate::month(date, label = TRUE),
         yday = lubridate::yday(date),
         season = case_when(yday < 151 ~ "summer", TRUE ~ "winter"),
         date2 = as.Date(date))





# Summary -----------------------------------------------------------------


AQMS_Tabsummary <- tableDataSummaryDB(df = dfMS %>%
                                                    pivot_longer(cols = so2: amb.temp, names_to = "variable") %>%
                                                    mutate(avg_period = "1 hour",
                                                           unit = "-",
                                                           place = station,
                                                           instrument = "SAAQIS") %>% distinct(),
                                                  begin = min(dfMS$date),
                                                  end = max(dfMS$date)) %>%
  relocate(firstObs, lastObs, .before = nobs) %>%
  relocate(variable, .after = place) %>%
  arrange(place) %>%
  drop_na()


AQMS_Summary <- dfMS %>%
  pivot_longer(cols = so2: amb.temp, names_to = "variable") %>%
  dplyr::summarize(
    novaAQMeng::tenpointsummary(value) , .by = c(station, variable)
  ) %>%
  drop_na() %>%
  arrange(station)


dfMS_Data <- dfMS %>%
  select(date, date2, o3, pm2.5, amb.wspeed, amb.wdirection, amb.temp, station, month, yday, season) %>%
  mutate(pm2.5 = ifelse(pm2.5 < 0, NA, pm2.5),
         pm2.5 = ifelse(pm2.5 == 0, NA, pm2.5)) %>%
  pivot_longer(cols = o3:amb.temp, names_to = "variable") %>%
  mutate(unit = case_when(
    variable == "pm2.5" ~ "Âµg.m-3",
    variable == "amb.wspeed" ~ "m/s",
    variable == "mb.wdirection" ~ "Deg",
    variable == "amb.temp" ~ "C",
    variable == "o3" ~ "ppb",
    TRUE ~ NA_character_
  )) %>%
  relocate(unit, .after = variable) %>%
  mutate(across(.cols = everything(), ~ ifelse(is.infinite(.x), NA, .x)),
         date = as.POSIXct(date, origin = "1970-01-01"),
         date2 = as.Date(date2, origin = "1970-01-01"))


dfMS_Tabsummary <- tableDataSummaryDB(df = dfMS_Data %>%
                                         mutate(avg_period = "1 hour",
                                                unit = "-",
                                                place = station,
                                                instrument = "SAAQIS") %>% distinct(),
                                       begin = min(dfMS_Data$date),
                                       end = max(dfMS_Data$date)) %>%
  relocate(firstObs, lastObs, .before = nobs) %>%
  relocate(variable, .after = place) %>%
  arrange(place) %>%
  drop_na()


AQMS_TenSummary <- dfMS_Data %>%
  dplyr::summarize(
    novaAQMeng::tenpointsummary(value) , .by = c(station, variable)
  ) %>%
  drop_na() %>%
  arrange(station)


# Temporal Averages ----------------------------------------------------------

dfMS_DataDaily <- dfMS_Data %>%
  pivot_wider(id_cols = c(date, date2, station, season, month, yday),
              names_from = c(variable), values_from = value) %>%
  group_by(station, season, month, yday, date, date2) %>%
  summarise(pm2.5 = mean(pm2.5, na.rm = TRUE),
            ws = mean(amb.wspeed, na.rm = TRUE),
            wd = mean(amb.wdirection, na.rm = TRUE),
            Temp = mean(amb.temp, na.rm = TRUE),
            o3 = mean(o3, na.rm = TRUE),
  ) %>%
  arrange(station, yday, month) %>%
  mutate(date = ymd(paste(date2))) %>%
  relocate(date, .before = station)

# NAAQS Daily exceedance --------------------------------------------------


dfMS_day <- dfMS_DataDaily %>%
  select(date, month, season, pm2.5) %>%
  pivot_longer(cols = pm2.5, names_to = "variable") %>%
  mutate(unit = case_when(variable == "pm2.5" ~ "µg.m-3"))

dfMS_daily_ex <- novaAQMeng::compareAQS(df = dfMS_day %>%
                                       ungroup() %>%
                                       datify() %>%
                                       mutate(place = station,
                                              instrument = "SAAQIS"),
                                     period = "day",
                                     by_period = quos(month)) %>%
  ungroup() %>%
  arrange(pollutant, month)

dfMS_daily_ex

save(dfMS, dfMS_Tabsummary,  AQMS_TenSummary, dfMS_daily_ex, file = "Data/MS_Data.Rda")
