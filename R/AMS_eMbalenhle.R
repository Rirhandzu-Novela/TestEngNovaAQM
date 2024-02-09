# Load packages -----------------------------------------------------------

library(tidyverse)
library(openair)
library(novaAQM)
library(slider)
library(openairmaps)
library(broom)
library(gridExtra)
library(corrplot)
library(Hmisc)

# Read dat files ----------------------------------------------------------

dr1 <- "Data/eMbalenhle_Raw/"
fn <- list.files(dr1)

ls_EAMS <- leesNWU(dr1)
names(ls_EAMS) <- fn

EAMS_Data <- tibble(filename = fn, data = ls_EAMS) %>%
  mutate(ncol = map_int(data, ncol),
         nrow = map_int(data, nrow),
         station = stringr::str_extract(filename, "[[:alpha:]]{8,10}"))

EAMS_Raw <- EAMS_Data %>%
  select(data, station) %>%
  unnest(data) %>%
  arrange(TIMESTAMP) %>%
  distinct() %>%
  relocate(station, .before = RECORD) %>%
  distinct() %>%
  select(TIMESTAMP, station, SO2_Avg, H2S_Avg, CO_Avg, Co2_Avg, NO_Avg,LineV_Avg, Batt_Volt_Avg,NO2_Avg, NOx_Avg, PM10_Avg, PM25_Avg,
         O3_Avg,WSpeed_Max, WSpeed_S_WVT,  WDir_D1_WVT, Temp_Avg, RH) %>%
  rename(so2 = SO2_Avg,
         h2s = H2S_Avg,
         co = CO_Avg,
         co2 = Co2_Avg,
         no = NO_Avg,
         no2 = NO2_Avg,
         nox = NOx_Avg,
         pm10 = PM10_Avg,
         pm2.5 = PM25_Avg,
         o3 = O3_Avg,
         ws = WSpeed_Max,
         ws_wvt = WSpeed_S_WVT,
         wd = WDir_D1_WVT,
         temp = Temp_Avg,
         relHum = RH,
         battv = Batt_Volt_Avg,
         linev = LineV_Avg,
         date = TIMESTAMP) %>%
  mutate(month = lubridate::month(date, label = TRUE),
         date2 = as.Date(date))

save(EAMS_Raw, file = "Report/EAMS_Raw.Rda")


# Plot time series --------------------------------------------------------

EAMS_HourlyR <- timeAverage(EAMS_Raw, avg.time = "hour", data.thresh = 20) %>%
  mutate(month = lubridate::month(date, label = TRUE),
         date2 = as.Date(date)) %>%
  mutate(station = all_of("eMbalenhle"))


EmbalenhlePtimeseriesR <- timePlot(selectByDate(EAMS_HourlyR, year = 2023),
                               pollutant = c("battv","linev", "pm2.5", "pm10", "no2", "nox", "so2"),
                               y.relation = "free")

EmbalenhleMtimeseriesR <- timePlot(selectByDate(EAMS_HourlyR, year = 2023),
                                   pollutant = c("battv", "linev", "ws", "temp", "relHum"),
                                   y.relation = "free")




# Clean Data --------------------------------------------------------------


#EAMS_clean <- EAMS_Raw %>%
#  mutate_at(vars(-date, -month, -date2), ~ifelse(. < 0, NA, .), ~ ifelse(is.infinite(.), NA, .)) %>%
#  mutate_at(vars(-date, -month, -date2), ~ifelse(battv > 12.8 | battv < 12.5, NA, .)) %>%
#  mutate(
#    pm2.5 = ifelse(pm2.5 > 1000, 1000, pm2.5),
#    pm10 = ifelse(pm10 > 1000, 1000, pm10),
#    o3 = ifelse(o3 < 1, 0.5, o3),
#    o3 = ifelse(o3 > 1000, 1000, o3),
#    so2 = ifelse(so2 < 2, 1,
so2),
#    so2 = ifelse(so2 > 1000, 1000, so2),
#    h2s = ifelse(h2s < 2, 1, h2s),
#    h2s = ifelse(h2s > 1000, 1000, h2s),
#    no = ifelse(no < 0.4, 0.2, no),
#    no = ifelse(no > 1000, 1000, no),
#    no2 = ifelse(no2 < 0.4, 0.2, no2),
#    no2 = ifelse(no2 > 1000, 1000, no2),
#    nox = ifelse(nox < 0.4, 0.2, nox),
#    nox = ifelse(nox > 1000, 1000, nox),
#    ws = ifelse(ws > 20, NA, ws))

#EAMS_clean <- EAMS_clean %>%
#  mutate(pm10 = case_when(pm10 > 500 ~ NA, .default = pm10)) %>%
#  mutate(pm10 = case_when(between(date, as.Date("2023-01-01"), as.Date("2023-04-24")) ~ NA, .default = pm10)) %>%
#  mutate(pm2.5 = case_when(between(date, as.Date("2023-01-01"), as.Date("2023-02-04")) ~ NA, .default = pm2.5)) %>%
#  mutate(pm2.5 = case_when(between(date, as.Date("2023-02-24"), as.Date("2023-03-11")) ~ NA, .default = pm2.5)) %>%
#  mutate(pm2.5 = case_when(between(date, as_datetime("2023-03-31 21:46:00"), as_datetime("2023-05-15 19:00:00")) ~ pm2.5 - 33, .default = pm2.5)) %>%
#  mutate(pm2.5 = case_when(between(date, as_datetime("2023-05-15 19:00:00"), as_datetime("2023-06-19 00:00:00")) ~ NA, .default = pm2.5)) %>%
#  mutate(pm2.5 = case_when(between(date, as_datetime("2023-09-23 11:00:00"), as_datetime("2023-10-31 00:00:00")) ~ NA, .default = pm2.5)) %>%
#  mutate(pm2.5 = case_when(between(date, as_datetime("2023-11-10 11:00:00"), as_datetime("2023-11-16 12:00:00")) ~ pm2.5 - 21, .default = pm2.5)) %>%
#  mutate(pm2.5 = case_when(between(date, as_datetime("2023-11-27 00:00:00"), as_datetime("2023-12-12 10:00:00")) ~ NA, .default = pm2.5)) %>%
#  mutate(pm2.5 = case_when(between(date, as_datetime("2023-12-26 10:00:00"), as_datetime("2024-01-09 12:00:00")) ~ NA, .default = pm2.5)) %>%
#  mutate(pm2.5 = case_when(between(date, as_datetime("2024-01-13 23:00:00"), as_datetime("2024-01-31 00:00:00")) ~ NA, .default = pm2.5)) %>%
#  mutate(pm2.5 = case_when(pm2.5 < 0 ~ NA, .default = pm2.5)) %>%
#  mutate(pm2.5 = case_when(pm2.5 > 500 ~ NA, .default = pm2.5)) %>%
#  mutate(so2 = case_when(between(date, as_datetime("2023-07-24 00:00:00"), as_datetime("2023-08-14 00:00:00")) ~ NA, .default = so2)) %>%
#  mutate(so2 = case_when(slide_dbl(so2, max, .before = 1000, .after = 1000) > 500 ~ NA, .default = so2)) %>%
#  mutate(across(where(is.numeric),
#                ~ if_else(between(date, as.Date("2023-08-01"), as.Date("2023-08-15")), NA_real_, .)))%>%
#  mutate(across(where(is.numeric),
#                ~ if_else(between(date, as.Date("2023-09-17"), as.Date("2023-09-22")), NA_real_, .))) %>%
#  filter(year(date) == "2023")


EAMS_clean <- EAMS_Raw %>%
  mutate(across(where(is.numeric), ~case_when(. <= 0 ~ NA,
                                              is.infinite(.) ~ NA,
                                              .default = .))) %>%
  mutate_at(vars(-date, -month, -date2), ~ifelse(battv > 12.8 | battv < 12.5, NA, .)) %>%
  mutate(batt_diff = slide_index_dbl(.x = battv, .i = date,  .f = ~{range(.x, na.rm = T)[2] - range(.x, na.rm = T)[1]}, .before = minutes(30), .after = minutes(30), .complete = F)) %>%
  mutate(pm10 = case_when(pm10 > 500 ~ NA, .default = pm10)) %>%
  mutate(pm10 = case_when(between(date, as.Date("2023-01-01"), as.Date("2023-04-24")) ~ NA, .default = pm10)) %>%
  mutate(pm2.5 = case_when(between(date, as.Date("2023-01-01"), as.Date("2023-02-04")) ~ NA, .default = pm2.5)) %>%
  mutate(pm2.5 = case_when(between(date, as.Date("2023-02-24"), as.Date("2023-03-11")) ~ NA, .default = pm2.5)) %>%
  mutate(pm2.5 = case_when(between(date, as_datetime("2023-03-31 21:46:00"), as_datetime("2023-05-15 19:00:00")) ~ pm2.5 - 33, .default = pm2.5)) %>%
  mutate(pm2.5 = case_when(between(date, as_datetime("2023-05-15 19:00:00"), as_datetime("2023-06-19 00:00:00")) ~ NA, .default = pm2.5)) %>%
  mutate(pm2.5 = case_when(between(date, as_datetime("2023-09-23 11:00:00"), as_datetime("2023-10-31 00:00:00")) ~ NA, .default = pm2.5)) %>%
  mutate(pm2.5 = case_when(between(date, as_datetime("2023-11-10 11:00:00"), as_datetime("2023-11-16 12:00:00")) ~ pm2.5 - 21, .default = pm2.5)) %>%
  mutate(pm2.5 = case_when(between(date, as_datetime("2023-11-27 00:00:00"), as_datetime("2023-12-12 10:00:00")) ~ NA, .default = pm2.5)) %>%
  mutate(pm2.5 = case_when(between(date, as_datetime("2023-12-26 10:00:00"), as_datetime("2024-01-09 12:00:00")) ~ NA, .default = pm2.5)) %>%
  mutate(pm2.5 = case_when(between(date, as_datetime("2024-01-13 23:00:00"), as_datetime("2024-01-31 00:00:00")) ~ NA, .default = pm2.5)) %>%
  mutate(no2 = case_when(between(date, as_datetime("2023-04-01 10:00:00"), as_datetime("2023-05-30 12:00:00")) ~ NA, .default = no2)) %>%
  mutate(nox = case_when(between(date, as_datetime("2023-04-01 23:00:00"), as_datetime("2023-05-30 00:00:00")) ~ NA, .default = nox)) %>%
  mutate(ws = case_when(between(date, as_datetime("2023-01-01 23:00:00"), as_datetime("2023-01-25 00:00:00")) ~ NA, .default = ws)) %>%
  mutate(pm2.5 = case_when(pm2.5 < 0 ~ NA, .default = pm2.5)) %>%
  mutate(pm2.5 = case_when(pm2.5 > 500 ~ NA, .default = pm2.5)) %>%
  mutate(ws = case_when(ws > 20 ~ NA, .default =  ws)) %>%
  mutate(so2 = case_when(between(date, as_datetime("2023-07-24 00:00:00"), as_datetime("2023-08-14 00:00:00")) ~ NA, .default = so2)) %>%
  mutate(so2 = case_when(slide_dbl(so2, max, .before = 1000, .after = 1000) > 750 ~ NA, .default = so2),
         nox = case_when(batt_diff > 0.02 | battv < 12.7 ~ NA, .default = nox),
         nox = case_when(month(date) %in% c(1, 2, 3, 12) ~ NA, .default = nox),
         co = case_when(batt_diff > 0.02 | battv < 12.7 ~ NA, .default = co),
         co = case_when(month(date) %in% c(1, 2, 3) | as.Date(date) == as.Date("2023-04-25") ~ NA, .default = co)
         # h2s = case_when(batt_diff > 0.02 | battv < 12.7 ~ NA, .default = h2s),
         # h2s = case_when(month(date) %in% c(1, 2, 3) ~ NA, .default = h2s),

  ) %>%
    filter(year(date) == "2023")

save(EAMS_clean, file = "Graph/EAMS_clean.Rda")

EAMS_HourlyC <- timeAverage(EAMS_clean,avg.time = "hour", data.thresh = 20) %>%
  mutate(month = lubridate::month(date, label = TRUE),
         date2 = as.Date(date)) %>%
  mutate(station = all_of("eMbalenhle"))



EmbalenhlePtimeseriesC <- timePlot(selectByDate(EAMS_HourlyC, year = 2023),
                                   pollutant = c("battv", "linev", "pm2.5", "pm10", "co", "no2", "nox", "so2"),
                                   y.relation = "free")


#pdf(file = "Graph/EmbalenhlePtimeseriesC.pdf")

EmbalenhleMtimeseriesC <- timePlot(selectByDate(EAMS_HourlyC, year = 2023),
                                   pollutant = c("battv","linev", "ws", "temp", "relHum"),
                                   y.relation = "free")
#pdf(file = "Graph/EmbalenhleMtimeseriesC.pdf")


save( EmbalenhlePtimeseriesC,
     EmbalenhleMtimeseriesC, file = "Graph/EAMS_Timeseriesplot.Rda")


# Summary AMS -------------------------------------------------------------


EAMS_clean_summary <- EAMS_clean %>%
  select(date, date2, station, so2, no, nox, no2, co, pm10, o3, pm2.5, ws, wd, temp, battv, relHum) %>%
  pivot_longer(cols = so2:relHum, names_to = "variable") %>%
  mutate(unit = case_when(
    variable == "pm2.5" ~ "Âµg.m-3",
    variable == "ws" ~ "m/s",
    variable == "wd" ~ "Deg",
    variable == "temp" ~ "C",
    variable == "o3" ~ "ppb",
    variable == "no" ~ "ppb",
    variable == "no2" ~ "ppb",
    variable == "nox" ~ "ppb",
    variable == "co" ~ "ppm",
    variable == "pm10" ~ "Âµg.m-3",
    variable == "battv" ~ "V",
    variable == "so2" ~ "ppb",
    variable == "relHum" ~ "perc",
    TRUE ~ NA_character_
  )) %>%
  relocate(unit, .after = variable)


EAMS_summary <- tabDataVasleggingDB(df = EAMS_clean_summary %>%
                      mutate(avg_period = "1 min",
                             unit = str_replace(unit, "ug/m3", "µg.m-3"),
                             instrument = "AMS",
                             place = station),
                    begin = min(EAMS_clean_summary$date),
                    end = max(EAMS_clean_summary$date)) %>%
  relocate(firstObs, lastObs, .before = nobs) %>%
  relocate(variable, .before = place) %>%
  arrange(variable, place)

save(EAMS_summary, file = "Graph/EAMS_summary.Rda")

# AMS Averages and exceedances --------------------------------------------

EAMS_date <- EAMS_HourlyC %>%
  select(date,  month, station, pm2.5, o3, co, no2, no, nox, so2, pm10) %>%
  pivot_longer(cols = c(pm2.5, o3, no2, no, nox, so2, co, pm10), names_to = "variable") %>%
  mutate(unit = case_when(
    variable == "pm2.5" ~ "Âµg.m-3",
    variable == "o3" ~ "ppb",
    variable == "no" ~ "ppb",
    variable == "no2" ~ "ppb",
    variable == "nox" ~ "ppb",
    variable == "co" ~ "ppm",
    variable == "pm10" ~ "Âµg.m-3",
    variable == "so2" ~ "ppb",
    TRUE ~ NA_character_
  ))

EAMS_monthly_hour_ex <- novaAQM::compareAQS(df = EAMS_date %>%
                                      ungroup() %>%
                                      datify() %>%
                                      mutate(place = station,
                                             instrument = "AMS"),
                                    period = "hour",
                                    by_period = quos(month, year)) %>%
  ungroup() %>%
  arrange(pollutant, month) %>%
  relocate(pollutant, .after = place)

EAMS_season_hour_ex <- novaAQM::compareAQS(df = EAMS_date %>%
                                              ungroup() %>%
                                              datify() %>%
                                              mutate(place = station,
                                                     instrument = "AMS"),
                                            period = "hour",
                                            by_period = quos(season, year)) %>%
  ungroup() %>%
  arrange(pollutant) %>%
  relocate(pollutant, .after = place)
season_order = tibble(season = c("autum", "winter", "spring", "summer"), season_nr = c(1, 2, 3, 4))
EAMS_season_hour_ex <- EAMS_season_hour_ex %>% left_join(season_order) %>% arrange(pollutant, season_nr)

EAMS_annual_hour_ex <- novaAQM::compareAQS(df = EAMS_date %>%
                                             ungroup() %>%
                                             datify() %>%
                                             mutate(place = station,
                                                    instrument = "AMS"),
                                           period = "hour",
                                           by_period = quos(year)) %>%
  ungroup() %>%
  arrange(pollutant, year) %>%
  relocate(pollutant, .after = place)

# Daily averages

EAMS_DailyC <- timeAverage(EAMS_HourlyC, avg.time = "day", data.thresh = 20) %>%
  mutate(month = lubridate::month(date, label = TRUE),
         date2 = as.Date(date)) %>%
  mutate(station = all_of("eMbalenhle"))


EAMS_Day  <- EAMS_DailyC %>%
  select(date, month, station, pm2.5, o3, no2, no, nox, so2, co, pm10) %>%
  pivot_longer(cols = c(pm2.5, o3,  no2, no, nox, so2, co, pm10), names_to = "variable") %>%
  mutate(unit = case_when(
    variable == "pm10" ~ "Âµg.m-3",
    variable == "pm2.5" ~ "Âµg.m-3",
    variable == "o3" ~ "ppb",
    variable == "so2" ~ "ppb",
    variable == "no" ~ "ppb",
    variable == "no2" ~ "ppb",
    variable == "nox" ~ "ppb",
    variable == "co" ~ "ppm",
    TRUE ~ NA_character_
  ))


EAMS_month_daily_ex <- novaAQM::compareAQS(df = EAMS_Day %>%
                                       ungroup() %>%
                                       datify() %>%
                                       mutate(place = station,
                                              instrument = "AMS"),
                                     period = "day",
                                     by_period = quos(month, year)) %>%
  #ungroup() %>%
  arrange(pollutant, month)



EAMS_season_daily_ex <- novaAQM::compareAQS(df = EAMS_Day %>%
                                       ungroup() %>%
                                       datify() %>%
                                       mutate(place = station,
                                              instrument = "AMS"),
                                     period = "day",
                                     by_period = quos(year, season)) %>%
  #ungroup() %>%
  arrange(pollutant, season)
season_order = tibble(season = c("autum", "winter", "spring", "summer"), season_nr = c(1, 2, 3, 4))
EAMS_season_daily_ex <- EAMS_season_daily_ex %>% left_join(season_order) %>% arrange(pollutant, season_nr)




EAMS_annual_daily_ex <- novaAQM::compareAQS(df = EAMS_Day %>%
                                              ungroup() %>%
                                              datify() %>%
                                              mutate(place = station,
                                                     instrument = "AMS"),
                                            period = "day",
                                            by_period = quos(year)) %>%
  #ungroup() %>%
  arrange(pollutant, year)

save(EAMS_monthly_hour_ex, EAMS_season_hour_ex, EAMS_month_daily_ex, EAMS_season_daily_ex,
     EAMS_annual_daily_ex, EAMS_annual_hour_ex,  file = "Graph/embalenhle_Exceedances.Rda")


# Density plots -----------------------------------------------------------


Densitypm2.5 <- EAMS_clean %>%
  ggplot(aes(x = log(pm2.5), color = station, fill = station)) +
  geom_histogram(aes(y=..density..), position="identity")+
  geom_density(alpha=.2) +
  labs(title= "PM2.5 density curve")
Densitypm2.5

Densitypm10 <- EAMS_clean %>%
  ggplot(aes(x = log(pm10), color = station, fill = station)) +
  geom_histogram(aes(y=..density..), position="identity")+
  geom_density(alpha=.2) +
  labs(title= "PM10 density curve")
Densitypm10


Densityno2 <- EAMS_clean %>%
  ggplot(aes(x = log(no2), color = station, fill = station)) +
  geom_histogram(aes(y=..density..), position="identity")+
  geom_density(alpha=.2) +
  labs(title= "NO2 density curve")
Densityno2

Densityso2 <- EAMS_clean %>%
  ggplot(aes(x = log(so2), color = station, fill = station)) +
  geom_histogram(aes(y=..density..), position="identity")+
  geom_density(alpha=.2) +
  labs(title= "SO2 density curve")
Densityso2

Densitynox <- EAMS_clean %>%
  ggplot(aes(x = log(nox), color = station, fill = station)) +
  geom_histogram(aes(y=..density..), position="identity")+
  geom_density(alpha=.2) +
  labs(title= "NOX density curve")
Densitynox

Densityco <- EAMS_clean %>%
  ggplot(aes(x = log(co), color = station, fill = station)) +
  geom_histogram(aes(y=..density..), position="identity")+
  geom_density(alpha=.2) +
  labs(title= "CO density curve")
Densityco

save(Densitypm2.5, Densitypm10,
     Densityso2, Densityno2, Densitynox,Densityco,
     file = "Graph/EAMS_densityplot.Rda")



# Line plot ---------------------------------------------------------------

# Diurnal plots

EAMS_HourlyN <- EAMS_HourlyC %>%
  datify() %>%
  mutate(hod = lubridate::hour(date))


hourlyPM2.5Compare <- ggplot(data = EAMS_HourlyN,
                             aes(x = date,
                                 y = pm2.5,
                                 group = station,
                                 colour = station)) +
  geom_line() +
  facet_wrap(~season, nrow = 2, scales = "free") +
  theme(legend.position = "bottom") +
  ggtitle(expression(paste("Hourly ", PM2.5)))
hourlyPM2.5Compare


hourlyPM10Compare <- ggplot(data = EAMS_HourlyN,
                            aes(x = date,
                                y = pm10,
                                group = station,
                                colour = station)) +
  geom_line() +
  facet_wrap(year~season, nrow = 2, scales = "free_x") +
  theme(legend.position = "bottom") +
  ggtitle(expression(paste("Hourly ", PM10)))
hourlyPM10Compare

hourlyso2Compare <- ggplot(data = EAMS_HourlyN,
                           aes(x = date,
                               y = so2,
                               group = station,
                               colour = station)) +
  geom_line() +
  facet_wrap(~season, nrow = 2, scales = "free") +
  theme(legend.position = "bottom") +
  ggtitle(expression(paste("Hourly ", so2)))
hourlyso2Compare


hourlyno2Compare <- ggplot(data = EAMS_HourlyN,
                           aes(x = date,
                               y = no2,
                               group = station,
                               colour = station)) +
  geom_line() +
  facet_wrap(~season, nrow = 2, scales = "free") +
  theme(legend.position = "bottom") +
  ggtitle(expression(paste("Hourly ", NO2)))
hourlyPMn02Compare


save(hourlyPM2.5Compare,
     hourlyPM10Compare,
     hourlyso2Compare,
     file = "Data/Ehourly_plot.Rda")

diurnalPM2.5Compare <- ggplot(data = EAMS_HourlyN,
                              aes(x = hod,
                                  y = pm2.5,
                                  group = date2,
                                  colour = station)) +
  geom_line(alpha = I(1/3)) +
  theme_bw() +
  facet_wrap(~season, nrow = 2, scales = "free") +
  theme(legend.position = "bottom") +
  ggtitle(expression(paste("Hourly ", PM[2.5])))
diurnalPM2.5Compare
#ggsave(plot = diurnalPMCompare, filename = "Graph/diurnalPMCompare.pdf")

diurnalPM10Compare <- ggplot(data = EAMS_HourlyN,
                             aes(x = hod,
                                 y = pm10,
                                 group = date2,
                                 colour = station)) +
  geom_line(alpha = I(1/3)) +
  theme_bw() +
  facet_wrap(~season, nrow = 2, scales = "free") +
  theme(legend.position = "bottom") +
  ggtitle(expression(paste("Hourly ", PM[10])))
diurnalPM10Compare

diurnalso2Compare <- ggplot(data = EAMS_HourlyN,
                            aes(x = hod,
                                y = so2,
                                group = date2,
                                colour = station)) +
  geom_line(alpha = I(1/3)) +
  theme_bw() +
  facet_wrap(~season, nrow = 2, scales = "free") +
  theme(legend.position = "bottom") +
  ggtitle(expression(paste("Hourly ", SO[2])))
diurnalSO2Compare

diurnalno2Compare <- ggplot(data = EAMS_HourlyN,
                            aes(x = hod,
                                y = no2,
                                group = date2,
                                colour = station)) +
  geom_line(alpha = I(1/3)) +
  theme_bw() +
  facet_wrap(~season, nrow = 2, scales = "free") +
  theme(legend.position = "bottom") +
  ggtitle(expression(paste("Hourly ", NO[2])))
diurnalNO2Compare

diurnalNoXCompare <- ggplot(data = EAMS_HourlyN,
                            aes(x = hod,
                                y = nox,
                                group = date2,
                                colour = station)) +
  geom_line(alpha = I(1/3)) +
  theme_bw() +
  facet_wrap(~season, nrow = 2, scales = "free") +
  theme(legend.position = "bottom") +
  ggtitle(expression(paste("Hourly ", SO[2])))
diurnalNOXCompare


save(diurnalPM2.5Compare,
     diurnalPM10Compare,
     diurnalso2Compare,
     file = "Data/Ediurnal_plot.Rda")


EmbadailyPM10Compare <- ggplot(data = EAMS_DailyC, aes(x = date2, y = pm10, group = station, colour = station)) +
  geom_line() +
  facet_wrap(~season , nrow = 2, scales = "free") +
  geom_hline(yintercept = 75, linetype = "dashed", color = "black") +
  theme_classic() +
  labs(
    x = "Date",
    y = "pm10",
    color = "station",
    title = "Daily PM10")
EmbadailyPM10Compare

EmbadailyPM2.5Compare <- ggplot(data = EAMS_DailyC, aes(x = date2, y = pm2.5, group = station, colour = station)) +
  geom_line() +
  facet_wrap(~season , nrow = 2, scales = "free") +
  geom_hline(yintercept = 40, linetype = "dashed", color = "black") +
  theme_classic() +
  labs(
    x = "Date",
    y = "pm2.5",
    color = "station",
    title = "Daily PM2.5")
EmbadailyPM2.5Compare

EmbadailySO2Compare <- ggplot(data = EAMS_DailyC, aes(x = date2, y = so2, group = station, colour = station)) +
  geom_line() +
  facet_wrap(~season , nrow = 2, scales = "free") +
  geom_hline(yintercept = 48, linetype = "dashed", color = "black") +
  theme_classic() +
  labs(
    x = "Date",
    y = "so2",
    color = "station",
    title = "Daily SO2")
EmbadailySO2Compare

EmbadailynOxCompare <- ggplot(data = EAMS_DailyC, aes(x = date2, y = nox, group = station, colour = station)) +
  geom_line() +
  facet_wrap(~season , nrow = 2, scales = "free") +
  theme_classic() +
  labs(
    x = "Date",
    y = "Nox",
    color = "station",
    title = "Daily NOx")
EmbadailynOxCompare

save(EmbadailyPM10Compare, EmbadailyPM2.5Compare,
     EmbadailySO2Compare,EmbadailynOxCompare,
     file = "Report/EAMS_dailyplot.Rda")



# Box plots ---------------------------------------------------------------

EBoxPM2.5Compare <- ggplot(data = EAMS_DailyC %>%
                             select(pm2.5, month) %>%
                             mutate(n = length(which(!is.na(pm2.5))) / n(), .by = month),
                           aes(x = month, y = pm2.5 )) +
  geom_boxplot(aes(fill = n)) +
  geom_hline(yintercept = 40, linetype = "dashed", color = "red") +
  theme_bw() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(
    x = "Month",
    y = "PM2.5",
    title = "Monthly statistical summary of PM2.5 at eMbalenhle ",
    caption = "Data from AMS") +
  theme(legend.position = "bottom")

EBoxPM2.5Compare

EBoxPM10Compare <- ggplot(data = EAMS_DailyC %>%
                            select(pm10, month) %>%
                            mutate(n = n(), .by = month),
                          aes(x = month, y = pm10 )) +
  geom_boxplot(aes(fill = n)) +
  geom_hline(yintercept = 75, linetype = "dashed", color = "red") +
  theme_bw() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(
    x = "Month",
    y = "PM10",
    title = "Monthly statistical summary of PM10 at eMbalenhle ",
    caption = "Data from AMS") +
  theme(legend.position = "none")

EBoxPM10Compare

EBoxso2Compare <- ggplot(data = EAMS_DailyC %>%
                           select(so2, month) %>%
                           mutate(n = n(), .by = month),
                         aes(x = month, y = so2 )) +
  geom_boxplot(aes(fill = n)) +
  geom_hline(yintercept = 48, linetype = "dashed", color = "red") +
  theme_bw() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(
    x = "Month",
    y = "SO2",
    title = "Monthly statistical summary of SO2 at eMbalenhle ",
    caption = "Data from AMS") +
  theme(legend.position = "none")

EBoxso2Compare

EBoxno2Compare <- ggplot(data = EAMS_DailyC %>%
                           select(no2, month) %>%
                           mutate(n = n(), .by = month),
                         aes(x = month, y = no2 )) +
  geom_boxplot(aes(fill = n)) +
  theme_bw() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(
    x = "Month",
    y = "NO2",
    title = "Monthly statistical summary of NO2 at eMbalenhle ",
    caption = "Data from AMS") +
  theme(legend.position = "none")

EBoxno2Compare

EBoxnoxCompare <- ggplot(data = EAMS_DailyC %>%
                           select(nox, month) %>%
                           mutate(n = n(), .by = month),
                         aes(x = month, y = nox )) +
  geom_boxplot(aes(fill = n)) +
  theme_bw() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(
    x = "Month",
    y = "NOx",
    title = "Monthly statistical summary of NOx at eMbalenhle ",
    caption = "Data from AMS") +
  theme(legend.position = "none")

EBoxnoxCompare

EBoxcoCompare <- ggplot(data = EAMS_DailyC %>%
                          select(co, month) %>%
                          mutate(n = n(), .by = month),
                        aes(x = month, y = co )) +
  geom_boxplot(aes(fill = n)) +
  theme_bw() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(
    x = "Month",
    y = "CO",
    title = "Monthly statistical summary of CO at eMbalenhle ",
    caption = "Data from AMS") +
  theme(legend.position = "none")

EBoxcoCompare

save(EBoxPM2.5Compare,
     EBoxPM10Compare,
     EBoxso2Compare,
     EBoxno2Compare,
     EBoxnoxCompare,
     EBoxcoCompare,
     file = "Graph/EBox_plot.Rda")


EPM10Tempplot <- timeVariation(EAMS_clean, stati="median", poll="pm10", conf.int = c(0.75, 0.99),
                      col = "firebrick", normalise = TRUE)
EPM2.5Tempplot <- timeVariation(EAMS_clean, stati="median", poll="pm2.5", conf.int = c(0.75, 0.99),
                              col = "firebrick", normalise = TRUE)

ESO2Tempplot <- timeVariation(EAMS_clean, stati="median", poll="so2", conf.int = c(0.75, 0.99),
                              col = "firebrick", normalise = TRUE)
ENO2Tempplot <- timeVariation(EAMS_clean, stati="median", poll="no2", conf.int = c(0.75, 0.99),
                              col = "firebrick", normalise = TRUE)
ENOxTempplot <- timeVariation(EAMS_clean, stati="median", poll="nox", conf.int = c(0.75, 0.99),
                              col = "firebrick", normalise = TRUE)

ECOTempplot <- timeVariation(EAMS_clean, stati="median", poll="co", conf.int = c(0.75, 0.99),
                              col = "firebrick", normalise = TRUE)

Etempplot <- timeVariation(EAMS_clean,
                          pollutant = c("pm2.5", "pm10", "nox", "no2", "co", "so2"),
                          normalise = TRUE)


save(EPM10Tempplot,
     EPM2.5Tempplot,
     ESO2Tempplot,
     ENO2Tempplot,
     ENOxTempplot,
     ECOTempplot,
     Etempplot,
     file = "Graph/ETempoaral_plot.Rda")

# Polar Plot ---------------------------------------------------------------


magnetic_declination <- -20.74


Epolar <- EAMS_clean %>%
  datify() %>%
  mutate(latitude = -26.56123474,
         longitude = 29.08227378,
         wd = case_when(wd - magnetic_declination > 360 ~ wd - magnetic_declination - 360,
                        .default = wd - magnetic_declination)
  )

allpolar <- polarMap(
  Epolar,
  latitude = "latitude",
  longitude = "longitude",
  pollutant = c("nox", "no2", "pm2.5", "co", "pm10", "so2"),
  provider = c("Satellite" = "Esri.WorldImagery")
)


EpolarstatsPM10 <- polarSummary(mydata = Epolar, stats = c("frequency", "mean", "weighted.mean", "stdev"),
                   pdf = TRUE,
                   fn = "Graph/EpolarstatsPM10.pdf",
                   tp = "PM10 Polar summary at eMbalenhle",
                   width = 10,
                   verbose = TRUE,
                   upper = 15,
                   adjust_upper = TRUE,
                   ncol = 2,
                   nrow = 2,
                   style = "polarPlot",
                   pollutant = "pm10")
grid.arrange(EpolarstatsPM10)
dev.off()
plot(EpolarstatsPM10)


EpolarstatsPM2.5 <- polarSummary(mydata = Epolar, stats = c("frequency", "mean", "weighted.mean", "stdev"),
                                pdf = TRUE,
                                fn = "Graph/Epolarstatspm2.5.pdf",
                                tp = "PM2.5 Polar summary at eMbalenhle",
                                width = 10,
                                verbose = TRUE,
                                upper = 15,
                                adjust_upper = TRUE,
                                ncol = 2,
                                nrow = 2,
                                style = "polarPlot",
                                pollutant = "pm2.5")

grid.arrange(EpolarstatsPM2.5)
dev.off()
plot(EpolarstatsPM2.5)

Epolarstatsso2 <- polarSummary(mydata = Epolar, stats = c("frequency", "mean", "weighted.mean", "stdev"),
                                pdf = TRUE,
                                fn = "Graph/Epolarstatsso2.pdf",
                                tp = "SO2 Polar summary at eMbalenhle",
                                width = 10,
                                verbose = TRUE,
                                upper = 15,
                                adjust_upper = TRUE,
                                ncol = 2,
                                nrow = 2,
                                style = "polarPlot",
                                pollutant = "so2")
grid.arrange(Epolarstatsso2)
dev.off()
plot(Epolarstatsso2)

Epolarstatsnox <- polarSummary(mydata = Epolar, stats = c("frequency", "mean", "weighted.mean", "stdev"),
                                pdf = TRUE,
                                fn = "Graph/Epolarstatsnox.pdf",
                                tp = "NOx Polar summary at eMbalenhle",
                                width = 10,
                                verbose = TRUE,
                                upper = 15,
                                adjust_upper = TRUE,
                                ncol = 2,
                                nrow = 2,
                                style = "polarPlot",
                                pollutant = "nox")
grid.arrange(Epolarstatsnox)
dev.off()
plot(Epolarstatsnox)

Epolarstatsno2 <- polarSummary(mydata = Epolar, stats = c("frequency", "mean", "weighted.mean", "stdev"),
                                pdf = TRUE,
                                fn = "Graph/Epolarstatsno2.pdf",
                                tp = "NO2 Polar summary at eMbalenhle",
                                width = 10,
                                verbose = TRUE,
                                upper = 15,
                                adjust_upper = TRUE,
                                ncol = 2,
                                nrow = 2,
                                style = "polarPlot",
                                pollutant = "no2")
grid.arrange(Epolarstatsno2)
dev.off()
plot(Epolarstatsno2)

Epolarstatsco <- polarSummary(mydata = Epolar, stats = c("frequency", "mean", "weighted.mean", "stdev"),
                                pdf = TRUE,
                                fn = "Graph/polarstatsco.pdf",
                                tp = "CO Polar summary at eMbalenhle",
                                width = 10,
                                verbose = TRUE,
                                upper = 15,
                                adjust_upper = TRUE,
                                ncol = 2,
                                nrow = 2,
                                style = "polarPlot",
                                pollutant = "co")
grid.arrange(Epolarstatsco)
dev.off()
plot(Epolarstatsco)


EpolarcorPM <- polarSummaryCor2(df = Epolar,
                                  pol1 = "pm2.5",
                                  pol2 = "pm10",
                                  place = "eMbalenhle",
                                  title = "Correlation between  PM2.5 and PM10 sources at eMbalenhle",
                                  k = 50,
                                  upper = 15,
                                  adjust_upper = TRUE,
                                  statistic = "mean")
EpolarcorPM <- gridExtra::grid.arrange(grobs = EpolarcorPM)


EpolarcorPMSO <- polarSummaryCor2(df = Epolar,
                                pol1 = "pm2.5",
                                pol2 = "so2",
                                place = "eMbalenhle",
                                title = "Correlation between  PM2.5 and SO2 sources at eMbalenhle",
                                k = 50,
                                upper = 15,
                                adjust_upper = TRUE,
                                statistic = "mean")
EpolarcorPMSO <- gridExtra::grid.arrange(grobs = EpolarcorPMSO)

EpolarcorPMNO <- polarSummaryCor2(df = Epolar,
                                  pol1 = "pm2.5",
                                  pol2 = "no2",
                                  place = "eMbalenhle",
                                  title = "Correlation between  PM2.5 and NO2 sources at eMbalenhle",
                                  k = 50,
                                  upper = 15,
                                  adjust_upper = TRUE,
                                  statistic = "mean")
EpolarcorPMNO <- gridExtra::grid.arrange(grobs = EpolarcorPMNO)

EpolarcorPMNX <- polarSummaryCor2(df = Epolar,
                                  pol1 = "pm2.5",
                                  pol2 = "nox",
                                  place = "eMbalenhle",
                                  title = "Correlation between  PM2.5 and NOX sources at eMbalenhle",
                                  k = 50,
                                  upper = 15,
                                  adjust_upper = TRUE,
                                  statistic = "mean")
EpolarcorPMNX <- gridExtra::grid.arrange(grobs = EpolarcorPMNX)

EpolarcorPMCO <- polarSummaryCor2(df = Epolar,
                                  pol1 = "pm2.5",
                                  pol2 = "co",
                                  place = "eMbalenhle",
                                  title = "Correlation between  PM2.5 and CO sources at eMbalenhle",
                                  k = 50,
                                  upper = 15,
                                  adjust_upper = TRUE,
                                  statistic = "mean")
EpolarcorPMCO <- gridExtra::grid.arrange(grobs = EpolarcorPMCO)


EpolarbandPM2.5 <- polar_heur(df = Epolar, wss = "ws", wdd = "wd",
                            poll = "pm2.5", n = 4, verbose = TRUE, pdf = FALSE, fn = NULL, title = "PM2.5 sources at different concentrations at eMbalenhle")


EpolarbandPM10 <- polar_heur(df = Epolar, wss = "ws", wdd = "wd",
                              poll = "pm10", n = 4, verbose = TRUE, pdf = FALSE, fn = NULL, title = "PM10 sources at different concentrations at eMbalenhle")

EpolarbandSO2 <- polar_heur(df = Epolar, wss = "ws", wdd = "wd",
                              poll = "so2", n = 4, verbose = TRUE, pdf = FALSE, fn = NULL, width = 10, title = "SO2 sources at different concentrations at eMbalenhle")

EpolarbandNO2 <- polar_heur(df = Epolar, wss = "ws", wdd = "wd",
                              poll = "no2", n = 4, verbose = TRUE, pdf = FALSE, fn = NULL, width = 10, title = "NO2 sources at different concentrations at eMbalenhle")

EpolarbandNOx <- polar_heur(df = Epolar, wss = "ws", wdd = "wd",
                              poll = "nox", n = 4, verbose = TRUE, pdf = FALSE, fn = NULL, width = 10, title = "NOx sources at different concentrations at eMbalenhle")

EpolarbandCO <- polar_heur(df = Epolar, wss = "ws", wdd = "wd",
                              poll = "co", n = 4, verbose = TRUE, pdf = FALSE, fn = NULL, width = 10, title = "CO sources at different concentrations at eMbalenhle")


summary(EAMS_clean)
# seasonal polar ----------------------------------------------------------


#magnetic_declination <- -20.74
#EsummerCor <- EAMS_clean %>%
#  datify() %>%
#  mutate(latitude = -26.56123474,
#         longitude = 29.08227378,
#         wd = case_when(wd - magnetic_declination > 360 ~ wd - magnetic_declination - 360,
#                        .default = wd - magnetic_declination)
#  )%>%
#  filter(season == "summer")

#ESummer_plots <- polarSummaryCor2(df = EsummerCor,
#                                 pol1 = "pm2.5",
#                                 pol2 = c("pm10", "so2"),
#                                 place = "eMbalenhle",
#                                 title = "Summer Correlation between  PM2.5, PM10, SO2",
#                                 k = 50,
#                                 upper = 15,
#                                 adjust_upper = TRUE,
#                                 statistic = "mean")
#ESummer_plots <- gridExtra::grid.arrange(grobs = ESummer_plots)


#EwinterCor <- EAMS_clean %>%
#  datify() %>%
#  mutate(latitude = -26.56123474,
#         longitude = 29.08227378,
#         wd = case_when(wd - magnetic_declination > 360 ~ wd - magnetic_declination - 360,
#                        .default = wd - magnetic_declination)
#  )%>%
#  filter(season == "winter")

#Ewinter_plots <- polarSummaryCor2(df = EwinterCor,
#                                  pol1 = "pm2.5",
#                                  pol2 = c("pm10", "so2"),
#                                  place = "eMbalenhle",
#                                  title = "Winter Correlation between  PM2.5, PM10, SO2",
#                                  k = 50,
#                                  upper = 15,
#                                  adjust_upper = TRUE,
#                                  statistic = "mean")
#Ewinter_plots <- gridExtra::grid.arrange(grobs = Ewinter_plots)


#EspringCor <- EAMS_clean %>%
#  datify() %>%
#  mutate(latitude = -26.56123474,
#         longitude = 29.08227378,
#         wd = case_when(wd - magnetic_declination > 360 ~ wd - magnetic_declination - 360,
#                        .default = wd - magnetic_declination)
#  )%>%
#  filter(season == "spring")

#Espring_plots <- polarSummaryCor2(df = EspringCor,
#                                  pol1 = "pm2.5",
#                                  pol2 = c("pm10", "so2"),
#                                  place = "eMbalenhle",
#                                  title = "spring Correlation between  PM2.5, PM10, SO2",
#                                  k = 50,
#                                  upper = 15,
#                                  adjust_upper = TRUE,
#                                  statistic = "mean")

#Espring_plots <- gridExtra::grid.arrange(grobs = Espring_plots)

#save(ESummer_plots, Ewinter_plots, Espring_plots,
#     file = "Report/eMbalenhle_summaryplots.Rda")



#Esummer_pm2.5 <- polar_heur(df = EsummerCor, wss = "ws", wdd = "wd",
#                            poll = "pm2.5", n = 4, verbose = TRUE, pdf = FALSE, fn = NULL)
#
#Ewinter_pm2.5 <- polar_heur(df = EwinterCor, wss = "ws", wdd = "wd",
#                            poll = "pm2.5", n = 4, verbose = TRUE, pdf = FALSE, fn = NULL)
#
#Espring_pm2.5 <- polar_heur(df = EspringCor, wss = "ws", wdd = "wd",
#                            poll = "pm2.5", n = 4, verbose = TRUE, pdf = FALSE, fn = NULL)


#save(Esummer_pm2.5, Ewinter_pm2.5, Espring_pm2.5,
#     file = "Report/eMbalenhle_pm2.5_cpfplots.Rda")

#Esummer_pm10 <- polar_heur(df = EsummerCor, wss = "ws", wdd = "wd",
#                           poll = "pm10", n = 4, verbose = TRUE, pdf = FALSE, fn = NULL)
#
#Ewinter_pm10 <- polar_heur(df = EwinterCor, wss = "ws", wdd = "wd",
#                           poll = "pm10", n = 4, verbose = TRUE, pdf = FALSE, fn = NULL)

#Espring_pm10 <- polar_heur(df = EspringCor, wss = "ws", wdd = "wd",
#                           poll = "pm10", n = 4, verbose = TRUE, pdf = FALSE, fn = NULL)
#save(Esummer_pm10, Ewinter_pm10, Espring_pm10,
#     file = "Report/eMbalenhle_pm10_cpfplots.Rda")



#Esummer_so2 <- polar_heur(df = EsummerCor, wss = "ws", wdd = "wd",
#                          poll = "so2", n = 4, verbose = TRUE, pdf = FALSE, fn = NULL)

#Ewinter_so2 <- polar_heur(df = EwinterCor, wss = "ws", wdd = "wd",
#                          poll = "so2", n = 4, verbose = TRUE, pdf = FALSE, fn = NULL)

#Espring_so2 <- polar_heur(df = EspringCor, wss = "ws", wdd = "wd",
#                          poll = "so2", n = 4, verbose = TRUE, pdf = FALSE, fn = NULL)

#save(Esummer_so2, Ewinter_so2, Espring_so2,
#     file = "Report/eMbalenhle_so2_cpfplots.Rda")



# Correlation -------------------------------------------------------------


ECOR <- EAMS_HourlyC %>%
  select(-date, -h2s, -co2, -no, -linev, -battv, -ws_wvt, -date2, -batt_diff, -month, -station)

Emba_hourlycor <- rcorr(as.matrix(ECOR), type = "pearson")
Embalenhle_hourlycor.coeff = Emba_hourlycor$r
Embalenhle_hourlycor.p = Emba_hourlycor$P
Embalenhle_hourlycor.coeff


Embalenhlehourlycorplot <- corrplot.mixed(Embalenhle_hourlycor.coeff, lower = 'number', upper = 'ellipse')



# Hysplit -----------------------------------------------------------------

dffeb <- Epolar %>%
  filter(month == "Feb")

trajMap(dffeb, longitude = "lon", latitude = "lat", colour = "date2")

# To CSV ---------------------------------------------------------------------

Embalenhle <- EAMS_Raw %>%
  select(-month, -yday, -season, -date2) %>%
  rename(temperature = temp,
         relhumd = relHum)

add_units <- function(col_name) {
  units <- switch(col_name,
                  so2 = "ppb",
                  h2s = "ppb",
                  no = "ppb",
                  linev = "v",
                  battv = "v",
                  no2 = "ppb",
                  nox = "ppb",
                  pm10 = "µg.m-3",
                  pm2.5 = "µg.m-3",
                  o3 = "ppb",
                  co = "ppb",
                  co2 = "ppb",
                  pressure = "mmhg",
                  ws = "m/s",
                  ws_wvt = "m/s",
                  wd = "deg",
                  temperature = "degc",
                  relhumd = "%",
                  NULL  # NULL to exclude columns without specified units
  )

  if (!is.null(units)) {
    return(paste(col_name, " (", units, ")", sep = ""))
  } else {
    return(col_name)
  }
}


# Apply the function to selected column names
selected_cols <- colnames(Embalenhle)[3:length(colnames(Embalenhle))]  # Exclude the first two columns
colnames(Embalenhle)[3:length(colnames(Embalenhle))] <- sapply(selected_cols, add_units)
write_csv(Embalenhle, file = "Report/Embalenhle-ams-level1.csv")





Lebohang <- EAMS_Raw %>%
  select(-month, -yday, -season, -date2) %>%
  rename(temperature = temp,
         relhumd = relHum)

selected_cols <- colnames(Lebohang)[3:length(colnames(Lebohang))]  # Exclude the first two columns
colnames(Lebohang)[3:length(colnames(Lebohang))] <- sapply(selected_cols, add_units)
write_csv(Lebohang, file = "Report/Lebohang-ams-level1.csv")









