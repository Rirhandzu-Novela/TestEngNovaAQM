# Load packages -----------------------------------------------------------

library(tidyverse)
library(openair)
library(novaAQM)
library(slider)
library(broom)
library(gridExtra)
library(corrplot)
library(Hmisc)


# Read dat files ----------------------------------------------------------

dr1 <- "Data/Lebohang_Raw/"
fn <- list.files(dr1)

ls_LAMS <- leesNWU(dr1)

names(ls_LAMS) <- fn

LAMS_Data <- tibble(filename = fn, data = ls_LAMS) %>%
  mutate(ncol = map_int(data, ncol),
         nrow = map_int(data, nrow),
         station = stringr::str_extract(filename, "[[:alpha:]]{8,10}"))

LAMS_Raw <- LAMS_Data %>%
  select(data) %>%
  unnest(data) %>%
  arrange(TIMESTAMP) %>%
  distinct() %>%
  select(TIMESTAMP, Amb_Press_Avg,LineV_Avg, SO2_Avg, CO_Avg,CO2_Avg, H2S_Avg,WS_ms_S_WVT, WS_ms_Max, WindDir_D1_WVT,NO_Avg, Batt_Volt_Avg ,NO2_Avg, NOX_Avg, PM10_Avg, PM25_Avg,
         O3_Avg, WSpeed_Avg, WDir_Avg, AirTC_Avg, RH) %>%
  rename(pressure = Amb_Press_Avg,
         so2 = SO2_Avg,
         co = CO_Avg,
         co2 = CO2_Avg,
         h2s = H2S_Avg,
         no = NO_Avg,
         no2 = NO2_Avg,
         nox = NOX_Avg,
         pm10 = PM10_Avg,
         pm2.5 = PM25_Avg,
         o3 = O3_Avg,
         ws_avg = WSpeed_Avg,
         ws =WS_ms_Max,
         ws_wvt = WS_ms_S_WVT,
         wd = WindDir_D1_WVT,
         wd_avg = WDir_Avg,
         temp = AirTC_Avg,
         relHum = RH,
        battv = Batt_Volt_Avg,
        linev = LineV_Avg,
         date = TIMESTAMP) %>%
  mutate(month = lubridate::month(date, label = TRUE),
         date2 = as.Date(date))%>%
  unite(ws, ws, ws_avg, sep = "_", na.rm = TRUE) %>%
  unite(wd, wd, wd_avg, sep = "_", na.rm = TRUE) %>%
  mutate(wd = as.numeric(gsub("~", "", wd))) %>%
  mutate(ws = as.numeric(gsub("~", "", ws)))

save(LAMS_Raw, file = "Report/LAMS_Raw.Rda")



# Plot time series --------------------------------------------------------

LAMS_HourlyR <- timeAverage(LAMS_Raw, avg.time = "hour", data.thresh = 20) %>%
  mutate(month = lubridate::month(date, label = TRUE),
         date2 = as.Date(date)) %>%
  mutate(station = all_of("Lebohang"))


LebohangPtimeseriesR <- timePlot(selectByDate(LAMS_HourlyR, year = 2023),
                                   pollutant = c("battv","linev", "pm2.5", "pm10", "no2", "nox", "so2"),
                                   y.relation = "free")

LebohangMtimeseriesR <- timePlot(selectByDate(LAMS_HourlyR, year = 2023),
                                   pollutant = c("battv", "linev", "ws", "wd", "temp", "relHum"),
                                   y.relation = "free")


# Clean Data --------------------------------------------------------------


#LAMS_clean <- LAMS_Raw %>%
#  mutate_at(vars(-date, -month, -date2), ~ifelse(. < 0, NA, .), ~ ifelse(is.infinite(.), NA, .)) %>%
#  mutate_at(vars(-date, -month, -date2), ~ifelse(battv > 12.9 | battv < 12.70, NA, .)) %>%
#  mutate(
#    pm2.5 = ifelse(pm2.5 > 1000, 1000, pm2.5),
#    pm10 = ifelse(pm10 > 1000, 1000, pm10),
#    o3 = ifelse(o3 < 1, 0.5, o3),
#    o3 = ifelse(o3 > 1000, 1000, o3),
#    so2 = ifelse(so2 < 2, 1, so2),
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


#  LAMS_clean <- LAMS_clean %>%
#    mutate(pm10 = case_when(pm10 > 500 ~ NA, .default = pm10)) %>%
#    mutate(pm10 = case_when(between(date, as.Date("2023-01-01"), as.Date("2023-02-04")) ~ NA, .default = pm10)) %>%
#    mutate(pm10 = case_when(between(date, as.Date("2023-02-09"), as.Date("2023-02-11")) ~ NA, .default = pm10)) %>%
#    mutate(pm10 = case_when(between(date, as.Date("2023-03-22"), as.Date("2023-03-28")) ~ NA, .default = pm10)) %>%
#    mutate(pm10 = case_when(between(date, as.Date("2023-12-03"), as.Date("2023-12-31")) ~ NA, .default = pm10)) %>%
#    mutate(pm2.5 = case_when(pm2.5 > 160 ~ NA, .default = pm2.5)) %>%
#    mutate(pm2.5 = case_when(between(date, as.Date("2023-01-01"), as.Date("2023-02-04")) ~ NA, .default = pm2.5)) %>%
#    mutate(pm2.5 = case_when(between(date, as.Date("2023-02-09"), as.Date("2023-02-11")) ~ NA, .default = pm2.5)) %>%
#    mutate(pm2.5 = case_when(between(date, as.Date("2023-12-02"), as.Date("2023-12-31")) ~ NA, .default = pm2.5)) %>%
#    mutate(so2 = case_when(between(date, as.Date("2023-02-20"), as.Date("2023-03-30")) ~ NA, .default = so2)) %>%
#    mutate(so2 = case_when(between(date, as.Date("2023-02-07"), as.Date("2023-02-09")) ~ NA, .default = so2)) %>%
#    mutate(so2 = case_when(between(date, as.Date("2023-01-01"), as.Date("2023-01-18")) ~ NA, .default = so2)) %>%
#    mutate(so2 = case_when(between(date, as.Date("2023-12-03"), as.Date("2023-12-31")) ~ NA, .default = so2)) %>%
#    mutate(ws = case_when(between(date, as.Date("2023-05-28"), as.Date("2023-08-15")) ~ NA, .default = ws)) %>%
#    mutate(so2 = case_when(so2 > 200 ~ NA, .default = so2)) %>%
#    mutate(nox = case_when(nox > 200 ~ NA, .default = no2)) %>%
#    mutate(across(where(is.numeric),
#                  ~ if_else(between(date, as.Date("2023-12-01"), as.Date("2023-12-31")), NA_real_, .)))%>%
#    mutate(across(where(is.numeric),
#                  ~ if_else(between(date, as.Date("2023-01-01"), as.Date("2023-01-31")), NA_real_, .)))%>%
#    mutate(station = all_of("Lebohang"))

LAMS_clean <- LAMS_Raw %>%
  mutate(across(where(is.numeric), ~case_when(. <= 0 ~ NA,
                                              is.infinite(.) ~ NA,
                                              .default = .))) %>%
  #mutate_at(vars(-date, -month, -date2), ~ifelse(battv > 12.8 | battv < 12.5, NA, .)) %>%
  # mutate(across(where(is.numeric), ~case_when(batt_diff > 0.02 | battv < 12.7 ~ NA, .default = .))) %>%
  mutate(batt_diff = slide_index_dbl(.x = battv, .i = date,  .f = ~{range(.x, na.rm = T)[2] - range(.x, na.rm = T)[1]}, .before = minutes(30), .after = minutes(30), .complete = F)) %>%
  mutate(pm10 = case_when(pm10 > 500 ~ NA, .default = pm10)) %>%
  mutate(pm10 = case_when(between(date, as.Date("2023-01-01"), as.Date("2023-02-04")) ~ NA, .default = pm10)) %>%
  mutate(pm10 = case_when(between(date, as.Date("2023-02-09"), as.Date("2023-02-11")) ~ NA, .default = pm10)) %>%
  mutate(pm10 = case_when(between(date, as.Date("2023-03-22"), as.Date("2023-03-28")) ~ NA, .default = pm10)) %>%
  mutate(pm10 = case_when(between(date, as.Date("2023-12-03"), as.Date("2023-12-31")) ~ NA, .default = pm10)) %>%
  mutate(pm2.5 = case_when(pm2.5 > 160 ~ NA, .default = pm2.5)) %>%
  mutate(pm2.5 = case_when(between(date, as.Date("2023-01-01"), as.Date("2023-02-04")) ~ NA, .default = pm2.5)) %>%
  mutate(pm2.5 = case_when(between(date, as.Date("2023-02-09"), as.Date("2023-02-11")) ~ NA, .default = pm2.5)) %>%
  mutate(pm2.5 = case_when(between(date, as.Date("2023-12-02"), as.Date("2023-12-31")) ~ NA, .default = pm2.5)) %>%
  mutate(so2 = case_when(between(date, as.Date("2023-02-20"), as.Date("2023-03-30")) ~ NA, .default = so2)) %>%
  mutate(so2 = case_when(between(date, as.Date("2023-01-01"), as.Date("2023-01-18")) ~ NA, .default = so2)) %>%
  mutate(so2 = case_when(between(date, as.Date("2023-12-03"), as.Date("2023-12-31")) ~ NA, .default = so2)) %>%
  mutate(ws = case_when(between(date, as.Date("2023-05-28"), as.Date("2023-08-15")) ~ NA, .default = ws)) %>%
  mutate(co = case_when(batt_diff > 0.02 | battv < 12.7 ~ NA, .default = co),
         co = case_when(between(date, as.Date("2023-04-01"), as.Date("2023-04-20")) ~ co, .default = NA),
         nox = case_when(batt_diff > 0.02 | battv < 12.7 ~ NA, .default = nox),
         nox = case_when(month(date) %in% c(1, 2, 3) ~ NA, .default = nox),
         no2 = case_when(batt_diff > 0.02 | battv < 12.7 ~ NA, .default = no2),
         no2 = case_when(month(date) %in% c(1, 2, 3) ~ NA, .default = no2)) %>%
  mutate(across(where(is.numeric),
                ~ if_else(between(date, as.Date("2023-12-01"), as.Date("2023-12-31")), NA_real_, .)))%>%
  mutate(across(where(is.numeric),
                ~ if_else(between(date, as.Date("2023-01-01"), as.Date("2023-02-02")), NA_real_, .)))%>%
  mutate(station = all_of("Lebohang"))

save(LAMS_clean, file = "Graph/LAMS_clean.Rda")

LAMS_HourlyC <- timeAverage(LAMS_clean, avg.time = "hour", data.thresh = 20) %>%
  mutate(month = lubridate::month(date, label = TRUE),
         date2 = as.Date(date)) %>%
  mutate(station = all_of("Lebohang"))


LebohangPtimeseriesC <- timePlot(selectByDate(LAMS_HourlyC, year = 2023),
                                 pollutant = c("battv","linev", "pm2.5", "pm10", "no2", "nox", "so2"),
                                 y.relation = "free")

LebohangMtimeseriesC <- timePlot(selectByDate(LAMS_HourlyC, year = 2023),
                                 pollutant = c("battv", "linev", "ws", "wd", "temp", "relHum"),
                                 y.relation = "free")

save(LebohangPtimeseriesC, LebohangMtimeseriesC, file = "Graph/LAMS_Timeseriesplot.Rda")

# Summary AMS -------------------------------------------------------------


LAMS_clean_summary <- LAMS_clean %>%
  select(date, date2, so2, no, nox, no2, co, pm10, o3, pm2.5, ws, wd, temp, battv, pressure, relHum,  month) %>%
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
    variable == "battv" ~ "v",
    variable == "so2" ~ "ppb",
    variable == "pressure" ~ "mmhg",
    variable == "relHum" ~ "perc",
    TRUE ~ NA_character_
  )) %>%
  relocate(unit, .after = variable)%>%
  mutate(station = all_of("Lebohang"))


LAMS_summary <- tabDataVasleggingDB(df = LAMS_clean_summary %>%
                      mutate(avg_period = "1 min",
                             unit = str_replace(unit, "ug/m3", "µg.m-3"),
                             instrument = "AMS",
                             place = station),
                    begin = min(LAMS_clean_summary$date),
                    end = max(LAMS_clean_summary$date)) %>%
  relocate(firstObs, lastObs, .before = nobs) %>%
  relocate(variable, .before = place) %>%
  arrange(variable, place)

save(LAMS_summary, file = "Graph/LAMS_summary.Rda")

# AMS Averages and exceedances --------------------------------------------


LAMS_date <- LAMS_HourlyC %>%
  select(date,  month, station, pm2.5, o3, no2, no, nox, so2, pm10) %>%
  pivot_longer(cols = c(pm2.5, o3, no2, no, nox, so2, pm10), names_to = "variable") %>%
  mutate(unit = case_when(
    variable == "pm2.5" ~ "Âµg.m-3",
    variable == "o3" ~ "ppb",
    variable == "no" ~ "ppb",
    variable == "no2" ~ "ppb",
    variable == "nox" ~ "ppb",
    variable == "pm10" ~ "Âµg.m-3",
    variable == "so2" ~ "ppb",
    TRUE ~ NA_character_
  ))

LAMS_monthly_hour_ex <- novaAQM::compareAQS(df = LAMS_date %>%
                                              ungroup() %>%
                                              datify() %>%
                                              mutate(place = station,
                                                     instrument = "AMS"),
                                            period = "hour",
                                            by_period = quos(month, year)) %>%
  ungroup() %>%
  arrange(pollutant, month) %>%
  relocate(pollutant, .after = place)

LAMS_season_hour_ex <- novaAQM::compareAQS(df = LAMS_date %>%
                                             ungroup() %>%
                                             datify() %>%
                                             mutate(place = station,
                                                    instrument = "AMS"),
                                           period = "hour",
                                           by_period = quos(season, year)) %>%
  ungroup() %>%
  arrange(pollutant, season) %>%
  relocate(pollutant, .after = place)
season_order = tibble(season = c("autum", "winter", "spring", "summer"), season_nr = c(1, 2, 3, 4))
LAMS_season_hour_ex <- LAMS_season_hour_ex %>% left_join(season_order) %>% arrange(pollutant, season_nr)

LAMS_annual_hour_ex <- novaAQM::compareAQS(df = LAMS_date %>%
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

LAMS_DailyC <- timeAverage(LAMS_HourlyC, avg.time = "day", data.thresh = 20) %>%
  mutate(month = lubridate::month(date, label = TRUE),
         yday = lubridate::yday(date),
         date2 = as.Date(date)) %>%
  mutate(station = all_of("Lebohang"))


LAMS_Day  <- LAMS_DailyC %>%
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

LAMS_month_daily_ex <- novaAQM::compareAQS(df = LAMS_Day %>%
                                             ungroup() %>%
                                             datify() %>%
                                             mutate(place = station,
                                                    instrument = "AMS"),
                                           period = "day",
                                           by_period = quos(year, month)) %>%
  #ungroup() %>%
  arrange(pollutant, month)



LAMS_season_daily_ex <- novaAQM::compareAQS(df = LAMS_Day %>%
                                              ungroup() %>%
                                              datify() %>%
                                              mutate(place = station,
                                                     instrument = "AMS"),
                                            period = "day",
                                            by_period = quos(year, season)) %>%
  #ungroup() %>%
  arrange(pollutant, season)

season_order = tibble(season = c("autum", "winter", "spring", "summer"), season_nr = c(1, 2, 3, 4))
LAMS_season_daily_ex <- LAMS_season_daily_ex %>% left_join(season_order) %>% arrange(pollutant, season_nr)

LAMS_annual_daily_ex <- novaAQM::compareAQS(df = LAMS_Day %>%
                                              ungroup() %>%
                                              datify() %>%
                                              mutate(place = station,
                                                     instrument = "AMS"),
                                            period = "day",
                                            by_period = quos(year)) %>%
  #ungroup() %>%
  arrange(pollutant, year)

save(LAMS_monthly_hour_ex, LAMS_season_hour_ex, LAMS_month_daily_ex, LAMS_season_daily_ex,
     LAMS_annual_hour_ex, LAMS_annual_daily_ex, file = "Graph/Lebohang_Exceedances.Rda")



# Density plots -----------------------------------------------------------


LDensitypm2.5 <- LAMS_clean %>%
  ggplot(aes(x = log(pm2.5), color = station, fill = station)) +
  geom_histogram(aes(y=..density..), position="identity")+
  geom_density(alpha=.2) +
  labs(title= "PM2.5 density curve")
LDensitypm2.5

LDensitypm10 <- LAMS_clean %>%
  ggplot(aes(x = log(pm10), color = station, fill = station)) +
  geom_histogram(aes(y=..density..), position="identity")+
  geom_density(alpha=.2) +
  labs(title= "PM10 density curve")
LDensitypm10


LDensityno2 <- LAMS_clean %>%
  ggplot(aes(x = log(no2), color = station, fill = station)) +
  geom_histogram(aes(y=..density..), position="identity")+
  geom_density(alpha=.2) +
  labs(title= "NO2 density curve")
LDensityno2

LDensityso2 <- LAMS_clean %>%
  ggplot(aes(x = log(so2), color = station, fill = station)) +
  geom_histogram(aes(y=..density..), position="identity")+
  geom_density(alpha=.2) +
  labs(title= "SO2 density curve")
LDensityso2

LDensitynox <- LAMS_clean %>%
  ggplot(aes(x = log(nox), color = station, fill = station)) +
  geom_histogram(aes(y=..density..), position="identity")+
  geom_density(alpha=.2) +
  labs(title= "NOX density curve")
LDensitynox

LDensityco <- LAMS_clean %>%
  ggplot(aes(x = log(co), color = station, fill = station)) +
  geom_histogram(aes(y=..density..), position="identity")+
  geom_density(alpha=.2) +
  labs(title= "CO density curve")
LDensityco

save(LDensitypm2.5, LDensitypm10, LDensityco,
     LDensityso2, LDensityno2, LDensitynox,
     file = "Graph/LAMS_densityplot.Rda")



# Line plot ---------------------------------------------------------------

LebdailyPM10Compare <- ggplot(data = LAMS_DailyC, aes(x = date2, y = pm10, group = station, colour = station)) +
  geom_line() +
  geom_hline(yintercept = 75, linetype = "dashed", color = "black") +
  theme_classic() +
  labs(
    x = "Date",
    y = "pm10",
    color = "station",
    title = "Daily PM10")
LebdailyPM10Compare

LebdailyPM2.5Compare <- ggplot(data = LAMS_DailyC, aes(x = date2, y = pm2.5, group = station, colour = station)) +
  geom_line() +
  geom_hline(yintercept = 40, linetype = "dashed", color = "black") +
  theme_classic() +
  labs(
    x = "Date",
    y = "pm2.5",
    color = "station",
    title = "Daily PM2.5")
LebdailyPM2.5Compare

LebdailySO2Compare <- ggplot(data = LAMS_DailyC, aes(x = date2, y = so2, group = station, colour = station)) +
  geom_line() +
  geom_hline(yintercept = 48, linetype = "dashed", color = "black") +
  theme_classic() +
  labs(
    x = "Date",
    y = "so2",
    color = "station",
    title = "Daily SO2")
LebdailySO2Compare

LebdailynOxCompare <- ggplot(data = LAMS_DailyC, aes(x = date2, y = nox, group = station, colour = station)) +
  geom_line() +
  theme_classic() +
  labs(
    x = "Date",
    y = "Nox",
    color = "station",
    title = "Daily NOx")
LebdailynOxCompare

save(LebdailyPM10Compare, LebdailyPM2.5Compare,
     LebdailySO2Compare,LebdailynOxCompare,
     file = "Report/LAMS_dailyplot.Rda")


# Polar Plot ---------------------------------------------------------------


magnetic_declination <- -20.74

Lpolar <- LAMS_clean %>%
datify() %>%
  mutate(latitude = -26.38097458,
         longitude = 28.93727302,
         wd = case_when(wd - magnetic_declination > 360 ~ wd - magnetic_declination - 360,
                        .default = wd - magnetic_declination)
  )%>%
  filter(ws <= 15)

allpolar <- polarMap(
  Lpolar,
  latitude = "latitude",
  longitude = "longitude",
  pollutant = c("nox", "no2", "pm2.5", "co", "pm10", "so2"),
  provider = c("Satellite" = "Esri.WorldImagery")
)


LpolarstatsPM10 <- polarSummary(mydata = Lpolar, stats = c("frequency", "mean", "weighted.mean", "stdev"),
                                pdf = TRUE,
                                fn = "Graph/LpolarstatsPM10.pdf",
                                tp = "PM10 Polar summary at Lebohang",
                                width = 10,
                                verbose = TRUE,
                                upper = 15,
                                adjust_upper = TRUE,
                                ncol = 2,
                                nrow = 2,
                                style = "polarPlot",
                                pollutant = "pm10")
grid.arrange(LpolarstatsPM10)
dev.off()
plot(LpolarstatsPM10)


LpolarstatsPM2.5 <- polarSummary(mydata = Lpolar, stats = c("frequency", "mean", "weighted.mean", "stdev"),
                                 pdf = TRUE,
                                 fn = "Graph/LpolarstatsPM2.5.pdf",
                                 tp = "PM2.5 Polar summary at Lebohang",
                                 width = 10,
                                 verbose = TRUE,
                                 upper = 15,
                                 adjust_upper = TRUE,
                                 ncol = 2,
                                 nrow = 2,
                                 style = "polarPlot",
                                 pollutant = "pm2.5")

grid.arrange(LpolarstatsPM2.5)
dev.off()
plot(LpolarstatsPM2.5)

Lpolarstatsso2 <- polarSummary(mydata = Lpolar, stats = c("frequency", "mean", "weighted.mean", "stdev"),
                               pdf = TRUE,
                               fn = "Graph/Lpolarstatsso2.pdf",
                               tp = "SO2 Polar summary at Lebohang",
                               width = 10,
                               verbose = TRUE,
                               upper = 15,
                               adjust_upper = TRUE,
                               ncol = 2,
                               nrow = 2,
                               style = "polarPlot",
                               pollutant = "so2")

grid.arrange(Lpolarstatsso2)
dev.off()
plot(Lpolarstatsso2)

Lpolarstatsnox <- polarSummary(mydata = Lpolar, stats = c("frequency", "mean", "weighted.mean", "stdev"),
                               pdf = TRUE,
                               fn = "Graph/Lpolarstatsnox.pdf",
                               tp = "NOx Polar summary at Lebohang",
                               width = 10,
                               verbose = TRUE,
                               upper = 15,
                               adjust_upper = TRUE,
                               ncol = 2,
                               nrow = 2,
                               style = "polarPlot",
                               pollutant = "nox")
grid.arrange(Lpolarstatsnox)
dev.off()
plot(Lpolarstatsnox)

Lpolarstatsno2 <- polarSummary(mydata = Lpolar, stats = c("frequency", "mean", "weighted.mean", "stdev"),
                               pdf = TRUE,
                               fn = "Graph/Lpolarstatsno2.pdf",
                               tp = "NO2 Polar summary at Lebohang",
                               width = 10,
                               verbose = TRUE,
                               upper = 15,
                               adjust_upper = TRUE,
                               ncol = 2,
                               nrow = 2,
                               style = "polarPlot",
                               pollutant = "no2")
grid.arrange(Lpolarstatsno2)
dev.off()
plot(Lpolarstatsno2)

Lpolarstatsco <- polarSummary(mydata = Lpolar, stats = c("frequency", "mean", "weighted.mean", "stdev"),
                              pdf = TRUE,
                              fn = "Graph/Lpolarstatsco.pdf",
                              tp = "co Polar summary at Lebohang",
                              width = 10,
                              verbose = TRUE,
                              upper = 15,
                              adjust_upper = TRUE,
                              ncol = 2,
                              nrow = 2,
                              style = "polarPlot",
                              pollutant = "co")

grid.arrange(Lpolarstatsco)
dev.off()
plot(Lpolarstatsco)


LpolarcorPM <- polarSummaryCor2(df = Lpolar,
                                pol1 = "pm2.5",
                                pol2 = "pm10",
                                place = "Lebohang",
                                title = "Correlation between  PM2.5 and PM10 sources at Lebohang",
                                k = 50,
                                upper = 15,
                                adjust_upper = TRUE,
                                statistic = "mean")
LpolarcorPM <- gridExtra::grid.arrange(grobs = LpolarcorPM)


LpolarcorPMSO <- polarSummaryCor2(df = Lpolar,
                                  pol1 = "pm2.5",
                                  pol2 = "so2",
                                  place = "Lebohang",
                                  title = "Correlation between  PM2.5 and SO2 sources at Lebohang",
                                  k = 50,
                                  upper = 15,
                                  adjust_upper = TRUE,
                                  statistic = "mean")
LpolarcorPMSO <- gridExtra::grid.arrange(grobs = LpolarcorPMSO)

LpolarcorPMNO <- polarSummaryCor2(df = Lpolar,
                                  pol1 = "pm2.5",
                                  pol2 = "no2",
                                  place = "Lebohang",
                                  title = "Correlation between  PM2.5 and NO2 sources at Lebohang",
                                  k = 50,
                                  upper = 15,
                                  adjust_upper = TRUE,
                                  statistic = "mean")
LpolarcorPMNO <- gridExtra::grid.arrange(grobs = LpolarcorPMNO)

LpolarcorPMNX <- polarSummaryCor2(df = Lpolar,
                                  pol1 = "pm2.5",
                                  pol2 = "nox",
                                  place = "Lebohang",
                                  title = "Correlation between  PM2.5 and NOX sources at Lebohang",
                                  k = 50,
                                  upper = 15,
                                  adjust_upper = TRUE,
                                  statistic = "mean")
LpolarcorPMNX <- gridExtra::grid.arrange(grobs = LpolarcorPMNX)

LpolarcorPMCO <- polarSummaryCor2(df = Lpolar,
                                  pol1 = "pm2.5",
                                  pol2 = "co",
                                  place = "Lebohang",
                                  title = "Correlation between  PM2.5 and CO sources at Lebohang",
                                  k = 50,
                                  upper = 15,
                                  adjust_upper = TRUE,
                                  statistic = "mean")
LpolarcorPMCO <- gridExtra::grid.arrange(grobs = LpolarcorPMCO)


LpolarbandPM2.5 <- polar_heur(df = Lpolar, wss = "ws", wdd = "wd",
                              poll = "pm2.5", n = 4, verbose = TRUE, pdf = FALSE, fn = NULL, title = "PM2.5 sources at different concentrations at Lebohang")

LpolarbandPM10 <- polar_heur(df = Lpolar, wss = "ws", wdd = "wd",
                             poll = "pm10", n = 4, verbose = TRUE, pdf = FALSE, fn = NULL,  title = "PM10 sources at different concentrations at Lebohang")
LpolarbandSO2 <- polar_heur(df = Lpolar, wss = "ws", wdd = "wd",
                            poll = "so2", n = 4, verbose = TRUE, pdf = FALSE, fn = NULL,  title = "SO2 sources at different concentrations at Lebohang")
LpolarbandNO2 <- polar_heur(df = Lpolar, wss = "ws", wdd = "wd",
                            poll = "no2", n = 4, verbose = TRUE, pdf = FALSE, fn = NULL, title = "NO2 sources at different concentrations at Lebohang")
LpolarbandNOx <- polar_heur(df = Lpolar, wss = "ws", wdd = "wd",
                            poll = "nox", n = 4, verbose = TRUE, pdf = FALSE, fn = NULL,  title = "NOx sources at different concentrations at Lebohang")
LpolarbandCO <- polar_heur(df = Lpolar, wss = "ws", wdd = "wd",
                           poll = "co", n = 4, verbose = TRUE, pdf = FALSE, fn = NULL, title = "CO sources at different concentrations at Lebohang")


# Boxplots ----------------------------------------------------------------


BoxPM2.5Compare <- ggplot(data = LAMS_DailyC %>%
                            select(pm2.5, month) %>%
                            mutate(n = n(), .by = month),
                       aes(x = month, y = pm2.5 )) +
  geom_boxplot(aes(fill = n)) +
  geom_hline(yintercept = 40, linetype = "dashed", color = "red") +
  theme_bw() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(
    x = "Month",
    y = "PM2.5",
    title = "Monthly statistical summary of PM2.5 at Lebohang ",
    caption = "Data from AMS") +
  theme(legend.position = "none")

BoxPM2.5Compare

BoxPM10Compare <- ggplot(data = LAMS_DailyC %>%
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
    title = "Monthly statistical summary of PM10 at Lebohang ",
    caption = "Data from AMS") +
  theme(legend.position = "none")

BoxPM10Compare

Boxso2Compare <- ggplot(data = LAMS_DailyC %>%
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
    title = "Monthly statistical summary of SO2 at Lebohang ",
    caption = "Data from AMS") +
  theme(legend.position = "none")

Boxso2Compare

Boxno2Compare <- ggplot(data = LAMS_DailyC %>%
                          select(no2, month) %>%
                          mutate(n = n(), .by = month),
                         aes(x = month, y = no2 )) +
  geom_boxplot(aes(fill = n)) +
  geom_hline(yintercept = 48, linetype = "dashed", color = "red") +
  theme_bw() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(
    x = "Month",
    y = "NO2",
    title = "Monthly statistical summary of NO2 at Lebohang",
    caption = "Data from AMS") +
  theme(legend.position = "none")

Boxno2Compare

BoxnoxCompare <- ggplot(data = LAMS_DailyC %>%
                          select(nox, month) %>%
                          mutate(n = n(), .by = month),
                         aes(x = month, y = nox )) +
  geom_boxplot(aes(fill = n)) +
  theme_bw() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(
    x = "Month",
    y = "NOx",
    title = "Monthly statistical summary of NOx at Lebohang ",
    caption = "Data from AMS") +
  theme(legend.position = "none")

BoxnoxCompare

BoxcoCompare <- ggplot(data = LAMS_DailyC %>%
                         select(co, month) %>%
                         mutate(n = n(), .by = month),
                        aes(x = month, y = co )) +
  geom_boxplot(aes(fill = n)) +
  theme_bw() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(
    x = "Month",
    y = "CO",
    title = "Monthly statistical summary of CO at Lebohang ",
    caption = "Data from AMS") +
  theme(legend.position = "none")

BoxcoCompare

save(BoxPM2.5Compare,
     BoxPM10Compare,
     Boxso2Compare,
     Boxno2Compare,
     BoxnoxCompare,
     BoxcoCompare,
     file = "Graph/LBox_plot.Rda")

LPM10Tempplot <- timeVariation(LAMS_clean, stati="median", poll="pm10", conf.int = c(0.75, 0.99),
                               col = "firebrick", normalise = TRUE)
LPM2.5Tempplot <- timeVariation(LAMS_clean, stati="median", poll="pm2.5", conf.int = c(0.75, 0.99),
                                col = "firebrick", normalise = TRUE)

LSO2Tempplot <- timeVariation(LAMS_clean, stati="median", poll="so2", conf.int = c(0.75, 0.99),
                              col = "firebrick", normalise = TRUE)
LNO2Tempplot <- timeVariation(LAMS_clean, stati="median", poll="no2", conf.int = c(0.75, 0.99),
                              col = "firebrick", normalise = TRUE)
LNOxTempplot <- timeVariation(LAMS_clean, stati="median", poll="nox", conf.int = c(0.75, 0.99),
                              col = "firebrick", normalise = TRUE)

LCOTempplot <- timeVariation(LAMS_clean, stati="median", poll="co", conf.int = c(0.75, 0.99),
                             col = "firebrick", normalise = TRUE)

Ltempplot <- timeVariation(LAMS_clean,
                           pollutant = c("pm2.5", "pm10", "nox", "no2", "co", "so2"),
                           normalise = TRUE)


save(LPM10Tempplot,
     LPM2.5Tempplot,
     LSO2Tempplot,
     LNO2Tempplot,
     LNOxTempplot,
     LCOTempplot,
     Ltempplot,
     file = "Graph/LTempoaral_plot.Rda")



# correlation -------------------------------------------------------------


LCOR <- LAMS_HourlyC %>%
  select(-date, -h2s, -co2, -co, -no, -pressure, -linev, -battv, -ws_wvt, -date2, -batt_diff, -month, -station) %>%
  relocate(starts_with("w"), .after = relHum)

Leb_hourlycor <- rcorr(as.matrix(LCOR), type = "pearson")
Lebohang_hourlycor.coeff = Leb_hourlycor$r
Lebohang_hourlycor.p = Leb_hourlycor$P
Lebohang_hourlycor.coeff


Lebohanghourlycorplot <- corrplot.mixed(Lebohang_hourlycor.coeff, lower = 'number', upper = 'ellipse')

