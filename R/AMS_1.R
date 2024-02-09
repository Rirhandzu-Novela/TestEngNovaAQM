# Load packages -----------------------------------------------------------

library(tidyverse)
library(openair)
library(novaAQM)


# Read AMS files ---------------------------------------------------------------

fls <- list.files("Data/AMS", pattern = ".csv", full.names = TRUE)

patt <- "Data/AMS/(Embalenhle|Lebohang)(-ams-level1.csv|-ams-level1.csv)"

dfAMS_Raw <- map(fls, ~{
  place <- unique(gsub(patt, "\\1", .))
  readr::read_csv(file = .) %>%
    pivot_longer(cols = -date) %>%
    tidyr::separate(col = name, into = c("variable", "unit"), sep = " ") %>%
    mutate(place = all_of(place)) %>%
    mutate(date = lubridate::`tz<-`(date, "Africa/Johannesburg"))%>%
    mutate(station = case_when(
      place == "Embalenhle" ~ "eMbalenhle_NWU",
      place == "Lebohang" ~ "Lebohang_NWU"))
}
)%>%
  dplyr::bind_rows() %>%
  mutate(month = lubridate::month(date, label = TRUE),
         yday = lubridate::yday(date),
         season = case_when(yday < 151 ~ "summer", TRUE ~ "winter"),
         date2 = as.Date(date)) %>%
  filter(!(unit == "(ppb)" & variable == "CO")) %>%
  pivot_wider(id_cols = c(date, place, station, season, month, yday, date2),
              names_from = c(variable), values_from = value) %>%
  select(-AirTemperature, -Battery_Voltage, -Line_Voltage, -Logger_Temperature, -Wind_Direction_Std, -Wind_Speed_Max, -Solar_Radiation, -Pm10, -Line) %>%
  setNames(tolower(colnames(.))) %>%
  rename(at = temperature,
         rh = relative_humidity,
         ws = wind_speed,
         wd = wind_direction,
         bp = pressure)


# Clean AMS ----------------------------------------------------------------

dfAMS_clean <- dfAMS_Raw %>%
  mutate_at(vars(-date, -month, -date2), ~ifelse(. < 0, NA, .), ~ ifelse(is.infinite(.), NA, .)) %>%
  mutate(
    pm2.5 = ifelse(pm2.5 > 1000, 1000, pm2.5),
    pm10 = ifelse(pm10 > 1000, 1000, pm10),
    co = ifelse(co < 0.04, 0.02, co),
    co = ifelse(co > 1, 1, co),
    o3 = ifelse(o3 < 1, 0.5, o3),
    o3 = ifelse(o3 > 1000, 1000, o3),
    so2 = ifelse(so2 < 2, 1, so2),
    so2 = ifelse(so2 > 1000, 1000, so2),
    h2s = ifelse(h2s < 2, 1, h2s),
    h2s = ifelse(h2s > 1000, 1000, h2s),
    no = ifelse(no < 0.4, 0.2, no),
    no = ifelse(no > 1000, 1000, no),
    no2 = ifelse(no2 < 0.4, 0.2, no2),
    no2 = ifelse(no2 > 1000, 1000, no2),
    nox = ifelse(nox < 0.4, 0.2, nox),
    nox = ifelse(nox > 1000, 1000, nox))



# Summary AMS -------------------------------------------------------------


dfAMSPOL_clean_summary <- dfAMS_clean %>%
  select(date, date2, place, co, so2, no, nox, no2, h2s, pm10, o3, pm2.5, ws, wd, at, rainfall, bp, bc, station, month, yday, season) %>%
  pivot_longer(cols = co:bc, names_to = "variable") %>%
  mutate(unit = case_when(
    variable == "pm2.5" ~ "Âµg.m-3",
    variable == "ws" ~ "m/s",
    variable == "wd" ~ "Deg",
    variable == "at" ~ "C",
    variable == "o3" ~ "ppb",
    variable == "no" ~ "ppb",
    variable == "no2" ~ "ppb",
    variable == "nox" ~ "ppb",
    variable == "h2s" ~ "ppb",
    variable == "pm10" ~ "Âµg.m-3",
    variable == "co" ~ "ppm",
    variable == "rainfall" ~ "mm",
    variable == "so2" ~ "ppb",
    variable == "bp" ~ "mmhg",
    variable == "bc" ~ "Âµg.m-3",
    TRUE ~ NA_character_
  )) %>%
  relocate(unit, .after = variable) %>%
  filter(variable == c("co", "so2", "no", "nox", "no2", "h2s", "pm10", "o3", "pm2.5")) %>%
  dplyr::summarize(
    novaAQM::tenpointsummary(value) , .by = c(station, variable)
  ) %>%
  drop_na() %>%
  arrange(station)


dfAMSMET_clean_summary <- dfAMS_clean %>%
  select(date, date2, place, co, so2, no, nox, no2, h2s, pm10, o3, pm2.5, ws, wd, at, rainfall, bp, bc, station, month, yday, season) %>%
  pivot_longer(cols = co:bc, names_to = "variable") %>%
  mutate(unit = case_when(
    variable == "pm2.5" ~ "Âµg.m-3",
    variable == "ws" ~ "m/s",
    variable == "wd" ~ "Deg",
    variable == "at" ~ "C",
    variable == "o3" ~ "ppb",
    variable == "no" ~ "ppb",
    variable == "no2" ~ "ppb",
    variable == "nox" ~ "ppb",
    variable == "h2s" ~ "ppb",
    variable == "pm10" ~ "Âµg.m-3",
    variable == "co" ~ "ppm",
    variable == "rainfall" ~ "mm",
    variable == "so2" ~ "ppb",
    variable == "bp" ~ "mmhg",
    variable == "bc" ~ "Âµg.m-3",
    TRUE ~ NA_character_
  )) %>%
  relocate(unit, .after = variable) %>%
  filter(variable == c("ws", "wd", "at", "rainfall", "bp", "bc")) %>%
  dplyr::summarize(
    novaAQM::tenpointsummary(value) , .by = c(station, variable)
  ) %>%
  drop_na() %>%
  arrange(station)



# AMS Averages and exceedances --------------------------------------------


dfAMS_Hourly <- dfAMS_clean %>%
  mutate(hod = lubridate::hour(date)) %>%
  group_by(place, station, season, month, yday, date2, hod) %>%
  summarise(perc = length(na.omit(pm2.5)) / length(pm2.5)  * 100 ,
            pm2.5 = mean(pm2.5, na.rm = TRUE),
            ws = mean(ws, na.rm = TRUE),
            wd = mean(wd, na.rm = TRUE),
            at = mean(at, na.rm = TRUE),
            o3 = mean(o3, na.rm = TRUE),
            no = mean(no, na.rm = TRUE),
            nox = mean(nox, na.rm = TRUE),
            h2s = mean(h2s, na.rm = TRUE),
            no2 = mean(no2, na.rm = TRUE),
            pm10 = mean(pm10, na.rm = TRUE),
            rainfall = mean(rainfall, na.rm = TRUE),
            so2 = mean(so2, na.rm = TRUE),
            bp = mean(bp, na.rm = TRUE),
            bc= mean(bc, na.rm = TRUE),
            co = mean(co, na.rm = TRUE),

  ) %>%
  arrange(place, date2, hod) %>%
  mutate(DateTime = ymd_h(paste(date2, hod)))

dfAMS_date <- dfAMS_Hourly %>% rename(date = DateTime) %>%
  select(date,place,  month, season, station, pm2.5, o3, co, no2, no, nox, so2, h2s, pm10) %>%
  pivot_longer(cols = c(pm2.5, o3, co, no2, no, nox, so2, h2s, pm10), names_to = "variable") %>%
  mutate(unit = case_when(
    variable == "co" ~ "ppm",
    variable == "pm2.5" ~ "Âµg.m-3",
    variable == "o3" ~ "ppb",
    variable == "no" ~ "ppb",
    variable == "no2" ~ "ppb",
    variable == "nox" ~ "ppb",
    variable == "h2s" ~ "ppb",
    variable == "pm10" ~ "Âµg.m-3",
    variable == "so2" ~ "ppb",
    TRUE ~ NA_character_
  ))

dfAMS_hour_ex <- novaAQM::compareAQS(df = dfAMS_date %>%
                                       ungroup() %>%
                                       datify() %>%
                                       mutate(place = station,
                                              instrument = "AMS"),
                                     period = "hour",
                                     by_period = quos(year)) %>%
  ungroup() %>%
  arrange(pollutant, year) %>%
  relocate(pollutant, .after = place)
dfAMS_hour_ex



# Daily averages

dfAMS_Daily <- dfAMS_clean %>%
  group_by(place, station, season, month, yday, date2) %>%
  summarise(perc = length(na.omit(pm2.5)) / length(pm2.5)  * 100 ,
            pm2.5 = mean(pm2.5, na.rm = TRUE),
            ws = mean(ws, na.rm = TRUE),
            wd = mean(wd, na.rm = TRUE),
            at = mean(at, na.rm = TRUE),
            o3 = mean(o3, na.rm = TRUE),
            no = mean(no, na.rm = TRUE),
            nox = mean(nox, na.rm = TRUE),
            h2s = mean(h2s, na.rm = TRUE),
            no2 = mean(no2, na.rm = TRUE),
            pm10 = mean(pm10, na.rm = TRUE),
            rainfall = mean(rainfall, na.rm = TRUE),
            so2 = mean(so2, na.rm = TRUE),
            bp = mean(bp, na.rm = TRUE),
            bc= mean(bc, na.rm = TRUE),
            co = mean(co, na.rm = TRUE)) %>%
  arrange(place, yday, month) %>%
  mutate(date = ymd(paste(date2))) %>%
  relocate(date, .before = place)


dfAMS_Day  <- dfAMS_Daily %>%
  select(date, month, place, season, station, pm2.5, o3, co, no2, no, nox, so2, h2s, pm10) %>%
  pivot_longer(cols = c(pm2.5, o3, co, no2, no, nox, so2, h2s, pm10), names_to = "variable") %>%
  mutate(unit = case_when(
    variable == "co" ~ "ppm",
    variable == "pm10" ~ "Âµg.m-3",
    variable == "pm2.5" ~ "Âµg.m-3",
    variable == "o3" ~ "ppb",
    variable == "so2" ~ "ppb",
    variable == "no" ~ "ppb",
    variable == "no2" ~ "ppb",
    variable == "nox" ~ "ppb",
    variable == "h2s" ~ "ppb",
    TRUE ~ NA_character_
  ))

dfAMS_daily_ex <- novaAQM::compareAQS(df = dfAMS_Day %>%
                                        ungroup() %>%
                                        datify() %>%
                                        mutate(place = station,
                                               instrument = "AMS"),
                                      period = "day",
                                      by_period = quos(year)) %>%
  #ungroup() %>%
  arrange(pollutant, year)

dfAMS_daily_ex


# Density plots -----------------------------------------------------------


Densitypm2.5 <- dfAMS_clean %>%
  pivot_wider(id_cols = c(date, place, season, month, yday, date2),
              names_from = c(variable), values_from = value) %>%
  ggplot(aes(x = log(pm2.5), color = place, fill = place)) +
  geom_histogram(aes(y=..density..), position="identity")+
  geom_density(alpha=.2) +
  labs(title= "PM2.5 density curve")
Densitypm2.5

Densitypm10 <- dfAMS_clean %>%
  pivot_wider(id_cols = c(date, place, season, month, yday, date2),
              names_from = c(variable), values_from = value) %>%
  ggplot(aes(x = log(pm10), color = place, fill = place)) +
  geom_histogram(aes(y=..density..), position="identity")+
  geom_density(alpha=.2) +
  labs(title= "PM10 density curve")
Densitypm10

Densityco <- dfAMS_clean %>%
  pivot_wider(id_cols = c(date, place, season, month, yday, date2),
              names_from = c(variable), values_from = value) %>%
  ggplot(aes(x = log(co), color = place, fill = place)) +
  geom_histogram(aes(y=..density..), position="identity")+
  geom_density(alpha=.2) +
  labs(title= "CO density curve")
Densityco

Densityno2 <- dfAMS_clean %>%
  pivot_wider(id_cols = c(date, place, season, month, yday, date2),
              names_from = c(variable), values_from = value) %>%
  ggplot(aes(x = log(no2), color = place, fill = place)) +
  geom_histogram(aes(y=..density..), position="identity")+
  geom_density(alpha=.2) +
  labs(title= "NO2 density curve")
Densityno2

Densityso2 <- dfAMS_clean %>%
  pivot_wider(id_cols = c(date, place, season, month, yday, date2),
              names_from = c(variable), values_from = value) %>%
  ggplot(aes(x = log(so2), color = place, fill = place)) +
  geom_histogram(aes(y=..density..), position="identity")+
  geom_density(alpha=.2) +
  labs(title= "SO2 density curve")
Densityso2

Densityo3 <- dfAMS_clean %>%
  pivot_wider(id_cols = c(date, place, season, month, yday, date2),
              names_from = c(variable), values_from = value) %>%
  ggplot(aes(x = log(03), color = place, fill = place)) +
  geom_histogram(aes(y=..density..), position="identity")+
  geom_density(alpha=.2) +
  labs(title= "O3 density curve")
Densityo3

save(Densitypm2.5, Densityco, Densitypm10,
     Densityso2, Densityno2, Densityo3,
     file = "Report/AMS_densityplot.Rda")

# Time series -------------------------------------------------------------


Embalenhle <- LAMS_clean %>%
  pivot_wider(id_cols = c(Date, town, season, month, yday, date2),
              names_from = c(variable), values_from = value) %>%
  filter(place == "Embalenhle") %>%
  select(where(~!all(is.na(.))))
eMbalenhletimeseries <- timePlot(selectByDate(Embalenhle, year = 2023),
                                 pollutant = c("co", "pm2.5", "no2", "so2", "pm10", "o3"),
                                 y.relation = "free")

Lebohang <- dfAMS_clean %>%
  pivot_wider(id_cols = c(date, place, season, month, yday, date2),
              names_from = c(variable), values_from = value) %>%
  filter(place == "Lebohang") %>%
  select(where(~!all(is.na(.))))
lebohangtimeseries <- timePlot(selectByDate(Lebohang, year = 2023),
                               pollutant = c("co", "pm2.5", "no2", "so2", "pm10", "o3"),
                               y.relation = "free")

save(eMbalenhletimeseries, lebohangtimeseries,
     file = "Report/AMS_timeseriesplot.Rda")

# Boxplot -----------------------------------------------------------------



BoxPM2.5Compare <- ggplot(data = dfAMS_Daily,
                          aes(x = place, y = pm2.5, fill = place)) +
  geom_boxplot() +
  geom_hline(yintercept = 40, linetype = "dashed", color = "red") +
  theme_classic() +
  labs(
    x = "place",
    y = "place",
    color = "place",
    title = "Seasonal statistical Daily summary of PM2.5 per town")
BoxPM2.5Compare

BoxPM10Compare <- ggplot(data = dfAMS_Daily,
                         aes(x = place, y = pm10, fill = place)) +
  geom_boxplot() +
  geom_hline(yintercept = 75, linetype = "dashed", color = "red") +
  theme_classic() +
  labs(
    x = "place",
    y = "place",
    color = "place",
    title = "Seasonal statistical Daily summary of PM10 per town")
BoxPM10Compare


Boxso2Compare <- ggplot(data = dfAMS_Daily,
                        aes(x = place, y = so2, fill = place)) +
  geom_boxplot() +
  geom_hline(yintercept = 48, linetype = "dashed", color = "red") +
  theme_classic() +
  labs(
    x = "place",
    y = "place",
    color = "place",
    title = "Seasonal statistical Daily summary of SO2 per town")
Boxso2Compare

HBoxcoCompare <- ggplot(data = dfAMS_Hourly,
                        aes(x = place, y = co, fill = place)) +
  geom_boxplot() +
  geom_hline(yintercept = 8.6, linetype = "dashed", color = "red") +
  theme_classic() +
  labs(
    x = "place",
    y = "place",
    color = "place",
    title = "Statistical Hourly summary of co per town")
HBoxcoCompare

HBoxso2Compare <- ggplot(data = dfAMS_Hourly,
                         aes(x = place, y = so2, fill = place)) +
  geom_boxplot() +
  geom_hline(yintercept = 136, linetype = "dashed", color = "red") +
  theme_classic() +
  labs(
    x = "place",
    y = "place",
    color = "place",
    title = "Statistical Hourly summary of SO2 per town")
HBoxso2Compare

HBoxno2Compare <- ggplot(data = dfAMS_Hourly,
                         aes(x = place, y = no2, fill = place)) +
  geom_boxplot() +
  geom_hline(yintercept = 106, linetype = "dashed", color = "red") +
  theme_classic() +
  labs(
    x = "place",
    y = "place",
    color = "place",
    title = "Statistical Hourly summary of NO2 per town")
HBoxno2Compare

save(BoxPM2.5Compare, BoxPM10Compare,Boxso2Compare, HBoxcoCompare, HBoxso2Compare,HBoxno2Compare,
     file = "Report/AMS_Boxplot.Rda")

# Line plot ---------------------------------------------------------------

EbamdailyPM10Compare <- ggplot(data = dfAMS_Daily, aes(x = yday, y = pm10, group = place, colour = place)) +
  geom_line() +
  theme_classic() +
  labs(
    x = "Yday",
    y = "pm10",
    color = "place",
    title = "Daily PM10 per Town")
EbamdailyPM10Compare

EbamdailyPM2.5Compare <- ggplot(data = dfAMS_Daily, aes(x = yday, y = pm2.5, group = place, colour = place)) +
  geom_line() +
  theme_classic() +
  labs(
    x = "Yday",
    y = "pm2.5",
    color = "place",
    title = "Daily PM2.5 per Town")
EbamdailyPM2.5Compare

EbamdailySO2Compare <- ggplot(data = dfAMS_Daily, aes(x = yday, y = so2, group = place, colour = place)) +
  geom_line() +
  theme_classic() +
  labs(
    x = "Yday",
    y = "so2",
    color = "place",
    title = "Daily SO2 per Town")
EbamdailySO2Compare

save(EbamdailyPM10Compare, EbamdailyPM2.5Compare,
     EbamdailySO2Compare,
     file = "Report/AMS_dailyplot.Rda")

EbamdailycoCompare <- ggplot(data = dfAMS_Hourly, aes(x = DateTime, y = co, group = place, colour = place)) +
  geom_line() +
  theme_classic() +
  labs(
    x = "DateTime",
    y = "co",
    color = "place",
    title = "Daily co per Town")
EbamdailycoCompare

save(EbamdailyPM10Compare, EbamdailyPM2.5Compare,
     EbamdailySO2Compare,
     file = "Report/AMS_dailyplot.Rda")


# Polar Plot ---------------------------------------------------------------
library(openair)
EbameMbalenhleHour <- dfAMS_Hourly %>%
  filter(place == "Embalenhle") %>%
  select(where(~!all(is.na(.))))

eMbapolplotpm2.5 <- polarPlot(EbameMbalenhleHour, pollutant = "pm2.5")
eMbapolplotpm10 <- polarPlot(EbameMbalenhleHour, pollutant = "pm10")
eMbapolplotno2 <- polarPlot(EbameMbalenhleHour, pollutant = "no2")
eMbapolplotso2 <- polarPlot(EbameMbalenhleHour, pollutant = "so2")
eMbapolplotco <- polarPlot(EbameMbalenhleHour, pollutant = "co")

EbamLebohangHour <- dfAMS_Hourly %>%
  filter(place == "Lebohang") %>%
  select(where(~!all(is.na(.))))
Lebpolplotpm2.5 <- polarPlot(EbamLebohangHour, pollutant = "pm2.5")
Lebpolplotpm10 <- polarPlot(EbamLebohangHour, pollutant = "pm10")
Lebpolplotso2 <- polarPlot(EbamLebohangHour, pollutant = "so2")
Lebpolplotno2 <- polarPlot(EbamLebohangHour, pollutant = "no2")
Lebpolplotco <- polarPlot(EbamLebohangHour, pollutant = "co")

save(eMbapolplotpm2.5, eMbapolplotpm10,
     eMbapolplotno2, eMbapolplotso2, eMbapolplotco, Lebpolplotpm2.5,
     Lebpolplotpm10, Lebpolplotso2,Lebpolplotno2,Lebpolplotco,
     file = "Report/AMS_Polplot.Rda")

# plot met ----------------------------------------------------------------

library(openair)

Temperature <- ggplot(data = dfAMS_Daily, aes(x = yday, y = at, group = place, colour = place)) +
  geom_line() +
  facet_wrap(~ place, nrow = 2, scales = "free") +
  theme_classic() +
  labs(
    x = "Yday",
    y = "Temperature",
    color = "place",
    title = "Tempearture at eMbalenhle and Lebohang",
    caption = "Data from AMS")
Temperature



Edata <- dfAMS_Hourly %>%
  filter(place == "Embalenhle")

eMbalenhleWindrose <- windRose(Edata)

Ldata <- dfAMS_Hourly %>%
  filter(place == "Lebohang")

Lebohangwindrose <- windRose(Ldata)

