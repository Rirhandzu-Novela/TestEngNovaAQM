# Load Packages -----------------------------------------------------------

library(tidyverse)
library(novaAQM)
library(openair)

# Read GM5000 files ----------------------------------------------------------

fls <- list.files("Data/GM5000", pattern = ".csv", full.names = TRUE)

patt <- "Data/GM5000/(Embalenhle|Lebohang)(_GM5000_level2.csv|_GM5000_level2-2.csv)"

GM_Raw <- map(fls, ~{
  place <- unique(gsub(patt, "\\1", .))
  readr::read_csv(file = .) %>%
    pivot_longer(cols = -Date) %>%
    tidyr::separate(col = name, into = c("station", "variable"), sep = "_") %>%
    tidyr::separate(col = variable, into = c("variable", "unit"), sep = " ") %>%
    mutate(instrument = "GM5000") %>%
    mutate(place = all_of(place)) %>%
    mutate(date = lubridate::`tz<-`(Date, "Africa/Johannesburg"))

}
) %>%
  dplyr::bind_rows() %>%
  mutate(month = lubridate::month(Date, label = TRUE),
         yday = lubridate::yday(Date),
         season = case_when(yday < 151 ~ "summer", TRUE ~ "winter"),
         date2 = as.Date(Date)) %>%
  pivot_wider(id_cols = c(Date, station, place, month, yday, season, date2),
              names_from = variable, values_from = value) %>%
  setNames(tolower(colnames(.)))



# Read Met files ----------------------------------------------------------


dr1 <- "Data/Met/"
fn <- list.files(dr1)

ls_met <- novaAQMeng::readNWU(dir = dr1)

Met_Raw <- tibble(filename = fn, data = ls_met) %>%
  mutate(ncol = map_int(data, ncol),
         nrow = map_int(data, nrow),
         town = gsub("([[:digit:]]{8})_([[:digit:]]{4})_([[:alpha:]]{8,10})_House[[:print:]]*.dat", "\\3", filename),
         station = gsub("([[:digit:]]{8})_([[:digit:]]{4})_([[:alpha:]]{8,10})_(House[[:digit:]]{1}[[:alpha:]]{1})_[[:print:]]*.dat", "\\4", filename),
         date = lubridate::ymd(gsub("([[:digit:]]{8})_([[:digit:]]{4})_([[:alpha:]]{8,10})_(House[[:digit:]]{1}[[:alpha:]]{1})_[[:print:]]*.dat", "\\1", filename))
  ) %>%
  filter(nrow >100) %>%
  select(data, station) %>%
  unnest(data) %>%
  arrange(TIMESTAMP) %>%
  relocate(station, .before = RECORD) %>%
  distinct()  %>%
  filter(year(TIMESTAMP) == "2023") %>%
  rename(date = TIMESTAMP)


#  select(TIMESTAMP,station, AirTC_Avg, RH_Avg, WSpeed_Avg, WDir_Avg) %>%
#  rename(temp = AirTC_Avg,
#         relhum = RH_Avg,
#         ws = WSpeed_Avg,
#         wd = WDir_Avg,
#         date = TIMESTAMP) %>%
#  mutate(n = n(), .by = TIMESTAMP, station) %>%
#  filter(n == 1) %>%
#  select(!n)


GMMet_Raw <- left_join(x = GM_Raw, y = Met_Raw, by = c("date", "station"))

PtimeseriesR <- timePlot(selectByDate(Met_Raw, year = 2023),
                         pollutant = c("ws", "wd", "relhum", "relhum"),
                         y.relation = "free")

MtimeseriesR <- timePlot(selectByDate(Met_Raw, year = 2023),
                         pollutant = c("RH_Avg", "AirTC_Avg", "WSpeed_Avg", "WDir_Avg"),
                         y.relation = "free")




save(GMMet_Raw, file = "Report/GMMet_Raw.Rda")


# Read EBAM FILES ---------------------------------------------------------------


fls <- list.files("Data/EBAM", pattern = ".csv", full.names = TRUE)

patt <- "Data/EBAM/(20230728_1640_|20230901_1101_)(Embalenhle|Lebohang)(_EBAM_SdB.csv|_EBAM_SdB.csv)"

dfEB_Raw <- map(fls, ~{
  place <- unique(gsub(patt, "\\2", .))
  readr::read_csv(file = .) %>%
    rename_with(~str_to_lower(.)) %>%
    pivot_longer(cols = -time) %>%
    tidyr::separate(col = name, into = c("variable", "unit"), sep = " ") %>%
    mutate(place = all_of(place)) %>%
    mutate(station = case_when(
      place == "Embalenhle" ~ "House3E",
      place == "Lebohang" ~ "House2L"))
}
)%>%
  dplyr::bind_rows() %>%
  mutate(month = lubridate::month(time, label = TRUE),
         yday = lubridate::yday(time),
         season = case_when(yday < 151 ~ "summer", TRUE ~ "winter"),
         date2 = as.Date(time)) %>%
  pivot_wider(id_cols = c(time, place, station, season, month, yday, date2),
              names_from = c(variable), values_from = value) %>%
  rename(pm2.5 = concrt,
         hourly_pm2.5 = conchr) %>%
  select(-...15, -pm, -status)


# GMMET DATA --------------------------------------------------------

# clean GMMET ----------------------------------------------------------

dfGMMet_clean <- GMMet_Raw %>%
  mutate_at(vars(-date, -month, -date2), ~ifelse(. < 0, NA, .), ~ ifelse(is.infinite(.), NA, .)) %>%
  mutate(
    pm2.5 = ifelse(pm2.5 < 1, 0.5, pm2.5),
    pm2.5 = ifelse(pm2.5 > 1500, 1500, pm2.5),
    co = ifelse(co < 0.020, 0.010, co),
    co = ifelse(co > 50, 50, co),
    o3 = ifelse(o3 < 5, 2.5, o3),
    o3 = ifelse(o3 > 500, 500, o3)) %>%
  pivot_longer(cols = co:wd, names_to = "variable") %>%
  mutate(unit = case_when(
    variable == "pm2.5" ~ "Âµg.m-3",
    variable == "ws" ~ "m/s",
    variable == "o3" ~ "ppb",
    variable == "co" ~ "ppm",
    variable == "wd" ~ "Deg",
    variable == "at" ~ "C",
    variable == "rh" ~ "perc.",
    TRUE ~ NA_character_
  )) %>%
  relocate(unit, .after = variable)

save(dfGMMet_clean,dfGMMet_hourly, dfGMMetDaily, file = "Report/dfGMMet_Clean.Rda")

# Summary GMMET -----------------------------------------------------------


EdfGM_clean_summary <- dfGMMet_clean %>%
  filter(variable == c("pm2.5", "co", "o3")) %>%
  dplyr::summarize(
    novaAQM::tenpointsummary(value) , .by = c(station, variable)
  ) %>%
  drop_na() %>%
  arrange(station)



# Hourly averages and standard GMMET --------------------------------------

dfGMMet_hourly <- dfGMMet_clean %>%
  distinct() %>%
  # dplyr::group_by(date, station, place, month, yday, season, date2, variable, unit) %>%
  # dplyr::summarise(n = dplyr::n(), vals = list(value), .groups = "drop") %>%
  # dplyr::filter(n > 1L)
  pivot_wider(id_cols = c(date, station, place, month, yday, season, date2),
              names_from = variable, values_from = value) %>%
  mutate(hod = lubridate::hour(date)) %>%
  group_by(place, station, season, month, yday, date2, hod) %>%
  summarise(perc = length(na.omit(pm2.5)) / length(pm2.5)  * 100 ,
            pm2.5 = mean(pm2.5, na.rm = TRUE),
            ws = mean(ws, na.rm = TRUE),
            o3 = mean(o3, na.rm = TRUE),
            co = mean(co, na.rm = TRUE),
            wd = mean(wd, na.rm = TRUE),
            at = mean(at, na.rm = TRUE),
            rh = mean(rh, na.rm = TRUE),
  ) %>%
  arrange(place, date2, hod) %>%
  mutate(DateTime = ymd_h(paste(date2, hod)))

dfGMMet_date <- dfGMMet_hourly %>% rename(date = DateTime) %>%
  select(date, place, month, season, station, pm2.5, o3, co) %>%
  pivot_longer(cols = c(pm2.5, o3, co), names_to = "variable") %>%
  mutate(unit = case_when(
    variable == "co" ~ "ppm",
    variable == "pm2.5" ~ "Âµg.m-3",
    variable == "o3" ~ "ppb",
    TRUE ~ NA_character_
  ))

dfGMMet_hour_ex <- novaAQM::compareAQS(df = dfGMMet_date %>%
                                          ungroup() %>%
                                          datify() %>%
                                          mutate(place = station,
                                                 instrument = "AMS"),
                                        period = "hour",
                                        by_period = quos(year)) %>%
  ungroup() %>%
  arrange(pollutant, year) %>%
  relocate(pollutant, .after = place)
dfGMMet_hour_ex


# Daily averages and standards GMMET --------------------------------------


dfGMMetDaily <- dfGMMet_clean %>%
  pivot_wider(id_cols = c(date, station, place, month, yday, season, date2),
              names_from = variable, values_from = value) %>%
  group_by(place, station, season, month, yday, date2) %>%
  summarise(perc = length(na.omit(pm2.5)) / length(pm2.5)  * 100 ,
            pm2.5 = mean(pm2.5, na.rm = TRUE),
            ws = mean(ws, na.rm = TRUE),
            o3 = mean(o3, na.rm = TRUE),
            co = mean(co, na.rm = TRUE),
            wd = mean(wd, na.rm = TRUE),
            at = mean(at, na.rm = TRUE),
            rh = mean(rh, na.rm = TRUE)) %>%
  arrange(place, yday, month) %>%
  mutate(date = ymd(paste(date2))) %>%
  relocate(date, .before = place)


dfGMMetDay  <- dfGMMetDaily %>%
  select(date, place, month, season, station, pm2.5, o3, co) %>%
  pivot_longer(cols = c(pm2.5, o3, co), names_to = "variable") %>%
  mutate(unit = case_when(
    variable == "co" ~ "ppm",
    variable == "pm2.5" ~ "Âµg.m-3",
    variable == "o3" ~ "ppb",
    TRUE ~ NA_character_
  ))

dfGMMet_daily_ex <- novaAQM::compareAQS(df = dfGMMetDay %>%
                                        ungroup() %>%
                                        datify() %>%
                                        mutate(place = station,
                                               instrument = "GM5000"),
                                      period = "day",
                                      by_period = quos(year)) %>%
  #ungroup() %>%
  arrange(pollutant, year)

dfGMMet_daily_ex



# EBAM DATA ---------------------------------------------------------------

# clean EBAM --------------------------------------------------------------

dfEB_clean <- dfEB_Raw %>%
  mutate_at(vars(-time, -month, -date2), ~ifelse(. < 0, NA, .), ~ ifelse(is.infinite(.), NA, .)) %>%
  select(time, place, station, season, month, yday, date2, pm2.5, ws, wd, at, rh, bp) %>%
  mutate(
      pm2.5 = ifelse(pm2.5 < 0, NA, pm2.5),
      pm2.5 = ifelse(pm2.5 > 1500, NA, pm2.5)) %>%
  pivot_longer(cols = pm2.5:bp, names_to = "variable") %>%
  mutate(unit = case_when(
      variable == "pm2.5" ~ "Âµg.m-3",
      variable == "ws" ~ "m/s",
      variable == "wd" ~ "Deg",
      variable == "at" ~ "C",
      variable == "rh" ~ "perc.",
      variable == "bp" ~ "mmHg",
      TRUE ~ NA_character_
    )) %>%
  relocate(unit, .after = variable)

save(dfEB_clean, dfEB_hourly, dfEBDaily, file = "Report/dfEB_Clean.Rda")

# Summary EBAM ------------------------------------------------------------

dfEB_clean_summary <- dfEB_clean %>%
  filter(variable == "pm2.5") %>%
  dplyr::summarize(
    novaAQM::tenpointsummary(value) , .by = c(station, variable)
  ) %>%
  drop_na() %>%
  arrange(station) %>%
  mutate(station = case_when(
                           station == "House3E" ~ "House3E_Ebam"))


# Daily Averages and exceedances EBAM -------------------------------------------


dfEB_hourly <- dfEB_clean %>%
  pivot_wider(id_cols = c(time, station, place, month, yday, season, date2),
              names_from = variable, values_from = value) %>%
  mutate(hod = lubridate::hour(time)) %>%
  group_by(place, station, season, month, yday, date2, hod) %>%
  summarise(perc = length(na.omit(pm2.5)) / length(pm2.5)  * 100 ,
            pm2.5 = mean(pm2.5, na.rm = TRUE),
            ws = mean(ws, na.rm = TRUE),
            bp = mean(bp, na.rm = TRUE),
            wd = mean(wd, na.rm = TRUE),
            at = mean(at, na.rm = TRUE),
            rh = mean(rh, na.rm = TRUE),
  ) %>%
  arrange(place, date2, hod) %>%
  mutate(DateTime = ymd_h(paste(date2, hod)))


# Daily averages

dfEBDaily <- dfEB_clean %>%
  pivot_wider(id_cols = c(time, station, place, month, yday, season, date2),
              names_from = variable, values_from = value) %>%
  group_by(place, station, season, month, yday, date2) %>%
  summarise(perc = length(na.omit(pm2.5)) / length(pm2.5)  * 100 ,
            pm2.5 = mean(pm2.5, na.rm = TRUE),
            ws = mean(ws, na.rm = TRUE),
            bp = mean(bp, na.rm = TRUE),
            wd = mean(wd, na.rm = TRUE),
            at = mean(at, na.rm = TRUE),
            rh = mean(rh, na.rm = TRUE)) %>%
  arrange(place, yday, month) %>%
  mutate(date = ymd(paste(date2))) %>%
  relocate(date, .before = place)


dfEBDay  <- dfEBDaily %>%
  select(date, month, season, station, pm2.5) %>%
  pivot_longer(cols = c(pm2.5), names_to = "variable") %>%
  mutate(unit = case_when(
    variable == "pm2.5" ~ "Âµg.m-3",
    TRUE ~ NA_character_
  ))

dfEB_daily_ex <- novaAQM::compareAQS(df = dfEBDay %>%
                                          ungroup() %>%
                                          datify() %>%
                                          mutate(place = station,
                                                 instrument = "Ebam"),
                                        period = "day",
                                        by_period = quos(season)) %>%
  #ungroup() %>%
  arrange(pollutant, year) %>%
  mutate(place = case_when(
    place == "House3E" ~ "House3E_Ebam",
    place == "House2L" ~ "House2L_Ebam"))

dfEB_daily_ex



# JOIN GM & EBAM DATA ------------------------------------------------------


GM_Exceedanes <- full_join(x = dfGMMet_daily_ex, y = dfEB_daily_ex, by = "place")%>%
  unite(year, year.x, year.y, sep = "_", na.rm = TRUE) %>%
  unite(pollutant, pollutant.x, pollutant.y, sep = "_", na.rm = TRUE) %>%
  unite(instrument, instrument.x, instrument.y, sep = "_", na.rm = TRUE) %>%
  unite(avg_period, avg_period.x, avg_period.y, sep = "_", na.rm = TRUE) %>%
  unite(unit, unit.x, unit.y, sep = "_", na.rm = TRUE) %>%
  unite(q25, q25.x, q25.y, sep = "_", na.rm = TRUE) %>%
  unite(median, median.x, median.y, sep = "_", na.rm = TRUE) %>%
  unite(mean, mean.x, mean.y, sep = "_", na.rm = TRUE) %>%
  unite(q75, q75.x, q75.y, sep = "_", na.rm = TRUE) %>%
  unite(q99, q99.x, q99.y, sep = "_", na.rm = TRUE) %>%
  unite(max, max.x, max.y, sep = "_", na.rm = TRUE) %>%
  unite(n_h, n_h.x, n_h.y, sep = "_", na.rm = TRUE) %>%
  unite(n_obs_h, n_obs_h.x, n_obs_h.y, sep = "_", na.rm = TRUE) %>%
  unite(complete, complete.x, complete.y, sep = "_", na.rm = TRUE) %>%
  unite(nEx_h, nEx_h.x, nEx_h.y, sep = "_", na.rm = TRUE) %>%
  unite(standard, standard.x, standard.y, sep = "_", na.rm = TRUE) %>%
  unite(std_nEx, std_nEx.x, std_nEx.y, sep = "_", na.rm = TRUE)






