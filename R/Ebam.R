# Load Packages -----------------------------------------------------------

library(tidyverse)
library(novaAQM)
library(openair)
library(gridExtra)
library(ggpubr)


# Load Data ---------------------------------------------------------------

load(file = "Data/GMMet_data.Rda")

fls <- list.files("Data/EBAM", pattern = ".csv", full.names = TRUE)

patt <- "Data/GM5000/(Embalenhle|Lebohang)(_GM5000_level2.csv|_GM5000_level2-2.csv)"

patt <- "Data/EBAM/(20230728_1640_|20230901_1101_)(Embalenhle|Lebohang)(_EBAM_SdB.csv|_EBAM_SdB.csv)"

dfEB <- map(fls, ~{
  town <- unique(gsub(patt, "\\2", .))
  readr::read_csv(file = .) %>%
    pivot_longer(cols = -Time) %>%
    tidyr::separate(col = name, into = c("variable", "unit"), sep = " ") %>%
    mutate(town = all_of(town))
}
)%>%
  dplyr::bind_rows() %>%
  mutate(month = lubridate::month(Time, label = TRUE),
         yday = lubridate::yday(Time),
         season = case_when(yday < 151 ~ "summer", TRUE ~ "winter"),
         date2 = as.Date(Time))

dfEBWide <- dfEB %>%
  pivot_wider(id_cols = c(Time, town, season, month, yday, date2),
              names_from = c(variable), values_from = value) %>%
  rename(pm2.5 = ConcRT)

# Count 99999 values -------------------------------------------------------------
# Total 99999 values (4488)

dfEBWide %>%
  filter(pm2.5 == 99999) %>%
  summarise(count = n())

# Remove 99999 when the flow rate  is less than 16.7 (835)
dfEBWide %>%
  mutate(pm2.5 = ifelse(Flow < 16.7 , NA, pm2.5)) %>%
  filter(pm2.5 == 99999) %>%
  summarise(count = n())

# Total >1500 values (4725)
dfEBWide %>%
  filter(pm2.5 > 1500) %>%
  summarise(count = n())

# Replace negative values and values above 1500 with NA

dfEB_Data <- dfEBWide %>%
  select(Time, town, season, month, yday, date2, pm2.5, WS, WD, AT, RH, BP, Flow) %>%
  mutate(
    pm2.5 = ifelse(pm2.5 < 0, NA, pm2.5),
    pm2.5 = ifelse(pm2.5 > 1500, NA, pm2.5))  %>%
  pivot_longer(cols = pm2.5:Flow, names_to = "variable") %>%
  mutate(unit = case_when(
    variable == "pm2.5" ~ "µg.m-3",
    variable == "WS" ~ "m/s",
    variable == "WD" ~ "Deg",
    variable == "AT" ~ "C",
    variable == "RH" ~ "perc.",
    variable == "BP" ~ "mmHg",
    variable == "Flow" ~ "lpm",
    TRUE ~ NA_character_
  )) %>%
  relocate(unit, .after = variable) %>%
  mutate(across(.cols = everything(), ~ ifelse(is.infinite(.x), NA, .x)),
         date = as.POSIXct(Time, origin = "1970-01-01"),
         date2 = as.Date(date2, origin = "1970-01-01"))

# Summary -----------------------------------------------------------------

Ebam_Tensummary <- dfEB_Data %>%
  dplyr::summarize(
    novaReport::tenpointsummary(value) , .by = c(town, variable)
  ) %>%
  drop_na()


Ebam_Tablesummary <- tabDataVasleggingDB(df = dfEB_Data %>%
                                       mutate(avg_period = "5 min",
                                              unit = unit,
                                              place = town,
                                              instrument = "EBAM"),
                                     begin = min(dfEB$Time),
                                     end = max(dfEB$Time)) %>%
  relocate(firstObs, lastObs, .before = nobs) %>%
  relocate(variable, .after = place) %>%
  arrange(place) %>%
  drop_na()



# Density plot ------------------------------------------------------------

Densitypm2.5 <- dfEB_Data %>%
  pivot_wider(id_cols = c(Time, town, season, month, yday, date2),
              names_from = c(variable), values_from = value) %>%
  ggplot(aes(x = log(pm2.5), color = town, fill = town)) +
  geom_histogram(aes(y=..density..), position="identity")+
  geom_density(alpha=.2) +
  labs(title= "PM density curve")
Densitypm2.5



# Averages ----------------------------------------------------------------


#Hourly averages


dfEB_DataHour <- dfEB_Data %>%
  pivot_wider(id_cols = c(date, town, season, month, yday, date2),
              names_from = c(variable), values_from = value) %>%
  mutate(hod = lubridate::hour(date)) %>%
  group_by(town, season, month, yday, date2, hod) %>%
  summarise(pm2.5 = mean(pm2.5, na.rm = TRUE),
            ws = mean(WS, na.rm = TRUE),
            wd = mean(WD, na.rm = TRUE),
            Temp = mean(AT, na.rm = TRUE),
            RH = mean(RH, na.rm = TRUE),
            BP = mean(BP, na.rm = TRUE),
            Flow = mean(Flow, na.rm = TRUE),
  ) %>%
  arrange(town, date2, hod) %>%
  mutate(DateTime = ymd_h(paste(date2, hod))) %>%
  relocate(DateTime, .before = town)


# Daily averages

dfEB_DataDaily <- dfEB_Data %>%
  pivot_wider(id_cols = c(date, town, season, month, yday, date2),
              names_from = c(variable), values_from = value) %>%
  group_by(town, season, month, yday, date2) %>%
  summarise(pm2.5 = mean(pm2.5, na.rm = TRUE),
            ws = mean(WS, na.rm = TRUE),
            wd = mean(WD, na.rm = TRUE),
            Temp = mean(AT, na.rm = TRUE),
            RH = mean(RH, na.rm = TRUE),
            BP = mean(BP, na.rm = TRUE),
            Flow = mean(Flow, na.rm = TRUE),
  ) %>%
  arrange(town, yday, month) %>%
  mutate(date = ymd(paste(date2))) %>%
  relocate(date, .before = town)


# NAAQS Daily exceedance --------------------------------------------------


dfEB_day <- dfEB_DataDaily %>%
  select(date, month, season, pm2.5) %>%
  pivot_longer(cols = pm2.5, names_to = "variable")

dfEB_daily_ex <- novaAQM::compareAQS(df = dfEB_day %>%
                                          ungroup() %>%
                                          datify() %>%
                                          mutate(place = town,
                                                 instrument = "Ebam"),
                                        period = "day",
                                        by_period = quos(month)) %>%
  ungroup() %>%
  arrange(pollutant, month)

dfEB_daily_ex


# Time series plot --------------------------------------------------------

# Embalenhle Time series

Embalenhle <- dfEB_DataDaily %>%
  filter(town == "Embalenhle") %>%
  select(where(~!all(is.na(.))))
eMbalenhletimeseries <- timePlot(selectByDate(Embalenhle, year = 2023),
                    pollutant = c("pm2.5", "ws", "wd", "Temp", "RH", "BP"),
                    y.relation = "free")

Lebohang <- dfEB_DataDaily %>%
  filter(town == "Lebohang") %>%
  select(where(~!all(is.na(.))))
lebohangtimeseries <- timePlot(selectByDate(Lebohang, year = 2023),
                       pollutant = c("pm2.5", "ws", "wd", "Temp", "RH", "BP"),
                       y.relation = "free")


# Diurnal -----------------------------------------------------------------

EbamhourlyPMCompare <- ggplot(data = dfEB_DataHour,
                          aes(x = DateTime,
                              y = pm2.5,
                              group = town,
                              colour = town)) +
  geom_line() +
  facet_wrap(~season, nrow = 2, scales = "free") +
  theme(legend.position = "bottom") +
  theme_classic() +
  labs(
    x = "town",
    y = "PM2.5",
    color = "town",
    title = "Hourly trends of PM2.5 per town",
    caption = "Data from EBAM")
EbamhourlyPMCompare



EbamdiurnalPMCompare <- ggplot(data = dfEB_DataHour %>% mutate(grp = paste(date2, town)),
                           aes(x = hod,
                               y = pm2.5,
                               group = grp,
                               colour = town)) +
  geom_line(alpha = I(1/3)) +
  theme_bw() +
  facet_wrap(season ~ town, nrow = 2, scales = "free") +
  theme_classic() +
  labs(
    x = "town",
    y = "PM2.5",
    color = "town",
    title = "Diurnal trends of PM2.5 per town",
    caption = "Data from EBAM")
EbamdiurnalPMCompare


# Boxplot -----------------------------------------------------------------

EbamBoxPMCompare <- ggplot(data = dfEB_DataDaily,
                       aes(x = town, y = pm2.5, fill = town)) +
  geom_boxplot() +
  geom_hline(yintercept = 40, linetype = "dashed", color = "red") +
  facet_wrap(~season, nrow = 2, scales = "free_y") +
  theme_classic() +
  labs(
    x = "town",
    y = "PM2.5",
    color = "town",
    title = "Seasonal statistical summary of PM2.5 per town",
    caption = "Data from EBAM")
EbamBoxPMCompare

# Line plot ---------------------------------------------------------------

EbamdailyPMCompare <- ggplot(data = dfEB_DataDaily, aes(x = yday, y = pm2.5, group = town, colour = town)) +
  geom_line() +
  facet_wrap(~season, nrow = 2, scales = "free") +
  theme_classic() +
  labs(
    x = "Yday",
    y = "pm2.5",
    color = "Town",
    title = "Daily PM per Town",
    caption = "Data from EBAM")
EbamdailyPMCompare


# Calender plot -----------------------------------------------------------

EbamCalPMEM <- calendarPlot(Embalenhle, pollutant = "pm2.5", year = 2023,
                             annotate = c("value"), lim = 40, key.header = "eMba_PM")

EbamCalPMLe <- calendarPlot(Lebohang, pollutant = "pm2.5", year = 2023,
                      annotate = c("value"), lim = 40, key.header = "Leb_PM")


# Polar Plot ---------------------------------------------------------------

EbameMbalenhleHour <- dfEB_DataHour %>%
  filter(town == "Embalenhle") %>%
  select(where(~!all(is.na(.))))

eMbapolplot <- polarPlot(EbameMbalenhleHour, pollutant = "pm2.5")

EbamLebohangHour <- dfEB_DataHour %>%
  filter(town == "Lebohang") %>%
  select(where(~!all(is.na(.))))
Lebpolplot <- polarPlot(EbamLebohangHour, pollutant = "pm2.5")

grid.arrange(eMbapolplot, Lebpolplot, nrow = 2)

save(dfEB, dfEBWide,
     Ebam_Tensummary, Ebam_Tablesummary, Densitypm2.5,
     dfEB_DataHour, dfEB_DataDaily, dfEB_daily_ex,
     eMbalenhletimeseries, lebohangtimeseries,
     EbamhourlyPMCompare, EbamdiurnalPMCompare, EbamBoxPMCompare,
     EbamdailyPMCompare, eMbapolplot, Lebpolplot,
     file = "Data/Ebam.Rda")



# GM & EBAM ---------------------------------------------------------------
##  eMbalenhle HOURLY

Emba3Ehourly <- dfGMMetHour %>%
  filter(house == "House3E") %>%
  select(DateTime, town, season, month, hourlyPM, date2, hod) %>%
  rename(EmbaGM_pm2.5 = hourlyPM)

EmbaEBhourly <- dfEB_DataHour %>%
  filter(town == "Embalenhle") %>%
  select(DateTime, town, season, month, pm2.5, date2, hod) %>%
  rename(EmbaEB_pm2.5 = pm2.5)

GMEB_emba_hour <- full_join(x = EmbaEBhourly,
                            y = Emba3Ehourly,
                            by = c("DateTime", "town", "season", "month", "date2", "hod"))




##  Lebohang HOURLY
Leb3Ehourly <- dfGMMetHour %>%
  filter(house == "House2L") %>%
  select(DateTime, town, season, month, hourlyPM, date2, hod) %>%
  rename(LebGM_pm2.5 = hourlyPM)

LebEBhourly <- dfEB_DataHour %>%
  filter(town == "Lebohang") %>%
  select(DateTime, town, season, month, pm2.5, date2, hod) %>%
  rename(LebEB_pm2.5 = pm2.5)

GMEB_leb_hour <- full_join(x = Leb3Ehourly,
                           y = LebEBhourly,
                           by = c("DateTime", "town", "season", "month", "date2", "hod"))

GMEB_hour <- full_join(x = GMEB_emba_hour,
                           y = GMEB_leb_hour,
                           by = c("DateTime", "town", "season", "month", "date2", "hod")) %>%
  select(DateTime, town, season, month, EmbaEB_pm2.5,EmbaGM_pm2.5, LebGM_pm2.5, LebEB_pm2.5, date2, hod) %>%
  pivot_longer(cols = -c(DateTime, town, season, month, date2, hod)) %>%
  tidyr::separate(col = name, into = c("site", "variable"), sep = "_") %>%
  rename(pm2.5 = value)


##  eMbalenhle Daily
Emba3EDaily <- dfGMMetDaily %>%
  filter(house == "House3E") %>%
  select(date, town, season, month, meanPM, yday) %>%
  rename(EmbaGM_pm2.5 = meanPM)

EmbaEBDaily <- dfEB_DataDaily %>%
  filter(town == "Embalenhle") %>%
  select(date, town, season, month, pm2.5, yday) %>%
  rename(EmbaEB_pm2.5 = pm2.5)

GMEB_emba_day <- full_join(x = EmbaEBDaily,
                            y = Emba3EDaily,
                            by = c("date", "town", "season", "month", "yday"))


##  Lebohang Daily
Leb3EDaily <- dfGMMetDaily %>%
  filter(house == "House2L") %>%
  select(date, town, season, month, meanPM, yday) %>%
  rename(LebGM_pm2.5 = meanPM)

LebEBDaily <- dfEB_DataDaily %>%
  filter(town == "Lebohang") %>%
  select(date, town, season, month, pm2.5, yday) %>%
  rename(LebEB_pm2.5 = pm2.5)

GMEB_Leb_day <- full_join(x = Leb3EDaily,
                           y = LebEBDaily,
                           by = c("date", "town", "season", "month", "yday"))

GMEB_day <- full_join(x = GMEB_emba_day,
                          y = GMEB_Leb_day,
                          by = c("date", "town", "season", "month", "yday")) %>%
  select(date, town, season, month, EmbaEB_pm2.5,EmbaGM_pm2.5, LebGM_pm2.5, LebEB_pm2.5, yday) %>%
  pivot_longer(cols = -c(date, town, season, month, yday)) %>%
  tidyr::separate(col = name, into = c("site", "variable"), sep = "_") %>%
  rename(pm2.5 = value)




save(GMEB_hour, GMEB_day, file = "Data/GM_EBAM.Rda")

# GM_EBAM PLOTS -----------------------------------------------------------

# Diurnal plot
GMEBdiurnalPMCompare <- ggplot(data = GMEB_hour %>% mutate(grp = paste(date2, site)),
                            aes(x = hod,
                                y = pm2.5,
                                group = grp,
                                colour = as.character(date2))) +
  geom_line(alpha = I(1/3)) +
  theme_bw() +
  facet_wrap(season ~ site, nrow = 2, scales = "free_x") +
  geom_smooth(mapping = aes(group = site, colour = town)) +
  theme(legend.position = "none") +
  ggtitle(expression(paste("Hourly ", PM[2.5])))
GMEBdiurnalPMCompare


# Daily plot

GMEBdailyPMCompare <- ggplot(data = GMEB_day %>% filter(season == "summer"), aes(x = yday, y = pm2.5, group = site, colour = site)) +
  geom_line() +
  facet_wrap(~town, nrow = 2, scales = "free") +
  theme_classic() +
  labs(
    x = "Yday",
    y = "PM2.5",
    color = "House",
    title = "Daily PM per Town",
    caption = "Data from GM5000 and Ebam")
GMEBdailyPMCompare


#Box plot

GMEBBoxPMCompare <- ggplot(data = GMEB_day,
                       aes(x = site, y = pm2.5, fill = site )) +
  geom_boxplot() +
  geom_hline(yintercept = 40, linetype = "dashed", color = "red") +
  facet_wrap(season ~ town, nrow = 2, scales = "free_x") +
  theme_classic() +
  labs(
    x = "site",
    y = "PM2.5",
    color = "site",
    title = "Seasonal statistical summary of PM2.5 per site",
    caption = "Data from GM5000 and EBAM")
GMEBBoxPMCompare

# scatter plot
GMEBpoint <- GMEB_hour %>%
  pivot_wider(id_cols = c(DateTime, town, season, month, hod, date2),
              names_from = c(site), values_from = pm2.5)


GMEBpointPMCompareE <- ggplot(GMEBpoint, aes(x=EmbaEB, y=EmbaGM)) +
  geom_point()+
  geom_smooth(method=lm)+
  coord_cartesian(xlim = c(0, 50), ylim = c(0, 50))
GMEBpointPMCompareE


GMEBCorrelationE <- ggscatter(GMEBpoint, x = "EmbaEB", y = "EmbaGM",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "EBAM PM", ylab = "GM5000 PM")
GMEBCorrelationE

GMEBpoint <- GMEBpoint[complete.cases(GMEBpoint$EmbaEB, GMEBpoint$EmbaGM), ]

model_poly <- lm(EmbaGM ~ poly(EmbaEB, degree = 2), data = GMEBpoint)
summary(model_poly)


save(GMEBdiurnalPMCompare, GMEBdailyPMCompare, GMEBBoxPMCompare, GMEBpointPMCompareE, GMEBCorrelationE,
     model_poly,
     file = "Data/GM_EBAMPLOT.Rda")

