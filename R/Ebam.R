# Load Packages -----------------------------------------------------------

library(tidyverse)
library(novaAQMeng)
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


