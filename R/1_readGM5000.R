# Load Packages -----------------------------------------------------------

library(tidyverse)
library(janitor)
library(novaAQMeng)
library(ggplot2)

# Read csv files ----------------------------------------------------------

fls <- list.files("Data/GM5000", pattern = ".csv", full.names = TRUE)

patt <- "Data/GM5000/(Embalenhle|Lebohang)(_GM5000_level2.csv|_GM5000_level2-2.csv)"

dfGM <- map(fls, ~{
  town <- unique(gsub(patt, "\\1", .))
  readr::read_csv(file = .) %>%
    pivot_longer(cols = -Date) %>%
    tidyr::separate(col = name, into = c("house", "variable"), sep = "_") %>%
    tidyr::separate(col = variable, into = c("variable", "unit"), sep = " ") %>%
    mutate(town = all_of(town))
}
) %>%
  dplyr::bind_rows() %>%
  mutate(month = lubridate::month(Date, label = TRUE),
         yday = lubridate::yday(Date),
         season = case_when(yday < 151 ~ "summer", TRUE ~ "winter"),
         date2 = as.Date(Date))


dfGMWide <- dfGM %>%
  pivot_wider(id_cols = c(Date, house, town, season, month, yday, date2),
              names_from = variable, values_from = value)

# Save --------------------------------------------------------------------

save(dfGM, dfGMWide, file = "Data/GM_Raw.Rda")




