# Load Packages -----------------------------------------------------------

library(tidyverse)
library(janitor)
library(novaAQMeng)
library(ggplot2)

# Read csv files ----------------------------------------------------------

fls <- list.files("Data/GM5000", pattern = ".csv", full.names = TRUE)

patt <- "Data/GM5000/(Embalenhle|Lebohang)(_GM5000_level2.csv|_GM5000_level2-2.csv)"

GM_Raw <- map(fls, ~{
  town <- unique(gsub(patt, "\\1", .))
  readr::read_csv(file = .) %>%
    pivot_longer(cols = -Date) %>%
    tidyr::separate(col = name, into = c("station", "variable"), sep = "_") %>%
    tidyr::separate(col = variable, into = c("variable", "unit"), sep = " ") %>%
    mutate(unit = gsub("[()]", "", unit), instrument = "GM5000") %>%
    mutate(Date = lubridate::ymd_hms(Date, tz = "Africa/Johannesburg")) %>%
    mutate(town = all_of(town))

}
) %>%
  dplyr::bind_rows() %>%
  pivot_wider(id_cols = c(Date, town, station),
              names_from = variable, values_from = value)

# Save --------------------------------------------------------------------

save(GM_Raw, file = "Data/GM_Raw.csv")




