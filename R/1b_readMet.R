# Load packages -----------------------------------------------------------

library(tidyverse)
library(openair)
library(magrittr)
library(novaAQMeng)


# Read dat files ----------------------------------------------------------

dr1 <- "Data/Met/"
fn <- list.files(dr1)

ls_met <- readNWU(dr1)
names(ls_met) <- fn

dfMet <- tibble(filename = fn, data = ls_met) %>%
  mutate(ncol = map_int(data, ncol),
         nrow = map_int(data, nrow),
         station = gsub("([[:digit:]]{8})_([[:digit:]]{4})_([[:alpha:]]{8,10})_(House[[:digit:]]{1}[[:alpha:]]{1})_[[:print:]]*.dat", "\\4", filename),
         Date = lubridate::ymd(gsub("([[:digit:]]{8})_([[:digit:]]{4})_([[:alpha:]]{8,10})_(House[[:digit:]]{1}[[:alpha:]]{1})_[[:print:]]*.dat", "\\1", filename))
         )

Met_Raw <- dfMet %>%
  filter(nrow >100) %>%
  select(data, station) %>%
  unnest(data) %>%
  arrange(TIMESTAMP) %>%
  distinct()

# Save --------------------------------------------------------------------

save(Met_Raw,  file = "Data/Met_Raw.Rda")

