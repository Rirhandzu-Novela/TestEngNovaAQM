# Load packages -----------------------------------------------------------

library(tidyverse)
library(openair)
library(magrittr)
library(novaAQM)
library(novaReport)

# Read dat files ----------------------------------------------------------

dr1 <- "Data/Met/"
fn <- list.files(dr1)

ls_weer <- leesNWU(dr1)
names(ls_weer) <- fn

dfMet <- tibble(filename = fn, data = ls_weer) %>%
  mutate(ncol = map_int(data, ncol),
         nrow = map_int(data, nrow),
         town = gsub("([[:digit:]]{8})_([[:digit:]]{4})_([[:alpha:]]{8,10})_House[[:print:]]*.dat", "\\3", filename),
         house = gsub("([[:digit:]]{8})_([[:digit:]]{4})_([[:alpha:]]{8,10})_(House[[:digit:]]{1}[[:alpha:]]{1})_[[:print:]]*.dat", "\\4", filename),
         Date = lubridate::ymd(gsub("([[:digit:]]{8})_([[:digit:]]{4})_([[:alpha:]]{8,10})_(House[[:digit:]]{1}[[:alpha:]]{1})_[[:print:]]*.dat", "\\1", filename))
         )

dfMet_data <- dfMet %>%
  filter(nrow >100) %>%
  select(data, town, house) %>%
  unnest(data) %>%
  arrange(TIMESTAMP) %>%
  distinct()

# Save --------------------------------------------------------------------

save(dfMet_data,  file = "Data/Met_data_Raw.Rda")

