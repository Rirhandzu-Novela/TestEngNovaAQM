# Load Packages -----------------------------------------------------------

library(tidyverse)
library(novaAQMeng)

# Load Data ---------------------------------------------------------------

fls <- fs::dir_ls("Data/AQMS/")

MS_Raw <- map_dfr(fls, ~{
  x <- readSAAQIS(.)
  data <- x$df_data
  data %>% mutate(station = x$file %>% str_extract("/.+$") %>% str_remove_all("/|.xlsx|AQMS"))
  })


# Save --------------------------------------------------------------------

save(MS_Raw, file = "Data/MS_Raw.Rda")


