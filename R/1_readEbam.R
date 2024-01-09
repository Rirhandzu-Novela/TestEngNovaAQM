# Load Packages -----------------------------------------------------------

library(tidyverse)
library(novaAQMeng)

# Load Data ---------------------------------------------------------------


fls <- list.files("Data/EBAM", pattern = ".csv", full.names = TRUE)

patt <- "Data/GM5000/(Embalenhle|Lebohang)(_GM5000_level2.csv|_GM5000_level2-2.csv)"

patt <- "Data/EBAM/(20230728_1640_|20230901_1101_)(Embalenhle|Lebohang)(_EBAM_SdB.csv|_EBAM_SdB.csv)"

EB_Raw <- map(fls, ~{
  town <- unique(gsub(patt, "\\2", .))
  readr::read_csv(file = .) %>%
    pivot_longer(cols = -Time) %>%
    tidyr::separate(col = name, into = c("variable", "unit"), sep = " ") %>%
    mutate(town = all_of(town))

}
)%>%
  dplyr::bind_rows()  %>%
  pivot_wider(id_cols = c(Time, town),
              names_from = c(variable), values_from = value)

# Save --------------------------------------------------------------------

save(EB_Raw, file = "Data/EB_Raw.Rda")
