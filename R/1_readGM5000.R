# Load Packages -----------------------------------------------------------

library(tidyverse)
library(novaAQMeng)

# Read csv files ----------------------------------------------------------

fls <- list.files("Data/GM5000", pattern = ".csv", full.names = TRUE)

patt <- "Data/GM5000/(Embalenhle|Lebohang)(_GM5000_level2.csv|_GM5000_level2-2.csv)"

GM_Raw <- map(fls, ~{
  town <- unique(gsub(patt, "\\1", .))
  readr::read_csv(file = .) %>%
    pivot_longer(cols = -Date) %>%
    tidyr::separate(col = name, into = c("station", "variable"), sep = "_") %>%
    mutate(instrument = "GM5000") %>%
    mutate(Date = lubridate::`tz<-`(Date, "Africa/Johannesburg"))

}
) %>%
  dplyr::bind_rows() %>%
  pivot_wider(id_cols = c(Date, station),
              names_from = variable, values_from = value)


# Read Met dat files ----------------------------------------------------------

dr1 <- "Data/Met/"
fn <- list.files(dr1)

ls_met <- readNWU(dir = dr1)

Met_Raw <- ls_met %>%
  mutate(ncol = map_int(data, ncol),
         nrow = map_int(data, nrow),
         station = gsub("([[:digit:]]{8})_([[:digit:]]{4})_([[:alpha:]]{8,10})_(House[[:digit:]]{1}[[:alpha:]]{1})_[[:print:]]*.dat", "\\4", file),
         Date = lubridate::ymd(gsub("([[:digit:]]{8})_([[:digit:]]{4})_([[:alpha:]]{8,10})_(House[[:digit:]]{1}[[:alpha:]]{1})_[[:print:]]*.dat", "\\1", file))
  ) %>%
  filter(nrow >100) %>%
  select(data, station) %>%
  unnest(data) %>%
  arrange(TIMESTAMP) %>%
  relocate(station, .before = RECORD) %>%
  distinct()


# Function to add specific units for each met variable

add_units <- function(col_name) {
  units <- switch(col_name,
                  WSpeed_Max = "m/s",
                  WSpeed_S_WVT = "m/s",
                  WSpeed_Avg = "m/s",
                  WDir_Avg = "Deg",
                  WDir_D1_WVT = "Deg",
                  WDir_SD1_WVT = "Deg",
                  Temp_Avg = "DegC",
                  Temp_Max = "DegC",
                  Temp_Min = "DegC",
                  AirTC_Avg = "DegC",
                  AirTC_Max = "DegC",
                  AirTC_Min = "DegC",
                  RH = "%",
                  RH_Avg = "%",
                  RH_Max = "%",
                  RH_Min = "%",
                  Rain_mm_Tot = "mm",
                  BattV_Min = "v",
                  BattV_Avg = "v",
                  BattV = "v",
                  BattV = "RN",
                  NULL  # NULL to exclude columns without specified units
  )

  if (!is.null(units)) {
    return(paste(col_name, "(", units, ")", sep = ""))
  } else {
    return(col_name)
  }
}


# Apply the function to selected column names
selected_cols <- colnames(Met_Raw)[3:length(colnames(Met_Raw))]  # Exclude the first two columns
colnames(Met_Raw)[3:length(colnames(Met_Raw))] <- sapply(selected_cols, add_units)

# Merge air poll and met data ---------------------------------------------


GMMet <- left_join(x = GM_Raw, y = Met_Raw, by = c("Date" = "TIMESTAMP", "station"))



# Save --------------------------------------------------------------------

House1E <- GMMet %>%
  filter(station == "House1E") %>%
  select(-station)

write_csv(House1E, file = "Station/House1E.csv")

House2E <- GMMet %>%
  filter(station == "House2E") %>%
  select(-station)

write_csv(House2E, file = "Station/House2E.csv")

House3E <- GMMet %>%
  filter(station == "House3E") %>%
  select(-station)

write_csv(House3E, file = "Station/House3E.csv")

House4E <- GMMet %>%
  filter(station == "House4E") %>%
  select(-station)

write_csv(House4E, file = "Station/House4E.csv")

House1L <- GMMet %>%
  filter(station == "House1L") %>%
  select(-station)

write_csv(House1L, file = "Station/House1L.csv")

House2L <- GMMet %>%
  filter(station == "House2L") %>%
  select(-station)

write_csv(House2L, file = "Station/House2L.csv")

House3L <- GMMet %>%
  filter(station == "House3L") %>%
  select(-station)

write_csv(House3L, file = "Station/House3L.csv")

House4L <- GMMet %>%
  filter(station == "House4L") %>%
  select(-station)

write_csv(House4L, file = "Station/House4L.csv")
