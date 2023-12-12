#' @title readAMS
#' @description read AQ station excel from readAMS
#' @param file character, path to file
#' @param long logical, whether to return long tibble or a list tibble for wide data
#' @param tz timezone default is set to "Africa/Johannesburg"
#' @return a tibble fro wide or a list of tibbles for long
#' @export


readAMS <- function(file, long = FALSE, tz = "Africa/Johannesburg") {


  df_header <- read.csv(file, rows = 1, colNames = F) %>%
    as.character() %>%
    I() %>%
    read_csv(col_names = F)  %>%
    suppressMessages() %>%
    pivot_longer(cols = everything(), names_to = "Meta_data") %>%
    rowwise() %>%
    mutate(Meta_data = str_extract(value, pattern = "^.+: ") %>%
             str_remove(": "),
           value =  str_remove_all(value, pattern = "^.+: ")) %>%
    pivot_wider(names_from = Meta_data,
                values_from = value) %>%
    janitor::clean_names()

  df <- read.csv(file, startRow = 2) %>%
    tibble()

  df_units <- df[1,2:ncol(df)] %>%
    rename_with(.fn = ~str_to_lower(.)) %>%
    pivot_longer(cols = everything(),
                 names_to = "variable",
                 values_to = "unit")

  df_footer <- df[(nrow(df)-9):nrow(df),]

  df_data <- df[-c(1, (nrow(df)-9):nrow(df)),] %>%
    rename(date = Date.Time) %>%
    mutate(date = lubridate::parse_date_time(date, orders = "HM dmy",
                                             tz = tz) - hours(1),
           across(.cols = !starts_with("date"), .fns = ~as.numeric(.))) %>%
    rename_with(.fn = ~str_to_lower(.))

  rm(df)

  if(isFALSE(long)) {
    return(list(df_header = df_header,
                df_footer = df_footer,
                df_units = df_units,
                df_data = df_data,
                file = file))
  }

  return(df_data %>%
           pivot_longer(cols = -date,
                        values_to = "value",
                        names_to = "variable") %>%
           left_join(df_units, by = "variable") %>%
           mutate(place = df_header$station_name,
                  instrument = "AMS",
                  avg_period = df_header$time_base %>% str_to_lower()))

}
