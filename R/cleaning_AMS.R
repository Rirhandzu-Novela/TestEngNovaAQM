EAMS_sizzerred <- EAMS_Raw %>%
  mutate(across(where(is.numeric), ~case_when(. <= 0 ~ NA,
                                              is.infinite(.) ~ NA,
                                              .default = .))) %>%
  mutate(batt_diff = slide_index_dbl(.x = battv, .i = date,  .f = ~{range(.x, na.rm = T)[2] - range(.x, na.rm = T)[1]}, .before = minutes(30), .after = minutes(30), .complete = F)) %>%
  mutate(pm10 = case_when(pm10 > 500 ~ NA, .default = pm10)) %>%
  mutate(pm10 = case_when(between(date, as.Date("2023-01-01"), as.Date("2023-04-24")) ~ NA, .default = pm10)) %>%
  mutate(pm2.5 = case_when(between(date, as.Date("2023-01-01"), as.Date("2023-02-04")) ~ NA, .default = pm2.5)) %>%
  mutate(pm2.5 = case_when(between(date, as.Date("2023-02-24"), as.Date("2023-03-11")) ~ NA, .default = pm2.5)) %>%
  mutate(pm2.5 = case_when(between(date, as_datetime("2023-03-31 21:46:00"), as_datetime("2023-05-15 19:00:00")) ~ pm2.5 - 33, .default = pm2.5)) %>%
  mutate(pm2.5 = case_when(between(date, as_datetime("2023-05-15 19:00:00"), as_datetime("2023-06-19 00:00:00")) ~ NA, .default = pm2.5)) %>%
  mutate(pm2.5 = case_when(between(date, as_datetime("2023-09-23 11:00:00"), as_datetime("2023-10-31 00:00:00")) ~ NA, .default = pm2.5)) %>%
  mutate(pm2.5 = case_when(between(date, as_datetime("2023-11-10 11:00:00"), as_datetime("2023-11-16 12:00:00")) ~ pm2.5 - 21, .default = pm2.5)) %>%
  mutate(pm2.5 = case_when(between(date, as_datetime("2023-11-27 00:00:00"), as_datetime("2023-12-12 10:00:00")) ~ NA, .default = pm2.5)) %>%
  mutate(pm2.5 = case_when(between(date, as_datetime("2023-12-26 10:00:00"), as_datetime("2024-01-09 12:00:00")) ~ NA, .default = pm2.5)) %>%
  mutate(pm2.5 = case_when(between(date, as_datetime("2024-01-13 23:00:00"), as_datetime("2024-01-31 00:00:00")) ~ NA, .default = pm2.5)) %>%
  mutate(pm2.5 = case_when(pm2.5 < 0 ~ NA, .default = pm2.5)) %>%
  mutate(pm2.5 = case_when(pm2.5 > 500 ~ NA, .default = pm2.5)) %>%
  mutate(so2 = case_when(between(date, as_datetime("2023-07-24 00:00:00"), as_datetime("2023-08-14 00:00:00")) ~ NA, .default = so2)) %>%
  mutate(so2 = case_when(slide_dbl(so2, max, .before = 1000, .after = 1000) > 750 ~ NA, .default = so2),
         nox = case_when(batt_diff > 0.02 | battv < 12.7 ~ NA, .default = nox),
         nox = case_when(month(date) %in% c(1, 2, 3, 12) ~ NA, .default = nox),
         co = case_when(batt_diff > 0.02 | battv < 12.7 ~ NA, .default = co),
         co = case_when(month(date) %in% c(1, 2, 3) | as.Date(date) == as.Date("2023-04-25") ~ NA, .default = co)
         # h2s = case_when(batt_diff > 0.02 | battv < 12.7 ~ NA, .default = h2s),
         # h2s = case_when(month(date) %in% c(1, 2, 3) ~ NA, .default = h2s)
  )

LAMS_sizzerred <- LAMS_Raw %>%
  mutate(across(where(is.numeric), ~case_when(. <= 0 ~ NA,
                                              is.infinite(.) ~ NA,
                                              .default = .))) %>%
  # mutate(across(where(is.numeric), ~case_when(batt_diff > 0.02 | battv < 12.7 ~ NA, .default = .))) %>%
  mutate(batt_diff = slide_index_dbl(.x = battv, .i = date,  .f = ~{range(.x, na.rm = T)[2] - range(.x, na.rm = T)[1]}, .before = minutes(30), .after = minutes(30), .complete = F)) %>%
  mutate(pm10 = case_when(pm10 > 500 ~ NA, .default = pm10)) %>%
  mutate(pm10 = case_when(between(date, as.Date("2023-01-01"), as.Date("2023-02-04")) ~ NA, .default = pm10)) %>%
  mutate(pm10 = case_when(between(date, as.Date("2023-02-09"), as.Date("2023-02-11")) ~ NA, .default = pm10)) %>%
  mutate(pm10 = case_when(between(date, as.Date("2023-03-22"), as.Date("2023-03-28")) ~ NA, .default = pm10)) %>%
  mutate(pm10 = case_when(between(date, as.Date("2023-12-03"), as.Date("2023-12-31")) ~ NA, .default = pm10)) %>%
  mutate(pm2.5 = case_when(pm2.5 > 160 ~ NA, .default = pm2.5)) %>%
  mutate(pm2.5 = case_when(between(date, as.Date("2023-01-01"), as.Date("2023-02-04")) ~ NA, .default = pm2.5)) %>%
  mutate(pm2.5 = case_when(between(date, as.Date("2023-02-09"), as.Date("2023-02-11")) ~ NA, .default = pm2.5)) %>%
  mutate(pm2.5 = case_when(between(date, as.Date("2023-12-02"), as.Date("2023-12-31")) ~ NA, .default = pm2.5)) %>%
  mutate(so2 = case_when(between(date, as.Date("2023-02-20"), as.Date("2023-03-30")) ~ NA, .default = so2)) %>%
  mutate(so2 = case_when(between(date, as.Date("2023-01-01"), as.Date("2023-01-18")) ~ NA, .default = so2)) %>%
  mutate(so2 = case_when(between(date, as.Date("2023-12-03"), as.Date("2023-12-31")) ~ NA, .default = so2)) %>%
  mutate(ws = case_when(ws > 20 ~ NA, .default =  ws)) %>%
  mutate(co = case_when(batt_diff > 0.02 | battv < 12.7 ~ NA, .default = co),
         co = case_when(between(date, as.Date("2023-04-01"), as.Date("2023-04-20")) ~ co, .default = NA),
         nox = case_when(batt_diff > 0.02 | battv < 12.7 ~ NA, .default = nox),
         nox = case_when(month(date) %in% c(1, 2, 3) ~ NA, .default = nox))
