> EAMS_DailyC %>%
  +     select(pm2.5, month)
# A tibble: 356 x 2
pm2.5 month
<dbl> <ord>
  1    NA Jan
2    NA Jan
3    NA Jan
4    NA Jan
5    NA Jan
6    NA Jan
7    NA Jan
8    NA Jan
9    NA Jan
10    NA Jan
# i 346 more rows
# i Use `print(n = ...)` to see more rows
> EAMS_DailyC %>%
  +     select(pm2.5, month) %>%
  + mutate(n = length(na.omit(pm2.5)))
# A tibble: 356 x 3
pm2.5 month     n
<dbl> <ord> <int>
  1    NA Jan     176
2    NA Jan     176
3    NA Jan     176
4    NA Jan     176
5    NA Jan     176
6    NA Jan     176
7    NA Jan     176
8    NA Jan     176
9    NA Jan     176
10    NA Jan     176
# i 346 more rows
# i Use `print(n = ...)` to see more rows
> pm2.5 <- EAMS_DailyC %>%
  +     select(pm2.5, month) %>%
  + mutate(n = length(na.omit(pm2.5)))
> table(pm2.5$n, pm2.5$month)

Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
176  22  28  31  30  31  30  31  31  30  31  30  31
> pm2.5 <- EAMS_DailyC %>%
  +     select(pm2.5, month) %>%
  + mutate(n = length(which(!is.na(pm2.5))), .by = month)
> table(pm2.5$n, pm2.5$month)

Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
0   22   0   0   0   0   0   0   0   0  31   0   0
12   0   0   0   0   0  30   0   0  30   0   0  31
13   0   0   0   0   0   0   0   0   0   0  30   0
15   0   0   0   0  31   0   0   0   0   0   0   0
18   0   0   0   0   0   0   0  31   0   0   0   0
20   0  28   0   0   0   0   0   0   0   0   0   0
21   0   0  31   0   0   0   0   0   0   0   0   0
23   0   0   0   0   0   0  31   0   0   0   0   0
30   0   0   0  30   0   0   0   0   0   0   0   0
> pm2.5 <- EAMS_DailyC %>%
  +     +     select(pm2.5, month) %>%
  +     + mutate(n = length(which(!is.na(pm2.5))) / n(), .by = month)
Error in Ops.data.frame(., select(pm2.5, month)) :
  ‘+’ only defined for equally-sized data frames
> pm2.5 <- EAMS_DailyC %>%select(pm2.5, month) %>%
  +     + mutate(n = length(which(!is.na(pm2.5))) / n(), .by = month)
Error in `n()`:
  ! Must only be used inside data-masking verbs like `mutate()`, `filter()`, and `group_by()`.
Run `rlang::last_trace()` to see where the error occurred.
> pm2.5 <- EAMS_DailyC %>%select(pm2.5, month) %>% mutate(n = length(which(!is.na(pm2.5))) / n(), .by = month)
> table(pm2.5$n, pm2.5$month)

Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
0                  22   0   0   0   0   0   0   0   0  31   0   0
0.387096774193548   0   0   0   0   0   0   0   0   0   0   0  31
0.4                 0   0   0   0   0  30   0   0  30   0   0   0
0.433333333333333   0   0   0   0   0   0   0   0   0   0  30   0
0.483870967741935   0   0   0   0  31   0   0   0   0   0   0   0
0.580645161290323   0   0   0   0   0   0   0  31   0   0   0   0
0.67741935483871    0   0  31   0   0   0   0   0   0   0   0   0
0.714285714285714   0  28   0   0   0   0   0   0   0   0   0   0
0.741935483870968   0   0   0   0   0   0  31   0   0   0   0   0
1                   0   0   0  30   0   0   0   0   0   0   0   0
> EBoxPM2.5Compare <- ggplot(data = EAMS_DailyC %>%
                               +                              select(pm2.5, month) %>%
                               +                              mutate(n = length(which(!is.na(pm2.5))) / n(), .by = month),
                             +                            aes(x = month, y = pm2.5 )) +
  +   geom_boxplot(aes(fill = n)) +
  +   geom_hline(yintercept = 40, linetype = "dashed", color = "red") +
  +   theme_bw() +
  +   scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  +   labs(
    +     x = "Month",
    +     y = "PM2.5",
    +     title = "Monthly statistical summary of PM2.5 at eMbalenhle ",
    +     caption = "Data from AMS") +
  +   theme(legend.position = "bottom")
> EBoxPM2.5Compare
Warning message:
  Removed 180 rows containing non-finite values (`stat_boxplot()`).
> en <- makeAqmConstants(
  +   lat = -26.38097458,
  +   lon = 28.93727302,
  +   begin = structure(1532088000, class = c("POSIXct", "POSIXt"), tzone = "Africa/Johannesburg"),
  +   einde = structure(1561938600, class = c("POSIXct", "POSIXt"), tzone = "Africa/Johannesburg"),
  +   pols = c("pm10", "pm2.5", "no2", "so2", "o3", "co"),
  +   met = c("relHum", "ws", "temp", "wd", "pressure"),
  +   NAAQS = TRUE,
  +   NAAQS_ppb = TRUE,
  +   NAAQS_ppm = TRUE,
  +   meltStandards = TRUE
  + )
-26.3809745828.9372730215320880001561938600c("pm10", "pm2.5", "no2", "so2", "o3", "co")c("relHum", "ws", "temp", "wd", "pressure")raw_data/TRUETRUETRUETRUEFALSE
Joining with `by = join_by(pollutant)`
Joining with `by = join_by(pollutant)`
Joining with `by = join_by(pollutant)`
Joining with `by = join_by(pollutant)`
Joining with `by = join_by(pollutant)`
Joining with `by = join_by(pollutant)`
Joining with `by = join_by(pollutant)`
Joining with `by = join_by(pollutant)`
Joining with `by = join_by(pollutant)`
> ls(env = en)
[1] "begin"            "df_standard_unit" "dfNAAQS"          "dfNAAQS_ppb"      "dfNAAQS_ppm"      "einde"
[7] "lat"              "lon"              "meltStandards"    "met"              "NAAQS"            "NAAQS_ppb"
[13] "NAAQS_ppm"        "pols"             "rawdir"           "sonderPunte"
> en$df_standard_unit
# A tibble: 14 x 7
pollutant unit   `10min` `24h`     a     h    h8
<chr>     <chr>    <dbl> <dbl> <dbl> <dbl> <dbl>
  1 co        µg.m-3      NA    NA    NA  30      10
2 co        ppb         NA    NA    NA  NA      NA
3 co        ppm         NA    NA    NA   8.7    26
4 h2s       ppb         40    NA    NA  40      NA
5 no        µg.m-3      NA    NA    NA  NA      NA
6 no        ppb         NA    NA    NA  NA      NA
7 no2       µg.m-3      NA    NA    40 200      NA
8 no2       ppb         NA    NA    21 106      NA
9 o3        µg.m-3      NA    NA    NA  NA     120
10 o3        ppb         NA    NA    NA  NA      61
11 pm10      µg.m-3      NA    75    40  NA      NA
12 pm2.5     µg.m-3      NA    40    20  NA      NA
13 so2       µg.m-3     500   125    50 350      NA
14 so2       ppb        191    48    19 134      NA
> summarise <- summarise_station(df = AMS_clean,
                                 +                               en = en,
                                 +                               pol_units = c(pm10 = "µg.m-3", pm2.5 = "ug.m-3", co = "ppm", no2 = "ppb", so2 = "ppb"),
                                 +                               df_standard_units = en$df_standard_unit,
                                 +                               verbose = TRUE,
                                 +                               debug = TRUE)
Error in anti_join(., df_standard_units) : object 'dfPolUnits' not found
> traceback()
4: anti_join(., df_standard_units)
3: dfPolUnits %>% anti_join(df_standard_units)
2: nrow(dfPolUnits %>% anti_join(df_standard_units))
1: summarise_station(df = AMS_clean, en = en, pol_units = c(pm10 = "µg.m-3",
                                                            pm2.5 = "ug.m-3", co = "ppm", no2 = "ppb", so2 = "ppb"),
                     df_standard_units = en$df_standard_unit, verbose = TRUE,
                     debug = TRUE)
> view(summarise_station)
> View(summarise_station)
> df_standard_units = en$df_standard_unit
>  pol_units <- subset(pol_units, names(pol_units) %in%
                         +       df_standard_units$pollutant)
Error in subset(pol_units, names(pol_units) %in% df_standard_units$pollutant) :
  object 'pol_units' not found
> pol_units = c(pm10 = "µg.m-3", pm2.5 = "ug.m-3", co = "ppm", no2 = "ppb", so2 = "ppb")
>  pol_units <- subset(pol_units, names(pol_units) %in%
                         +       df_standard_units$pollutant)
> pol_units
pm10    pm2.5       co      no2      so2
"µg.m-3" "ug.m-3"    "ppm"    "ppb"    "ppb"
> class(pol_units)
[1] "character"
> as.data.frame(pol_units)
pol_units
pm10     µg.m-3
pm2.5    ug.m-3
co          ppm
no2         ppb
so2         ppb
> summarise <- summarise_station(df = AMS_clean,
                                 +                               en = en,
                                 +                               pol_units = c(pm10 = "µg.m-3", pm2.5 = "ug.m-3", co = "ppm", no2 = "ppb", so2 = "ppb"),
                                 +                               df_standard_units = en$df_standard_unit,
                                 +                               verbose = TRUE,
                                 +                               debug = TRUE)
Error in anti_join(., df_standard_units) : object 'dfPolUnits' not found
