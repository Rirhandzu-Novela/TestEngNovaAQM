# Load packages -----------------------------------------------------------

library(tidyverse)
library(openair)
library(novaAQM)


# Complete process with NovaAQM -------------------------------------------

# Functions from NovaAQM

# Read in data with Lees functions (leesNWU, readSAAQIS...)
# Clean data with wasStasie
# Inspect cleaned data with FindRuns, vindLopies, inspect_station, inspect_day
# Get summary from tabDataVaslegging (min, max completeness..), tenpointsummary(quantiles, skewness, kurtosis)

# DATA ANALYSIS
# Datity - to get seasons, month, day, hour... columns
# vergelykStasiesUur - compare hourly data within the station
# vergelykStasiesMaand - compare different stations on monthly basis
# compareAQS - used columns from datify, give summary (like tenpointsummary) and exceedances
# MakeAQMConstant - creates contants (to be used in the other functions)
# summarise_station - to get hourly, daily summarised df, and summary like compareAQS
# verpakStasies - Depends on en from MakeAQMConstants, and use summarise_station, gives list of hourly, daily and standards data

# PLOTS

# plotPollutionDays -  takes results from verpakStasies and daily time series (mean, minimum, maximum) per substance per station
# plotPollutionMonths - takes results from verpakStasies and summarize multiple substances per month
# polar_heur - polarPlot heuristics
# polarSummary - Creates a diagram with four polar plots c("frequency", "mean", "weighted.mean", "stdev")
# polarSummaryCor - polar plots for correlation analysis
#

# REPORT
# rapporteer_stof - generate a report for a pollutant with time series, polarsummary, timevariation
# beskryf_oorskry_kalender - describes exceedances in a specified period
# beskryf_oorskryding - describes exceedance
# maakOorskySeksie - Report of exceedances using verpakStasie and beskryf_oorskryding






# Load data ---------------------------------------------------------------

#load(file = "Graph/EAMS_clean.Rda")
#load(file = "Graph/LAMS_clean.Rda")

#AMS_clean <- full_join(x = EAMS_clean, y = LAMS_clean)

#save(AMS_clean, file = "Graph/AMS_clean.Rda")

load(file = "Graph/AMS_clean.Rda")


# Compare stations --------------------------------------------------------

en <- makeAqmConstants(
  lat = -26.38097458,
  lon = 28.93727302,
  begin = structure(1532088000, class = c("POSIXct", "POSIXt"), tzone = "Africa/Johannesburg"),
  einde = structure(1561938600, class = c("POSIXct", "POSIXt"), tzone = "Africa/Johannesburg"),
  pols = c("pm10", "pm2.5", "no2", "so2", "co"),
  met = c("relHum", "ws", "temp", "wd", "pressure"),
  NAAQS = TRUE,
  NAAQS_ppb = TRUE,
  NAAQS_ppm = TRUE,
  meltStandards = TRUE
)

dfAMS <- AMS_clean %>%
  rename(place = station)


stationSum <- summarise_station(df = dfAMS, en = en)


Packed <- verpakStasies(df = df)





# Report pollution ----------------------------------------------------------


PM2.5report <- rapporteer_stof(df = dfAMS,
                pol = "pm2.5",
                pleknaam = "Embalenhle",
                opskrif = TRUE,
                praat = TRUE,
                datumnaam = "date",
                tyd = TRUE,
                pool = TRUE,
                upper = 20,
                k = 20,
                instrument = "AMS",
                timeVariation = c(stati="median", poll="pm10", conf.int = c(0.75, 0.99), col = "firebrick"))
