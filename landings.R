# landings.R
######################################
# Janelle L. Morano

# Quantify value of stocks by landing and ex-vessel

# last updated 10 December 2024
###############################################
###############################################

library(tidyverse)
library(janitor)
library(cowplot)

# Suresh Sethi pulled data via https://www.fisheries.noaa.gov/national/sustainable-fisheries/fisheries-united-states 
landings <- read.csv("/Users/janellemorano/Git/spatial-info-ms/WestStates_AllSppYears_landings.csv", header = TRUE)
colnames(landings)

landings2 <- landings |>
  group_by(State) |>
  filter(Year == 2020) |>
  slice_max(Metric.Tons, n = 5)

landings3 <- landings |>
  filter(Year > 2020) |>
  group_by(State) |>
  mutate(TotalDollars = sum(Dollars)) |> #messing this up somehow
  slice_max(TotalDollars, n = 5) 
