# landings.R
######################################
# Janelle L. Morano

# Quantify value of stocks by landings and ex-vessel

# last updated 17 December 2024
###############################################
###############################################

library(tidyverse)
library(janitor)
library(cowplot)

#----- Data Preparation
# Landings, by region 2000-2023 from https://www.fisheries.noaa.gov/national/sustainable-fisheries/fisheries-united-states 
landings <- read.csv("/Users/janellemorano/Git/spatial-info-ms/data/FOSS_landings_USRegions-2000-2023.csv", header = TRUE, na.strings = c(""))
colnames(landings)
# 
landings <- landings |>
  filter(Region.Name != "Great Lakes")

# landings$Pounds <- as.numeric(landings$Pounds)
landings$Metric.Tons <- as.numeric(landings$Metric.Tons)
landings$Dollars <- as.numeric(landings$Dollars)
# landings$Year <- as.integer(landings$Year)
str(landings)

# Adjust dollars for inflation
inflt <- read.csv("/Users/janellemorano/Git/spatial-info-ms/data/US_CPI_Deflate.csv", header = TRUE)
inflt <- inflt |>
  mutate(RefYr2023HistoricalPriceInflator = 304.702/Annual)

# Multiply Dollars in landings by the price inflation
landings <- landings %>%
  left_join(inflt, 
            by = c("Year"),relationship = "many-to-many") |>
  mutate(Dollars.Adj = Dollars * RefYr2023HistoricalPriceInflator)
range(landings$Dollars.Adj,na.rm=TRUE)


#----- Summarize Landings by stock and region
landings2 <- landings |>
  group_by(NMFS.Name, Region.Name) |>
  summarise(Ave.Metric.Tons = mean(Metric.Tons))

top.landings <- landings2 |>
  group_by(Region.Name) |>
  slice_max(Ave.Metric.Tons, n = 15)




#----- Summarize Ex-Vessel Price (adjusted) by stock and region
landings.dol <- landings |>
  group_by(Region.Name, NMFS.Name, Scientific.Name) |>
  drop_na(Dollars.Adj) |>
  summarise(Max.Dollars.Adj = max(Dollars.Adj))

top.dollars <- landings.dol |>
  group_by(Region.Name) |>
  slice_max(Max.Dollars.Adj, n = 15)


#----- Bring landings and dollars into stock database
si <- read.csv("/Users/janellemorano/Git/spatial-info-ms/data/US Marine Fisheries Stocks and Assessments_20241212.csv", header = TRUE, na.strings = c(""))
colnames(si)

unique(landings$Region.Name)
# [1] "Alaska"          "Gulf"            "Middle Atlantic" "New England"     "Pacific Coast"  
# [6] "South Atlantic"  "Hawaii" 

# Add Region.Name to match to landings
si <- si |>
  mutate(NOAA.Region.Name = case_when(Council.Abbv == "NPFMC" ~ "Alaska",
                                 Council.Abbv == "GMFMC" ~ "Gulf",
                                 Council.Abbv == "MAFMC" ~ "Middle Atlantic",
                                 Council.Abbv == "NEFMC" | Council.Abbv == "NEFMC-MAFMC" ~ "New England",
                                 Council.Abbv == "PFMC" ~ "Pacific Coast",
                                 Council.Abbv == "SAFMC" | Council.Abbv == "CFMC" | Council.Abbv == "GMFMC-SAFMC" ~ "South Atlantic",
                                 Council.Abbv == "WPFMC" | Council.Abbv == "PFMC-WPFMC" ~ "Hawaii"))

landings.dol <- landings.dol |>
  rename(NOAA.Region.Name = Region.Name)

new.si <-  si |>
  left_join(landings.dol, by=c("Scientific.Name", "NOAA.Region.Name"))

#ICCAT species are not included. They need to be summed and added manually

write.csv(new.si, "/Users/janellemorano/Git/spatial-info-ms/data/US Marine Fisheries Stocks and Assessments_20241212_dollars.csv")
