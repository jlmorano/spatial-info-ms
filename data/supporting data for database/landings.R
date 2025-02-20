# landings.R
######################################
# Janelle L. Morano

# THIS IS FOR INTERMEDIATE STOCK DATABASE BUILDING
# PRODUCTS HERE ARE NOT FOR FINAL ANALYSIS
# Quantify value of stocks by landings and ex-vessel
# This is used to integrate back into stock database, but this had to be done manually

# last updated 6 January 2025
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


# Summarize Landings & Dollars.Adj by stock and region
landings <- landings |>
  group_by(NMFS.Name, Region.Name, Scientific.Name, Collection) |>
  summarise(Ave.Metric.Tons = mean(Metric.Tons, na.rm = TRUE),
            Ave.Dollars.Adj = mean(Dollars.Adj, na.rm = TRUE)) |>
  pivot_wider(names_from = Collection, values_from = c(Ave.Metric.Tons, Ave.Dollars.Adj)) |>
  select(!Ave.Dollars.Adj_Recreational)

top.Comm.landings <- landings |>
  select(!c(Ave.Metric.Tons_Recreational, Ave.Dollars.Adj_Commercial)) |>
  filter_at(vars(Ave.Metric.Tons_Commercial), all_vars(!is.na(.))) |>
  group_by(Region.Name) |>
  arrange(desc(Ave.Metric.Tons_Commercial)) 

top.Rec.landings <- landings |>
  select(!c(Ave.Metric.Tons_Commercial, Ave.Dollars.Adj_Commercial)) |>
  filter_at(vars(Ave.Metric.Tons_Recreational), all_vars(!is.na(.))) |>
  group_by(Region.Name) |>
  arrange(desc(Ave.Metric.Tons_Recreational)) 

top.Comm.dollars <- landings |>
  select(!c(Ave.Metric.Tons_Commercial, Ave.Metric.Tons_Recreational)) |>
  filter_at(vars(Ave.Dollars.Adj_Commercial), all_vars(!is.na(.))) |>
  group_by(Region.Name) |>
  arrange(desc(Ave.Dollars.Adj_Commercial))



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
                                 Council.Abbv == "WPFMC" | Council.Abbv == "PFMC-WPFMC" ~ "Hawaii",
                                 .default = "NOAA HMS, ICCAT"))

landings <- landings |>
  rename(NOAA.Region.Name = Region.Name)

new.si <-  si |>
  left_join(landings, by=c("Scientific.Name", "NOAA.Region.Name"))


# Standardize Ave.Dollars.Adj by Metric Tons, such that it's USD$/ton
new.si$PerTon.Ave.Dollars.Adj_Commercial <- (new.si$Ave.Dollars.Adj_Commercial/new.si$Ave.Metric.Tons_Commercial)/1000

## NOTE: Lots of recreational landings are missing in new.si due to missing scientific names and NMFS Name not being readily connected the the si database. Therefore, recreational data had to be manually verified and added.

write.csv(new.si, "/Users/janellemorano/Git/spatial-info-ms/data/US Marine Fisheries Stocks and Assessments_20241212_dollars.csv")
## ^^this was used to add columns of landings and $$ to the database
##
## NOTE: some species are not in this "new.si" because they have scientific.name missing and/or are Atlantic HMS and landings across east coast regions must be summed. These were done by hand by reviewing "landings" and were manually added to the final dataset.
##
