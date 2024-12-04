# surveys-stocks.R
######################################
# Janelle L. Morano

# Quantify surveys used for stock assessment

# last updated 27 November 2024
###############################################
###############################################

library(tidyverse)
library(janitor)
library(cowplot)


# It would be great to get updated data and with the Council column
surveys <- read.csv("/Users/janellemorano/Git/spatial-info-ms/surveys and stocks.csv", header = TRUE)
colnames(surveys)
unique(surveys$Stock.Region)

# Remove salmon
surveys <- surveys |> 
  filter(!grepl("salmon", Entity.Name))


surveys2 <- surveys |>
  mutate(Region = case_when( grepl("Alaska", Ecosystem) | grepl("Bering", Ecosystem) ~ "NPFMC",
                             grepl("Atlantic Highly", Ecosystem) ~ "ICCAT",
                             grepl("California", Ecosystem) ~ "PFMC",
                             grepl("Caribbean", Ecosystem) ~ "CFMC",
                             grepl("Mexico", Ecosystem) ~ "GMFMC",
                             grepl("Northeast", Ecosystem) ~ "NEFMC",
                             grepl("Pacific Highly", Ecosystem) | grepl("Pacific Islands", Ecosystem) ~ "WPFMC",
                             grepl("Southeast", Ecosystem) ~ "SAFMC",
                             .default = "other")
         ) |>
  # Add columns to identify fisheries-independent or fisheries-dependent surveys
  mutate(N.fishIndep = case_when(Survey.Type == "FINSS Survey" | Survey.Type == "Other Survey" ~ 1,
                               .default = 0),
         N.fishDep = case_when(Survey.Type == "Commercial CPUE" | Survey.Type == "Recreational CPUE" ~ 1,
                               .default = 0),
         N.fishOther = case_when(Survey.Type == "Other Method" ~ 1,
                                 .default = 0)
  )

# Summarize the number of fisheries-independent/dependent/other surveys per stock
summarysurveys <- surveys2 |>
  select(Stock, Stock.Region, Region, N.fishIndep, N.fishDep, N.fishOther) |>
  group_by(Stock, Stock.Region, Region) |>
  summarise(Tot.fishInd = sum(N.fishIndep),
            Tot.fishDep = sum(N.fishDep),
            Tot.fishOth = sum(N.fishOther)
            )

# Summarize the number of stocks per FMC region that have only fisheries-dependent data or have at least some fisheries independent data (may also have dependent data)
summary.region <- summarysurveys |>
  group_by(Region) |>
  rename(Council.Abbv = Region) |>
  summarise(Stocks.fishDep = sum(Tot.fishDep > 0 & Tot.fishInd < 1 & Tot.fishOth <1),
            Stocks.fishInd = sum(Tot.fishInd > 0), 
            count = n()) |>
  mutate(Prop.fishDep = Stocks.fishDep / count)

ggplot(summary.region, aes(x = Council.Abbv, y = Prop.fishDep)) + 
  geom_bar(stat = "identity") +
  theme_classic()


#### Run spatial-info-in-stock-assessment.R to get si.council.stack ####
test <- summary.region |> 
  left_join(si.council.stack, by = join_by(Council.Abbv) )

test2 <- test |>
  pivot_wider(names_from = c(Step, Response),
              values_from = Total) |>
  mutate(Prop.boundariesY = boundaries_Y / count,
         Prop.dataprep = dataprep_Y / count,
         Prop.modelY = model_Y / count,
         Prop.sciY = sciadvice_Y / count)


a <- ggplot() + 
  geom_point(data = test2, aes(x = Prop.boundariesY, y = Prop.fishDep, color = Council.Abbv)) +
  theme_classic() +
  ggtitle("Stock Boundaries") +
  theme(legend.position = "none")

b <- ggplot() + 
  geom_point(data = test2, aes(x = Prop.dataprep, y = Prop.fishDep, color = Council.Abbv)) +
  theme_classic() +
  ggtitle("Data Preparation") 

c <- ggplot() + 
  geom_point(data = test2, aes(x = Prop.modelY, y = Prop.fishDep, color = Council.Abbv)) +
  theme_classic() +
  ggtitle("Assessment Model") +
  theme(legend.position = "none")

d <- ggplot() + 
  geom_point(data = test2, aes(x = Prop.sciY, y = Prop.fishDep, color = Council.Abbv)) +
  theme_classic() +
  ggtitle("Scientific advice")

plot_grid(a, b, c, d, ncol = 2)

