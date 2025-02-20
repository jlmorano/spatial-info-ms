# spatial-info-predictors-bycouncil.R
######################################
# Janelle L. Morano

# Quantify spatial information in US marine stocks, by Councils

# last updated 13 February 2025
###############################################
###############################################

library(tidyverse)
library(janitor)
library(lme4)
library(corrplot)


#####---- Data preparation
#####
# Use "predictors" csv since it was cleaned in "summary-spatial-info-in-stock-assessments.R"
si <- read.csv("/Users/janellemorano/Git/spatial-info-ms/data/4-US Marine Fisheries Stocks and Assessments-predictors_20250130.csv", header = TRUE, na.strings = c(""))
str(si)

si$Assessment.Model.Type <- as.numeric(si$Assessment.Model.Type)
si$Rebuilding.Program.Status <- as.numeric(si$Rebuilding.Program.Status)
si$SI.stock.boundaries <- as.numeric(si$SI.stock.boundaries)
si$SI.data.prep <- as.numeric(si$SI.data.prep)
si$SI.model <- as.numeric(si$SI.sci.advice)
si$SI.other <- as.numeric(si$SI.other)
si$Ave.Metric.Tons_Commercial <- as.numeric(si$Ave.Metric.Tons_Commercial)
si$Ave.Metric.Tons_Recreational <- as.numeric(si$Ave.Metric.Tons_Recreational)
si$Ave.Dollars.Adj_Commercial <- as.numeric(si$Ave.Dollars.Adj_Commercial)
si$Schooling <- as.numeric(si$Schooling)
si$Max.Length.m <- as.numeric(si$Max.Length.m)
si$Transboundary <- as.numeric(si$Transboundary)
si$Fisheries.Dep.Only <- as.numeric(si$Fisheries.Dep.Only)
si$PerTon_Ave.Dollars.Adj_Commercial <- as.numeric(si$PerTon_Ave.Dollars.Adj_Commercial)

summary(si)


#---- For analyses with landings volume and value, there are duplicates by species and unavailability for multi-species complexes
# Drop duplicate assessments for the same species and multispecies complexes
si.2 <- distinct(si, pick(Common.Name, Council.Abbv, SI.data.prep, SI.model, SI.other, Assessment.Model.Type, Ave.Metric.Tons_Commercial, Ave.Metric.Tons_Recreational, Ave.Dollars.Adj_Commercial, Ecological, Schooling, Max.Length.m), .keep_all = TRUE)

# Drop multispecies complexes
si.2 <- filter(si.2, !Common.Name == "Multispecies Complex")

# Scale the averages
si.2$PerTon_Ave.Dollars.Adj_Commercial <- scale(si.2$PerTon_Ave.Dollars.Adj_Commercial)


# Group by council and convert counts to proportions
by.council <- si.2 %>%
  group_by(Council.Abbv) %>%
  summarize(n.Assessments = n(),
            n.Species = n_distinct(Scientific.Name),
            boundaries.Y = sum(SI.stock.boundaries == 1, na.rm = TRUE),
            dataprep.Y = sum(SI.data.prep == 1, na.rm = TRUE),
            model.Y = sum(SI.model == 1, na.rm = TRUE),
            sciadv.Y = sum(SI.sci.advice == 1, na.rm = TRUE),
            other.Y = sum(SI.other == 1, na.rm = TRUE),
            n.SpImplicit = sum(Spatially.Explict.Implict == 1, na.rm = TRUE),
            n.SpExplicit = sum(Spatially.Explict.Implict == 2, na.rm = TRUE),
            rebuild.Y = sum(Rebuilding.Program.Status == 1, na.rm = TRUE),
            Ave.Metric.Tons_Commercial = mean(Ave.Metric.Tons_Commercial, na.rm = TRUE),
            Ave.Metric.Tons_Recreational = mean(Ave.Metric.Tons_Recreational, na.rm = TRUE),
            Ave.Dollars.Adj_Commercial = mean(Ave.Dollars.Adj_Commercial, na.rm = TRUE),
            PerTon_Ave.Dollars.Adj_Commercial = mean(PerTon_Ave.Dollars.Adj_Commercial, na.rm = TRUE),
            Eco.Bathy = sum(Ecological == "bathydemersal"),
            Eco.BenthPlank = sum(Ecological == "demersal/benthic, planktivorous"),
            Eco.BenthPisc = sum(Ecological == "demersal/benthic, piscivorous/omnivorous"),
            Eco.PelagBenthPlank = sum(Ecological == "benthopelagic, planktivorous"),
            Eco.PelagBenthPisc = sum(Ecological == "benthopelagic, piscivorous/omnivorous"),
            Eco.PelagOceanPisc = sum(Ecological == "pelagic-oceanic, piscivorous/omnivorous"),
            Eco.PelagNeritPlank = sum(Ecological == "pelagic-neritic, planktivorous"),
            Eco.PelagNeritPisc = sum(Ecological == "pelagic-neritic, piscivorous/omnivorous"),
            Eco.Reef = sum(Ecological == "reef-associated, piscivorous/omnivorous"),
            Schooling = sum(Schooling == 1, na.rm = TRUE),
            Transboundary = sum(Transboundary == 1, na.rm = TRUE),
            MaxSize = mean(Max.Length.m, na.rm = TRUE),
            Fisheries.Dep.Only = sum(Fisheries.Dep.Only == 1, na.rm = TRUE)
  ) |>
  mutate(prop.bound.y = boundaries.Y/n.Assessments,
         prop.data.y = dataprep.Y/n.Assessments,
         prop.model.y = model.Y/n.Assessments,
         prop.sciadv.y = sciadv.Y/n.Assessments,
         prop.spimplicit = n.SpImplicit/n.Assessments,
         prop.spexplicit = n.SpExplicit/n.Assessments,
         prop.Eco.Bathy = Eco.Bathy/n.Assessments,
         prop.Eco.BenthPlank = Eco.BenthPlank/n.Assessments,
         prop.Eco.BenthPisc = Eco.BenthPisc/n.Assessments,
         prop.Eco.PelagBenthPlank = Eco.PelagBenthPlank/n.Assessments,
         prop.Eco.PelagOceanPisc = Eco.PelagOceanPisc/n.Assessments,
         prop.Eco.PelagNeritPlank = Eco.PelagNeritPlank/n.Assessments,
         prop.Eco.PelagNeritPisc = Eco.PelagNeritPisc/n.Assessments,
         prop.Eco.Reef = Eco.Reef/n.Assessments,
         prop.Schooling = Schooling/n.Assessments,
         prop.Transboundary = Transboundary/n.Assessments,
         prop.Fisheries.Dep.Only = Fisheries.Dep.Only/n.Assessments
  )

#Change NA to 0
by.council[14, 13] <- 0



#####---- Correlation
c <- cor(by.council[sapply(by.council, is.numeric)])
corrplot(c[c(28:33), 
           c(10:14, 34:44)], method = 'color')


#-----
# Bar graph of tons of commercial, tons of recreational, and order councils large to small left to right
ggplot(by.council, aes(y = Ave.Dollars.Adj_Commercial, x = Council.Abbv, )) +
  geom_point()



