# spatial-info-predictors-bycouncil.R
######################################
# Janelle L. Morano

# Quantify spatial information in US marine stocks, by Councils

# last updated 20 February 2025
###############################################
###############################################

library(tidyverse)
library(janitor)
library(lme4)
library(corrplot)


#####---- Data preparation
#####
# Use "5-US Marine Fisheries Stocks and Assessments-predictors-byCouncil_20250130") csv since it was cleaned in "2-spatial-info-predictors-national.R"
sic <- read.csv("/Users/janellemorano/Git/spatial-info-ms/data/5-US Marine Fisheries Stocks and Assessments-predictors-byCouncil_20250130", header = TRUE, na.strings = c(""))
str(sic)

sic <- sic |> 
  mutate_at(c("SI.stock.boundaries",
            "SI.data.prep",
            "SI.model",
            "SI.sci.advice",
            "Assessment.Model.Type",
            "Spatially.Explict.Implict",
            "Rebuilding.Program.Status",
            "Ave.Metric.Tons_Commercial",
            "Ave.Metric.Tons_Recreational",
            "Ave.Dollars.Adj_Commercial",
            "PerTon_Ave.Dollars.Adj_Commercial",
            "Schooling",
            "Max.Length.m",
            "Transboundary",
            "Fisheries.Dep.Only",
            "Ecological.Num"), 
          as.numeric)
# Warning about coercion is ok


# Group by council and convert counts to proportions
by.council <- sic %>%
  group_by(Mgmt.Council) %>%
  summarize(n.Assessments = n(),
            n.Species = n_distinct(Scientific.Name),
            boundaries.Y = sum(SI.stock.boundaries == 1, na.rm = TRUE),
            dataprep.Y = sum(SI.data.prep == 1, na.rm = TRUE),
            model.Y = sum(SI.model == 1, na.rm = TRUE),
            sciadv.Y = sum(SI.sci.advice == 1, na.rm = TRUE),
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


#####---- Correlation
c <- cor(by.council[sapply(by.council, is.numeric)])
# Plot the proportional values only
corrplot(c[c(27:32), 
           c(33:43)], method = 'color')

# High positive correlation between Bathydemersal fisheries and Sci.Advice
# High positive correlation between Pelagic and use in all stages
# High negative correlation between reef-associated in all stages
# Positive correlation between schooling and Data.Prep
# Positive correlation between FisheriesDep only and model

# Conclude: regional management is the biggest influence on the use of spatial information

#-----
# Bar graph of tons of commercial, tons of recreational, and order councils large to small left to right
ggplot(by.council, aes(y = Ave.Dollars.Adj_Commercial, x = Mgmt.Council, )) +
  geom_point()





#######----- Predicting factors on use of spatial info BY COUNCIL REGION with a Binomial Logistic Regression or Poisson with random intercepts (for Councils)
# Convert to factors
sip$Ecological <- as.factor((sip$Ecological))
sip$Mgmt.Council <- as.factor(sip$Mgmt.Council)

# Hypotheses: Factors that drive stock assessments to incorporate spatial info differ for each region...

##-- H1. If catch-only data is only available (**Fisheries.Dep.Only**), spatial info is LESS LIKELY to be used in data prep or more advanced model types 
h1 <- lmer(SI.data.prep ~ Assessment.Model.Type + (1 | Mgmt.Council), data = sip)
summary(h1)
confint(h1)
ranef(h1)$Mgmt.Council

##-- H2. If the fishery is in rebuilding status (**Rebuilding.Program.Status**), spatial info is MORE LIKELY to be used in data prep or more advanced model types 
h2 <- lmer(SI.data.prep ~ Rebuilding.Program.Status + (1 | Mgmt.Council), data = sip)
summary(h2)

##-- H3. If the species is pelagic-oceanic, piscivorous/omnivorous or pelagic-neritic, piscivorous/omnivorous (**Ecological**), spatial info is MORE LIKELY to be used in data prep or more advanced model types (following Neubauer et al. 2018)
h3 <- lmer(SI.data.prep ~ Ecological + (1|Mgmt.Council), data = sip)
summary(h3)

##-- H4. If the species is larger-sized (**Max.Length.m**), spatial info is MORE LIKELY to be used in data prep or more advanced model types
h4 <- lmer(SI.data.prep ~ Max.Length.m + (1|Mgmt.Council), data = sip)
summary(h4)

##-- H5. If the total commercial value or per ton value is high (**Ave.Metric.Tons_Commercial; PerTon_Ave.Dollars.Adj_Commercial**), spatial info is MORE LIKELY to be used in data prep or more advanced model types
h5 <- lmer(SI.data.prep ~ PerTon_Ave.Dollars.Adj_Commercial + (1|Mgmt.Council), data = sip)
summary(h5)

##-- H6. If the recreational catch is high (**Ave.Metric.Tons_Recreational**), spatial info is MORE LIKELY to be used in data prep or more advanced model types
h6 <- lmer(SI.data.prep ~ Ave.Metric.Tons_Recreational + (1|Mgmt.Council), data = sip)
summary(h6)





