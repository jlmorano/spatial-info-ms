# spatial-info-predictors.R
######################################
# Janelle L. Morano

# Quantify spatial information in US marine stocks

# last updated 17 January 2025
###############################################
###############################################

library(tidyverse)
library(janitor)

si <- read.csv("/Users/janellemorano/Git/spatial-info-ms/data/US Marine Fisheries Stocks and Assessments-Analysis_20250116.csv", header = TRUE, na.strings = c(""))
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
si$Fisheries.Ind <- as.numeric(si$Fisheries.Ind)

# Convert Ave.Dollars.Adj_Commercial to price per ton
si$PerTon_Ave.Dollars.Adj_Commercial <- si$Ave.Dollars.Adj_Commercial/si$Ave.Metric.Tons_Commercial
si[sapply(si, is.infinite)] <- 0

summary(si)



#---- For analyses with landings volume and value, there are duplicates by species and unavailability for multi-species complexes
# Drop duplicate assessments for the same species and multispecies complexes
si.2 <- distinct(si, pick(Common.Name, Council.Abbv, SI.data.prep, SI.model, SI.other, Assessment.Model.Type, Ave.Metric.Tons_Commercial, Ave.Metric.Tons_Recreational, Ave.Dollars.Adj_Commercial, Ecological, Schooling, Max.Length.m), .keep_all = TRUE)

# Drop multispecies complexes
si.2 <- filter(si.2, !Common.Name == "Multispecies Complex")




#######----- Predicting ecological factors on use of spatial info
library(mgcv)
library(lme4)

# Binomial Logistic Regression for national stocks using spatial info in data prep based on ecological; 
m1 <- glm(SI.data.prep ~ Ecological, data = si.2, family = binomial(link = "logit"))
summary(m1)
m2 <- glm(SI.data.prep ~ Schooling, data = si.2, family = binomial(link = "logit"))
summary(m2)
m3 <- glm(SI.data.prep ~ PerTon_Ave.Dollars.Adj_Commercial, data = si.2, family = binomial(link = "logit"))
summary(m3)
m4 <- glm(SI.data.prep ~ Assessment.Model.Type, data = si.2, family = binomial(link = "logit"))
summary(m4)

m5 <- glm(SI.model ~ Ecological + Council.Abbv, data = si.2, family = binomial(link = "logit"))
summary(m5)
m6 <- glm(SI.model ~ Schooling + Council.Abbv, data = si.2, family = binomial(link = "logit"))
summary(m6)
m7 <- glm(SI.model ~ PerTon_Ave.Dollars.Adj_Commercial + Council.Abbv, data = si.2, family = binomial(link = "logit"))
summary(m7)
m8 <- glm(SI.model ~ Assessment.Model.Type + Council.Abbv, data = si.2, family = binomial(link = "logit"))
summary(m8)


# Hypothesis: Factors that drive stock assessments to incorporate spatial info differ for each region.
# Test for predictors of number of stocks using spatial info for each region


si.2$Council.Abbv <- as.factor(si.2$Council.Abbv)
library(car)
vif(glmer(SI.model ~  PerTon_Ave.Dollars.Adj_Commercial + (1|Council.Abbv), data = si.2, family = binomial))

si.2$PerTon_Ave.Dollars.Adj_Commercial <- scale(si.2$PerTon_Ave.Dollars.Adj_Commercial)
m10 <- glmer(SI.model ~  PerTon_Ave.Dollars.Adj_Commercial + (1|Council.Abbv), data = si.2, family = binomial)
summary(m10)

