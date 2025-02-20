# spatial-info-predictors-national.R
######################################
# Janelle L. Morano

# Quantify spatial information in US marine stocks

# last updated 20 February 2025
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

# Convert NAs in Ave.Metric.Tons_Recreational to 0
si <- si |> mutate(Ave.Metric.Tons_Commercial = ifelse(is.na(Ave.Metric.Tons_Commercial), 0, Ave.Metric.Tons_Commercial),
                   Ave.Metric.Tons_Recreational = ifelse(is.na(Ave.Metric.Tons_Recreational), 0, Ave.Metric.Tons_Recreational),
                   Ave.Dollars.Adj_Commercial = ifelse(is.na(Ave.Dollars.Adj_Commercial), 0, Ave.Dollars.Adj_Commercial)
                   )

summary(si)


#---- For analyses with landings volume and value, there are duplicates by species and unavailability for multi-species complexes
# Drop duplicate assessments for the same species and multispecies complexes
si.2 <- distinct(si, pick(Common.Name, Council.Abbv, SI.data.prep, SI.model, SI.other, Assessment.Model.Type, Ave.Metric.Tons_Commercial, Ave.Metric.Tons_Recreational, Ave.Dollars.Adj_Commercial, Ecological, Schooling, Max.Length.m), .keep_all = TRUE)

# Drop multispecies complexes
si.2 <- filter(si.2, !Common.Name == "Multispecies Complex")

# Scale the averages
si.2$PerTon_Ave.Dollars.Adj_Commercial <- scale(si.2$PerTon_Ave.Dollars.Adj_Commercial)




###############################################################################################
##### Try a PCA
###############################################################################################
# library(ggcorrplot)
# library("FactoMineR")
# 
# colSums(is.na(si.2))
# # Drop SI.other column
# 
# # Keep only numerical data...ok, so that means I need to reformat factor data to numerical...PAUSE
# corrdata <- select(si.2, c("Council.Abbv", "Rebuilding.Program.Status", "SI.stock.boundaries", "SI.data.prep", "SI.model", "SI.sci.advice", "Spatially.Explict.Implict", "Ave.Metric.Tons_Commercial", "Ave.Metric.Tons_Recreational", "Ave.Dollars.Adj_Commercial", "Ecological"))



#####----- The probability of spatial info being used in DATA PREPARATION based on the following:
#####
# 1. Assessment.Model.Type
si.2$Assessment.Model.Type <- as.factor(si.2$Assessment.Model.Type)
# 2. Rebuilding.Program.Status
# 3. Ecological
si.2$Ecological <- as.factor((si.2$Ecological))
# 4. Schooling
# 5. Max.Length.m
# 6. Ave.Metric.Tons_Commercial
# 7. Ave.Metric.Tons_Recreational
# 8. Ave.Dollars.Adj_Commercial
# 9. PerTon_Ave.Dollars.Adj_Commercial
# 10. Fisheries.Dep.Only
# With a Binomial Logistic Regression or Poisson for national stocks using spatial info in data prep


## SIGNIFICANT 1. Assessment.Model.Type
m1 <- glm(SI.data.prep ~ as.factor(Assessment.Model.Type), data = si.2, family = binomial(link = "logit"))
summary(m1)
plot(m1)
m1.res <- resid(m1)
plot(fitted(m1), m1.res, main = "Binomial")
summary(m1)$coefficients
pred.m1 <- predict(m1, type = "response") #remember, these are probabilities for each point
unique(si.2$Assessment.Model.Type)
newdata.1 <- data.frame(Assessment.Model.Type = as.factor(seq(1, 5)))
pred.m1 <- predict(m1, newdata = newdata.1, type = "response", se.fit = TRUE)
barplot(pred.m1$fit)
siglist <- list(summary(m1)$coefficients)
names(siglist)[1] <- "Asst.Model.Type"


# 2. Rebuilding.Program.Status
m2 <- glm(SI.data.prep ~ Rebuilding.Program.Status, data = si.2, family = binomial(link = "logit"))
summary(m2)
summary(m2)$coefficients
nolist <- list(summary(m2)$coefficients)
names(nolist)[1] <- "Rebuilding.Program.Status"


# 3. SIGNIFICANT Ecological (pelagic-oceanic, piscivorous/omnivorous)
## Ecologicalpelagic-oceanic, piscivorous/omnivorous Positively Significant
m3 <- glm(SI.data.prep ~ as.factor(Ecological), data = si.2, family = binomial(link = "logit"))
summary(m3)
plot(m3)
summary(m3)$coefficients
m3.res <- resid(m3)
plot(fitted(m3), m3.res, main = "Binomial")
pred.m3 <- predict(m3, type = "response") #remember, these are probabilities for each point
unique(si.2$Ecological)
newdata.m3 <- data.frame(Ecological = as.factor(unique(si.2$Ecological)))
pred.m3 <- predict(m3, newdata = newdata.m3, type = "response", se.fit = TRUE)
barplot(pred.m3$fit)
siglist[[2]] <- summary(m3)$coefficients
names(siglist)[[2]] <- "Ecological"



# 4. Schooling
m4 <- glm(SI.data.prep ~ Schooling, data = si.2, family = binomial(link = "logit"))
summary(m4)
summary(m4)$coefficients
m4.res <- resid(m4)
plot(fitted(m4), m4.res, main = "Binomial")
pred.m4 <- predict(m4, type = "response", se.fit = TRUE) #remember, these are probabilities for each point
barplot(pred.m4$fit)
nolist[[3]] <- summary(m4)$coefficients
names(nolist)[[2]] <- "Schooling"


# SIGNIFICANT 5. Max.Length.m
m5 <- glm(SI.data.prep ~ Max.Length.m, data = si.2, family = binomial(link = "logit"))
summary(m5)
summary(m5)$coefficients
m5.res <- resid(m5)
plot(fitted(m5), m5.res, main = "Binomial")
pred.m5 <- predict(m5, type = "response", se.fit = TRUE)
plot(pred.m5$fit)
siglist[[3]] <- summary(m5)$coefficients
names(siglist)[[3]] <- "Max.Length.m"


# 6. Ave.Metric.Tons_Commercial
m6 <- glm(SI.data.prep ~ Ave.Metric.Tons_Commercial, data = si.2, family = binomial(link = "logit"))
summary(m6)
summary(m6)$coefficients
m6.res <- resid(m6)
plot(fitted(m6), m6.res, main = "Binomial")
pred.m6 <- predict(m6, type = "response", se.fit = TRUE) #remember, these are probabilities for each point
plot(pred.m6$fit)
nolist[[4]] <- summary(m6)$coefficients
names(nolist)[[4]] <- "Ave.Metric.Tons_Commercial"


# 7. Ave.Metric.Tons_Recreational
m7 <- glm(SI.data.prep ~ Ave.Metric.Tons_Recreational, data = si.2, family = binomial(link = "logit"))
summary(m7)
summary(m7)$coefficients
m7.res <- resid(m7)
plot(fitted(m7), m7.res, main = "Binomial")
pred.m7 <- predict(m7, type = "response", se.fit = TRUE) #remember, these are probabilities for each point
plot(pred.m7$fit)
nolist[[5]] <- summary(m7)$coefficients
names(nolist)[[5]] <- "Ave.Metric.Tons_Recreational"


# SIGNIFICANT 8. Ave.Dollars.Adj_Commercial
m8 <- glm(SI.data.prep ~ Ave.Dollars.Adj_Commercial, data = si.2, family = binomial(link = "logit"))
summary(m8)
summary(m8)$coefficients
m8.res <- resid(m8)
plot(fitted(m8), m8.res, main = "Binomial")
pred.m8 <- predict(m8, type = "response", se.fit = TRUE)
plot(pred.m8$fit)
siglist[[4]] <- summary(m8)$coefficients
names(siglist)[[4]] <- "Ave.Dollars.Adj_Commercial"


# 9. PerTon_Ave.Dollars.Adj_Commercial
m9 <- glm(SI.data.prep ~ PerTon_Ave.Dollars.Adj_Commercial, data = si.2, family = binomial(link = "logit"))
summary(m9)
summary(m9)$coefficients
m9.res <- resid(m9)
plot(fitted(m9), m9.res, main = "Binomial")
nolist[[5]] <- summary(m9)$coefficients
names(nolist)[[5]] <- "PerTon_Ave.Dollars.Adj_Commercial"


# SIGNIFICANT 10. Fisheries.Dep.Only
m10 <- glm(SI.data.prep ~ Fisheries.Dep.Only, data = si.2, family = binomial(link = "logit"))
summary(m10)
summary(m10)$coefficients
m10.res <- resid(m10)
plot(fitted(m10), m10.res, main = "Binomial")
pred.m10 <- predict(m10, type = "response", se.fit = TRUE)
plot(pred.m10$fit)
siglist[[5]] <- summary(m10)$coefficients
names(siglist)[[5]] <- "Fisheries.Dep.Only"



#####----- Predicting factors on use of spatial info in ASSESSMENT MODEL
#####
# Test for predictors of number of stocks using spatial info for each region as separate models
# 1. Assessment.Model.Type
n1 <- glm(SI.model ~ Assessment.Model.Type, data = si.2, family = binomial(link = "logit"))
summary(n1)
summary(n1)$coefficients
n1.res <- resid(n1)
plot(fitted(n1), n1.res, main = "Binomial")



# 2. Rebuilding.Program.Status
n2 <- glm(SI.model ~ Rebuilding.Program.Status, data = si.2, family = binomial(link = "logit"))
summary(n2)
summary(n2)$coefficients
n2.res <- resid(n2)
plot(fitted(n2), n2.res, main = "Binomial")



# 3. Ecological
n3 <- glm(SI.model ~ Ecological, data = si.2, family = binomial(link = "logit"))
summary(n3)
summary(n3)$coefficients
n3.res <- resid(n3)
plot(fitted(n3), n3.res, main = "Binomial")



# 4. Schooling
n4 <- glm(SI.model ~ Schooling, data = si.2, family = binomial(link = "logit"))
summary(n4)
summary(n4)$coefficients
n4.res <- resid(n4)
plot(fitted(n4), n4.res, main = "Binomial")



# 5. Max.Length.m
n5 <- glm(SI.model ~ Max.Length.m, data = si.2, family = binomial(link = "logit"))
summary(n5)
summary(n5)$coefficients
n5.res <- resid(n5)
plot(fitted(n5), n5.res, main = "Binomial")



# 6. Ave.Metric.Tons_Commercial
n6 <- glm(SI.model ~ Ave.Metric.Tons_Commercial, data = si.2, family = binomial(link = "logit"))
summary(n6)
summary(n6)$coefficients
n6.res <- resid(n6)
plot(fitted(n6), n6.res, main = "Binomial")



# 7. Ave.Metric.Tons_Recreational
n7 <- glm(SI.model ~ Ave.Metric.Tons_Recreational, data = si.2, family = binomial(link = "logit"))
summary(n7)
summary(n7)$coefficients
n7.res <- resid(n7)
plot(fitted(n7), n7.res, main = "Binomial")



# SIGNIFICANT 8. Ave.Dollars.Adj_Commercial
n8 <- glm(SI.model ~ Ave.Dollars.Adj_Commercial, data = si.2, family = binomial(link = "logit"))
summary(n8)
summary(n8)$coefficients
n8.res <- resid(n8)
plot(fitted(n8), n8.res, main = "Binomial")
pred.n8 <- predict(n8, type = "response", se.fit = TRUE)
plot(pred.n8$fit)
siglist[[6]] <- summary(n8)$coefficients
names(siglist)[[6]] <- "model-Ave.Dollars.Adj_Commercial"


# 9. PerTon_Ave.Dollars.Adj_Commercial
n9 <- glm(SI.model ~ PerTon_Ave.Dollars.Adj_Commercial, data = si.2, family = binomial(link = "logit"))
summary(n9)
summary(n9)$coefficients
n9.res <- resid(n9)
plot(fitted(n9), n9.res, main = "Binomial")



# 10. Fisheries.Dep.Only
n10 <- glm(SI.model ~ Fisheries.Dep.Only, data = si.2, family = binomial(link = "logit"))
summary(n10)
summary(n10)$coefficients
n10.res <- resid(n10)
plot(fitted(n10), n10.res, main = "Binomial")



############
#### YOU ARE HERE. YOU NEED TO CREATE NEWDATA THAT INCLUDES COUNCIL NAME AS A FACTOR TO PREDICT
###########
#######----- Predicting factors on use of spatial info BY COUNCIL REGION
# Hypothesis: Factors that drive stock assessments to incorporate spatial info differ for each region.


si.2$Council.Abbv <- as.factor(si.2$Council.Abbv)

## SIGNIFICANT for NATIONAL 1. Assessment.Model.Type
o1 <- glm(SI.data.prep ~ as.factor(Assessment.Model.Type) + as.factor(Council.Abbv), data = si.2, family = binomial)
summary(o1)
summary(o1)$coefficients

pred.m1 <- predict(m1, type = "response") #remember, these are probabilities for each point
unique(si.2$Assessment.Model.Type)
newdata.1 <- data.frame(Assessment.Model.Type = as.factor(seq(1, 5)))
pred.m1 <- predict(m1, newdata = newdata.1, type = "response", se.fit = TRUE)
barplot(pred.m1$fit, )


## SIGNIFICANT Ecological
## Ecologicalpelagic-oceanic, piscivorous/omnivorous Positively Significant
o2 <- glm(SI.data.prep ~ Ecological + (1|Council.Abbv), data = si.2, family = binomial)


#######----- Predicting factors on use of spatial info BY COUNCIL REGION with a Binomial Logistic Regression or Poisson with random intercepts (for Councils)

# Hypotheses: Factors that drive stock assessments to incorporate spatial info differ for each region...

# H1. If catch-only data is only available (**Fisheries.Dep.Only**), spatial info is LESS LIKELY to be used in data prep or more advanced model types 

# H2. If the fishery is in rebuilding status (**Rebuilding.Program.Status**), spatial info is MORE LIKELY to be used in data prep or more advanced model types 

# H3. If the species is pelagic-oceanic, piscivorous/omnivorous or pelagic-neritic, piscivorous/omnivorous (**Ecological**), spatial info is MORE LIKELY to be used in data prep or more advanced model types (following Neubauer et al. 2018)

# H3b. If the species is reef-associated (**Ecological**), spatial info is LESS LIKELY to be used in data prep or more advanced model types

# H4. If the species is larger-sized (**Max.Length.m**), spatial info is MORE LIKELY to be used in data prep or more advanced model types

# H5. If the total commercial value or per ton value is high (**Ave.Metric.Tons_Commercial; PerTon_Ave.Dollars.Adj_Commercial**), spatial info is MORE LIKELY to be used in data prep or more advanced model types

# H6. If the recreational catch is high (**Ave.Metric.Tons_Recreational**), spatial info is MORE LIKELY to be used in data prep or more advanced model types


# Convert to factors
si.2$Ecological <- as.factor((si.2$Ecological))
si.2$Council.Abbv <- as.factor(si.2$Council.Abbv)


#### H1. If catch-only data is only available (**Fisheries.Dep.Only**), spatial info is LESS LIKELY to be used in data prep or more advanced model types 
h1 <- lmer(SI.data.prep ~ Assessment.Model.Type + (1 | Council.Abbv), data = si.2)
summary(h1)
confint(h1)
ranef(h1)$Council.Abbv

## SIGNIFICANT Ecological
## Ecologicalpelagic-oceanic, piscivorous/omnivorous Positively Significant
o2 <- glm(SI.data.prep ~ Ecological + (1|Council.Abbv), data = si.2, family = binomial)



