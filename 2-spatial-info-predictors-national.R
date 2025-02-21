# spatial-info-predictors-national.R
######################################
# Janelle L. Morano

# Quantify spatial information in US marine stocks

# last updated 20 February 2025
###############################################
###############################################

library(tidyverse)
library(janitor)
library(ggfortify)
library(lme4)
library(corrplot)



#####---- Data preparation
#####
# Use "predictors" csv since it was cleaned in "summary-spatial-info-in-stock-assessments.R"
sip <- read.csv("/Users/janellemorano/Git/spatial-info-ms/data/4-US Marine Fisheries Stocks and Assessments-predictors_20250130.csv", header = TRUE, na.strings = c(""))

# Keep only columns needed for quantitative analyses
sip <- sip |> 
  select("Scientific.Name", 
         "Mgmt.Council", 
         "SI.stock.boundaries",
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
         "Ecological") |>
  mutate(Ecological.Num = case_when(Ecological == "bathydemersal" ~ 1,
                                    Ecological == "benthopelagic, planktivorous"  ~ 2,                 
                                    Ecological == "benthopelagic, piscivorous/omnivorous" ~ 3,
                                    Ecological == "pelagic-oceanic, piscivorous/omnivorous" ~ 4,
                                    Ecological == "pelagic-neritic, planktivorous" ~ 5, 
                                    Ecological == "pelagic-neritic, piscivorous/omnivorous" ~ 6,
                                    Ecological == "demersal/benthic, planktivorous" ~ 7,
                                    Ecological == "demersal/benthic, piscivorous/omnivorous" ~ 8,
                                    Ecological == "reef-associated, piscivorous/omnivorous" ~ 9)
         ) |> 
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
# Warnings about coercion are OK


# Convert NAs in Commercial and Recreation landings to 0
sip <- sip |> mutate(Ave.Metric.Tons_Commercial = ifelse(is.na(Ave.Metric.Tons_Commercial), 0, Ave.Metric.Tons_Commercial),
                   Ave.Metric.Tons_Recreational = ifelse(is.na(Ave.Metric.Tons_Recreational), 0, Ave.Metric.Tons_Recreational),
                   Ave.Dollars.Adj_Commercial = ifelse(is.na(Ave.Dollars.Adj_Commercial), 0, Ave.Dollars.Adj_Commercial)
                   )

summary(sip)

# Commercial Averages must be scaled to be comparable
sip$Ave.Metric.Tons_Commercial <- scale(sip$Ave.Metric.Tons_Commercial)
sip$Ave.Metric.Tons_Recreational <- scale(sip$Ave.Metric.Tons_Recreational)
sip$Ave.Dollars.Adj_Commercial <- scale(sip$Ave.Dollars.Adj_Commercial)
sip$PerTon_Ave.Dollars.Adj_Commercial <- scale(sip$PerTon_Ave.Dollars.Adj_Commercial)

# Write and save for "3-spatial-info-predictors-bycouncil.R"
# write.csv(sip, "/Users/janellemorano/Git/spatial-info-ms/data/5-US Marine Fisheries Stocks and Assessments-predictors-byCouncil_20250130")



#####
#####---- PCA (on National data)
#####
# Prep data for PCA

# Drop character categories of Scientific.Name, Mgmt.Council, Ecological
sip.pca <- select(sip, -c("Scientific.Name", "Mgmt.Council", "Ecological"))
# Drop rows with NAs
sip.pca <- drop_na(sip.pca)
# # Convert NAs to zeros
# sip.pca[is.na(sip.pca)] <- 0

# Conduct a PCA on the covariance matrix (prcomp()) on the sip.pca data
pca <- prcomp(sip.pca, center= TRUE, scale= TRUE)

summary(pca) #ids the PCs and their stdev

# Remember for PCA...U = XW
# X = the data (LBDA matrix)
# W = weights; columns are EOFs/eigenvectors/principle axes; elements are loadings
# U = columns are PCAs/transformed X values; elements are scores
# total variance (sum of diagonals of cov matrix) is the sum of the eigenvalues

# names(pca)
pca$x #U matrix
pca$rotation #W matrix
pca$sdev #sqrt of eigenvalues/lambdas of covariance matrix; sdev^2 are the lambdas
# variance explained by the PC = sdev^2/ sum(sdev^2)

# Calculate and report the variance explained by each of the EOFs
var_explained <- pca$sdev^2 / sum(pca$sdev^2)
print(var_explained)

# Scree plot and +/- 1 standard error for the eigenvalues
# scree plot is the variance explained or proportion of variation by each PC (there are 16)
# Put eigenvalues into df
eigen_df <- data.frame(PC= paste0("PC",1:16), #there are 16 PCs
                       EOF=pca$sdev^2)
print(eigen_df)

# add SE
# SE = lambda(i) * sqrt(2/n)
SE <- pca$sdev^2 * sqrt(2/length(pca$sdev))
# add to df
eigen_df$SE <- SE
head(eigen_df)

# Plot Scree
# pca$sdev by cols of U pca$x OR var_explained_df$var_explained by var_explained_df$PC
scree <- eigen_df[1:16,]
# order PCs
scree <- scree %>%
  arrange(desc(EOF)) %>%
  mutate(PC=factor(PC, levels=PC))

ggplot(data = scree, aes(x = PC, y = EOF, group = 1)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=EOF-SE, ymax=EOF+SE)) +
  xlab("Principal Component") +
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  theme_bw()

# North's Rule is to make a scree plot and look for where the SE of eigenvalues are not overlapping, because when the SEs are overlapping, they are not different from each other and the uncertainty between the values is too great and they should be ignored. 
# Looking at this graph, PC 1 is (mostly) diff from PC2-15 and diff from PC16

# Plot Loadings
biplot(pca)

# Plot nicer Loadings figure
autoplot(prcomp(sip.pca, center= TRUE, scale= TRUE), loadings = TRUE, data = sip, colour = "Mgmt.Council",
         loadings.colour = 'black',
         loadings.label = TRUE,
         loadings.label.color = "darkblue",
         loadings.label.size = 4) +
  theme(panel.background = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(colour = "black"))

#PC1 is mostly made of -0.5 SI.model; SpatiallyEx/Imp
#  *** IT SEEMS LIKE MGMT.COUNCIL MAY EXPLAIN THE RESULTS BEST ***



#####
#####---- GLM (on National data)
#####
# Use "sip" data
# The probability of spatial info being used in DATA PREPARATION based on the following:
# 1. Assessment.Model.Type SIGNIFICANT
sip$Assessment.Model.Type <- as.factor(sip$Assessment.Model.Type)
# 2. Rebuilding.Program.Status
# 3. Ecological SIGNIFICANT
sip$Ecological <- as.factor((sip$Ecological))
# 4. Schooling
# 5. Max.Length.m SIGNIFICANT
# 6. Ave.Metric.Tons_Commercial
# 7. Ave.Metric.Tons_Recreational
# 8. Ave.Dollars.Adj_Commercial SIGNIFICANT
# 9. PerTon_Ave.Dollars.Adj_Commercial
# 10. Fisheries.Dep.Only

# With a Binomial Logistic Regression or Poisson for national stocks using spatial info in data prep


## SIGNIFICANT 1. Assessment.Model.Type
m1 <- glm(SI.data.prep ~ as.factor(Assessment.Model.Type), data = sip, family = binomial(link = "logit"))
summary(m1)
plot(m1)
m1.res <- resid(m1)
plot(fitted(m1), m1.res, main = "Binomial")
summary(m1)$coefficients
pred.m1 <- predict(m1, type = "response") #remember, these are probabilities for each point
unique(sip$Assessment.Model.Type)
newdata.1 <- data.frame(Assessment.Model.Type = as.factor(seq(1, 5)))
pred.m1 <- predict(m1, newdata = newdata.1, type = "response", se.fit = TRUE)
pred.m1.df <- data.frame(Assessment.Model.Type = seq(1:5), probability = pred.m1$fit, se = pred.m1$se.fit)
ggplot() +
  geom_bar(data = pred.m1.df, aes(x = Assessment.Model.Type, y = probability), stat = "identity") +
  geom_errorbar(data = pred.m1.df, aes(x=Assessment.Model.Type, ymin=probability-se, ymax=probability+se), width = 0.3) +
  theme_classic()
siglist <- list(summary(m1)$coefficients)
names(siglist)[1] <- "Asst.Model.Type"


# 2. Rebuilding.Program.Status
m2 <- glm(SI.data.prep ~ Rebuilding.Program.Status, data = sip, family = binomial(link = "logit"))
summary(m2)
summary(m2)$coefficients
nolist <- list(summary(m2)$coefficients)
names(nolist)[1] <- "Rebuilding.Program.Status"


# 3. SIGNIFICANT Ecological (pelagic-oceanic, piscivorous/omnivorous)
## Ecologicalpelagic-oceanic, piscivorous/omnivorous Positively Significant
# drop NAs in Ecological.Num
sip.nona <- sip[!is.na(sip$Ecological.Num), ]
m3 <- glm(SI.data.prep ~ as.factor(Ecological.Num), data = sip.nona, family = binomial(link = "logit"))
summary(m3)
plot(m3)
summary(m3)$coefficients
m3.res <- resid(m3)
plot(fitted(m3), m3.res, main = "Binomial")
pred.m3 <- predict(m3, type = "response") #remember, these are probabilities for each point
unique(sip.nona$Ecological.Num)
newdata.m3 <- data.frame(Ecological.Num = as.factor(unique(sip.nona$Ecological.Num)))
pred.m3 <- predict(m3, newdata = newdata.m3, type = "response", se.fit = TRUE)
pred.m3.df <- data.frame(Ecological.Num = seq(1:9), probability = pred.m3$fit, se = pred.m3$se.fit)
pred.m3.df$Ecological.Num <- as.factor(pred.m3.df$Ecological.Num)
ggplot() +
  geom_bar(data = pred.m3.df, aes(x = Ecological.Num, y = probability), stat = "identity") +
  geom_errorbar(data = pred.m3.df, aes(x=Ecological.Num, ymin=probability-se, ymax=probability+se), width = 0.3) +
  theme_classic()
siglist[[2]] <- summary(m3)$coefficients
names(siglist)[[2]] <- "Ecological"



# 4. Schooling
m4 <- glm(SI.data.prep ~ Schooling, data = sip, family = binomial(link = "logit"))
summary(m4)
summary(m4)$coefficients
m4.res <- resid(m4)
plot(fitted(m4), m4.res, main = "Binomial")
pred.m4 <- predict(m4, type = "response", se.fit = TRUE) #remember, these are probabilities for each point
barplot(pred.m4$fit)
nolist[[3]] <- summary(m4)$coefficients
names(nolist)[[2]] <- "Schooling"


# SIGNIFICANT 5. Max.Length.m
m5 <- glm(SI.data.prep ~ Max.Length.m, data = sip, family = binomial(link = "logit"))
summary(m5)
summary(m5)$coefficients
m5.res <- resid(m5)
plot(fitted(m5), m5.res, main = "Binomial")
newdata <- data.frame(Max.Length.m = unique(sip$Max.Length.m))
pred.m5 <- predict(m5, type = "response", newdata = newdata, se.fit = TRUE)

ggplot(sip, aes(x=Max.Length.m, y=SI.data.prep)) + geom_point() +
  stat_smooth(method="glm", color="blue", se=TRUE, 
              method.args = list(family=binomial))


pred.m5.df <- data.frame(Max.Length.m = unique(sip$Max.Length.m), probability = pred.m5$fit, se = pred.m5$se.fit)
pred.m5.df <- sort_by(pred.m5.df, ~Max.Length.m)
ggplot() +
  geom_ribbon(data = pred.m5.df, aes(x = Max.Length.m, ymin=probability-se, ymax=probability+se), fill = "grey70") +
  geom_line(data = pred.m5.df, aes(x = Max.Length.m, y = probability), color = "darkblue") +
  theme_classic()
# plot(pred.m5$fit)
siglist[[3]] <- summary(m5)$coefficients
names(siglist)[[3]] <- "Max.Length.m"


# 6. Ave.Metric.Tons_Commercial
m6 <- glm(SI.data.prep ~ Ave.Metric.Tons_Commercial, data = sip, family = binomial(link = "logit"))
summary(m6)
summary(m6)$coefficients
m6.res <- resid(m6)
plot(fitted(m6), m6.res, main = "Binomial")
pred.m6 <- predict(m6, type = "response", se.fit = TRUE) #remember, these are probabilities for each point
plot(pred.m6$fit)
nolist[[4]] <- summary(m6)$coefficients
names(nolist)[[4]] <- "Ave.Metric.Tons_Commercial"


# 7. Ave.Metric.Tons_Recreational
m7 <- glm(SI.data.prep ~ Ave.Metric.Tons_Recreational, data = sip, family = binomial(link = "logit"))
summary(m7)
summary(m7)$coefficients
m7.res <- resid(m7)
plot(fitted(m7), m7.res, main = "Binomial")
pred.m7 <- predict(m7, type = "response", se.fit = TRUE) #remember, these are probabilities for each point
plot(pred.m7$fit)
nolist[[5]] <- summary(m7)$coefficients
names(nolist)[[5]] <- "Ave.Metric.Tons_Recreational"


# SIGNIFICANT 8. Ave.Dollars.Adj_Commercial
m8 <- glm(SI.data.prep ~ Ave.Dollars.Adj_Commercial, data = sip, family = binomial(link = "logit"))
summary(m8)
summary(m8)$coefficients
m8.res <- resid(m8)
plot(fitted(m8), m8.res, main = "Binomial")
pred.m8 <- predict(m8, type = "response", se.fit = TRUE)

ggplot(sip, aes(x=Ave.Dollars.Adj_Commercial, y=SI.data.prep)) + geom_point() +
  stat_smooth(method="glm", color="blue", se=TRUE, 
              method.args = list(family=binomial)) +
  ylab("probability") +
  theme_classic()

pred.m8 <- predict(m8, type = "response", se.fit = TRUE)
plot(pred.m8$fit)
siglist[[4]] <- summary(m8)$coefficients
names(siglist)[[4]] <- "Ave.Dollars.Adj_Commercial"


# 9. PerTon_Ave.Dollars.Adj_Commercial
m9 <- glm(SI.data.prep ~ PerTon_Ave.Dollars.Adj_Commercial, data = sip, family = binomial(link = "logit"))
summary(m9)
summary(m9)$coefficients
m9.res <- resid(m9)
plot(fitted(m9), m9.res, main = "Binomial")
nolist[[5]] <- summary(m9)$coefficients
names(nolist)[[5]] <- "PerTon_Ave.Dollars.Adj_Commercial"


# SIGNIFICANT 10. Fisheries.Dep.Only
m10 <- glm(SI.data.prep ~ Fisheries.Dep.Only, data = sip, family = binomial(link = "logit"))
summary(m10)
summary(m10)$coefficients
m10.res <- resid(m10)
plot(fitted(m10), m10.res, main = "Binomial")
pred.m10 <- predict(m10, type = "response", se.fit = TRUE)
ggplot(sip, aes(x=Fisheries.Dep.Only, y=SI.data.prep)) + geom_point() +
  stat_smooth(method="glm", color="blue", se=TRUE, 
              method.args = list(family=binomial)) +
  ylab("probability") +
  theme_classic()
siglist[[5]] <- summary(m10)$coefficients
names(siglist)[[5]] <- "Fisheries.Dep.Only"



#####----- Predicting factors on use of spatial info in ASSESSMENT MODEL
#####
# Test for predictors of number of stocks using spatial info for each region as separate models
# 1. Assessment.Model.Type
n1 <- glm(SI.model ~ Assessment.Model.Type, data = sip, family = binomial(link = "logit"))
summary(n1)
summary(n1)$coefficients
n1.res <- resid(n1)
plot(fitted(n1), n1.res, main = "Binomial")



# 2. Rebuilding.Program.Status
n2 <- glm(SI.model ~ Rebuilding.Program.Status, data = sip, family = binomial(link = "logit"))
summary(n2)
summary(n2)$coefficients
n2.res <- resid(n2)
plot(fitted(n2), n2.res, main = "Binomial")



# 3. Ecological
n3 <- glm(SI.model ~ Ecological, data = sip, family = binomial(link = "logit"))
summary(n3)
summary(n3)$coefficients
n3.res <- resid(n3)
plot(fitted(n3), n3.res, main = "Binomial")



# 4. Schooling
n4 <- glm(SI.model ~ Schooling, data = sip, family = binomial(link = "logit"))
summary(n4)
summary(n4)$coefficients
n4.res <- resid(n4)
plot(fitted(n4), n4.res, main = "Binomial")



# 5. Max.Length.m
n5 <- glm(SI.model ~ Max.Length.m, data = sip, family = binomial(link = "logit"))
summary(n5)
summary(n5)$coefficients
n5.res <- resid(n5)
plot(fitted(n5), n5.res, main = "Binomial")



# 6. Ave.Metric.Tons_Commercial
n6 <- glm(SI.model ~ Ave.Metric.Tons_Commercial, data = sip, family = binomial(link = "logit"))
summary(n6)
summary(n6)$coefficients
n6.res <- resid(n6)
plot(fitted(n6), n6.res, main = "Binomial")



# 7. Ave.Metric.Tons_Recreational
n7 <- glm(SI.model ~ Ave.Metric.Tons_Recreational, data = sip, family = binomial(link = "logit"))
summary(n7)
summary(n7)$coefficients
n7.res <- resid(n7)
plot(fitted(n7), n7.res, main = "Binomial")



# SIGNIFICANT 8. Ave.Dollars.Adj_Commercial
n8 <- glm(SI.model ~ Ave.Dollars.Adj_Commercial, data = sip, family = binomial(link = "logit"))
summary(n8)
summary(n8)$coefficients
n8.res <- resid(n8)
plot(fitted(n8), n8.res, main = "Binomial")
pred.n8 <- predict(n8, type = "response", se.fit = TRUE)
ggplot(sip, aes(x=Ave.Dollars.Adj_Commercial, y=SI.model)) + geom_point() +
  stat_smooth(method="glm", color="blue", se=TRUE, 
              method.args = list(family=binomial))+
  ylab("probability") +
  theme_classic()
plot(pred.n8$fit)
siglist[[6]] <- summary(n8)$coefficients
names(siglist)[[6]] <- "model-Ave.Dollars.Adj_Commercial"


# 9. PerTon_Ave.Dollars.Adj_Commercial
n9 <- glm(SI.model ~ PerTon_Ave.Dollars.Adj_Commercial, data = sip, family = binomial(link = "logit"))
summary(n9)
summary(n9)$coefficients
n9.res <- resid(n9)
plot(fitted(n9), n9.res, main = "Binomial")



# 10. Fisheries.Dep.Only
n10 <- glm(SI.model ~ Fisheries.Dep.Only, data = sip, family = binomial(link = "logit"))
summary(n10)
summary(n10)$coefficients
n10.res <- resid(n10)
plot(fitted(n10), n10.res, main = "Binomial")





#######----- Predicting factors on use of spatial info BY COUNCIL REGION
#######s
# Hypothesis: Factors that drive stock assessments to incorporate spatial info differ for each region.


sip$Mgmt.Council <- as.factor(sip$Mgmt.Council)

## Assessment.Model.Type
o1 <- glm(SI.data.prep ~ as.factor(Assessment.Model.Type) + as.factor(Mgmt.Council), data = sip, family = binomial)
summary(o1)
summary(o1)$coefficients
newdata <- data.frame()
for (i in unique(sip$Mgmt.Council)) {
  new <- data.frame(Mgmt.Council = i,
                    Assessment.Model.Type = sort(unique(sip$Assessment.Model.Type)))
  newdata <- rbind(newdata, new)
}
pred.o1 <- predict(o1, newdata = newdata, type = "response", se.fit = TRUE)
pred.o1.df <- cbind(newdata, pred.o1)
ggplot() +
  geom_bar(data = pred.o1.df, aes(x = Assessment.Model.Type, y = fit, fill = Mgmt.Council), position = "dodge", stat = "identity") +
  # geom_errorbar(data = pred.o1.df, aes(x=Ecological.Num, ymin=probability-se, ymax=probability+se), width = 0.3) +
  theme_classic()


## Ecological
## Ecologicalpelagic-oceanic, piscivorous/omnivorous Positively Significant
o2 <- glm(SI.data.prep ~ as.factor(Ecological) + as.factor(Mgmt.Council), data = sip, family = binomial)
summary(o2)

## Max.Length.m
o3 <- glm(SI.data.prep ~ Max.Length.m + as.factor(Mgmt.Council), data = sip, family = gaussian)
summary(o3)
summary(o3)$coefficients
newdata <- data.frame()
for (i in unique(sip$Mgmt.Council)) {
  new <- data.frame(Mgmt.Council = i,
                    Max.Length.m = median(sip$Max.Length.m[sip$Mgmt.Council == i], na.rm=TRUE))
  newdata <- rbind(newdata, new)
}
pred.o3 <- predict(o3, newdata = newdata, type = "response", se.fit = TRUE)
pred.o3.df <- cbind(newdata, pred.o3)
ggplot() +
  geom_point(data = pred.o3.df, aes(x = Max.Length.m, y = fit, color = Mgmt.Council)) +
  # geom_errorbar(data = pred.o1.df, aes(x=Ecological.Num, ymin=probability-se, ymax=probability+se), width = 0.3) +
  theme_classic()
