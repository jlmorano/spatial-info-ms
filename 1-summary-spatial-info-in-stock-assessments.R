# 1-summary-spatial-info-in-stock-assessments.R
######################################
# Janelle L. Morano

# Quantify spatial information in US marine stocks
# First step in analysis to generate information for tables and figures 
# illustrating characteristics of species in the assessment database

# last updated 17 February 2025
###############################################
###############################################


sessionInfo()
# R version 4.4.1 (2024-06-14)
# Platform: x86_64-apple-darwin20
# Running under: macOS Ventura 13.6.9

library(tidyverse)
library(janitor)
library(cowplot)


#----- Data Preparation
######
si <- read.csv("/Users/janellemorano/Git/spatial-info-ms/data/3-US Marine Fisheries Stocks and Assessments-forR_20250130.csv", header = TRUE, na.strings = c(""))
str(si)
colnames(si)
# [1] "Common.Name"                  "Scientific.Name"              "Asmt.Year"                  
# [4] "Stock.Assessment.Area"        "Stock.Status.Area"            "Council"                   
# [7] "Council.Abbv"                 "FMP"                          "Assessment.Model"          
# [10] "Assessment.Model.Type"        "Science.Center.Region"        "Rebuilding.Program.Status" 
# [13] "SI.stock.boundaries"          "SI.data.prep"                 "SI.model"                  
# [16] "SI.sci.advice"                "SI.other"                     "NOAA.Region.Name"            
# [19] "NMFS.Name"                    "Ave.Metric.Tons_Commercial"   "Ave.Metric.Tons_Recreational"
# [22] "Ave.Dollars.Adj_Commercial"   "Ecological"                   "Schooling"                   
# [25] "Max.Length.m"                 "Transboundary"    

si$Assessment.Model.Type <- as.numeric(si$Assessment.Model.Type)
si$Rebuilding.Program.Status <- as.numeric(si$Rebuilding.Program.Status)
si$SI.stock.boundaries <- as.numeric(si$SI.stock.boundaries)
si$SI.data.prep <- as.numeric(si$SI.data.prep)
si$SI.model <- as.numeric(si$SI.model)
si$SI.sci.advice <- as.numeric(si$SI.sci.advice)
si$SI.other <- as.numeric(si$SI.other)
si$Ave.Metric.Tons_Commercial <- as.numeric(si$Ave.Metric.Tons_Commercial)
si$Ave.Metric.Tons_Recreational <- as.numeric(si$Ave.Metric.Tons_Recreational)
si$Ave.Dollars.Adj_Commercial <- as.numeric(si$Ave.Dollars.Adj_Commercial)
si$Schooling <- as.numeric(si$Schooling)
si$Max.Length.m <- as.numeric(si$Max.Length.m)
si$Transboundary <- as.numeric(si$Transboundary)
si$Fisheries.Dep.Only <- as.numeric(si$Fisheries.Dep.Only)


# Convert Ave.Dollars.Adj_Commercial to price per ton
si$PerTon_Ave.Dollars.Adj_Commercial <- si$Ave.Dollars.Adj_Commercial/si$Ave.Metric.Tons_Commercial
si[sapply(si, is.infinite)] <- 0

# Omit Nassau grouper, because it's ESA listed and not assessed
si <- si[!(si$Common.Name %in% "Nassau grouper"),]

# Add column to designate management: US Council, US Council Cooperation, US-International Mgmt
unique(si$Council.Abbv)
# [1] "NEFMC"             "NPFMC"             "NOAA HMS-ICCAT"    "PFMC-P-RFMO"      
# [5] "WPFMC-P-RFMO"      "PFMC"              "MAFMC"             "SAFMC"            
# [9] "WPFMC"             "CFMC"              "GMFMC-SAFMC"       "GMFMC"            
# [13] "NEFMC-MAFMC"       "WPFMC-PFMC-P-RFMO"

si <- si |>
  mutate(Mgmt.Council = case_when(Council.Abbv == "NEFMC" ~ "New England",
                                  Council.Abbv == "MAFMC" ~ "Mid-Atlantic",
                                  Council.Abbv == "SAFMC" ~ "South Atlantic",
                                  Council.Abbv == "CFMC" ~ "Caribbean",
                                  Council.Abbv == "GMFMC" ~ "Gulf of Mexico",
                                  Council.Abbv == "NPFMC" ~ "North Pacific",
                                  Council.Abbv == "PFMC" ~ "Pacific",
                                  Council.Abbv == "WPFMC" ~ "Western Pacific",
                                  Council.Abbv == "GMFMC-SAFMC" | Council.Abbv == "NEFMC-MAFMC" ~ "Joint Council Mgmt, Atlantic",
                                  Council.Abbv == "NOAA HMS-ICCAT" ~ "International, Atlantic",
                                  Council.Abbv == "PFMC-P-RFMO" | Council.Abbv == "WPFMC-P-RFMO" | Council.Abbv == "WPFMC-PFMC-P-RFMO" ~ "International, Pacific")
         )


######
#----- Overall stats (Table 1)
######
metrics <- data.frame("Summary" = c("Species.with.Assessments", "Multispecies.Stocks", "Species.with.Multiple.Assessments", "Total.Assessments", "Total.WithAnySpatial"), "Total" = 0)

# Species with Assessments
metrics[1, 2] <- tally(si |> count(si$Common.Name))

# Multispecies Complexes
q <- si |> count(si$Common.Name == "Multispecies Complex")
metrics[2, 2] <- q[2,2]

# Species with more than one Assessment
assessments <- si |> count(si$Scientific.Name)
metrics[3, 2] <- sum(assessments$n > 1)

# Total number of Assessments
metrics[4, 2] <- nrow(si)

# Number of Assessments using spatial information in at least 1 step
a <- si |> count(si$SI.stock.boundaries == 1 |
              si$SI.data.prep ==1 |
              si$SI.model == 1 |
              si$SI.sci.advice == 1)
metrics[5, 2] <- a[2,2]


print(metrics)


# Number of Assessments using spatial info in each step
si |> count(si$SI.stock.boundaries == 1)
si |> count(si$SI.data.prep ==1)
si |> count(si$SI.model == 1)
si |> count(si$SI.sci.advice == 1)




######
#----- Other summaries (SuppTable 2)
######
# Species managed by different jurisdictions
multi <- si |>
  select(Common.Name, Council.Abbv) |>
  add_count(Common.Name) |>
  filter(n>1) |>
  distinct()
# This is including species with multiple assessments in the same Council. I can't seem to drop those.
# Write to .csv to deal with outside of R
# write.csv(multi, "/Users/janellemorano/Git/spatial-info-ms/data/managed-by-multiple-Councils.csv")

#----- Write new csv for regression analyses
# write.csv(si, "/Users/janellemorano/Git/spatial-info-ms/data/4-US Marine Fisheries Stocks and Assessments-predictors_20250130.csv")



######
#----- Stock Characteristics by Councils (Mgmt.Council) (SuppTable 3)
######
# Data by assessment
by.council.asmt <- si %>%
  group_by(Mgmt.Council) %>%
  summarize(n.Assessments = n(),
            n.Species = n_distinct(Scientific.Name),
            boundaries.Y = sum(SI.stock.boundaries == 1, na.rm = TRUE),
            dataprep.Y = sum(SI.data.prep == 1, na.rm = TRUE),
            model.Y = sum(SI.model == 1, na.rm = TRUE),
            sciadv.Y = sum(SI.sci.advice == 1, na.rm = TRUE),
            other.Y = sum(SI.other == 1, na.rm = TRUE)
  )
# Data by species (because some is duplicated by assessment)
by.council.sp <- si %>%
  distinct(Scientific.Name, Council.Abbv, .keep_all = TRUE) |> #Remove duplicate species info
  group_by(Mgmt.Council) %>%
  summarize(n.Species2 = n(),
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
  )

# Combine
by.council <- cbind(by.council.asmt, by.council.sp)
by.council <- by.council[, -(9:10)]

print(by.council)

# Write to .csv
# write.csv(by.council, "/Users/janellemorano/Git/spatial-info-ms/data/summary-by-Council.csv")  




######
#----- Ecological Type by Council
######
eco.stack <- by.council %>%
  select(Mgmt.Council, Eco.Bathy, Eco.BenthPlank, Eco.BenthPisc, Eco.PelagBenthPlank, Eco.PelagBenthPisc, Eco.PelagOceanPisc, Eco.PelagNeritPlank, Eco.PelagNeritPisc, Eco.Reef) |>
  pivot_longer(cols = Eco.Bathy: Eco.Reef,
               names_to = c("Ecology"),
               values_to = "Total") |>
  mutate(Ecology = factor(Ecology, levels=c("Eco.PelagOceanPisc", "Eco.PelagNeritPisc", "Eco.PelagNeritPlank", "Eco.Reef", "Eco.PelagBenthPisc", "Eco.PelagBenthPlank", "Eco.BenthPisc", "Eco.BenthPlank", "Eco.Bathy"))) |>
  mutate(Mgmt.Council = factor(Mgmt.Council, levels=c("Caribbean", "Joint Council Mgmt, Atlantic", "Mid-Atlantic", "Western Pacific", "International, Pacific", "Gulf of Mexico", "International, Atlantic", "New England", "South Atlantic", "North Pacific", "Pacific")))

purptang = c("#420F75FF", "#7640A9FF", "#AD72D6FF", "#E7A8FBFF", "#dcdce5", "#F8B150FF", "#C17D17FF", "#8A4D00FF", "#552000FF") ##F3F3F3FF
ggplot(data = eco.stack, aes(x = Mgmt.Council, y = Total, fill = Ecology)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = purptang) +
  ylab("Number of Species Assessed") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.95)) +
  theme(panel.background = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.x = element_blank()) +
  theme(text = element_text(size = 14)) 
# save 800x600


######
#----- Fisheries-Dependent data only (Figure 3)
######
by.council$Prop.FishDepOnly <- by.council$Fisheries.Dep.Only/by.council$n.Assessments
by.council$Prop.DataPrep <- by.council$dataprep.Y/by.council$n.Assessments
by.council$Prop.Model <- by.council$model.Y/by.council$n.Assessments

by.council <- by.council |> mutate(Mgmt.Council = fct_reorder(Mgmt.Council, Prop.FishDepOnly))

# Panel A
ggplot(by.council, aes(Mgmt.Council, Prop.FishDepOnly, fill = Mgmt.Council)) +
  geom_bar(stat = 'identity') +
  scale_fill_viridis_d() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.95)) +
  theme(panel.background = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.x = element_blank()) +
  theme(text = element_text(size = 14)) +
  ylab("Proportion of Species with Fisheries-Dependent Data Only") +
  theme(legend.position="none")
# save 800x600

# Panel B
ggplot(by.council, aes(Prop.FishDepOnly, Prop.DataPrep, color = Mgmt.Council)) +
  scale_color_viridis_d() +
  geom_point(size = 4) +
  xlim(0,1) +
  ylim(0,1) +
  theme(panel.background = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(text = element_text(size = 14)) +
  ylab("Prop. Spatial in Data Preparation") +
  xlab("Prop. Assessments with Fisheries-Dependent Data Only")
# save 750x500

# Panel C
ggplot(by.council, aes(Prop.FishDepOnly, Prop.Model, color = Mgmt.Council)) +
  scale_color_viridis_d() +
  geom_point(size = 4) +
  xlim(0,1) +
  ylim(0,1) +
  theme(panel.background = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(text = element_text(size = 14)) +
  ylab("Prop. Spatial in Assessment Model") +
  xlab("Prop. Assessments with Fisheries-Dependent Data Only")
# save 750x500


#----- Landings & Value Stats
Comm.landings <- si |>
  select(c(Common.Name, Scientific.Name, Mgmt.Council, Ave.Metric.Tons_Commercial)) |>
  na.omit() |>
  arrange(desc(Ave.Metric.Tons_Commercial))
Comm.landings <- distinct(Comm.landings)



#######----- By Assessment Step 
#----- National trends
si.national <- si %>%
  summarise(boundaries.Y = sum(SI.stock.boundaries == 1, na.rm = TRUE),
            boundaries.N = sum(SI.stock.boundaries == 0, na.rm = TRUE),
            boundaries.NA = sum(is.na(SI.stock.boundaries)),
            
            dataprep.Y = sum(SI.data.prep == 1, na.rm = TRUE),
            dataprep.N = sum(SI.data.prep == 0, na.rm = TRUE),
            dataprep.NA = sum(is.na(SI.data.prep)),
            
            model.Y = sum(SI.model == 1, na.rm = TRUE),
            model.N = sum(SI.model == 0, na.rm = TRUE),
            model.NA = sum(is.na(SI.model)),
            
            sciadvice.Y = sum(SI.sci.advice == 1, na.rm = TRUE),
            sciadvice.N = sum(SI.sci.advice == 0, na.rm = TRUE),
            sciadvice.NA = sum(is.na(SI.sci.advice))
            
            # other.Y = sum(SI.other == 1, na.rm = TRUE),
            # other.N = sum(SI.other == 0, na.rm = TRUE),
            # other.NA = sum(is.na(SI.other))
  )

si.national.stack <- si.national %>%
  pivot_longer(cols = boundaries.Y: sciadvice.NA,
               names_to = c("Step"),
               values_to = "Total") %>%
  separate_wider_delim(Step,
                       ".",
                       names = c("Step", "Response")) %>%
  mutate(Step = factor(Step, levels=c("boundaries", "dataprep", "model", "sciadvice"))) %>%
  mutate(Response = factor(Response, levels=c("NA", "N", "Y"))) #switched so easier to see total of Y; levels=c("Y", "N", "NA")

# Graph National trends
# cols <- c("#0571B0", "#CA0020","grey90") # "#F7F7F7"
cols <- c("grey90", "#FFE9CE", "#6B9AC4")
ggplot(data = si.national.stack, aes(x = Step, y = Total, fill = Response)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = cols) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.95)) +
  theme(panel.background = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.x = element_blank()) +
  theme(text = element_text(size = 14)) +
  ggtitle("National")


#----- Regional Council Trends
# Category of spatial info use by Regional Council
si.council <- si %>%
  group_by(Mgmt.Council) %>%
  summarise(boundaries.Y = sum(SI.stock.boundaries == 1, na.rm = TRUE),
            boundaries.N = sum(SI.stock.boundaries == 0, na.rm = TRUE),
            boundaries.NA = sum(is.na(SI.stock.boundaries)),
            
            dataprep.Y = sum(SI.data.prep == 1, na.rm = TRUE),
            dataprep.N = sum(SI.data.prep == 0, na.rm = TRUE),
            dataprep.NA = sum(is.na(SI.data.prep)),
            
            model.Y = sum(SI.model == 1, na.rm = TRUE),
            model.N = sum(SI.model == 0, na.rm = TRUE),
            model.NA = sum(is.na(SI.model)),
            
            sciadvice.Y = sum(SI.sci.advice == 1, na.rm = TRUE),
            sciadvice.N = sum(SI.sci.advice == 0, na.rm = TRUE),
            sciadvice.NA = sum(is.na(SI.sci.advice))
            
            # other.Y = sum(SI.other == 1, na.rm = TRUE),
            # other.N = sum(SI.other == 0, na.rm = TRUE),
            # other.NA = sum(is.na(SI.other))
  )

si.council.stack <- si.council %>%
  pivot_longer(cols = boundaries.Y: sciadvice.NA,
               names_to = c("Step"),
               values_to = "Total") %>%
  separate_wider_delim(Step,
                       ".",
                       names = c("Step", "Response")) %>%
  mutate(Step = factor(Step, levels=c("boundaries", "dataprep", "model", "sciadvice"))) %>%
  mutate(Response = factor(Response, levels=c("NA", "N", "Y")))


# Graph by Region
names <- unique(si.council.stack$Mgmt.Council)

# without drawing x axis or legend
for (z in 1:length(unique(names))) {
  print(
    ggplot(data = subset(si.council.stack, Mgmt.Council %in% names[z]), aes(x = Step, y = Total, fill = Response)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = cols) +
    theme(axis.text.x = element_blank()) +
    theme(panel.background = element_blank()) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(axis.line = element_line(colour = "black")) +
    theme(axis.title.x = element_blank()) +
    theme(text = element_text(size = 30)) +
    ggtitle(names[z]) +
    theme(legend.position = "none")
  )
}



