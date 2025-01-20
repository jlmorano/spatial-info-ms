# summary-spatial-info-in-stock-assessments.R
######################################
# Janelle L. Morano

# Quantify spatial information in US marine stocks
# Generate information for tables and figures illustrating characteristics
# of species in the assessment database

# last updated 20 January 2025
###############################################
###############################################

library(tidyverse)
library(janitor)
library(cowplot)


#----- Data Preparation
si <- read.csv("/Users/janellemorano/Git/spatial-info-ms/data/US Marine Fisheries Stocks and Assessments-Analysis_20250116.csv", header = TRUE, na.strings = c(""))
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

# Omit Nassau grouper, because it's ESA listed and not assessed
si <- si[!(si$Common.Name %in% "Nassau grouper"),]

# Add column to indicate if stock used spatial info in any step, model, or other
si <- si |>
  mutate(Use = case_when(SI.other == 1 ~ 3,
                         SI.model == 1 ~ 2,
                         SI.stock.boundaries == 1 | SI.data.prep == 1 | SI.sci.advice == 1 ~ 1,
                       .default = 0))


#----- Overall stats
metrics <- data.frame("Summary" = c("Species.with.Assessments", "Multispecies.Stocks", "Species.with.Multiple.Assessments", "Total.Assessments"), "Total" = 0)

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

print(metrics)



#----- Stock Characteristics by Councils
by.council <- si %>%
  group_by(Council.Abbv) %>%
  summarize(n = n(),
            boundaries.Y = sum(SI.stock.boundaries == 1, na.rm = TRUE),
            dataprep.Y = sum(SI.data.prep == 1, na.rm = TRUE),
            model.Y = sum(SI.model == 1, na.rm = TRUE),
            sciadv.Y = sum(SI.sci.advice == 1, na.rm = TRUE),
            other.Y = sum(SI.other == 1, na.rm = TRUE),
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
            MaxSize = mean(Max.Length.m, na.rm = TRUE)
            )

print(by.council)


#----- Landings & Value Stats
Comm.landings <- si |>
  select(c(Common.Name, Scientific.Name, Council.Abbv, Ave.Metric.Tons_Commercial)) |>
  na.omit() |>
  arrange(desc(Ave.Metric.Tons_Commercial))
Comm.landings <- distinct(Comm.landings)





#######----- Waffle plot of stocks with spatial info, by region
si.waffle <- si |>
  group_by(Council.Abbv) |>
  summarise(no.si = sum(Use == 0, na.rm = TRUE),
            yes.si = sum(Use == 1, na.rm = TRUE),
            model.si = sum(Use == 2, na.rm = TRUE),
            other.si = sum(Use == 3, na.rm = TRUE))
si.waffle.l <- si.waffle |>
  pivot_longer(cols = no.si: other.si,
               names_to = c("Use"),
               values_to = "Count") |>
  mutate(Use = factor(Use, levels=c("no.si", "yes.si", "model.si", "other.si")))
            
library(waffle)

unique(si$Council.Abbv)

ggplot(data = subset(si.waffle.l, Council.Abbv %in% c("CFMC")), aes(fill = Use, values = Count)) +
  geom_waffle(n_rows = 4) +
  scale_fill_manual(name = NULL,
                    values = c("#FFE9CE", "#97D8C4", "#F4B942","#6B9AC4"),
                    labels = c("No", "Yes", "Model", "Other")) +
  coord_equal() +
  theme_void() +
  ggtitle("CFMC")

ggplot(data = subset(si.waffle.l, Council.Abbv %in% c("GMFMC")), aes(fill = Use, values = Count)) +
  geom_waffle(n_rows = 4) +
  scale_fill_manual(name = NULL,
                    values = c("#FFE9CE", "#97D8C4", "#F4B942","#6B9AC4"),
                    labels = c("No", "Yes", "Model", "Other")) +
  coord_equal() +
  theme_void() +
  ggtitle("GMFMC")

ggplot(data = subset(si.waffle.l, Council.Abbv %in% c("GMFMC-SAFMC")), aes(fill = Use, values = Count)) +
  geom_waffle(n_rows = 4) +
  scale_fill_manual(name = NULL,
                    values = c("#FFE9CE", "#97D8C4", "#F4B942","#6B9AC4"),
                    labels = c("No", "Yes", "Model", "Other")) +
  coord_equal() +
  theme_void() +
  ggtitle("GMFMC-SAFMC")


ggplot(data = subset(si.waffle.l, Council.Abbv %in% c("MAFMC")), aes(fill = Use, values = Count)) +
  geom_waffle(n_rows = 4) +
  scale_fill_manual(name = NULL,
                    values = c("#FFE9CE", "#97D8C4", "#F4B942","#6B9AC4"),
                    labels = c("No", "Yes", "Model", "Other")) +
  coord_equal() +
  theme_void() +
  ggtitle("MAFMC")

ggplot(data = subset(si.waffle.l, Council.Abbv %in% c("NEFMC")), aes(fill = Use, values = Count)) +
  geom_waffle(n_rows = 4) +
  scale_fill_manual(name = NULL,
                    values = c("#FFE9CE", "#97D8C4", "#F4B942","#6B9AC4"),
                    labels = c("No", "Yes", "Model", "Other")) +
  coord_equal() +
  theme_void() +
  ggtitle("NEFMC")


ggplot(data = subset(si.waffle.l, Council.Abbv %in% c("NEFMC-MAFMC")), aes(fill = Use, values = Count)) +
  geom_waffle(n_rows = 4) +
  scale_fill_manual(name = NULL,
                    values = c("#FFE9CE", "#97D8C4", "#F4B942","#6B9AC4"),
                    labels = c("No", "Yes", "Model", "Other")) +
  coord_equal() +
  theme_void() +
  ggtitle("NEFMC-MAFMC")

ggplot(data = subset(si.waffle.l, Council.Abbv %in% c("NOAA HMS, ICCAT")), aes(fill = Use, values = Count)) +
  geom_waffle(n_rows = 4) +
  scale_fill_manual(name = NULL,
                    values = c("#FFE9CE", "#97D8C4", "#F4B942","#6B9AC4"),
                    labels = c("No", "Yes", "Model", "Other")) +
  coord_equal() +
  theme_void() +
  ggtitle("NOAA HMS, ICCAT")

ggplot(data = subset(si.waffle.l, Council.Abbv %in% c("NPFMC")), aes(fill = Use, values = Count)) +
  geom_waffle(n_rows = 8) +
  scale_fill_manual(name = NULL,
                    values = c("#FFE9CE", "#97D8C4", "#F4B942","#6B9AC4"),
                    labels = c("No", "Yes", "Model", "Other")) +
  coord_equal() +
  theme_void() +
  ggtitle("NPFMC")

ggplot(data = subset(si.waffle.l, Council.Abbv %in% c("PFMC")), aes(fill = Use, values = Count)) +
  geom_waffle(n_rows = 8) +
  scale_fill_manual(name = NULL,
                    values = c("#FFE9CE", "#97D8C4", "#F4B942","#6B9AC4"),
                    labels = c("No", "Yes", "Model", "Other")) +
  coord_equal() +
  theme_void() +
  ggtitle("PFMC")

ggplot(data = subset(si.waffle.l, Council.Abbv %in% c("PFMC-WPFMC")), aes(fill = Use, values = Count)) +
  geom_waffle(n_rows = 4) +
  scale_fill_manual(name = NULL,
                    values = c("#FFE9CE", "#97D8C4", "#F4B942","#6B9AC4"),
                    labels = c("No", "Yes", "Model", "Other")) +
  coord_equal() +
  theme_void() +
  ggtitle("PFMC-WPFMC")

ggplot(data = subset(si.waffle.l, Council.Abbv %in% c("SAFMC")), aes(fill = Use, values = Count)) +
  geom_waffle(n_rows = 4) +
  scale_fill_manual(name = NULL,
                    values = c("#FFE9CE", "#97D8C4", "#F4B942","#6B9AC4"),
                    labels = c("No", "Yes", "Model", "Other")) +
  coord_equal() +
  theme_void() +
  ggtitle("SAFMC")

ggplot(data = subset(si.waffle.l, Council.Abbv %in% c("WPFMC")), aes(fill = Use, values = Count)) +
  geom_waffle(n_rows = 4) +
  scale_fill_manual(name = NULL,
                    values = c("#FFE9CE", "#97D8C4", "#F4B942","#6B9AC4"),
                    labels = c("No", "Yes", "Model", "Other")) +
  coord_equal() +
  theme_void() +
  ggtitle("WPFMC")



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
            sciadvice.NA = sum(is.na(SI.sci.advice)),
            
            other.Y = sum(SI.other == 1, na.rm = TRUE),
            other.N = sum(SI.other == 0, na.rm = TRUE),
            other.NA = sum(is.na(SI.other))
  )

si.national.stack <- si.national %>%
  pivot_longer(cols = boundaries.Y: other.NA,
               names_to = c("Step"),
               values_to = "Total") %>%
  separate_wider_delim(Step,
                       ".",
                       names = c("Step", "Response")) %>%
  mutate(Step = factor(Step, levels=c("boundaries", "dataprep", "model", "sciadvice", "other"))) %>%
  mutate(Response = factor(Response, levels=c("Y", "N", "NA")))

# Graph National trends
cols <- c("#0571B0", "#CA0020","grey90") # "#F7F7F7"
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
  group_by(Council.Abbv) %>%
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
            sciadvice.NA = sum(is.na(SI.sci.advice)),
            
            other.Y = sum(SI.other == 1, na.rm = TRUE),
            other.N = sum(SI.other == 0, na.rm = TRUE),
            other.NA = sum(is.na(SI.other))
            )

si.council.stack <- si.council %>%
  pivot_longer(cols = boundaries.Y: other.NA,
               names_to = c("Step"),
               values_to = "Total") %>%
  separate_wider_delim(Step,
                       ".",
                       names = c("Step", "Response")) %>%
  mutate(Step = factor(Step, levels=c("boundaries", "dataprep", "model", "sciadvice", "other"))) %>%
  mutate(Response = factor(Response, levels=c("Y", "N", "NA")))
  

# Graph by Region

#Tried a loop but I'm off my game
# names <- unique(si.council.stack$Council.Abbv)
# parts <- LETTERS[1:length(unique(si.council.stack$Council.Abbv))]
# junk <- c()
# #Use cols from national graph above
# for (z in names) {
#   for (y in parts) {
#      junk[1] <- ggplot(data = subset(si.council.stack, Council.Abbv %in% c(z)), aes(x = Step, y = Total, fill = Response)) +
#     geom_bar(stat = "identity") +
#     # theme(axis.text.x = element_text(angle = 45, hjust = 0.95)) +
#     scale_fill_manual(values = cols) +
#     theme(axis.text.x = element_blank()) +
#     theme(panel.background = element_blank()) +
#     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
#     theme(axis.line = element_line(colour = "black")) +
#     theme(axis.title.x = element_blank()) +
#     theme(text = element_text(size = 14)) +
#     ylim(0, 100) +
#     ggtitle(z) +
#     theme(legend.position = "none") 
#   }
# }
  
  
a <- ggplot(data = subset(si.council.stack, Council.Abbv %in% c("CFMC")), aes(x = Step, y = Total, fill = Response)) +
  geom_bar(stat = "identity") +
  # theme(axis.text.x = element_text(angle = 45, hjust = 0.95)) +
  scale_fill_manual(values = cols) +
  theme(axis.text.x = element_blank()) +
  theme(panel.background = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.x = element_blank()) +
  theme(text = element_text(size = 14)) +
  ggtitle("CFMC") +
  theme(legend.position = "none") 

b <- ggplot(data = subset(si.council.stack, Council.Abbv %in% c("GMFMC")), aes(x = Step, y = Total, fill = Response)) +
  geom_bar(stat = "identity") +
  # theme(axis.text.x = element_text(angle = 45, hjust = 0.95)) +
  scale_fill_manual(values = cols) +
  theme(axis.text.x = element_blank()) +
  theme(panel.background = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.x = element_blank()) +
  theme(text = element_text(size = 14)) +
  ggtitle("GMFMC") +
  theme(legend.position = "none")

c <- ggplot(data = subset(si.council.stack, Council.Abbv %in% c("GMFMC-SAFMC")), aes(x = Step, y = Total, fill = Response)) +
  geom_bar(stat = "identity") +
  # theme(axis.text.x = element_text(angle = 45, hjust = 0.95)) +
  scale_fill_manual(values = cols) +
  theme(axis.text.x = element_blank()) +
  theme(panel.background = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.x = element_blank()) +
  theme(text = element_text(size = 14)) +
  ggtitle("GMFMC-SAFMC") +
  theme(legend.position = "none")

d <- ggplot(data = subset(si.council.stack, Council.Abbv %in% c("MAFMC")), aes(x = Step, y = Total, fill = Response)) +
  geom_bar(stat = "identity") +
  # theme(axis.text.x = element_text(angle = 45, hjust = 0.95)) +
  scale_fill_manual(values = cols) +
  theme(axis.text.x = element_blank()) +
  theme(panel.background = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.x = element_blank()) +
  theme(text = element_text(size = 14)) +
  ggtitle("MAFMC") +
  theme(legend.position = "none")

e <- ggplot(data = subset(si.council.stack, Council.Abbv %in% c("NEFMC")), aes(x = Step, y = Total, fill = Response)) +
  geom_bar(stat = "identity") +
  # theme(axis.text.x = element_text(angle = 45, hjust = 0.95)) +
  scale_fill_manual(values = cols) +
  theme(axis.text.x = element_blank()) +
  theme(panel.background = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.x = element_blank()) +
  theme(text = element_text(size = 14)) +
  ggtitle("NEFMC") +
  theme(legend.position = "none")

f <- ggplot(data = subset(si.council.stack, Council.Abbv %in% c("NEFMC-MAFMC")), aes(x = Step, y = Total, fill = Response)) +
  geom_bar(stat = "identity") +
  # theme(axis.text.x = element_text(angle = 45, hjust = 0.95)) +
  scale_fill_manual(values = cols) +
  theme(axis.text.x = element_blank()) +
  theme(panel.background = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.x = element_blank()) +
  theme(text = element_text(size = 14)) +
  ggtitle("NEFMC-MAFMC") +
  theme(legend.position = "none")

g <- ggplot(data = subset(si.council.stack, Council.Abbv %in% c("NOAA HMS, ICCAT")), aes(x = Step, y = Total, fill = Response)) +
  geom_bar(stat = "identity") +
  # theme(axis.text.x = element_text(angle = 45, hjust = 0.95)) +
  scale_fill_manual(values = cols) +
  theme(axis.text.x = element_blank()) +
  theme(panel.background = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.x = element_blank()) +
  theme(text = element_text(size = 14)) +
  ggtitle("ICCAT") +
  theme(legend.position = "none")


h <- ggplot(data = subset(si.council.stack, Council.Abbv %in% c("NPFMC")), aes(x = Step, y = Total, fill = Response)) +
  geom_bar(stat = "identity") +
  # theme(axis.text.x = element_text(angle = 45, hjust = 0.95)) +
  scale_fill_manual(values = cols) +
  theme(axis.text.x = element_blank()) +
  theme(panel.background = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.x = element_blank()) +
  theme(text = element_text(size = 14)) +
  ggtitle("NPFMC") +
  theme(legend.position = "none")

i <- ggplot(data = subset(si.council.stack, Council.Abbv %in% c("PFMC")), aes(x = Step, y = Total, fill = Response)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = cols) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.95)) +
  theme(panel.background = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.x = element_blank()) +
  theme(text = element_text(size = 14)) +
  ggtitle("PFMC") +
  theme(legend.position = "none")

j <- ggplot(data = subset(si.council.stack, Council.Abbv %in% c("PFMC-WPFMC")), aes(x = Step, y = Total, fill = Response)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = cols) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.95)) +
  theme(panel.background = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.x = element_blank()) +
  theme(text = element_text(size = 14)) +
  ggtitle("PFMC-WPFMC") + #ITTAC
  theme(legend.position = "none")

k <- ggplot(data = subset(si.council.stack, Council.Abbv %in% c("SAFMC")), aes(x = Step, y = Total, fill = Response)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = cols) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.95)) +
  theme(panel.background = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.x = element_blank()) +
  theme(text = element_text(size = 14)) +
  ggtitle("SAFMC") +
  theme(legend.position = "none")

l <- ggplot(data = subset(si.council.stack, Council.Abbv %in% c("WPFMC")), aes(x = Step, y = Total, fill = Response)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = cols) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.95)) +
  theme(panel.background = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.x = element_blank()) +
  theme(text = element_text(size = 14)) +
  ggtitle("WPFMC")+
  theme(legend.position = "none")


plot_grid(a, b, c, d, e, f, g, h, i, j, k, l, ncol = 4, nrow = 3) 


