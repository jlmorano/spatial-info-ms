# spatial-info-in-stock-assessments.R
######################################
# Janelle L. Morano

# Quantify spatial information in US marine stocks

# last updated 5 November 2024
###############################################
###############################################

library(tidyverse)
library(janitor)
library(cowplot)



si <- read.csv("/Users/janellemorano/Git/spatial-info-ms/US Marine Fisheries Stocks and Assessments.csv", header = TRUE)
# colnames(si)
# Double-check, but NAs appear starting row 358 and should be removed
# si <- si[rowSums(is.na(si)) != ncol(si), ] #won't work because character columns with blanks
si <- si[1:357,1:17]

# Add column to indicate if stock used spatial info in any step, model, or other
si <- si |>
  mutate(Use = case_when(SI.other == 1 ~ 3,
                         SI.model == 1 ~ 2,
                         SI.stock.boundaries == 1 | SI.data.prep == 1 | SI.sci.advice == 1 ~ 1,
                       .default = 0))


#----- Overall stats
metrics <- data.frame("Summary" = c("Species.with.Assessments", "Multispecies.Stocks", "Species.with.Multiple.Assessments", "Total.Stocks", "Total.Assessments"), "Total")

# Species with Assessments
metrics[1, 2] <- tally(si |> count(si$Common.Name))

# Multispecies Complexes
q <- si |> count(si$Common.Name == "Multispecies Complex")
metrics[2, 2] <- q[2,2]

# Species with more than one Assessment
assessments <- si |> count(si$Scientific.Name)
metrics[3, 2] <- sum(assessments$n > 1)

# Total number of Stocks
metrics[4, 2] <- tally(si |> count(si$Scientific.Name))

# Total number of Assessments
metrics[5, 2] <- nrow(si)

print(metrics)


# Total number of stocks in rebuilding
df <- si |> group_by(Council.Abbv) |>
  count(si$Rebuilding.Program.Status)
print(df, n = 25)



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
ggplot(data = si.national.stack, aes(x = Step, y = Total, fill = Response)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.95)) +
  theme(panel.background = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.x = element_blank()) +
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
a <- ggplot(data = subset(si.council.stack, Council.Abbv %in% c("CFMC")), aes(x = Step, y = Total, fill = Response)) +
  geom_bar(stat = "identity") +
  # theme(axis.text.x = element_text(angle = 45, hjust = 0.95)) +
  theme(axis.text.x = element_blank()) +
  theme(panel.background = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.x = element_blank()) +
  ylim(0, 100) +
  ggtitle("CFMC") +
  theme(legend.position = "none") 

b <- ggplot(data = subset(si.council.stack, Council.Abbv %in% c("GMFMC")), aes(x = Step, y = Total, fill = Response)) +
  geom_bar(stat = "identity") +
  # theme(axis.text.x = element_text(angle = 45, hjust = 0.95)) +
  theme(axis.text.x = element_blank()) +
  theme(panel.background = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.x = element_blank()) +
  ylim(0, 100) +
  ggtitle("GMFMC") +
  theme(legend.position = "none")

c <- ggplot(data = subset(si.council.stack, Council.Abbv %in% c("GMFMC-SAFMC")), aes(x = Step, y = Total, fill = Response)) +
  geom_bar(stat = "identity") +
  # theme(axis.text.x = element_text(angle = 45, hjust = 0.95)) +
  theme(axis.text.x = element_blank()) +
  theme(panel.background = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.x = element_blank()) +
  ylim(0, 100) +
  ggtitle("GMFMC-SAFMC") +
  theme(legend.position = "none")

d <- ggplot(data = subset(si.council.stack, Council.Abbv %in% c("MAFMC")), aes(x = Step, y = Total, fill = Response)) +
  geom_bar(stat = "identity") +
  # theme(axis.text.x = element_text(angle = 45, hjust = 0.95)) +
  theme(axis.text.x = element_blank()) +
  theme(panel.background = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.x = element_blank()) +
  ylim(0, 100) +
  ggtitle("MAFMC") +
  theme(legend.position = "none")

e <- ggplot(data = subset(si.council.stack, Council.Abbv %in% c("NEFMC")), aes(x = Step, y = Total, fill = Response)) +
  geom_bar(stat = "identity") +
  # theme(axis.text.x = element_text(angle = 45, hjust = 0.95)) +
  theme(axis.text.x = element_blank()) +
  theme(panel.background = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.x = element_blank()) +
  ylim(0, 100) +
  ggtitle("NEFMC") +
  theme(legend.position = "none")

f <- ggplot(data = subset(si.council.stack, Council.Abbv %in% c("NEFMC-MAFMC")), aes(x = Step, y = Total, fill = Response)) +
  geom_bar(stat = "identity") +
  # theme(axis.text.x = element_text(angle = 45, hjust = 0.95)) +
  theme(axis.text.x = element_blank()) +
  theme(panel.background = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.x = element_blank()) +
  ylim(0, 100) +
  ggtitle("NEFMC-MAFMC") +
  theme(legend.position = "none")

g <- ggplot(data = subset(si.council.stack, Council.Abbv %in% c("NOAA")), aes(x = Step, y = Total, fill = Response)) +
  geom_bar(stat = "identity") +
  # theme(axis.text.x = element_text(angle = 45, hjust = 0.95)) +
  theme(axis.text.x = element_blank()) +
  theme(panel.background = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.x = element_blank()) +
  ylim(0, 100) +
  ggtitle("NOAA") +
  theme(legend.position = "none")

h <- ggplot(data = subset(si.council.stack, Council.Abbv %in% c("NOAA HMS, ICCAT")), aes(x = Step, y = Total, fill = Response)) +
  geom_bar(stat = "identity") +
  # theme(axis.text.x = element_text(angle = 45, hjust = 0.95)) +
  theme(axis.text.x = element_blank()) +
  theme(panel.background = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.x = element_blank()) +
  ylim(0, 100) +
  ggtitle("NOAA HMS, ICCAT") +
  theme(legend.position = "none")

i <- ggplot(data = subset(si.council.stack, Council.Abbv %in% c("NPFMC")), aes(x = Step, y = Total, fill = Response)) +
  geom_bar(stat = "identity") +
  # theme(axis.text.x = element_text(angle = 45, hjust = 0.95)) +
  theme(axis.text.x = element_blank()) +
  theme(panel.background = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.x = element_blank()) +
  ylim(0, 100) +
  ggtitle("NPFMC") +
  theme(legend.position = "none")

j <- ggplot(data = subset(si.council.stack, Council.Abbv %in% c("PFMC")), aes(x = Step, y = Total, fill = Response)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.95)) +
  theme(panel.background = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.x = element_blank()) +
  ylim(0, 100) +
  ggtitle("PFMC") +
  theme(legend.position = "none")

k <- ggplot(data = subset(si.council.stack, Council.Abbv %in% c("PFMC-WPFMC")), aes(x = Step, y = Total, fill = Response)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.95)) +
  theme(panel.background = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.x = element_blank()) +
  ylim(0, 100) +
  ggtitle("PFMC-WPFMC") + #ITTAC
  theme(legend.position = "none")

l <- ggplot(data = subset(si.council.stack, Council.Abbv %in% c("SAFMC")), aes(x = Step, y = Total, fill = Response)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.95)) +
  theme(panel.background = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.x = element_blank()) +
  ylim(0, 100) +
  ggtitle("SAFMC") +
  theme(legend.position = "none")

m <- ggplot(data = subset(si.council.stack, Council.Abbv %in% c("WPFMC")), aes(x = Step, y = Total, fill = Response)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.95)) +
  theme(panel.background = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.x = element_blank()) +
  ylim(0, 100) +
  ggtitle("WPFMC")+
  theme(legend.position = "none")


plot_grid(a, b, c, d, e, f, h, i, j, k, l, m, ncol = 4, nrow = 3) #left out g
