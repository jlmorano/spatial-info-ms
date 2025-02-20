# 4-exploratory-analyses-not-in-ms.R
######################################
# Janelle L. Morano

# A collection of analyses and graphing that was ultimately
# not kept in the manuscript, but may be useful over revisions.

# last updated 20 February 2025
###############################################
###############################################



#####----- Waffle plots of how spatial info is used by stock, region
#####

# # Add column to indicate if stock used spatial info in any step or in the model, or other
# si <- si |>
#   mutate(Use = case_when(SI.model == 1 ~ 2,
#                          SI.stock.boundaries == 1 | SI.data.prep == 1 | SI.sci.advice == 1 & SI.model == 0 ~ 1,
#                          .default = 0)) |>
#   mutate(Use.Other = case_when(SI.other == 1 ~ 1,
#                                .default = 0))

# #######----- Waffle plot of stocks with spatial info, by region
# si.waffle <- si |>
#   group_by(Council.Abbv) |>
#   summarise(no.si = sum(Use == 0, na.rm = TRUE),
#             yes.si = sum(Use == 1, na.rm = TRUE),
#             model.si = sum(Use == 2, na.rm = TRUE),
#             other.si = sum(Use.Other == 1, na.rm = TRUE))
# si.waffle.l <- si.waffle |>
#   pivot_longer(cols = no.si: other.si,
#                names_to = c("Use"),
#                values_to = "Count") |>
#   mutate(Use = factor(Use, levels=c("no.si", "yes.si", "model.si")))
#             
# library(waffle)
# 
# unique(si$Council.Abbv)
# 
# ggplot(data = subset(si.waffle.l, Council.Abbv %in% c("CFMC")), aes(fill = Use, values = Count)) +
#   geom_waffle(n_rows = 5, colour = "black") +
#   scale_fill_manual(name = NULL,
#                     values = c("#FFE9CE", "#97D8C4","#6B9AC4"),
#                     labels = c("No", "Yes", "Model")) +
#   coord_equal() +
#   theme_void() +
#   theme(legend.position="none") +
#   ggtitle("CFMC")
# 
# ggplot(data = subset(si.waffle.l, Council.Abbv %in% c("GMFMC")), aes(fill = Use, values = Count)) +
#   geom_waffle(n_rows = 5, colour = "black") +
#   scale_fill_manual(name = NULL,
#                     values = c("#FFE9CE", "#97D8C4","#6B9AC4"),
#                     labels = c("No", "Yes", "Model")) +
#   coord_equal() +
#   theme_void() +
#   theme(legend.position="none") +
#   ggtitle("GMFMC")
# 
# ggplot(data = subset(si.waffle.l, Council.Abbv %in% c("GMFMC-SAFMC")), aes(fill = Use, values = Count)) +
#   geom_waffle(n_rows = 4, size = 0.33, colour = "white") +
#   scale_fill_manual(name = NULL,
#                     values = c("#FFE9CE", "#97D8C4","#6B9AC4"),
#                     labels = c("No", "Yes", "Model")) +
#   coord_equal() +
#   theme_void() +
#   theme(legend.position="none") +
#   ggtitle("GMFMC-SAFMC")
# 
# 
# ggplot(data = subset(si.waffle.l, Council.Abbv %in% c("MAFMC")), aes(fill = Use, values = Count)) +
#   geom_waffle(n_rows = 5, colour = "black") +
#   scale_fill_manual(name = NULL,
#                     values = c("#FFE9CE", "#97D8C4","#6B9AC4"),
#                     labels = c("No", "Yes", "Model")) +
#   coord_equal() +
#   theme_void() +
#   theme(legend.position="none") +
#   ggtitle("MAFMC")
# 
# ggplot(data = subset(si.waffle.l, Council.Abbv %in% c("NEFMC")), aes(fill = Use, values = Count)) +
#   geom_waffle(n_rows = 5, colour = "black") +
#   scale_fill_manual(name = NULL,
#                     values = c("#FFE9CE", "#97D8C4","#6B9AC4"),
#                     labels = c("No", "Yes", "Model")) +
#   coord_equal() +
#   theme_void() +
#   theme(legend.position="none") +
#   ggtitle("NEFMC")
# 
# 
# ggplot(data = subset(si.waffle.l, Council.Abbv %in% c("NEFMC-MAFMC")), aes(fill = Use, values = Count)) +
#   geom_waffle(n_rows = 5, colour = "black") +
#   scale_fill_manual(name = NULL,
#                     values = c("#FFE9CE", "#97D8C4","#6B9AC4"),
#                     labels = c("No", "Yes", "Model")) +
#   coord_equal() +
#   theme_void() +
#   theme(legend.position="none") +
#   ggtitle("NEFMC-MAFMC")
# 
# ggplot(data = subset(si.waffle.l, Council.Abbv %in% c("NOAA HMS-ICCAT")), aes(fill = Use, values = Count)) +
#   geom_waffle(n_rows = 5, colour = "black") +
#   scale_fill_manual(name = NULL,
#                     values = c("#FFE9CE", "#97D8C4","#6B9AC4"),
#                     labels = c("No", "Yes", "Model")) +
#   coord_equal() +
#   theme_void() +
#   theme(legend.position="none") +
#   ggtitle("NOAA HMS-ICCAT")
# 
# ggplot(data = subset(si.waffle.l, Council.Abbv %in% c("NPFMC")), aes(fill = Use, values = Count)) +
#   geom_waffle(n_rows = 10) +
#   scale_fill_manual(name = NULL,
#                     values = c("#FFE9CE", "#97D8C4","#6B9AC4"),
#                     labels = c("No", "Yes", "Model")) +
#   coord_equal() +
#   theme_void() +
#   theme(legend.position="none") +
#   ggtitle("NPFMC")
# 
# ggplot(data = subset(si.waffle.l, Council.Abbv %in% c("PFMC")), aes(fill = Use, values = Count)) +
#   geom_waffle(n_rows = 10) +
#   scale_fill_manual(name = NULL,
#                     values = c("#FFE9CE", "#97D8C4","#6B9AC4"),
#                     labels = c("No", "Yes", "Model")) +
#   coord_equal() +
#   theme_void() +
#   theme(legend.position="none") +
#   ggtitle("PFMC")
# 
# ggplot(data = subset(si.waffle.l, Council.Abbv %in% c("PFMC-P-RFMO", "WPFMC-P-RFMO", "WPFMC-PFMC-P-RFMO")), aes(fill = Use, values = Count)) +
#   geom_waffle(n_rows = 5) +
#   scale_fill_manual(name = NULL,
#                     values = c("#FFE9CE", "#97D8C4","#6B9AC4"),
#                     labels = c("No", "Yes", "Model")) +
#   coord_equal() +
#   theme_void() +
#   theme(legend.position="none") +
#   ggtitle("PFMC/WPFMC-RFMO")
# 
# ggplot(data = subset(si.waffle.l, Council.Abbv %in% c("SAFMC")), aes(fill = Use, values = Count)) +
#   geom_waffle(n_rows = 5) +
#   scale_fill_manual(name = NULL,
#                     values = c("#FFE9CE", "#97D8C4","#6B9AC4"),
#                     labels = c("No", "Yes", "Model")) +
#   coord_equal() +
#   theme_void() +
#   theme(legend.position="none") +
#   ggtitle("SAFMC")
# 
# ggplot(data = subset(si.waffle.l, Council.Abbv %in% c("WPFMC")), aes(fill = Use, values = Count)) +
#   geom_waffle(n_rows = 5) +
#   scale_fill_manual(name = NULL,
#                     values = c("#FFE9CE", "#97D8C4","#6B9AC4"),
#                     labels = c("No", "Yes", "Model")) +
#   coord_equal() +
#   theme_void() +
#   theme(legend.position="none") +
#   ggtitle("WPFMC")


