# fishbase-life.R
######################################
# Janelle L. Morano

# Pull in ecological variables
# This is used to integrate back into stock database

# last updated 19 December 2024
###############################################
###############################################

library(tidyverse)
library(janitor)
library(cowplot)

library("rfishbase")

fb_tbl("ecosystem")

library(FishLife)


sp <- c("Brevoortia tyrannus", "Gadus macrocephalus")
ecol <- ecology(species_list = sp)
ecosys <- ecosystem(species_list = sp)
swm <- swimming(species_list = sp)
country <- country(species_list = sp)
