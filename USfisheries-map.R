# USfisheries-map.R
######################################
# Janelle L. Morano

# Global map of US and territories 


# last updated 29 April 2025
###############################################
###############################################
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)



# Bring in shp of world, US and territories
world <- st_read("/Users/janellemorano/Git/spatial-info-ms/data/world-administrative-boundaries/world-administrative-boundaries.shp")
us <- st_read("/Users/janellemorano/Git/spatial-info-ms/data/US/cb_2023_us_region_500k.shp")
usstates <- st_read("/Users/janellemorano/Git/spatial-info-ms/data/USstates/cb_2023_us_state_500k.shp")
amsam <- st_read("/Users/janellemorano/Git/spatial-info-ms/data/AmericanSamoa/cb_2023_60_place_500k.shp")
cnmi <- st_read("/Users/janellemorano/Git/spatial-info-ms/data/CNMI/cb_2023_69_place_500k.shp")
guam <- st_read("/Users/janellemorano/Git/spatial-info-ms/data/Guam/cb_2023_66_place_500k.shp")
pr <- st_read("/Users/janellemorano/Git/spatial-info-ms/data/PuertoRico/cb_2023_72_place_500k.shp")
vi <- st_read("/Users/janellemorano/Git/spatial-info-ms/data/USVirginIs/cb_2023_78_place_500k.shp")

# Set the CRS of the maps to EPSG:4326 (WGS 84) to match the world
us <- st_transform(us, 4326)
usstates <- st_transform(usstates, 4326)
amsam <- st_transform(amsam, 4326)
cnmi <- st_transform(cnmi, 4326)
guam <- st_transform(guam, 4326)
pr <- st_transform(pr, 4326)
vi <- st_transform(vi, 4326)


ggplot(data = world) +
  geom_sf(color = "gray95", fill = "gray95") +
  # geom_sf(data = us, color = "gray60", fill = "#3b528b") +
  geom_sf(data = usstates, color = "aquamarine4", fill = "aquamarine4") + #firebrick3, can use size = 0.01 to change thickness of state lines, but then loose visibility of Hawai'i
  geom_sf(data = amsam, color = "aquamarine4", fill = "aquamarine4") +
  geom_sf(data = cnmi, color = "aquamarine4", fill = "aquamarine4") +
  geom_sf(data = guam, color = "aquamarine4", fill = "aquamarine4") +
  geom_sf(data = pr, color = "aquamarine4", fill = "aquamarine4") +
  geom_sf(data = vi, color = "aquamarine4", fill = "aquamarine4") +
  coord_sf(crs = "+proj=laea +lat_0=0 +lon_0=-120 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs ") +
  theme_void() +
  theme(panel.background = element_rect(fill = "white")) + # slategray2
  theme (axis.text = element_blank())

ggsave(file = "/Users/janellemorano/Git/spatial-info-ms/figures/globalmap-of-US.png", dpi= 400, width = 9, height = 9)


# Bring in shp of coastal US states to be able to shade by FMC regions
# Northeast
# Maine, New Hampshire, Massachusetts, Rhode Island, Connecticut

# Mid-Atlantic
# New York, New Jersey, Pennsylvania, Delaware, Maryland, Virginia

# South Atlantic
# North Carolina, South Caro


# Read world map from rnaturalearth
coast <- ne_coastline(scale = "medium", returnclass = "sf") 

ggplot(data = coast) +
  geom_sf(color = "gray95") +
  geom_sf(data = usstates, color = "black", fill = "white") + #firebrick3, can use size = 0.01 to change thickness of state lines, but then loose visibility of Hawai'i
  geom_sf(data = amsam, color = "black", fill = "black") +
  geom_sf(data = cnmi, color = "black", fill = "black") +
  geom_sf(data = guam, color = "black", fill = "black") +
  geom_sf(data = pr, color = "black", fill = "black") +
  geom_sf(data = vi, color = "black", fill = "black") +
  coord_sf(crs = "+proj=laea +lat_0=0 +lon_0=-120 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs ") +
  theme_void() +
  theme(panel.background = element_rect(fill = "white")) + # slategray2
  theme (axis.text = element_blank())

ggsave(file = "/Users/janellemorano/Git/spatial-info-ms/figures/USmanagement-regions.png", dpi = 400, width = 9, height = 9)
