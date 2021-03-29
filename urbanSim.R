#### LIBS ####

library(sf)
library(tmap)
library(tmaptools)
library(igraph)

#### Reading DATA ####

tube <- st_read("Data/underground/underground.shp")


#### Overview of data  ####

tmap_mode("view")

qtm(tube)