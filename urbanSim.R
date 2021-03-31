#### LIBS ####
library(sp)
library(sf)
require(sp)
library(tmap)
library(tmaptools)
library(igraph)
require(broom)
require(rgeos)
require(rgdal)
library(stplanr)
library(tigris)
library(vroom)
library(leaflet)
library(tibble)
library(dplyr)


#### Reading DATA ####

projection <-  "+proj=longlat +datum=WGS84 +no_defs"

tube <- st_read("Data/underground/underground.shp") %>% st_transform(.,crs=projection)

#### Overview of data  ####

tmap_mode("view")

qtm(tube)

tube_graph <- tube %>% as(.,"Spatial")  %>% SpatialLinesNetwork()

tube_graph

warnings()

#### Manipulating the data #### 

Stations <- tube %>% st_coordinates() %>%
  as_tibble() %>%
  rename(edgeID = L1) %>% 
  mutate(name = edgeID) 


for (i in 1:410) {
  Stations$name[2*i-1] <- tube$station_1_[i]
  Stations$name[2*i] <- tube$station_2_[i]
}
  
Stations <- Stations %>% group_by(edgeID) %>%
  slice(c(1, n())) %>%
  ungroup() #%>%
  # mutate(start_end = rep(c('start', 'end'), times = n()/2)) 

Stations <-  Stations %>%
  distinct(name, .keep_all = TRUE) %>%
  select(-c(edgeID)) %>%
  st_as_sf(coords = c('X', 'Y')) %>%
  st_set_crs(projection)

Stations

qtm(Stations)

qtm(tube)

l <- leaflet(Stations) %>% addCircles(.,radius = 10, popup = ~name)
l
