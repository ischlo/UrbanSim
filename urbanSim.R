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
library(maptools)


#### Reading DATA ####

projection <-  "+proj=longlat +datum=WGS84 +no_defs"

tube <- st_read("Data/underground/underground.shp") %>% st_transform(.,crs=projection)

#### Overview of data  ####

tmap_mode("view")

qtm(tube)

#### Creating graph and spatil lines from tube ####

tube_graph <- tube %>% as(.,"Spatial")  %>% SpatialLinesNetwork() %>% .@g %>% simplify(.,remove.multiple = F,remove.loops = T)

simpli(tube_graph)

tube_sl <- tube %>% as(.,"Spatial")  %>% SpatialLinesNetwork() %>% .@sl %>% st_as_sf()

warnings()

#### Assembling stations data set #### 

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

tube_statcoords <-  tube_sl %>% st_coordinates() %>% as_tibble() %>% 
  rename(edgeID = L1) %>%
  mutate(start_end = rep(c('start', 'end'), times = n()/2))


qtm(Stations)

qtm(tube)

#### Centrality measures  ####

# edge betweenness
tube_eb <- edge_betweenness(tube_graph, directed = F, weights = tube_sl$length)

tube_sl$eb <- tube_eb

#betweenness

tube_b <- betweenness(tube_graph, directed = F, weights = tube_sl$length, normalized = T)

Stations$b <- tube_b

# node degree

tube_deg <- degree(tube_graph,mode = "all", loops = F, normalized = F)

tube_degdistr <- degree.distribution(tube_graph,cumulative = F)

hist(tube_deg)

qtm(tube_sl)

tube_trans <- transitivity(tube_graph, type = "local", isolates = "zero")

hist(tube_trans)

Stations$trans <- tube_trans

avTrans <- transitivity(tube_graph)

#### Mapping #### 


# 
# testl <- leaflet() %>% addTiles()
# 
# for (i in 1:(nrow(tube_statcoords)/2)) {
#   testl <- testl %>% addPolylines(tube_statcoords, lat = tube_statcoords$Y[(2*i-1):(2*i)],lng = tube_statcoords$X[(2*i-1):(2*i)])
# }
# testl

hist(tube_b)

# Thanks for the following procedure to https://stackoverflow.com/questions/32940617/change-color-of-leaflet-marker 

Stations$translvl <- cut(Stations$trans
                         ,breaks = unlist(makeBins(Stations$trans,4)[1])
                         ,labels = unlist(makeBins(Stations$trans,4)[2]) 
                         ,right = F
                         ,include.lowest = T
                         ,dig.lab = 2
)

transCol <- colorFactor(palette = 'viridis', Stations$translvl)

Stations$lvl <- cut(Stations$b
            ,breaks = unlist(makeBins(Stations$b)[1])
            ,labels = unlist(makeBins(Stations$b)[2]) 
            ,right = F
            ,include.lowest = T
            ,dig.lab = 5
)

beatCol <- colorFactor(palette = 'plasma', Stations$lvl)

beatCol(Stations$lvl)

tube_sl$lvl <- cut(tube_eb
                   ,breaks = unlist(makeBins(tube_eb)[1])
                   ,labels = unlist(makeBins(tube_eb)[2]) 
                   ,right = F
                   ,include.lowest = T
                   ,dig.lab = 5
)

ebCol <- colorFactor(palette = "plasma"
                     ,tube_sl$lvl)

l <- leaflet(Stations) %>% 
  addTiles() %>% 
  addCircleMarkers(radius = 1
                   ,popup = ~name
                   ,opacity = 1
                   ,color = beatCol(Stations$lvl)
                   ,group = "Stations") %>% 
  addCircleMarkers(radius = 1
                   ,popup = ~name
                   ,opacity = 1
                   ,color = transCol(Stations$translvl)
                   ,group = "Transitivity") %>% 
  # Layers control
  addLayersControl(
    baseGroups = "OSM",
    overlayGroups = c("Stations", "Links", "Transitivity"),
    options = layersControlOptions(collapsed = FALSE)
  ) 

l

for (i in 1:(nrow(tube_statcoords)/2)) {
  l <- l %>% addPolylines(data = tube_statcoords
                 ,lng = tube_statcoords$X[(2*i-1):(2*i)]
                 ,lat = tube_statcoords$Y[(2*i-1):(2*i)]
                 ,group = "Links"
                 ,popup = tube_sl$eb[i]
                 ,weight = 3
                 ,opacity = .7
                 ,color = ebCol(tube_sl$lvl[i]))
}

l <- l %>% addLegend('bottomright', pal = beatCol, values = Stations$lvl,
                     title = 'Betweenness score of <br>tube stations (normalized)',
                     opacity = 1) %>% 
  addLegend('bottomleft'
            ,pal = ebCol
            ,values = tube_sl$lvl
            ,title = 'Edge Betweenness score of lines'
            ,opacity = 1) %>% 
  addLegend('bottomleft'
            ,pal = transCol
            ,values = Stations$translvl
            ,title = paste('Transitivity, average = ',as.character(avTrans)
                           ,sep = "")
            ,opacity = 1) 

l

#### Functions #### 

makeBins <- function(dat, n = 5) {
  min <-  min(dat)
  max <- max(dat)
  step <- (max-min)/(n-1)
  bins <- min
  lab <- as.character(min)
  for (i in 1:(n-2)) {
    bins <- c(bins,min + i*step)
    lab <- c(lab,as.character(min + i*step))
  }
  bins <- c(as.numeric(bins),max)
  #lab <- c(lab,as.character(max))
  return(list(bins,lab))
}



#### Function tests ####

unlist(makeBins(tube_b))



