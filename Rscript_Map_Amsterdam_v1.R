
# MAP Amsterdam

#library
library(tidyverse)
library(osmdata) # package for working with streets
library(showtext) # for custom fonts
library(ggmap)
library(sf)


#####################################################################
#position
CityName = "Amsterdam"
long_center = 4.89804
lat_center = 52.37436
#
min_lon <- long_center-0.03
max_lon <- long_center+0.03
min_lat <- lat_center-0.02
max_lat <- lat_center+0.02
CityPosition <- rbind(x=c(min_lon,max_lon), y=c(min_lat,max_lat))
colnames(CityPosition) <- c("min","max")


#####################################################################
# https://wiki.openstreetmap.org/wiki/Map_features
#data water lines, canals, ...
waterway_river <- opq(CityPosition) %>% 
  add_osm_feature(key = 'waterway') %>%
  osmdata_sf

#data water polygons / all water bodies
waterbody <- opq(CityPosition) %>% 
  add_osm_feature(key = 'natural', value = c('water')) %>%
  add_osm_feature(key = 'water', value = c('river')) %>%
  osmdata_sf 

#data buildings
building <- opq(CityPosition) %>% 
  add_osm_feature(key = 'building') %>% 
  osmdata_sf() 

#data Parks and forest 
landuse <- opq(CityPosition) %>% 
  add_osm_feature(key = "landuse", value=c("allotments", "farmland", "farmyard",
                                           "forest", "meadow", "orchard", "vineyard")) %>%
  osmdata_sf()
gras <- opq(CityPosition) %>% 
  add_osm_feature(key = "landuse", value=c("grass", "greenfield")) %>%
  osmdata_sf()


#####################################################################
#get the cicle
center <- c(long = long_center,
            lat = lat_center)
circle <- tibble(lat = center["lat"], long = center["long"]) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  st_transform(crs = 6384) %>% # https://epsg.io/6384
  st_buffer(dist = 2000) %>% 
  st_transform(crs = 4326)

#cut all map / round 
waterway_river_cropped <- waterway_river$osm_lines %>% st_intersection(circle)
waterbody1_cropped <- waterbody$osm_polygons %>% st_intersection(circle)
sf::sf_use_s2(FALSE)
waterbody2_cropped <- waterbody$osm_multipolygons %>% st_intersection(circle)
building_cropped <- building$osm_polygons %>% st_intersection(circle)
landuse_cropped <- landuse$osm_polygons %>% st_intersection(circle)
gras_cropped <- gras$osm_polygons %>% st_intersection(circle)


#####################################################################
#colors map
ColorMap_Water <- "#363F73"
ColorMap_Building <- "#BB2020"
ColorMap_LandusePark <- "#F2AC57"
Color_background <- "grey"
Color_CityText <- "#BB2020"


#####################################################################
#for the text
library(showtext)
font_add_google("Mr De Haviland", "Mr De Haviland") #https://fonts.google.com/specimen/Homemade+Apple#about
showtext_auto()
#plot
plot1 <- ggplot() +
  geom_sf(data = circle,
          fill = Color_background,
          color = NA)+
  geom_sf(data = landuse_cropped, 
          fill = ColorMap_LandusePark,
          color = NA,
          size = 0, alpha = 0.65)+
  geom_sf(data = gras_cropped, 
          fill = ColorMap_LandusePark,  
          color = NA,
          size = 0, alpha = 0.65)+
  geom_sf(data = waterway_river_cropped, 
          color = ColorMap_Water,
          size= .8)+
  geom_sf(data = waterbody1_cropped,
          color = ColorMap_Water, 
          fill = ColorMap_Water,
          size=.05, )+
  geom_sf(data = waterbody2_cropped,
          color = ColorMap_Water, 
          fill = ColorMap_Water,
          size=.05)+
  geom_sf(data = building_cropped, 
          fill = ColorMap_Building,
          color = NA,
          size = 0, alpha = 0.5)+
  theme_void() +
  geom_text(aes(label = CityName),
            x = long_center, y = lat_center-.0195,
            family = "Mr De Haviland", size = 25,
            colour = Color_CityText, fontface = "bold")+
  theme(plot.background = element_rect(fill = NA, colour = NA),
        legend.position = "none")+
  scale_y_continuous(limits = c(min_lat-.001, max_lat+.001))
   
 
#save
ggsave(file=paste(CityName, "_Map1",".pdf", sep=""),
      plot1, 
      units="cm", width = 20, height=22)

