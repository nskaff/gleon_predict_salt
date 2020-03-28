library(ggspatial)
library(tigris)
library(mapview)
library(viridisLite)
states <- states(cb = TRUE)
states_sf<- st_as_sf(states)

# 12) High lakes (man figure) ####
b = allLagos.out %>% 
  filter(pred.50 > 50) %>% 
  mutate(cols = 
           case_when(pred.50 < 100 ~ 1,
                     pred.50 >= 100 & pred.50 <260 ~ 2,
                     pred.50 > 260 ~ 3)) %>% 
  mutate(expCl = pred.50) #%>% 

#filter(obsLakes == FALSE)

mapHighLakes = ggplot(data = b %>% filter(obsLakes == FALSE)) +
  geom_sf(data=states_sf[states_sf$NAME %in% c('New York','Vermont','New Hampshire','Maine','Rhode Island',
                                               'Iowa','Missouri','Illinois','Ohio','Indiana','Pennsylvania','New Jersey',
                                               'Massachusetts','Connecticut','Wisconsin','Minnesota','Michigan'),], 
          fill="grey90", size = 0.5)+
  geom_point(aes(x=nhd_long, y=nhd_lat), size = 0.8, shape = 16) +
  theme_bw(base_size = 9) +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank())
mapHighLakes
ggsave(filename = 'LAGOS_prediction/Figures/Map_HighLakes.png',width = 7, height = 4)


# m = b %>% mapview(zcol = "expCl", layer.name = 'Predicted Chloride (mg/L)')
# m
# mapshot(m, url = paste0(getwd(), "/html/map.html"))


# esri_ocean <- paste0('https://services.arcgisonline.com/arcgis/rest/services/',
#                      'Ocean/World_Ocean_Base/MapServer/tile/${z}/${y}/${x}.jpeg')
esri_land <-    paste0('https://services.arcgisonline.com/arcgis/rest/services/NatGeo_World_Map/MapServer/tile/${z}/${y}/${x}.jpeg')
esri_streets <- paste0('https://services.arcgisonline.com/arcgis/rest/services/World_Street_Map/MapServer/tile/${z}/${y}/${x}.jpeg')
world_gray <-   paste0('https://services.arcgisonline.com/arcgis/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/${z}/${y}/${x}.jpeg')

# Chicagoland 
chicago.map = b %>% 
  filter(nhd_lat < 42.4 & nhd_lat > 41.6) %>% 
  filter(nhd_long > -88.5 & nhd_long < -87.5) %>% 
  st_as_sf(coords = c('nhd_long','nhd_lat'),crs = 4326)

map.chicago = 
  ggplot(chicago.map) +
  annotation_map_tile(type = world_gray, zoom = 10) +  
  geom_sf(size = 0.8, color = 'black',
          show.legend = "point", inherit.aes = FALSE) +
  annotate('text',x =-87.57, y = 41.9, label = 'Chicago', colour = "grey30", size = 2) +
  theme_bw(base_size = 9) +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))


# Boston
boston.map = b %>% 
  filter(nhd_lat < 42.5 & nhd_lat > 41.7) %>% 
  filter(nhd_long > -71.6 & nhd_long < -70.6) %>% 
  st_as_sf(coords = c('nhd_long','nhd_lat'),crs = 4326)

map.boston = ggplot(boston.map) +
  annotation_map_tile(type = world_gray, zoom = 10) +  
  geom_sf( size = 0.8, color = 'black',
          show.legend = "point", inherit.aes = FALSE) +
  annotate('text',x = -71, y = 42.35, label = 'Boston', colour = "grey30", size = 2) +
  annotate('text',x = -71.25, y = 41.77, label = 'Providence', colour = "grey30", size = 2) +
  theme_bw(base_size = 9) +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

# mapCities = plot_grid(map.chicago, map.boston, labels = c('b', 'c'), label_size = 10, nrow = 1, align = 'h')
# ggsave(plot = mapCities, filename = 'LAGOS_prediction/Map_cities.png',width = 7, height = 4)
# 
# 
# mapCombo = plot_grid(mapHighLakes, mapCities, labels = c('a', ''), label_size = 10, ncol = 1)
# ggsave(plot = mapCombo, filename = 'LAGOS_prediction/Map_Combo.png',width = 7, height = 5)

mapCombo2 = plot_grid(mapHighLakes, map.chicago, map.boston, labels = c('a', 'b', 'c'), 
                      label_size = 10, nrow = 1,rel_widths = c(0.4,0.3,0.3))
ggsave(plot = mapCombo2, filename = 'LAGOS_prediction/Figure6_Map_Cities.png',width = 7, height = 2.5)
 

