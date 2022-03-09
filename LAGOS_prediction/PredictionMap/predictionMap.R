library(ggspatial)
library(tigris)
library(mapview)
library(MetBrewer)
library(tidyverse)
library(sf)
states <- states(cb = TRUE)
states_sf<- st_as_sf(states)

# 12) High lakes (man figure) ####
allLagos.out = read_csv('LAGOS_prediction/output_data_allLagosPredictions_2020_03_28.csv')

b = allLagos.out %>% 
  select(lagoslakeid:lakeconnection, pred.50) |> 
  mutate(pred.50.bins = cut(pred.50, breaks = c(0,20,100,230, Inf))) |> 
  arrange(pred.50)

#filter(obsLakes == FALSE)

mapLakes = ggplot(data = b) +
  geom_sf(data=states_sf[states_sf$NAME %in% c('New York','Vermont','New Hampshire','Maine','Rhode Island',
                                               'Iowa','Missouri','Illinois','Ohio','Indiana','Pennsylvania','New Jersey',
                                               'Massachusetts','Connecticut','Wisconsin','Minnesota','Michigan'),], 
          fill="grey90", size = 0.5) +
  geom_point(aes(x=nhd_long, y=nhd_lat, color = pred.50.bins), size = 0.8, shape = 16) +
  scale_color_manual(values=met.brewer("Renoir", 4), labels = c('0-20', '20-100','100-230','230 +'), name = 'Chloride (mg/L)') +
  theme_bw(base_size = 9) +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank()); mapLakes

ggsave(filename = 'LAGOS_prediction/PredictionMap/predictionMap.png',width = 7, height = 4)

