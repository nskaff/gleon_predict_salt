head(dat)

tt = dat %>% 
  filter(!is.na(WS.Dev.Low)) %>% 
  mutate(Month = month(ActivityStartDate)) %>%
  group_by(lagoslakeid) %>% 
  dplyr::summarise_if(is.numeric,list(mean)) %>% 
  select_(.dots = c('lagoslakeid',names(rf_cov)))

head(tt)  
summary(tt$WS.RoadDensity)


tt = tt %>% mutate(app.road = WS.RoadDensity * WS.Area * 7.891 ) %>%  # kg of salt = 7.891 kg/m of road salt * watershed area
  mutate(app.fertilizer = 0.01 * WS.Crops * WS.Area * 97) %>%  # kg of fertilizer = 97 kg/ha
  mutate(app.manure = 0.01 * WS.PastureHay * WS.Area * 13.78012496) %>%  # kg of manure = 13.78012496 kg/ha 
  mutate(app.road.per = app.road / (app.road + app.fertilizer + app.manure)) %>%
  mutate(app.fertilizer.per = app.fertilizer / (app.road + app.fertilizer + app.manure)) %>%
  mutate(app.manure.per = app.manure / (app.road + app.fertilizer + app.manure)) 

ggplot(tt) + geom_boxplot(aes(y = app.road.per), color = 'brown') +
  geom_boxplot(aes(y = app.fertilizer.per), color = 'green') +
  geom_boxplot(aes(y = app.manure.per), color = 'blue')


