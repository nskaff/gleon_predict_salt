# allLagos = 
if (!exists('nhd1')) {
    nhd1 = st_read('~/Dropbox/currentprojects/SaltBayes/LAGOS_GIS/LAGOS_NE_All_Lakes_1ha/',stringsAsFactors = F)
  }
modeledLakes = dat_rf$lagoslakeid
############# ############# ############# ############# ############# ############# 
############# Compare to Wisconsin field data ############# 
wi.1 = read_csv('~/Dropbox/currentprojects/RoadSalt/1998_2000_LandscapePosition/cl_data1998_2000.csv') %>% 
  dplyr::select(lat = latitude, long = longitude, name = lake, wbic = wbic, chloride = cl) %>% 
  mutate(group = 'wi.1')

wi.2 = read_csv('~/Dropbox/currentprojects/RoadSalt/2004LakeSurvey_Hanson/cl_data2004.csv') %>%
  mutate(name = NA) %>% 
  dplyr::select(lat = latitude, long = longitude, name, wbic = wbic, chloride = cl) %>% 
  mutate(group = 'wi.2')

wi.3 = read_csv('~/Dropbox/currentprojects/RoadSalt/2015LakeSurvey/LakeSurvey.csv') %>% 
  mutate(wbic = NA) %>% 
  dplyr::select(lat = Y, long = X, name = WATERBODY_, wbic = wbic, chloride = Value) %>% 
  mutate(group = 'wi.3')

wi.4 = read_csv('~/Dropbox/currentprojects/RoadSalt/2017_Loken_Flame/NHLDLakes_WaterChem_middle.csv') %>% 
  mutate(wbic = NA) %>% 
  dplyr::select(lat = Latitude, long = Longitude, name = LakeName, wbic = wbic, chloride = Cl) %>% 
  mutate(group = 'wi.4')

wi.5 = read_csv('~/Dropbox/currentprojects/SaltBayes/WI_field_results/conductivity_cl_lakes_2018.csv') %>% 
  mutate(wbic = NA) %>% 
  dplyr::select(lat = Latitude, long = Longitude, name = `Lake Name`, wbic = wbic, chloride = `Cl (mg/L)`) %>% 
  mutate(group = 'wi.5')


wilakes = wi.1 %>% bind_rows(wi.2,wi.3,wi.4,wi.5) %>% 
  filter(!is.na(long)) %>% 
  st_as_sf(coords = c('long','lat'),crs = 4326) %>%
  st_transform(crs = st_crs(nhd1)) 

wioverlap = wilakes %>% st_join(nhd1, left=F) %>% 
  dplyr::select(lagoslakeid = lagoslakei, GNIS_Name,name:chloride,group)
st_geometry(wioverlap) = NULL

predictionsWI = wioverlap %>% left_join(allLagos) %>% 
  mutate(newcl = exp(predictionAug)) %>%
  mutate(cldiff = abs(newcl - chloride)) %>% 
  filter(!lagoslakeid %in% dat$lagoslakeid)  # which haven't been used in the model 

ggplot(predictionsWI, aes(x = chloride, y = newcl, color = group)) + geom_point() + 
  geom_abline(intercept = 0, slope = 1) +
  ylab(bquote('Predicted Chloride'~(mg~L^-1))) + xlab(bquote('Observed Chloride'~(mg~L^-1))) +
  labs(title = 'Predicted WI chloride') +
  scale_y_continuous(trans = log2_trans()) + scale_x_continuous(trans = log2_trans()) +
  scale_colour_viridis_d(direction = -1)

# ggsave(filename = 'LAGOS_data_Austin/Figure_predictions_WI.png',width = 6, height = 4, units = 'in')

############# ############# ############# ############# ############# ############# 
############# Compare to Rhode Island field data ############# 
lakes = read_csv('~/Dropbox/currentprojects/SaltBayes/RI_data/lakeLocations.csv') %>% 
  dplyr::select(lat = Lat, long = Lon, name = Site_Name, code = WB_code, Zmax__m_)

cldata = read_csv('~/Dropbox/currentprojects/SaltBayes/RI_data/RI_all lakes.csv') %>% 
  dplyr::select(name = Site, code = `WB Code`, chloride = Cl, Date) %>% 
  filter(!is.na(chloride)) %>% inner_join(lakes, by = 'code') 

clmean = cldata %>% 
  # dplyr::group_by(code) %>%
  # dplyr::summarise_if(is.numeric,list(~mean)) %>% 
  st_as_sf(coords = c('long','lat'),crs = 4326) %>%
  st_transform(crs = st_crs(nhd1)) 

rioverlap = clmean %>% st_join(nhd1, left=TRUE) %>% 
  dplyr::select(lagoslakeid = lagoslakei, Zmax__m_,GNIS_Name,code:chloride)
st_geometry(rioverlap) = NULL

predictionsRI = rioverlap %>% left_join(allLagos) %>% 
  mutate(newcl = exp(predictionAug)) %>%
  mutate(cldiff = abs(newcl - chloride)) %>% 
  filter(!lagoslakeid %in% modeledLakes) # which haven't been used in the model 

b = predictionsRI %>% group_by(lagoslakeid) %>% 
  summarise(min = min(chloride), max = max(chloride), mean = mean(chloride), newcl = first(newcl), depth = mean(Zmax__m_))

ggplot(b, aes(x = mean, y = newcl), alpha = 0.7) + geom_point(aes(color = depth)) +
  geom_errorbarh(aes(xmax = min, xmin = max), color = 'black',alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1) +
  ylab(bquote('Predicted Chloride'~(mg~L^-1))) + xlab(bquote('Observed Chloride'~(mg~L^-1))) +
  labs(title = 'Predicted RI chloride') +
  scale_y_continuous(trans = log2_trans()) + scale_x_continuous(trans = log2_trans()) +
  scale_colour_viridis_c(direction = -1)

# ggsave(filename = 'LAGOS_data_Austin/Figure_predictions_RI_max.png',width = 6, height = 4, units = 'in')

# library(maps)
# ## Get the states map, turn into sf object
# US <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
# US = st_transform(US, st_crs(nhd1))
# # Make it a spatial dataframe, using the same coordinate system as the US spatial dataframe
# #.. and perform a spatial join!
# 
# test = rioverlap %>% left_join(select(predictionsRI,lagoslakeid,cldiff)) %>%
#   mutate(cldifflog = log(cldiff))
# plot(st_geometry(US[38,]),border = 'grey', axes = TRUE)
# plot(test["cldifflog"], pch = 16,add=T)
# st_write(test,dsn = '~/Dropbox/currentprojects/SaltBayes/RI_data/GIS/rhodeislandCL.shp')


############# ############# ############# ############# ############# ############# 
############# Compare to Minnesota field data ############# 
mnin = read_csv('~/Dropbox/currentprojects/SaltBayes/MN_results_MPCA/Chloride_Lakes_02202019.csv')

mnlakes =  mnin %>%
  dplyr::rename(cl = REPORT_RESULT_VALUE) %>% 
  dplyr::filter(SAMPLE_DATE > as.Date('1990-01-01')) %>%
  filter(!is.na(cl)) %>%
  # dplyr::group_by(SYS_LOC_CODE) %>%
  # dplyr::summarise_if(is.numeric,list(~mean)) %>% 
  # dplyr::left_join(distinct(dplyr::select(mnin,SYS_LOC_CODE,LOC_DESC,SAMPLE_DATE))) %>% 
  st_as_sf(coords = c('LONGITUDE','LATITUDE'),crs = 4326) %>% st_transform(crs = st_crs(nhd1))

# All data 
mnoverlapALL = mnlakes %>% st_join(nhd1, left=TRUE) %>%
  dplyr::select(lagoslakeid = lagoslakei,Lake_Area_,Y_COORD,X_COORD,SYS_LOC_CODE,LOC_DESC,cl,SAMPLE_DATE) #%>%

mnoverlapALL_pred = mnoverlapALL %>% dplyr::left_join(allLagos) %>% 
  mutate(newcl = exp(predictionAug)) %>%
  mutate(cldiff = abs(newcl - cl)) %>% 
  mutate(clerror = abs(newcl - cl)/(newcl+cl)) %>% 
  mutate(month = month(SAMPLE_DATE)) %>% 
  filter(!is.na(newcl)) %>% 
  filter(month == 8)

predictionsMN = mnoverlapALL_pred %>% filter(!lagoslakeid %in% dat_rf$lagoslakeid)
st_geometry(predictionsMN) = NULL

b = mnoverlapALL_pred %>% group_by(lagoslakeid) %>% 
  summarise(min = min(cl), max = max(cl), mean = mean(cl), newcl = first(newcl))

library(ggrepel)
ggplot(mnoverlapALL_pred, aes(x = cl, y = newcl), alpha = 0.7) + 
  geom_point() + 
  geom_point(data = predictionsMN,aes(x = cl, y = newcl), color = 'red3', size = 2, alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1) +
  ylab('Predicted Chloride (mg/L)') + xlab('Observed Chloride (mg/L)') +
  labs(title = 'Predicted MN chloride') +
  scale_y_continuous(trans = log2_trans()) + scale_x_continuous(trans = log2_trans()) #+
  # geom_label_repel(aes(label = ifelse(clerror > 0.6,gnis_name,'')),
  #                  box.padding   = 0.35, 
  #                  point.padding = 0.5,
  #                  segment.color = 'grey50') 
# ggsave(filename = 'LAGOS_data_Austin/Figure_predictions_MN_labels.png',width = 6, height = 4, units = 'in')


ggplot(b, aes(x = mean, y = newcl), alpha = 0.7) + geom_point() +
  geom_errorbarh(aes(xmax = min, xmin = max), color = 'black',alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1) +
  geom_point(data = predictionsMN,aes(x = cl, y = newcl), color = 'red3', size = 2, alpha = 0.6) +
  ylab('Predicted Chloride (mg/L)') + xlab('Observed Chloride (mg/L)') +
  labs(title = 'Predicted MN chloride') +
  scale_y_continuous(trans = log2_trans()) + scale_x_continuous(trans = log2_trans()) #+
# geom_label_repel(aes(label = ifelse(clerror > 0.6,gnis_name,'')),
#                  box.padding   = 0.35, 
#                  point.padding = 0.5,
#                  segment.color = 'grey50') 
# ggsave(filename = 'LAGOS_data_Austin/Figure_predictions_MN_labels.png',width = 6, height = 4, units = 'in')



# Ah but there's overlap in the sites between the training and validation data. 
# sum(!dat$lagoslakeid %in% predictions$lagoslakeid)
# test = dat %>% inner_join(predictions, by = 'lagoslakeid')
# plot(test$Chloride,test$cl)


############# ############# ############# ############# ############# ############# 
############# Compare to all field data ############# 
allPredictions = predictionsWI %>% mutate(State = 'Wisconsin') %>% dplyr::select(lagoslakeid,State, chloride, newcl) %>% 
  bind_rows(predictionsRI %>% mutate(State = 'Rhode Island') %>% dplyr::select(lagoslakeid,State, chloride, newcl)) %>% 
  bind_rows(predictionsMN %>% mutate(State = 'Minnesota') %>% dplyr::select(lagoslakeid,State, chloride = cl, newcl))

ggplot(allPredictions, aes(x = chloride, y = newcl, fill = State)) + geom_point(shape = 21,alpha = 0.9) + 
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed') +
  ylab(bquote('Predicted Chloride'~(mg~L^-1))) + xlab(bquote('Observed Chloride'~(mg~L^-1))) +
  labs(title = 'Predicted chloride concentrations') +
  scale_y_continuous(trans = log2_trans()) + scale_x_continuous(trans = log2_trans()) +
  theme_bw() +
  scale_color_manual(name = "legend", values = c('#203731','red3','#4F2683')) 
ggsave(filename = 'LAGOS_prediction/Figure_predictions_WIMNRI.png',width = 7, height = 5, units = 'in')

