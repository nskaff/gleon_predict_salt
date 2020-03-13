library(patchwork)

# allLagos = 
if (!exists('nhd1')) {
    nhd1 = st_read('~/Dropbox/currentprojects/SaltBayes/LAGOS_GIS/LAGOS_NE_All_Lakes_1ha/',stringsAsFactors = F)
  }
modeledLakes = dat_rf$lagoslakeid
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
############# Wisconsin field data ############# 
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

wioverlap = wilakes %>% st_join(nhd1, left=FALSE) %>% 
  dplyr::select(lagoslakeid = lagoslakei, GNIS_Name,name:chloride,group)
st_geometry(wioverlap) = NULL

predictionsWI = wioverlap %>% left_join(allLagos.out) %>% 
  mutate(pred.Mean = exp(predictionAug2), pred.Median = exp(prediction.50)) %>%
  # mutate(cldiff = abs(newcl - chloride)) %>% 
  filter(!lagoslakeid %in% dat$lagoslakeid)  # which haven't been used in the model 

b.WI = predictionsWI %>% group_by(lagoslakeid) %>% 
  summarise(min = min(chloride), max = max(chloride), mean = mean(chloride), 
            pred.Mean = first(pred.Mean), pred.Median = first(pred.Median)) %>% 
  filter(!is.na(mean) & !is.na(pred.Median))

ggplot(b.WI, alpha = 0.7) + 
  # geom_point(aes(x = mean, y = pred.Mean), color = 'red3') +
  geom_point(aes(x = mean, y = pred.Median), color = 'navy') +
  geom_errorbarh(aes(xmax = min, xmin = max, y = pred.Median), color = 'black',alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1) +
  ylab(bquote('Predicted Chloride'~(mg~L^-1))) + xlab(bquote('Observed Chloride'~(mg~L^-1))) +
  labs(title = 'Predicted RI chloride') +
  scale_y_continuous(trans = log2_trans()) + scale_x_continuous(trans = log2_trans()) +
  scale_colour_viridis_c(direction = -1)
# ggsave(filename = 'LAGOS_data_Austin/Figure_predictions_WI.png',width = 6, height = 4, units = 'in')

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
############# Rhode Island field data ############# 
lakes = read_csv('~/Dropbox/currentprojects/SaltBayes/RI_data/lakeLocations.csv') %>% 
  dplyr::select(lat = Lat, long = Lon, name = Site_Name, code = WB_code, Zmax__m_)

cldata = read_csv('~/Dropbox/currentprojects/SaltBayes/RI_data/RI_all lakes.csv') %>% 
  dplyr::select(name = Site, code = `WB Code`, chloride = Cl, Date) %>% 
  filter(!is.na(chloride)) %>% inner_join(lakes, by = 'code') %>% 
  filter(Date > as.Date('1990-01-01'))

clmean = cldata %>% 
  # dplyr::group_by(code) %>%
  # dplyr::summarise_if(is.numeric,list(~mean)) %>% 
  st_as_sf(coords = c('long','lat'),crs = 4326) %>%
  st_transform(crs = st_crs(nhd1)) 

rioverlap = clmean %>% st_join(nhd1, left = FALSE) %>% 
  dplyr::select(lagoslakeid = lagoslakei, Zmax__m_,GNIS_Name,code:chloride)
st_geometry(rioverlap) = NULL

predictionsRI = rioverlap %>% left_join(allLagos.out) %>% 
  mutate(pred.Mean = exp(predictionAug2), pred.Median = exp(prediction.50)) %>%
  # mutate(cldiff = abs(newcl - chloride)) %>% 
  filter(!lagoslakeid %in% modeledLakes) # which haven't been used in the model 

b.RI = predictionsRI %>% group_by(lagoslakeid) %>% 
  summarise(min = min(chloride), max = max(chloride), mean = mean(chloride), 
            pred.Mean = first(pred.Mean), pred.Median = first(pred.Median), depth = mean(Zmax__m_)) %>% 
  filter(!is.na(mean) & !is.na(pred.Median))

ggplot(b.RI, alpha = 0.7) + 
  # geom_point(aes(x = mean, y = pred.Mean), color = 'red3') +
  geom_point(aes(x = mean, y = pred.Median), color = 'navy') +
  geom_errorbarh(aes(xmax = min, xmin = max, y = pred.Median), color = 'black',alpha = 0.6) +
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


## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
############# Minnesota field data ############# 
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
mnoverlapALL = mnlakes %>% st_join(nhd1, left= FALSE) %>%
  dplyr::select(lagoslakeid = lagoslakei,Lake_Area_,Y_COORD,X_COORD,SYS_LOC_CODE,LOC_DESC,cl,SAMPLE_DATE) #%>%

mnoverlapALL_pred = mnoverlapALL %>% dplyr::left_join(allLagos.out) %>% 
  mutate(pred.Mean = exp(predictionAug2), pred.Median = exp(prediction.50)) %>%
  # mutate(month = month(SAMPLE_DATE)) %>% 
  filter(!is.na(pred.Mean))  
  # filter(month == 8)

predictionsMN = mnoverlapALL_pred %>% filter(!lagoslakeid %in% dat_rf$lagoslakeid)
st_geometry(predictionsMN) = NULL

b.MN = predictionsMN %>% group_by(lagoslakeid) %>% 
  summarise(min = min(cl), max = max(cl), mean = mean(cl), lakearea = mean(Lake_Area_), pred.Mean = first(pred.Mean), pred.Median = first(pred.Median)) %>% 
  filter(!is.na(mean) & !is.na(pred.Median))

library(ggrepel)

ggplot(b.MN, alpha = 0.7) + #geom_point(aes(x = mean, y = pred.Mean), color = 'red3', size = 2, alpha = 0.7) +
  geom_point(aes(x = mean, y = pred.Median), color = 'navy', size = 2, alpha = 0.7) +
  geom_errorbarh(aes(xmax = min, xmin = max, y = pred.Median), color = 'black',alpha = 0.3) +
  geom_abline(intercept = 0, slope = 1) +
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

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
############# NLA data ############# 
nla07site = read_csv('~/Documents/nla2007_alldata/NLA2007_SampledLakeInformation_20091113.csv') %>% 
  dplyr::select(SITE_ID, lat = LAT_DD, long = LON_DD, state = ST, name = NHDNAME)
nla07data = read_csv('~/Documents/nla2007_alldata/NLA2007_WaterQuality_20091123.csv') %>% 
  dplyr::select(SITE_ID,cl = CL_PPM,clflag = CL_FLAG)
nla07 = nla07site %>% inner_join(nla07data)

nla12site = read_csv('~/Documents/nla2007_alldata/nla2012_wide_siteinfo_08232016.csv') %>% 
  dplyr::select(SITE_ID, UID, lat = LAT_DD83, long = LON_DD83, state = STATE, name = NARS_NAME)
nla12data = read_csv('~/Documents/nla2007_alldata/nla2012_waterchem_wide.csv') %>% 
  dplyr::select(UID,cl = CHLORIDE_RESULT,clflag = CHLORIDE_FLAG)
nla12 = nla12site %>% inner_join(nla12data) %>% dplyr::select(-UID)


nla = nla07 %>% bind_rows(nla12) %>% 
  filter(!is.na(long)) %>% 
  st_as_sf(coords = c('long','lat'),crs = 4326) %>%
  st_transform(crs = st_crs(nhd1)) 

nla.overlap = nla %>% st_join(nhd1, left=F) %>% 
  dplyr::select(lagoslakeid = lagoslakei, SITE_ID, GNIS_Name,state:clflag) 
st_geometry(nla.overlap) = NULL

nla.overlap = nla.overlap %>% group_by(lagoslakeid) %>% 
  summarise_all(list(first)) 

predictionsNLA = nla.overlap %>% left_join(allLagos.out) %>% 
  mutate(pred.Mean = exp(predictionAug2), pred.Median = exp(prediction.50)) %>%
  # mutate(cldiff = abs(newcl - cl)) %>% 
  filter(!lagoslakeid %in% dat$lagoslakeid)  %>%  # which haven't been used in the model 
  select(lagoslakeid:nhd_long,predictionAug:prediction.95,pred.Mean,pred.Median) %>% 
  group_by(lagoslakeid) %>% 
  dplyr::summarise_at(vars(cl,predictionAug:prediction.95,pred.Mean,pred.Median,nhd_lat,nhd_long), mean, na.rm=T) %>% 
  left_join(distinct(dplyr::select(nla.overlap,lagoslakeid,state,name))) %>% 
  filter(!is.na(pred.Median))

fitsO <- lm(prediction.50 ~ log(cl), data = predictionsNLA); summary(fitsO) #r2 = 0.81
summary(fitsO)
fitsO = data.frame(r2 = paste0('r2 = ',round(summary(fitsO)$r.squared,2)),
                   cl = 0.1,
                   prediction.50 = 100)

# predictionsNLA = predictionsNLA %>% mutate(train = ifelse(state %in% c('MN','WI','NY','VT','MI'),TRUE, FALSE))

pnla = ggplot(predictionsNLA, aes(x = cl, y = pred.Median)) + 
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed') +
  geom_point(aes(fill = nhd_lat), shape = 21) + 
  ylab(bquote('Predicted Chloride'~(mg~L^-1))) + xlab(bquote('Observed Chloride'~(mg~L^-1))) +
  # labs(title = 'Predicted NLA chloride') +
  scale_y_continuous(trans = log2_trans(), breaks = c(0,1,10,100), limits = c(0.07,220)) + 
  scale_x_continuous(trans = log2_trans(), breaks = c(0,1,10,100), limits = c(0.07,220)) +
  scale_fill_viridis_c(direction = -1,name = 'Latitude') +
  annotate("text",x = 0.5, y = 120, size = 3, label = paste0('r2 = ',
          round(cor(predictionsNLA$cl, predictionsNLA$pred.Median, use = "complete.obs") ^ 2,2))) +  
  # geom_text(data = fitsO, aes(label = r2),hjust = 1,vjust = 0, color = 'black') +
  theme_bw() +
  theme(#legend.position="bottom",
        legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid', size = 0.1),
        legend.text=element_text(size=6), legend.title = element_text(size = 6),
        legend.position=c(.7,.1), legend.direction = "horizontal",
        legend.margin=ggplot2::margin(c(2,2,0,2)))
# ggsave(filename = 'LAGOS_prediction/Figure_predictions_NLA.png',width = 7, height = 5, units = 'in')


# Tichigan Lake
# http://www.sewrpc.org/SEWRPCFiles/Publications/CAPR/capr-283_vol-01_waterford_impoundment.pdf
# Observed: 127.153000	Predicted: 52.535753
# In the surface waters of Tichigan Lake, chloride averaged about 69 mg/l in the
# spring, 50 mg/l in summer, 42 mg/l in fall and winter (1973-2004)

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
############# Compare to all field data ############# 
b.all = b.WI %>% mutate(State = 'WI') %>% 
  bind_rows(b.RI %>% mutate(State = 'RI') %>% dplyr::select(-depth)) %>% 
  bind_rows(b.MN %>% mutate(State = 'MN')) %>% 
  filter(!is.na(pred.Median)) %>% filter(!is.na(mean))

summary(lm(pred.Mean ~ mean, data = b.all)) # r2 = 0.48
summary(lm(pred.Median ~ mean, data = b.all)) # r2 = 0.50

pstates = ggplot(b.all, aes(x = mean, y = pred.Median, fill = State)) +
  geom_errorbarh(aes(xmax = min, xmin = max, y = pred.Median), color = 'black',alpha = 0.3) +
  geom_point(shape = 22,alpha = 0.9) + 
  scale_fill_viridis_d() +
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed') +
  ylab(bquote('Predicted Chloride'~(mg~L^-1))) + xlab(bquote('Observed Chloride'~(mg~L^-1))) +
  # labs(title = 'Predicted chloride concentrations') +
  scale_y_continuous(trans = log2_trans(), limits = c(0.1,400)) + scale_x_continuous(trans = log2_trans(), limits = c(0.1,400)) +
  theme_bw() +
  scale_color_manual(name = "legend", values = c('#203731','red3','#4F2683')) +
  annotate("text",x = 0.5, y = 200, size = 3, label = paste0('r2 = ',
        round(cor(b.all$pred.Median, b.all$mean, use = "complete.obs") ^ 2,2))) +  
  theme(#legend.position="bottom",
        legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid', size = 0.1),
        legend.text=element_text(size=6), legend.title = element_text(size = 6),
        legend.position=c(.7,.1), legend.direction = "horizontal",
        legend.margin=ggplot2::margin(c(0.5,2,0.5,2)))





## patchwork: combine grids 
pstates + pnla + plot_annotation(tag_levels = 'a') & theme(plot.tag = element_text(size = 10))
# plot_grid(pstates, pnla, labels = c('a', 'b'), label_size = 10, nrow = 1, align = 'h')
ggsave(filename = 'LAGOS_prediction/Figure_predictions_holdout.png',width = 7, height = 3.5, units = 'in')





 
