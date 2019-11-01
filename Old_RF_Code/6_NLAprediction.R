# allLagos = 
if (!exists('nhd1')) {
    nhd1 = st_read('~/Dropbox/currentprojects/SaltBayes/LAGOS_GIS/LAGOS_NE_All_Lakes_1ha/',stringsAsFactors = F)
  }
modeledLakes = dat_rf$lagoslakeid

############# ############# ############# ############# ############# ############# 
############# Compare to Wisconsin field data ############# 
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

predictionsNLA = nla.overlap %>% left_join(allLagos.out) %>% 
  mutate(pred.Mean = exp(predictionAug2), pred.Median = exp(prediction.50)) %>%
  # mutate(cldiff = abs(newcl - cl)) %>% 
  filter(!lagoslakeid %in% dat$lagoslakeid)  %>%  # which haven't been used in the model 
  select(lagoslakeid:nhd_long,predictionAug:prediction.95,pred.Mean,pred.Median) %>% 
  group_by(lagoslakeid) %>% 
  dplyr::summarise_at(vars(cl,predictionAug:prediction.95,pred.Mean,pred.Median,nhd_lat,nhd_long), mean, na.rm=T) %>% 
  left_join(distinct(dplyr::select(nla.overlap,lagoslakeid,state,name))) %>% 
  filter(!lagoslakeid %in% dat_rf$lagoslakeid) # None of the lakes are duplicated


fitsO <- lm(predictionAug2 ~ log(cl), data = predictionsNLA); summary(fitsO) #r2 = 0.8
fitsO <- lm(prediction.50 ~ log(cl), data = predictionsNLA); summary(fitsO) #r2 = 0.81

fitsO = data.frame(r2 = paste0('r2 = ',round(summary(fitsO)$r.squared,2)),
                   cl = 0.1,
                   prediction.50 = 100)

# predictionsNLA = predictionsNLA %>% mutate(train = ifelse(state %in% c('MN','WI','NY','VT','MI'),TRUE, FALSE))
  
ggplot(predictionsNLA, aes(x = cl, y = pred.Median)) + 
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed') +
  geom_point(aes(fill = nhd_lat), shape = 21) + 
  ylab(bquote('Predicted Chloride'~(mg~L^-1))) + xlab(bquote('Observed Chloride'~(mg~L^-1))) +
  labs(title = 'Predicted NLA chloride') +
  scale_y_continuous(trans = log2_trans(), breaks = c(0,1,10,100)) + 
  scale_x_continuous(trans = log2_trans(), breaks = c(0,1,10,100)) +
  scale_fill_viridis_c(direction = -1) +
  # geom_text(data = fitsO, aes(label = r2),hjust = 1,vjust = 0, color = 'black') +
  theme_bw() 
ggsave(filename = 'LAGOS_prediction/Figure_predictions_NLA.png',width = 7, height = 5, units = 'in')
 

# Tichigan Lake
# http://www.sewrpc.org/SEWRPCFiles/Publications/CAPR/capr-283_vol-01_waterford_impoundment.pdf
# Observed: 127.153000	Predicted: 52.535753
# In the surface waters of Tichigan Lake, chloride averaged about 69 mg/l in the
# spring, 50 mg/l in summer, 42 mg/l in fall and winter (1973-2004)


