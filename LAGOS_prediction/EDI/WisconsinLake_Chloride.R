# Load NHD dataset 
if (!exists('nhd1')) {
  nhd1 = st_read('~/Dropbox/currentprojects/SaltBayes/LAGOS_GIS/LAGOS_NE_All_Lakes_1ha/',stringsAsFactors = F)
}
traininglakes = read_csv('LAGOS_prediction/EDI/lakeCL_trainingData.csv')
modeledLakes = traininglakes$lagoslakeid

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
############# Wisconsin field data ############# 

wi.5 = read_csv('~/Dropbox/currentprojects/SaltBayes/WI_field_results/conductivity_cl_lakes_2018.csv') %>% 
  dplyr::select(lat = Latitude, long = Longitude, name = `Lake Name`, Date, chloride = `Cl (mg/L)`) 

wilakes = wi.5 %>%
  filter(!is.na(long), !is.na(chloride)) %>% 
  st_as_sf(coords = c('long','lat'),crs = 4326) %>%
  st_transform(crs = st_crs(nhd1)) 

wioverlap = wilakes %>% st_join(nhd1, left=FALSE) %>% 
  dplyr::select(lagoslakeid = lagoslakei, GNIS_Name,name:chloride)

# Output dataset 
output = wioverlap %>% st_transform(crs = 4326) %>% 
  left_join(select(allLagos.out, lagoslakeid:WS.Area,pred.50)) %>% 
  filter(!is.na(pred.50)) %>% 
  # Not used in model 
  filter(!lagoslakeid %in% dat$lagoslakeid) %>%  # which haven't been used in the model 
  rename(LakeName = name, Chloride_mgL = chloride) %>% 
  bind_cols(do.call(rbind, st_geometry(output)) %>% 
  as_tibble() %>% setNames(c("lon","lat")))

df.out = output %>% select(lagoslakeid,nhdid,GNIS_Name,LakeName,Date, Chloride_mgL, lon, lat) %>% 
  mutate(lat = round(lat, 4), lon = round(lon, 4))
st_geometry(df.out) = NULL
write_csv(df.out, 'LAGOS_prediction/EDI/WisconsinLakes_Chloride.csv')


ggplot(output, alpha = 0.7) + 
  geom_point(aes(x = Chloride_mgL, y = pred.50), color = 'navy') +
  geom_abline(intercept = 0, slope = 1) +
  ylab(bquote('Predicted Chloride'~(mg~L^-1))) + xlab(bquote('Observed Chloride'~(mg~L^-1))) +
  scale_y_continuous(trans = log2_trans()) + scale_x_continuous(trans = log2_trans())
