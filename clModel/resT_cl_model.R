allLagos = read_csv('LAGOS_prediction/data5_LAGOS_allLakes.csv') %>%
  filter(state_zoneid != 'OUT_OF_COUNTY_STATE') 
colNames = read_csv('LAGOS_prediction/ColNames.csv')
names(allLagos) = colNames$NewLagos

allLagos <- allLagos %>%
  mutate(Month = 8) %>% 
  mutate_at(vars(LakeArea,WS.Area,wlconnections_allwetlands_count:RoadDistance),log01) %>%
  filter(!is.na(WS.Dev.Low))

nrow(allLagos %>% filter(LakeArea >= log(10)))

lakeArea = median(exp(allLagos$LakeArea)) * 10000 #m2
volume = lakeArea * 5 #m3
WS.area = median(exp(allLagos$WS.Area)) * 10000 #m2 


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### Change residence time ####
roadDensity = median(exp(allLagos$WS.RoadDensity)) #metersPerHectare (median lagos)
roadDensity = roadDensity / 10000 #meters/m2
road.meters = roadDensity * WS.area  #m (15 tons/mile is WI/MN average, 9.3 tons/km)
roadSaltUse = (9.3 * 1000000 /1000) * road.meters #g /m * m

# HYDROLAKES: Mean and median hydraulic residence times for all lakes were
# computed to be 1834 days and 451 days, respectively
res = c(0.25,0.5,1,2,5)
clLake = data.frame(Year = seq(1,30,by = 0.25), 
                    RT_0.25 = 0, RT_0.5 = 0, RT_1 = 0, RT_2 = 0, RT_5 = 0) #conc = g/m3

for (r in 1:5) {
  resT = res[r]
  for (i in 2:nrow(clLake)) {
    lakeMass = clLake[i-1,r+1] * volume #grams
    lakeMassLost = lakeMass * 1/(resT/0.25)
    lakeMassNew = (roadSaltUse*0.25) + lakeMass - lakeMassLost
    clLake[i,r+1] = (lakeMassNew / volume)  #g/m3
  }
}
clLakeLong = gather(clLake, resT, cl, - Year)
clLakeLong$resT <- factor(clLakeLong$resT , levels = c('RT_0.25','RT_0.5','RT_1','RT_2', 'RT_5'))

p1 = ggplot(clLakeLong) + geom_line(aes(x = Year, y = cl, color = resT)) +
  scale_color_viridis_d() +
  ylab(bquote('Predicted Chloride'~(mg~L^-1))) +
  labs(title = 'Changing Residence Time') +
  # ylim(0,50) +
  theme_bw() +
  theme(legend.position="bottom",legend.title=element_blank())
p1

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### Change Road Density ####


paste(quantile(exp(allLagos$WS.RoadDensity),probs = seq(0.1,0.9,by = 0.2)),collapse = ', ')
roadDensity = as.numeric(quantile(exp(allLagos$WS.RoadDensity),probs = seq(0.1,0.9,by = 0.2))) #metersPerHectare (mean is 27)
roadDensity = roadDensity / 10000 #meters/m2
road.meters = roadDensity * WS.area  #m (15 tons/mile is WI/MN average, 9.3 tons/km)
roadSaltUse = (9.3 * 1000000 /1000) * road.meters #g /m * m

# HYDROLAKES: Mean and median hydraulic residence times for all lakes were
# computed to be 1834 days and 451 days, respectively
resT = 2
clLake.RD = data.frame(Year = seq(1, 30, by = 0.25), 
                    RD_0.001 = 0, RD_10.3 = 0, RD_18.3 = 0, RD_30.7 = 0, RD_61.2 = 0) #conc = g/m3

for (r in 1:5) {
  roadSaltUse.r = roadSaltUse[r]
  for (i in 2:nrow(clLake.RD)) {
    lakeMass = clLake.RD[i-1,r+1] * volume #grams
    lakeMassLost = lakeMass * 1/(resT/0.25)
    lakeMassNew = (roadSaltUse.r*0.25) + lakeMass - lakeMassLost
    clLake.RD[i,r+1] = (lakeMassNew / volume)  #g/m3
  }
}
clLake.RDLong = gather(clLake.RD, resT, cl, - Year)
clLake.RDLong$resT <- factor(clLake.RDLong$resT , levels = names(clLake.RD)[-1])

p3 = ggplot(clLake.RDLong) + geom_line(aes(x = Year, y = cl, color = resT)) +
  scale_color_viridis_d() +
  ylab(bquote('Predicted Chloride'~(mg~L^-1))) +
  labs(title = 'Changing Road Density') +
  guides(color=guide_legend(nrow=2)) +
  # ylim(0,45) +
  theme_bw() +
  theme(legend.position="bottom",legend.title=element_blank()) 
p3

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
############ Lake Mendota ############
# Currently 55 mg/L

mendota = allLagos %>% filter(gnis_name == 'Lake Mendota') %>% 
  slice(1)

lakeArea = exp(mendota$LakeArea) * 10000 #m2
volume = lakeArea * 12.5 #m3
WS.area = exp(mendota$WS.Area) * 10000 #m2 

roadDensity = median(exp(allLagos$WS.RoadDensity)) #metersPerHectare (median lagos)
roadDensity = roadDensity / 10000 #meters/m2
road.meters = roadDensity * WS.area  #m (15 tons/mile is WI/MN average, 9.3 tons/km)
roadSaltUse = (9.3 * 1000000 /1000) * road.meters #g /m * m


# HYDROLAKES: Mean and median hydraulic residence times for all lakes were
# computed to be 1834 days and 451 days, respectively
res = c(2,4,6)
clMendota = data.frame(Year = seq(1,30,by = 0.25), 
                    RT_2 = 0, RT_4 = 0, RT_6 = 0) #conc = g/m3

for (r in 1:3) {
  resT = res[r]
  for (i in 2:nrow(clMendota)) {
    lakeMass = clMendota[i-1,r+1] * volume #grams
    lakeMassLost = lakeMass * 1/(resT/0.25)
    lakeMassNew = (roadSaltUse*0.25) + lakeMass - lakeMassLost
    clMendota[i,r+1] = (lakeMassNew / volume)  #g/m3
  }
}
clMendotaLong = gather(clMendota, resT, cl, - Year)
clMendotaLong$resT <- factor(clMendotaLong$resT , levels = c('RT_2', 'RT_4', 'RT_6'))

ggplot(clMendotaLong) + 
  geom_hline(aes(yintercept = 55), linetype = 2, color = 'grey50') +
  geom_line(aes(x = Year, y = cl, color = resT)) +
  scale_color_viridis_d() +
  ylab(bquote('Predicted Chloride'~(mg~L^-1))) +
  labs(title = 'Lake Mendota, WI') +
  theme_bw() +
  theme(legend.position="bottom",legend.title=element_blank())
  

### Join plots ##########3
plot_grid(p1,p3, labels = c('a','b'), label_size = 10, ncol = 2, align = 'h')
ggsave(filename = 'clModel/resT_cl_model.png',width = 8,height = 3.5)


######## Vollenweider Modeling ###############

# P = L / z * p
# 
# P = ceoncentration of cl in lake water mg/m3
# L = annnual loading per unit of lake surface area mg/m2/yr
# z = mean depth of lake
# p = hydrualic flushing rate yr-1

roadDensity = median(exp(allLagos$WS.RoadDensity)) #metersPerHectare (median lagos)
roadDensity = roadDensity / 10000 #meters/m2
road.meters = roadDensity * WS.area  #m (15 tons/mile is WI/MN average, 9.3 tons/km)
roadSaltUse = (9.3 * 1000000 /1000) * road.meters #g /m * m

L = roadSaltUse * 1000 / lakeArea # mg/m2

RT = 5 # residence time
P = L / (5 * (1/RT)) #(mg/m3), mean depth = 5 m
P.mgL = P /1000
P.mgL




