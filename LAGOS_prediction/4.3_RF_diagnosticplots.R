# For diagnostic plots, must fun RF script first to have the correct objects in working environment 

#### 1) PCA Plot of observations vs. Lagos ####
library(ggfortify)

getLagos = read_csv('LAGOS_prediction/data5_LAGOS_allLakes.csv') %>%
  mutate(month = 8) %>% 
  filter(state_zoneid != 'OUT_OF_COUNTY_STATE') %>%
  filter(!is.na(iws_nlcd2011_pct_22))  %>% 
  dplyr::select(lagoslakeid, lake_area_ha,iws_ha,
                iws_nlcd2011_pct_0:iws_nlcd2011_pct_95,iws_roaddensity_density_mperha,
                coastdist:TonsPerMile) %>% 
  mutate_at(vars(lake_area_ha:TonsPerMile),log01) %>%
  dplyr::select(-contains('_ha_'))

# Pull out observational data from lagos data
lagos.pca = getLagos %>% filter(!lagoslakeid %in% getDat$lagoslakeid) %>% 
  dplyr::select(-lagoslakeid) %>% mutate(group = 'lagos')

# Pull out observational data from lagos data
obs.pca = getLagos %>% filter(lagoslakeid %in% getDat$lagoslakeid) %>% 
  mutate(group = 'obs') %>% 
  # mutate(group = ifelse(lagoslakeid %in% useID,'highCIspread',group)) %>% 
  dplyr::select(-lagoslakeid) 
  

combo = lagos.pca %>% bind_rows(obs.pca) #%>% dplyr::select(-lake_area_ha,-iws_ha, -coastdist) 
names(combo) = c('lakeArea','wsArea','p0','p11','p21','p22','p23','p24','p31','p41','p42','p43','p52','p71','p81','p82','p90','p95',
                 'RD','coast','rDI','rDR','winter','SaltTons','group')
combo.pca = combo %>% dplyr::select(-group)

a = autoplot(prcomp(combo.pca, center = T, scale = T), data = combo, fill = 'group', alpha = 0.5, colour = 'group', loadings = TRUE,
             loadings.colour = 'black',loadings.label.colour = 'black',
             loadings.label = TRUE, loadings.label.size = 4)

a + scale_fill_manual(values = c("grey80","red4",'navy')) + 
  scale_color_manual(values = c("grey80","red4",'navy')) + 
  scale_alpha_manual(values = 0.5) +
  theme_bw()
ggsave('LAGOS_prediction/Figure_PCA.png',width = 7,height = 5)



#### 1) Correlation Matrix ####
names(dat) %in% names(rf_cov)

  png(file = "LAGOS_prediction/correlationPlot.png",width = 15,height = 15, units = 'in', res = 300)
    dat.cor = round(cor(dat[,names(dat) %in% names(rf_cov)],use = 'complete.obs'),2)
    # dat.cor = round(cor(dat[,c(8:12,14:30,67:71)],use = 'complete.obs'),2)
    corrplot(dat.cor, method = "ellipse")
  dev.off()
#Correlation Matrix on log values 
  png(file = "LAGOS_prediction/correlationPlot_log.png",width = 15,height = 15, units = 'in', res = 300)
    dat.cor = round(cor(dat[,names(dat) %in% names(rf_cov)],use = 'complete.obs'),2)
    corrplot(dat.cor, method = "ellipse")
  dev.off()


#### 2) Box and Whisker Plots of scaled range in predictor variables ####
getDat = read_csv("LAGOS_prediction/data3_LAGOS_ChlorideCovariates.csv") %>% dplyr::filter(Chloride < 10000 & Chloride >=0) %>%
  dplyr::mutate(Chloride = ifelse(Chloride == 0, 0.0001, Chloride)) %>%
  dplyr::filter(ActivityDepthHeightMeasure.MeasureValue < 10 | is.na(ActivityDepthHeightMeasure.MeasureValue)) %>%
  dplyr::filter(ActivityStartDate > as.Date('1990-01-01')) %>% 
  filter(!is.na(iws_nlcd2011_pct_22)) %>% 
  filter(!is.na(TonsPerMile)) 

getLagos = read_csv('LAGOS_prediction/data5_LAGOS_allLakes.csv') %>%
  mutate(month = 8) %>% 
  filter(state_zoneid != 'OUT_OF_COUNTY_STATE') %>%
  filter(!is.na(iws_nlcd2011_pct_22))  %>% 
  dplyr::mutate(group = 'lagos') %>% 
  dplyr::select(group, lagoslakeid, lake_area_ha,iws_ha,
                iws_nlcd2011_pct_0:iws_roaddensity_density_mperha,
                buffer500m_nlcd2011_pct_0:TonsPerMile) %>% 
  mutate_at(vars(-group,-lagoslakeid),scale)

# Pull out observational data from lagos data
getDat2 = getLagos %>% filter(lagoslakeid %in% getDat$lagoslakeid) %>% 
  mutate(group = 'obs')


# High PI Lagos lakes
highPI = allLagos.out %>% filter(PIrange >= 4)
getDat3 = getLagos %>% filter(lagoslakeid %in% highPI$lagoslakeid) %>% 
  mutate(group = 'highLagos')

# Combination of all group (observational, Lagos, highPI interval lagos lakes)
getCombo = getDat2 %>% 
  # bind_rows(getLagos) %>% 
  bind_rows(getDat3) %>% 
  dplyr::select(-lagoslakeid) %>% 
  dplyr::select(-contains('_ha_'))

# Diagnostic Plotting
long_all = gather(getCombo,key = attribute, value = value, - group) 

ggplot(long_all, aes(x = attribute, y = value, col = group)) + geom_boxplot(outlier.shape = NA) +
  theme(axis.text.x = element_text(angle = 90)) +
  ylim(-2,4) +
  coord_cartesian()

# 3) Month of Observations ####
head(getDat)
ggplot(getDat) + geom_bar(aes(x = month(ActivityStartDate)), fill = 'red4', alpha = 0.8) +
  scale_x_continuous(breaks = 1:12) +
  xlab('Month of Observation') + ylab('Count')
ggsave(filename = 'LAGOS_prediction/Figure_observationsMonth.png',width = 4,height = 3)

# 3) feature contributions for forestfloor ####
source("ranger_RFadaptor.R")

ff_rf_model<-ranger_RFadaptor(rf_model,dat_rf$Chloride)
ffra = forestFloor(ff_rf_model,rf_cov,calc_np = TRUE)

#color by most important feature
Col = fcol(ffra ,1)
plot(ffra, plot_seq=c(1,2,4,6,8),plot_GOF=F, limitY=F, col=Col,orderByImportance = T)


# 3) ### Plots of mean predictions ####
library(lme4)
fitsO <- lm(pred ~ meanCl, data=dat.out.mean) 
summary(fitsO)
fitsO = data.frame(r2 = paste0('r2 = ',round(summary(fitsO)$r.squared,2)),
                   meanCl = 7,
                   pred = 0.1)

fits2 <- lm(pred.50 ~ meanCl, data=dat.out.mean) 
fits2 = data.frame(r2 = paste0('r2 = ',round(summary(fits2)$r.squared,2)),
                   meanCl = 7,
                   pred.50 = 0.1)

p1 = ggplot(dat.out.mean, aes(x = meanCl, y = pred, color = log(count))) + geom_point(alpha = 0.6) +
  geom_point(aes(y = pred, color = log(count)),  alpha = 0.6) +
  geom_errorbarh(aes(xmin = min, xmax = max), alpha = 0.6) +
  xlab(bquote('Observed Chloride'~(mg~L^-1))) +
  ylab(bquote('Mean Predicted Chloride'~(mg~L^-1))) +
  xlim(-3,8) + ylim(-3,8) +
  scale_color_viridis_c() +
  geom_abline(linetype = 'dashed') +
  theme_bw() +
  geom_text(data = fitsO, aes(label = r2),hjust = 1,vjust = -1, color = 'black') 

p2 = ggplot(dat.out.mean, aes(x = meanCl, y = pred.50, color = log(count))) + geom_point(alpha = 0.6) +
  geom_point(aes(y = pred, color = log(count)),  alpha = 0.6) +
  geom_errorbarh(aes(xmin = min, xmax = max), alpha = 0.6) +
  xlab(bquote('Observed Chloride'~(mg~L^-1))) +
  ylab(bquote('Mean Predicted Chloride'~(mg~L^-1))) +
  xlim(-3,8) + ylim(-3,8) +
  scale_color_viridis_c() +
  geom_abline(linetype = 'dashed') +
  theme_bw() +
  geom_text(data = fits2, aes(label = r2),hjust = 1,vjust = -1, color = 'black') 

plot_grid(p1, p2, labels = c('A: mean', 'B: median'), label_size = 10, nrow = 2, align = 'hv')
ggsave('LAGOS_prediction/Figure_modelCorMean_Range.png',width = 7,height = 7)


ggplot(dat.out.mean) + geom_hline(yintercept = 0, linetype = 2) +
  geom_point(aes(x=count, y = residuals), color = 'red3', alpha = 0.6) +
  geom_point(aes(x=count, y = residuals.50), color = 'navy', alpha = 0.6) +
  ylab(bquote('Log Mean Residual Chloride'~(mg~L^-1))) +
  xlab('Number of Observations') + 
  theme_bw()
ggsave('LAGOS_prediction/Figure_ModelResiduals.png',width = 7,height = 5)


# Mean correlation
p1 = ggplot(dat.out.mean, aes(x = exp(meanCl), y = exp(pred))) + geom_point() + geom_abline(linetype = 'dashed') +
  ylab(bquote('Predicted Mean Chloride'~(mg~L^-1))) + xlab(bquote('Observed Mean Chloride'~(mg~L^-1))) +
  labs(title = paste0('Mean, Modeled chloride (n = ',nrow(dat_rf),')')) +
  scale_y_continuous(trans = log2_trans()) + scale_x_continuous(trans = log2_trans()) +
  geom_text(data = fitsO, aes(label = r2),hjust = 1,vjust = -1, color = 'black') +
  theme_bw() +
  theme(legend.justification = c(0, 1), legend.position = c(0.02, 0.97),legend.box.background = element_rect(colour = "black")) +
  scale_color_viridis_c(name = "% Forest")

p2 = ggplot(dat.out.mean, aes(x = exp(meanCl), y = exp(pred.50))) + geom_point() + geom_abline(linetype = 'dashed') +
  ylab(bquote('Predicted Mean-Median Chloride'~(mg~L^-1))) + xlab(bquote('Observed Mean Chloride'~(mg~L^-1))) +
  labs(title = paste0('Median, Modeled chloride (n = ',nrow(dat_rf),')')) +
  scale_y_continuous(trans = log2_trans()) + scale_x_continuous(trans = log2_trans()) +
  geom_text(data = fits2, aes(label = r2),hjust = 1,vjust = -1, color = 'black') +
  theme_bw() +
  theme(legend.justification = c(0, 1), legend.position = c(0.02, 0.97),legend.box.background = element_rect(colour = "black")) +
  scale_color_viridis_c(name = "% Forest")
plot_grid(p1, p2, labels = c('A', 'B'), label_size = 10, nrow = 2, align = 'hv')
ggsave(plot = p1,'LAGOS_prediction/Figure_modelCorMean.png',width = 6,height = 5)

# Mean correlation by lake type
fits1 <- lme4::lmList(pred.50 ~ meanCl | lakeconn, data=dat.out.mean) 
fits1 = data.frame(r2 = paste0('r2 = ',round(summary(fits1)$r.squared,2)), lakeconn = unique(fits1@groups), 
                   meanCl = 7,
                   pred.50 = 0.1)

ggplot(dat.out.mean, aes(x = exp(meanCl), y = exp(pred.50))) + geom_point() + geom_abline(linetype = 'dashed') +
  ylab(bquote('Predicted Median Chloride'~(mg~L^-1))) + xlab(bquote('Observed Chloride'~(mg~L^-1))) +
  facet_wrap(~lakeconn) +
  labs(title = paste0('Modeled chloride (n =',nrow(dat_rf),')')) +
  scale_y_continuous(trans = log2_trans()) + scale_x_continuous(trans = log2_trans()) +
  geom_text(data = fits1, aes(label = r2),hjust = 1,vjust = -1, color = 'black') +
  theme_bw() +
  theme(legend.justification = c(0, 1), legend.position = c(0.02, 0.97),legend.box.background = element_rect(colour = "black")) +
  scale_color_viridis_c(name = "% Forest")
ggsave(plot = p2,'LAGOS_prediction/Figure_modelCorMean_LakeType.png',width = 6,height = 5)

# 4) Bar chart: Number of Lakes within CI interval ####
PerWithin = dat.out.mean %>% 
  mutate(group = ifelse(count > 10, 11, count)) %>% 
  group_by(group) %>% 
  summarise(totalWithin = sum(withinPI), nLakes = n()) %>% 
  mutate(perWithin = totalWithin/nLakes) %>% 
  mutate(labels = ifelse(group == 11, '10+',group))

ggplot(PerWithin) + geom_bar(aes(x = group, y = perWithin), stat = 'identity') +
  scale_x_continuous(breaks = PerWithin$group, labels = PerWithin$labels) +
  scale_y_continuous(breaks = seq(0,1,by = 0.1)) +
  annotate(geom = "text", x = 1:11, y = 1.05, label = paste0('n = ',PerWithin$nLakes), size = 3) +
  xlab('Number of Observations per Lake') +
  ylab('Percent of lakes with all observations falling within 90% PI') +
  theme_bw()
ggsave('LAGOS_prediction/Figure_LakesWithinPI.png',width = 6,height = 5)


# 6) ## Mapping ####
library(tigris)
library(mapview)
library(viridisLite)
states <- states(cb = TRUE)
states_sf<- st_as_sf(states)

# LAGOS region residuals
ggplot(data=dat.out.mean) + 
  geom_sf(data=states_sf[states_sf$NAME %in% c('New York','Vermont','New Hampshire','Maine','Rhode Island',
                                               'Iowa','Missouri','Illinois','Ohio','Indiana','Pennsylvania','New Jersey',
                                               'Massachusetts','Connecticut','Wisconsin','Minnesota','Michigan'),], fill="white")+
  geom_point(aes(x=long, y=lat, col=abs(residuals.50), size = abs(residuals)), alpha=.5 )+
  scale_color_viridis_c(option="magma")+
  theme_bw()
ggsave(filename = 'LAGOS_prediction/Map_RF_modelResiduals.png',width = 7, height = 5)

# LAGOS region PI interval (no observation lakes)
ggplot(data = allLagos.out %>% filter(obsLakes == FALSE) %>% arrange(desc(PIrange))) + 
  geom_sf(data=states_sf[states_sf$NAME %in% c('New York','Vermont','New Hampshire','Maine','Rhode Island',
                                               'Iowa','Missouri','Illinois','Ohio','Indiana','Pennsylvania','New Jersey',
                                               'Massachusetts','Connecticut','Wisconsin','Minnesota','Michigan'),], fill="white")+
  geom_point(aes(x=nhd_long, y=nhd_lat, col=PIrange), alpha=.5, size = 0.8, shape = 16)+
  scale_color_viridis_c(option="magma")+
  theme_bw()
ggsave(filename = 'LAGOS_prediction/Map_RF_modelPIinterval.png',width = 7, height = 5)


# High lakes 
b = allLagos.out %>% 
  filter(predictionAug2 > log(50)) %>% 
  mutate(cols = 
           case_when(exp(predictionAug) < 100 ~ 1,
                     exp(predictionAug) >= 100 & exp(predictionAug) <260 ~ 2,
                     exp(predictionAug) > 260 ~ 3)) %>% 
  mutate(expCl = exp(predictionAug)) #%>% 
  # st_as_sf(coords = c('nhd_long','nhd_lat'),crs = 4326)

ggplot(data = b %>% filter(obsLakes == FALSE)) +
  geom_sf(data=states_sf[states_sf$NAME %in% c('New York','Vermont','New Hampshire','Maine','Rhode Island',
                                               'Iowa','Missouri','Illinois','Ohio','Indiana','Pennsylvania','New Jersey',
                                               'Massachusetts','Connecticut','Wisconsin','Minnesota','Michigan'),], fill="white")+
  geom_point(aes(x=nhd_long, y=nhd_lat, col=predictionAug2), alpha=.5, size = 0.8, shape = 16)+
  scale_color_viridis_c(option="magma")+
  theme_bw()
ggsave(filename = 'LAGOS_prediction/Map_HighLakes.png',width = 7, height = 5)


m = b %>% mapview(zcol = "expCl", layer.name = 'Predicted Chloride (mg/L)')
m
mapshot(m, url = paste0(getwd(), "/html/map.html"))

