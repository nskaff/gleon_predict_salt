# For diagnostic plots, must fun RF script first to have the correct objects in working environment 
#load('4.2RF_Environment.RData')
library(RColorBrewer)

#### 1) Box and Whisker Plots of scaled range in predictor variables (sup figure) ####
combo = allLagos %>% mutate(group = ifelse(lagoslakeid %in% dat.out.mean$lagoslakeid,'Training Lakes','Lagos Lakes')) %>% 
  select_(.dots = c('group',names(rf_cov))) %>% 
  select(-Month) %>% 
  arrange(group) %>% 
  mutate(WinterSeverity = log(WinterSeverity))

# Diagnostic Plotting
long_all = gather(combo, key = attribute, value = value, - group) 

ggplot(long_all, aes(x = attribute, y = value, col = group)) + geom_boxplot(outlier.shape = NA) +
  scale_color_manual(values = c("grey60","darkslategray3"), name = 'Group') +
  theme_bw(base_size = 9) +
  theme(axis.text.x = element_text(angle = 90)) +
  # ylim(-6,7) +
  xlab("") + ylab("Log Value") 
ggsave('LAGOS_prediction/Figure_CompareObsLagos_logbw.png',width = 7,height = 4)


#### 2) PCA Plot of observations vs. Lagos (sup figure) ####
library(ggfortify)
# Pull out observational data from lagos data
combo = allLagos %>% mutate(group = ifelse(lagoslakeid %in% dat.out.mean$lagoslakeid,'Training Lakes','Lagos Lakes')) %>% 
  select_(.dots = c('group',names(rf_cov))) %>% 
  select(-Month) %>% 
  arrange(group)
lagos.pca = combo %>% select(-group)
a = autoplot(prcomp(lagos.pca, center = T, scale = T), data = combo, fill = 'group', alpha = 0.5, colour = 'group', loadings = TRUE,
             loadings.colour = 'black',loadings.label.colour = 'black',
             loadings.label = TRUE, loadings.label.size = 3)

a + scale_fill_manual(values = c("grey90","darkslategray3"), name = 'Group') + 
  scale_color_manual(values = c("grey90","darkslategray3"), name = 'Group') + 
  scale_alpha_manual(values = 0.5, name = 'Group') +
  theme_bw() 
ggsave('LAGOS_prediction/Figure_PCA.png',width = 7,height = 5)

a = autoplot(prcomp(lagos.pca, center = T, scale = T), data = combo, fill = 'group', alpha = 0.5, colour = 'group', loadings = TRUE,
             loadings.colour = 'black',loadings.label.colour = NA,
             loadings.label = TRUE, loadings.label.size = 3)

a + scale_fill_manual(values = c("grey90","darkslategray3"), name = 'Group') + 
  scale_color_manual(values = c("grey90","darkslategray3"), name = 'Group') + 
  scale_alpha_manual(values = 0.5, name = 'Group') +
  theme_bw() 
ggsave('LAGOS_prediction/Figure_PCA_nolabels.png',width = 7,height = 4)

  #### 3) Correlation Matrix (sup figure) ####
names(dat) %in% names(rf_cov)

dat.group = dat_rf %>% group_by(lagoslakeid) %>% 
  dplyr::summarise_if(is.numeric,list(mean)) %>%
  select(names(rf_cov)[-1])
  
png(file = "LAGOS_prediction/correlationPlot.png",width = 10,height = 10, units = 'in', res = 300)
    dat.cor = round(cor(dat.group,use = 'complete.obs'),2)
    # dat.cor = round(cor(dat[,c(8:12,14:30,67:71)],use = 'complete.obs'),2)
    corrplot(dat.cor, method = "ellipse",tl.col = 'black')
dev.off()


# 4) Month of Observations (sup figure) ####
ggplot(dat_rf) + geom_bar(aes(x = month(ActivityStartDate)), fill = 'darkslategray3', alpha = 0.8) +
  scale_x_continuous(breaks = 1:12) +
  xlab('Month of Observation') + ylab('Count')
table(dat_rf$Month)
nrow(dat_rf %>% filter(Month >=5 & Month <= 9))/nrow(dat_rf)

ggsave(filename = 'LAGOS_prediction/Figure_observationsMonth.png',width = 4,height = 3)

sum(!is.na(allLagos$MaxDepth))
sum(!is.na(dat_rf %>% group_by(lagoslakeid) %>% summarise(depth = mean(MaxDepth)) %>% 
             select(depth)))

# 5*) Feature contributions for forestfloor + variable importance (man figure) ####
#variable importance
v<-as.numeric(rf_model$variable.importance)
w<-as.character(names(rf_model$variable.importance))
DF<-data.frame(w=w,v=as.numeric(v)) %>% arrange(v)
DF$w <- factor(DF$w, levels = DF$w)

pvar = ggplot(DF, aes(x=w, y=v,fill=v))+
  geom_bar(stat="identity", position="dodge") + coord_flip() +
  scale_fill_gradient(low = 'lightsteelblue3',high = 'lightsteelblue4') +
  ylab("Variable Importance") + xlab("")+
  theme_bw(base_size = 9) +
  # theme(axis.text = element_text(size=8), axis.title=element_text(size=8)) +
  guides(fill=F)


# Amend Matrix 
source("ranger_RFadaptor.R")
source("ranger_plot.forestFloor.HD.R")
ff_rf_model <- ranger_RFadaptor(rf_model,dat_rf$logChloride)
ffra = forestFloor(ff_rf_model,rf_cov,calc_np = T)

ffra2 = ffra 
ffra2$X$WS.Dev.LowMed = log(exp(ffra2$X$WS.Dev.Low) + exp(ffra2$X$WS.Dev.Med))
ffra2$importance[23] = ffra2$importance[7] + ffra2$importance[8]
ffra2$FCmatrix = cbind(ffra2$FCmatrix, ffra2$FCmatrix[,"WS.Dev.Low"] + ffra2$FCmatrix[,"WS.Dev.Med"])
attr(ffra2$FCmatrix,'dimnames')[[2]][23] = 'WS.Dev.LowMed'
ffra2$imp_ind = order(ffra2$importance,decreasing = T)

# Scatter plot between crops and dev
sum_low_dev_crop <- ffra2$FCmatrix[,"WS.Dev.LowMed"]+ffra2$FCmatrix[,"WS.Crops"]
p67 = ggplot()+ geom_point( aes(x=exp(ffra2$X$WS.Dev.LowMed), y=exp(ffra2$X$WS.Crops), fill = sum_low_dev_crop), pch = 21, color = 'grey50') + 
  scale_fill_distiller(palette='RdYlBu', direction = 1, name = 'Feature \nContribution') +
  xlab('Watershed Low + Med Development %') + ylab('Watershed Crop %') +
  scale_y_continuous(trans = log2_trans(),labels = scales::number_format(accuracy = 0.01)) + 
  scale_x_continuous(trans = log2_trans(),labels = scales::number_format(accuracy = 0.01)) +
  theme_bw(base_size = 9) +
  theme(legend.text = element_text(size=6),legend.title = element_text(size=6))
   
legend <- get_legend(p67)
p68 <- p67 + theme(legend.position='none')
topside = plot_grid(pvar, p67, nrow = 1, align = 'h', labels = c('a','b'),label_size = 10, rel_widths = c(0.4,0.6))


#Color by most important feature
Col = fcol.HD(ffra2,1)
# plot(ffra, plot_seq=c(1,2,3), plot_GOF=F, limitY=F, col=Col, orderByImportance = T, pch = 16)
# pp = plot.forestFloor.HD(ffra,plot_seq=c(1,2,3,5,8,18),cols = Col)
pp = plot.forestFloor.HD(ffra2,plot_seq=c(1,4,5,6,7,19), cols = Col)
do.call(plot_grid, c(pp, list(nrow = 2, align = 'hv')))

bottomside = do.call(plot_grid, c(pp, list(labels = c('c','d','e','f','g','h'), label_size = 10, nrow = 2, align = 'hv')))
# bottomside = plot_grid(rightside,legend,nrow = 1, rel_widths = c(0.9,0.1))
plot_grid(topside, bottomside, label_size = 10, ncol = 1, align = 'v')
ggsave(filename = 'LAGOS_prediction/Figure_VariableImportance_FeaturePlots.png',width = 7,height = 6)

# 6*) Prediction intervals of model + LAGOS lakes (man figure) + histogram #####
p4 = ggplot(data = allLagos.out) + 
  geom_errorbar(aes(ymin=exp(prediction.05)[order(prediction.50)],
                    ymax=exp(prediction.95)[order(prediction.50)],
                    x=1:length(prediction.50)), alpha=.1, color = 'grey50', size = 0.3) +
  scale_y_continuous(trans = log2_trans()) +
  xlab('Lake Number') + ylab(bquote('Predicted Cl'~(mg~L^-1))) +
  theme_bw() +
  geom_errorbar(data = dupLakes, aes(ymin=exp(prediction.05),ymax=exp(prediction.95),
                    x = id), alpha=.8, color = 'black', size = 0.3) +
  geom_point(aes(y=exp(prediction.50)[order(prediction.50)], 
                 x=1:length(prediction.50)), 
             color="gold", alpha = 0.5, size = 0.7, shape = 16) +
  geom_point(data = dupLakes, aes(y=exp(prediction.50),x= id), 
             color="red3", alpha = 0.8, size = 0.3, shape = 16) 
# ggsave(plot = p4, filename = 'LAGOS_prediction/Figure_Predictions_LAGOS_PI.png',width = 7,height = 3.5)

# Plot prediction histogram ##
p5 = ggplot() + 
  geom_density(data = allLagos.out, aes(x = exp(prediction.05)), fill = "transparent", linetype = 2, alpha = 0.3, size = 0.3, color = 'grey50') +
  geom_density(data = allLagos.out, aes(x = exp(prediction.95)), fill = "transparent", linetype = 3, alpha = 0.3, size = 0.3, color = 'grey50') +
  geom_density(data = allLagos.out, aes(x = exp(prediction.50)), alpha = 0.3, fill = 'gold') +
  scale_x_continuous(trans='log10') +
  ylab('Density') + xlab(bquote('Predicted Chloride'~(mg~L^-1))) +
  # ggtitle("Predicted Chloride Concentrations in Lagos") +
  theme_bw() +
  geom_vline(xintercept = c(230,860),linetype = 2) +
  annotate(geom='text',label = 'Cl = 230, EPA Chronic chloride toxicity',x = 190, y = 0.7, angle = 90, size = 2.5) +
  annotate(geom='text',label = 'Cl = 860, EPA Acute chloride toxicity',x = 720, y = 0.7, angle = 90, size = 2.5)
ggsave(p5,filename = 'LAGOS_prediction/Figure_LAGOShistogram.png', width  = 7, height=3)

plot_grid(p4,p5, labels = c('a','b'), label_size = 10, ncol = 1, align = 'v',rel_heights = c(0.4,0.6))
ggsave('LAGOS_prediction/Figure_LAGOSpredictions.png',width = 7,height = 5)


# 7) ### Plots of mean predictions ####
library(lme4)
fits2 <- lm(pred.50 ~ log(medianCl), data = dat.out.mean) 
fits2 = data.frame(r2 = paste0('r2 = ',round(summary(fits2)$r.squared,2)),
                   medianCl = 7,
                   pred.50 = 6)

p2 = ggplot(dat.out.mean, aes(x = log(medianCl), y = pred.50, color = log(count))) + geom_point(alpha = 0.6) +
  geom_errorbarh(aes(xmin = log(min), xmax = log(max)), alpha = 0.6) +
  xlab(bquote('Observed Chloride'~(mg~L^-1))) +
  ylab(bquote('Mean Predicted Chloride'~(mg~L^-1))) +
  xlim(-3,8) + ylim(-3,8) +
  scale_color_viridis_c() +
  geom_abline(linetype = 'dashed') +
  theme_bw() +
  geom_text(data = fits2, aes(label = r2),hjust = 1,vjust = -1, color = 'black') 

# plot_grid(p1, p2, labels = c('A: mean', 'B: median'), label_size = 10, nrow = 2, align = 'hv')
ggsave(plot = p2, filename = 'LAGOS_prediction/Figure_modelCorMean_Range.png',width = 7,height = 3.5)


ggplot(dat.out.mean) + geom_hline(yintercept = 0, linetype = 2) +
  # geom_point(aes(x=count, y = residuals), color = 'red3', alpha = 0.6) +
  geom_point(aes(x=count, y = residuals.50), shape = 21, fill = 'lightskyblue4', alpha = 0.6) +
  ylab(bquote('Log Mean Residual Chloride'~(mg~L^-1))) +
  xlab('Number of Observations') + 
  theme_bw()
ggsave('LAGOS_prediction/Figure_ModelResiduals.png',width = 7,height = 3)


# 8*) Mean correlation between lakes (man fig) #### 
p1 = ggplot(dat.out, aes(x = Chloride, y = exp(pred.50))) +
  geom_point(alpha = 0.8, shape = 21, size = 0.8, fill = viridis_pal()(20)[20]) +
  # ylim(-3.1,8) + xlim(-3.1,8) +
  xlab(bquote('Observed Chloride'~(mg~L^-1))) + ylab(bquote('Predicted Chloride'~(mg~L^-1))) +
  geom_abline(linetype = 'dashed') +
  scale_y_continuous(trans = log2_trans(),limits = c(0.1,3000)) + scale_x_continuous(trans = log2_trans(),limits = c(0.1,3000)) +
  annotate("text",x = 2.5, y = 600, size = 3,
           label = paste0('r2 = ',
           round(cor(dat.out$pred.50, dat.out$logChloride, use = "complete.obs") ^ 2,2))) +
  labs(title = paste0('Per observation (n = ',nrow(dat.out),')')) +
  theme_bw() +
  theme(plot.title = element_text(size=12))

p2 = ggplot(dat.out.mean, aes(x = medianCl, y = exp(pred.50))) + 
  geom_errorbarh(aes(xmin = min, xmax = max), alpha = 0.6, color = 'grey70') +
  geom_point(alpha = 0.8, shape = 21, size = 0.8, fill = viridis_pal()(20)[10]) + 
  geom_abline(linetype = 'dashed') +
  ylab(bquote('Predicted Chloride'~(mg~L^-1))) + xlab(bquote('Observed Median Chloride'~(mg~L^-1))) +
  scale_y_continuous(trans = log2_trans(),limits = c(0.1,3000)) + scale_x_continuous(trans = log2_trans()) +
  annotate("text",x = 2.5, y = 600, size = 3,
           label = paste0('r2 = ',
           round(cor(dat.out.mean$pred.50, log(dat.out.mean$medianCl), use = "complete.obs") ^ 2,2))) +  
  labs(title = paste0('Per lake (n = ',nrow(dat.out.mean),')')) +
  theme_bw() +
  theme(plot.title = element_text(size=12))

plot_grid(p1, p2, labels = c('a', 'b'), label_size = 10, nrow = 1, align = 'h')
ggsave('LAGOS_prediction/Figure_modelCorMean.png',width = 7,height = 3.5)

# Mean correlation by lake type
fits1 <- lme4::lmList(pred.50 ~ log(medianCl) | lakeconn, data=dat.out.mean) 
fits1 = data.frame(r2 = paste0('r2 = ',round(summary(fits1)$r.squared,2)), lakeconn = unique(fits1@groups), 
                   medianCl = 1,
                   pred.50 = 2)

ggplot(data = dat.out.mean, aes(x = medianCl, y = exp(pred.50))) + geom_point() + geom_abline(linetype = 'dashed') +
  ylab(bquote('Predicted Median Chloride'~(mg~L^-1))) + xlab(bquote('Observed Chloride'~(mg~L^-1))) +
  facet_wrap(~lakeconn) +
  labs(title = paste0('Modeled chloride (n = ',nrow(dat.out.mean),')')) +
  scale_y_continuous(trans = log2_trans()) + scale_x_continuous(trans = log2_trans()) +
  geom_text(data = fits1, aes(label = r2),hjust = 1,vjust = -1, color = 'black') +
  theme_bw() +
  theme(legend.justification = c(0, 1), legend.position = c(0.02, 0.97),legend.box.background = element_rect(colour = "black")) +
  scale_color_viridis_c(name = "% Forest")
ggsave(plot = p2,'LAGOS_prediction/Figure_modelCorMean_LakeType.png',width = 6,height = 5)

# 9) Bar chart: Number of Lakes within CI interval (sup figure) ####
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

# 10)  Box and Whisker of ws values ############# 
boxplot.df = dat %>% select(WS.OpenWater:WS.EmergentWetlands) %>% 
  gather(parameter, value)
ggplot(boxplot.df) + geom_boxplot(aes(parameter,value)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# 11) ## Mapping Observational Data ####
#### Create Mean Chloride DF ####
obsmap = dat.out.mean %>%
  st_as_sf(coords = c('long','lat'),crs = 4326)
m = obsmap %>% mapview(zcol = "medianCl", layer.name = 'Predicted Chloride (mg/L)')
m

# 11) ## Mapping ####
library(tigris)
library(mapview)
library(viridisLite)
states <- states(cb = TRUE)
states_sf<- st_as_sf(states)

# LAGOS region residuals
mapResiduals = ggplot(data=dat.out.mean) + 
  geom_sf(data=states_sf[states_sf$NAME %in% c('New York','Vermont','New Hampshire','Maine','Rhode Island',
                                               'Iowa','Missouri','Illinois','Ohio','Indiana','Pennsylvania','New Jersey',
                                               'Massachusetts','Connecticut','Wisconsin','Minnesota','Michigan'),], fill="white")+
  geom_point(aes(x=long, y=lat, col=abs(residuals.50)), alpha=.6)+
  scale_color_viridis_c(option="magma",direction = -1,name = 'Residuals') +
  theme_bw() +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank())
ggsave(filename = 'LAGOS_prediction/Map_RF_modelResiduals.png',width = 7, height = 4)

# LAGOS region PI interval (no observation lakes)
mapPI = ggplot(data = allLagos.out %>% filter(obsLakes == FALSE) %>% arrange(desc(PIrange))) + 
  geom_sf(data=states_sf[states_sf$NAME %in% c('New York','Vermont','New Hampshire','Maine','Rhode Island',
                                               'Iowa','Missouri','Illinois','Ohio','Indiana','Pennsylvania','New Jersey',
                                               'Massachusetts','Connecticut','Wisconsin','Minnesota','Michigan'),], fill="white")+
  geom_point(aes(x=nhd_long, y=nhd_lat, col=PIrange), alpha=.6, size = 0.4, shape = 16)+
  scale_color_viridis_c(option="magma",direction = -1) +
  theme_bw() +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank())
ggsave(filename = 'LAGOS_prediction/Map_RF_modelPIinterval.png',width = 7, height = 4)

mapDiag = plot_grid(mapResiduals, mapPI, labels = c('a', 'b'), label_size = 10, ncol = 1, align = 'v')
ggsave(plot = mapDiag, filename = 'LAGOS_prediction/Map_diagnostics.png',width = 7, height = 6)



# 13) Outliers = Lake Calhoun, MN is a good example Bde Maka Ska ####

a = dat %>% filter(lagoslakeid ==  1696) #169 ha WS
p13 = ggplot(a) + geom_point(aes(x = ActivityStartDate, y = ResultMeasureValue,fill = month(ActivityStartDate, label = T)), pch = 21) +
  # geom_point(data = filter(a, ResultMeasureValue < 10),aes(x = ActivityStartDate, y = ResultMeasureValue), 
             # fill = 'gold', size = 3,pch = 23) +
  scale_fill_viridis_d(name = '') +
  xlab('Observation Date') + ylab(bquote('Chloride'~(mg~L^-1))) +
  labs(title = 'Bde Maka Ska, MN') +
  theme_bw() +
  theme(legend.position="none")


allLagos.out %>% filter(lagoslakeid == 173) %>% select(prediction.50)
diamondLake = dat %>% filter(lagoslakeid == 173) # 20 ha, 463 ha WS
mean(diamondLake$Chloride)
range(diamondLake$Chloride)
median(diamondLake$Chloride)

ggplot(diamondLake) + geom_point(aes(x = month(ActivityStartDate), y = Chloride))

p14 = ggplot(diamondLake) + geom_point(aes(x = ActivityStartDate, y = ResultMeasureValue, fill = month(ActivityStartDate, label = T)), pch = 21) +
  scale_fill_viridis_d(name = '') +
  xlab('Observation Date') + ylab(bquote('Chloride'~(mg~L^-1))) +
  xlim(as.Date('2003-01-01'),NA) +
  labs(title = 'Diamond Lake, MN') +
  theme_bw() +
  guides(fill=guide_legend(nrow = 5)) +
  theme(legend.box.background = element_rect(colour = "black"),
        legend.position=c(.75,.8),
        legend.direction="horizontal",
        legend.text = element_text(size=6),legend.title = element_text(size=6),
        legend.key.height =unit(3,"pt"), legend.key.width =unit(15,"pt"))
  


plot_grid(p14, p13, labels = c('a', 'b'), label_size = 10, nrow = 1, align = 'h')
ggsave(filename = 'LAGOS_prediction/Figure_Lake1696.png',width = 7, height = 2.5)
 

