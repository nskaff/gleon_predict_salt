
ggplot(allLagos.out) + geom_point(aes(x = (prediction.50), y = PIrange))

#### Investigation relationships with PI range ####
# Group data in high PI and low PI with a cutoff of 240
quantile(allLagos.out$PIrange, 0.9)

test = allLagos.out %>% mutate(group = ifelse(PIrange > 240.7, 'High PI','Low PI')) %>% 
  select(lagoslakeid, PIrange, group) %>% 
  left_join(select_(.data = allLagos,.dots = c('lagoslakeid',names(rf_cov)))) %>% 
  mutate(group2 = ifelse(lagoslakeid %in% dat.out.mean$lagoslakeid,'Training Lakes','Lagos Lakes')) %>% 
  select(-Month, - lagoslakeid) %>% 
  arrange(desc(group))

table(test$group) / nrow(test) # top 10% of lakes 
rename(count(test, group, group2), Freq = n) #breakdown of groups



test.long = test %>% select(-PIrange, -group2) %>% 
  mutate(WinterSeverity = log(WinterSeverity)) %>% 
  gather(key = attribute, value = value, -group) 

# Box plot comparing attributes 
ggplot(test.long, aes(x = attribute, y = value, col = group)) + geom_boxplot(outlier.shape = NA) +
  scale_color_manual(values = c("grey60","darkslategray3"), name = 'Group') +
  theme_bw(base_size = 9) +
  theme(axis.text.x = element_text(angle = 90)) +
  ylim(NA,10) +
  xlab("") + ylab("Log Value") 

# Test pca
test.pca = test %>% select(-PIrange, -group, - group2)
library(ggfortify)
a = autoplot(prcomp(test.pca, center = T, scale = T), data = test, fill = 'group', size = 'group2',
             alpha = 0.5, colour = 'group', loadings = TRUE,
             loadings.colour = 'black',loadings.label.colour = 'black',
             loadings.label = TRUE, loadings.label.size = 3) 

a + scale_fill_manual(values = c("red3","darkslategray3"), name = 'Group') + 
  scale_color_manual(values = c("red3","darkslategray3"), name = 'Group') +
  # scale_color_manual(values = c("grey90","black"), name = 'Group2') + 
  scale_alpha_manual(values = 0.5, name = 'Group') +
  scale_size_manual(values = c(0.5,4), name = 'Group2') +
  theme_bw() 

# Compare some predictors
ggplot(test) + geom_boxplot(aes(y = (WS.Area), fill = group))
ggplot(test) + geom_boxplot(aes(y = (WS.Dev.Med), fill = group))
ggplot(test) + geom_boxplot(aes(y = (WS.Dev.Med + WS.Dev.Low), fill = group))

ggplot(test) + geom_point(aes(x = WS.Area, y = PIrange))

ggplot(test) + geom_point(aes(x = WS.Area, y = PIrange, color = WS.Dev.Low))

summary(exp(filter(test, group == 'High PI')$WS.Area))
summary(exp(test$WS.Area))

summary(exp(filter(test, group == 'High PI')$WS.Dev.Low))
summary(exp(filter(test, group == 'High PI')$WS.Dev.Med))



