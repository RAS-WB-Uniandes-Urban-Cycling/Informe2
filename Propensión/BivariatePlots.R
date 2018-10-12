library(gridExtra)
library(ggplot2)

##### Accidents
varY <- "Chance of Bicycle Commuting"
varX <- "Count of historical accidents involving bicycle users"
varXR <- "Count of historical accidents involving bicycle users per km of route distance"

plotO <- ggplot(Data,aes(x = NAccidentes.O,y = as.numeric(BicycleCommuting),color = sex, fill = sex)) + 
  stat_smooth(aes(linetype = "GAM"),method = "gam",method.args = list(family = "binomial"), formula = y~s(x), alpha = 0.2) +
  geom_rug(data = filter(Data, !BicycleCommuting),sides = "b",position = "jitter")+
  geom_rug(data = filter(Data, BicycleCommuting),sides = "t",position = "jitter")+
  scale_color_manual(name = "Sex", labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_fill_manual(name = "Sex",labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_linetype_manual(name = "Fit", values = c(1, 3))+
  guides(lty = guide_legend(override.aes = list(col = 'black')))+
  labs(x=varX,y=varY,title = "Origin buffer")+
  theme_light()+
  coord_cartesian(ylim=c(0,0.2))

plotD <- ggplot(Data,aes(x = NAccidentes.D,y = as.numeric(BicycleCommuting),color = sex, fill = sex)) + 
  stat_smooth(aes(linetype = "GAM"),method = "gam",method.args = list(family = "binomial"), formula = y~s(x), alpha = 0.2) +
  geom_rug(data = filter(Data, !BicycleCommuting),sides = "b",position = "jitter")+
  geom_rug(data = filter(Data, BicycleCommuting),sides = "t",position = "jitter")+
  scale_color_manual(name = "Sex", labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_fill_manual(name = "Sex",labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_linetype_manual(name = "Fit", values = c(1, 3))+
  guides(lty = guide_legend(override.aes = list(col = 'black')))+
  labs(x=varX,y=varY,title = "Destination buffer")+
  theme_light() + 
  coord_cartesian(ylim=c(0,0.2))

plotR <- ggplot(Data,aes(x = NAccidentes,y = as.numeric(BicycleCommuting),color = sex, fill = sex)) + 
  stat_smooth(aes(linetype = "GAM"),method = "gam",method.args = list(family = "binomial"), formula = y~s(x), alpha = 0.2) +
  geom_rug(data = filter(Data, !BicycleCommuting),sides = "b",position = "jitter")+
  geom_rug(data = filter(Data, BicycleCommuting),sides = "t",position = "jitter")+
  scale_color_manual(name = "Sex", labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_fill_manual(name = "Sex",labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_linetype_manual(name = "Fit", values = c(1, 3))+
  guides(lty = guide_legend(override.aes = list(col = 'black')))+
  labs(x=varXR,y=varY,title = "Route buffer")+
  theme_light() +
  coord_cartesian(ylim=c(0,0.2))

plotlist = list(plotO,plotD,plotR)

grid.arrange(
  grobs = plotlist,
  layout_matrix = rbind(c(1,1,2,2),
                        c(NA,3,3,NA)))


##### SiTP
varX <- "Count of bus stops"
varXR <- "Count of bus stops per km of route distance"

plotO <- ggplot(Data,aes(x = NSiTP.O,y = as.numeric(BicycleCommuting),color = sex, fill = sex)) + 
  stat_smooth(aes(linetype = "GAM"),method = "gam",method.args = list(family = "binomial"), formula = y~s(x), alpha = 0.2) +
  geom_rug(data = filter(Data, !BicycleCommuting),sides = "b",position = "jitter")+
  geom_rug(data = filter(Data, BicycleCommuting),sides = "t",position = "jitter")+
  scale_color_manual(name = "Sex", labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_fill_manual(name = "Sex",labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_linetype_manual(name = "Fit", values = c(1, 3))+
  guides(lty = guide_legend(override.aes = list(col = 'black')))+
  labs(x=varX,y=varY,title = "Origin buffer")+
  theme_light()+
  coord_cartesian(ylim=c(0,0.2))

plotD <- ggplot(Data,aes(x = NSiTP.D,y = as.numeric(BicycleCommuting),color = sex, fill = sex)) + 
  stat_smooth(aes(linetype = "GAM"),method = "gam",method.args = list(family = "binomial"), formula = y~s(x), alpha = 0.2) +
  geom_rug(data = filter(Data, !BicycleCommuting),sides = "b",position = "jitter")+
  geom_rug(data = filter(Data, BicycleCommuting),sides = "t",position = "jitter")+
  scale_color_manual(name = "Sex", labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_fill_manual(name = "Sex",labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_linetype_manual(name = "Fit", values = c(1, 3))+
  guides(lty = guide_legend(override.aes = list(col = 'black')))+
  labs(x=varX,y=varY,title = "Destination buffer")+
  theme_light()+
  coord_cartesian(ylim=c(0,0.2))

plotR <- ggplot(Data,aes(x = NSiTP,y = as.numeric(BicycleCommuting),color = sex, fill = sex)) + 
  stat_smooth(aes(linetype = "GAM"),method = "gam",method.args = list(family = "binomial"), formula = y~s(x), alpha = 0.2) +
  geom_rug(data = filter(Data, !BicycleCommuting),sides = "b",position = "jitter")+
  geom_rug(data = filter(Data, BicycleCommuting),sides = "t",position = "jitter")+
  scale_color_manual(name = "Sex", labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_fill_manual(name = "Sex",labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_linetype_manual(name = "Fit", values = c(1, 3))+
  guides(lty = guide_legend(override.aes = list(col = 'black')))+
  labs(x=varXR,y=varY,title = "Route buffer")+
  theme_light()+
  coord_cartesian(ylim=c(0,0.2))

plotlist = list(plotO,plotD,plotR)

grid.arrange(
  grobs = plotlist,
  layout_matrix = rbind(c(1,1,2,2),
                        c(NA,3,3,NA)))

##### TM
varX <- "Count of BRT Stations"
varXR <- "Count of BRT Stations per km of route distance"

plotO <- ggplot(Data,aes(x = NTM.O,y = as.numeric(BicycleCommuting),color = sex, fill = sex)) + 
  stat_smooth(aes(linetype = "GAM"),method = "gam",method.args = list(family = "binomial"), formula = y~s(x,k=6), alpha = 0.2) +
  geom_rug(data = filter(Data, !BicycleCommuting),sides = "b",position = "jitter")+
  geom_rug(data = filter(Data, BicycleCommuting),sides = "t",position = "jitter")+
  scale_color_manual(name = "Sex", labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_fill_manual(name = "Sex",labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_linetype_manual(name = "Fit", values = c(1, 3))+
  guides(lty = guide_legend(override.aes = list(col = 'black')))+
  labs(x=varX,y=varY,title = "Origin buffer")+
  theme_light()+
  coord_cartesian(ylim=c(0,0.1))

plotD <- ggplot(Data,aes(x = NTM.D,y = as.numeric(BicycleCommuting),color = sex, fill = sex)) + 
  stat_smooth(aes(linetype = "GAM"),method = "gam",method.args = list(family = "binomial"), formula = y~s(x,k=6), alpha = 0.2) +
  geom_rug(data = filter(Data, !BicycleCommuting),sides = "b",position = "jitter")+
  geom_rug(data = filter(Data, BicycleCommuting),sides = "t",position = "jitter")+
  scale_color_manual(name = "Sex", labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_fill_manual(name = "Sex",labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_linetype_manual(name = "Fit", values = c(1, 3))+
  guides(lty = guide_legend(override.aes = list(col = 'black')))+
  labs(x=varX,y=varY,title = "Destination buffer")+
  theme_light()+
  coord_cartesian(ylim=c(0,0.1))

plotR <- ggplot(Data,aes(x = NTM,y = as.numeric(BicycleCommuting),color = sex, fill = sex)) + 
  stat_smooth(aes(linetype = "GAM"),method = "gam",method.args = list(family = "binomial"), formula = y~s(x), alpha = 0.2) +
  geom_rug(data = filter(Data, !BicycleCommuting),sides = "b",position = "jitter")+
  geom_rug(data = filter(Data, BicycleCommuting),sides = "t",position = "jitter")+
  scale_color_manual(name = "Sex", labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_fill_manual(name = "Sex",labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_linetype_manual(name = "Fit", values = c(1, 3))+
  guides(lty = guide_legend(override.aes = list(col = 'black')))+
  labs(x=varXR,y=varY,title = "Route buffer")+
  theme_light()+
  coord_cartesian(ylim=c(0,0.1))

plotlist = list(plotO,plotD,plotR)

grid.arrange(
  grobs = plotlist,
  layout_matrix = rbind(c(1,1,2,2),
                        c(NA,3,3,NA)))

##### CPark
varX <- "Count of bicycle parkings"
varXR <- "Count of bicycle parkings per km of route distance"

plotO <- ggplot(Data,aes(x = NCPark.O,y = as.numeric(BicycleCommuting),color = sex, fill = sex)) + 
  stat_smooth(aes(linetype = "GAM"),method = "gam",method.args = list(family = "binomial"), formula = y~s(x), alpha = 0.2) +
  geom_rug(data = filter(Data, !BicycleCommuting),sides = "b",position = "jitter")+
  geom_rug(data = filter(Data, BicycleCommuting),sides = "t",position = "jitter")+
  scale_color_manual(name = "Sex", labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_fill_manual(name = "Sex",labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_linetype_manual(name = "Fit", values = c(1, 3))+
  guides(lty = guide_legend(override.aes = list(col = 'black')))+
  labs(x=varX,y=varY,title = "Origin buffer")+
  theme_light()+
  coord_cartesian(ylim=c(0,0.2))

plotD <- ggplot(Data,aes(x = NCPark.D,y = as.numeric(BicycleCommuting),color = sex, fill = sex)) + 
  stat_smooth(aes(linetype = "GAM"),method = "gam",method.args = list(family = "binomial"), formula = y~s(x), alpha = 0.2) +
  geom_rug(data = filter(Data, !BicycleCommuting),sides = "b",position = "jitter")+
  geom_rug(data = filter(Data, BicycleCommuting),sides = "t",position = "jitter")+
  scale_color_manual(name = "Sex", labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_fill_manual(name = "Sex",labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_linetype_manual(name = "Fit", values = c(1, 3))+
  guides(lty = guide_legend(override.aes = list(col = 'black')))+
  labs(x=varX,y=varY,title = "Destination buffer")+
  theme_light()+
  coord_cartesian(ylim=c(0,0.2))

plotR <- ggplot(Data,aes(x = NCPark,y = as.numeric(BicycleCommuting),color = sex, fill = sex)) + 
  stat_smooth(aes(linetype = "GAM"),method = "gam",method.args = list(family = "binomial"), formula = y~s(x), alpha = 0.2) +
  geom_rug(data = filter(Data, !BicycleCommuting),sides = "b",position = "jitter")+
  geom_rug(data = filter(Data, BicycleCommuting),sides = "t",position = "jitter")+
  scale_color_manual(name = "Sex", labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_fill_manual(name = "Sex",labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_linetype_manual(name = "Fit", values = c(1, 3))+
  guides(lty = guide_legend(override.aes = list(col = 'black')))+
  labs(x=varXR,y=varY,title = "Route buffer")+
  theme_light()+
  coord_cartesian(ylim=c(0,0.2))

plotlist = list(plotO,plotD,plotR)

grid.arrange(
  grobs = plotlist,
  layout_matrix = rbind(c(1,1,2,2),
                        c(NA,3,3,NA)))

##### CicloRuta
varX <- "Length Bicycle Lanes [km]"
varXR <- "Length Bicycle Lanes [km] per km of route distance"

plotO <- ggplot(Data,aes(x = CiclR.O,y = as.numeric(BicycleCommuting),color = sex, fill = sex)) + 
  stat_smooth(aes(linetype = "GAM"),method = "gam",method.args = list(family = "binomial"), formula = y~s(x), alpha = 0.2) +
  geom_rug(data = filter(Data, !BicycleCommuting),sides = "b",position = "jitter")+
  geom_rug(data = filter(Data, BicycleCommuting),sides = "t",position = "jitter")+
  scale_color_manual(name = "Sex", labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_fill_manual(name = "Sex",labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_linetype_manual(name = "Fit", values = c(1, 3))+
  guides(lty = guide_legend(override.aes = list(col = 'black')))+
  labs(x=varX,y=varY,title = "Origin buffer")+
  theme_light()+
  coord_cartesian(ylim=c(0,0.2))

plotD <- ggplot(Data,aes(x = CiclR.D,y = as.numeric(BicycleCommuting),color = sex, fill = sex)) + 
  stat_smooth(aes(linetype = "GAM"),method = "gam",method.args = list(family = "binomial"), formula = y~s(x), alpha = 0.2) +
  geom_rug(data = filter(Data, !BicycleCommuting),sides = "b",position = "jitter")+
  geom_rug(data = filter(Data, BicycleCommuting),sides = "t",position = "jitter")+
  scale_color_manual(name = "Sex", labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_fill_manual(name = "Sex",labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_linetype_manual(name = "Fit", values = c(1, 3))+
  guides(lty = guide_legend(override.aes = list(col = 'black')))+
  labs(x=varX,y=varY,title = "Destination buffer")+
  theme_light()+
  coord_cartesian(ylim=c(0,0.2))

plotR <- ggplot(Data,aes(x = CiclR,y = as.numeric(BicycleCommuting),color = sex, fill = sex)) + 
  stat_smooth(aes(linetype = "GAM"),method = "gam",method.args = list(family = "binomial"), formula = y~s(x), alpha = 0.2) +
  geom_rug(data = filter(Data, !BicycleCommuting),sides = "b",position = "jitter")+
  geom_rug(data = filter(Data, BicycleCommuting),sides = "t",position = "jitter")+
  scale_color_manual(name = "Sex", labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_fill_manual(name = "Sex",labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_linetype_manual(name = "Fit", values = c(1, 3))+
  guides(lty = guide_legend(override.aes = list(col = 'black')))+
  labs(x=varXR,y=varY,title = "Route buffer")+
  theme_light()+
  coord_cartesian(ylim=c(0,0.2))

plotlist = list(plotO,plotD,plotR)

grid.arrange(
  grobs = plotlist,
  layout_matrix = rbind(c(1,1,2,2),
                        c(NA,3,3,NA)))


##### LTS
varX <- "Proportion of LTS 1 roads"
varXR <- "Proportion of LTS 1 roads"

plotO <- ggplot(Data,aes(x = LTS1.O,y = as.numeric(BicycleCommuting),color = sex, fill = sex)) + 
  stat_smooth(aes(linetype = "GAM"),method = "gam",method.args = list(family = "binomial"), formula = y~s(x), alpha = 0.2) +
  geom_rug(data = filter(Data, !BicycleCommuting),sides = "b",position = "jitter")+
  geom_rug(data = filter(Data, BicycleCommuting),sides = "t",position = "jitter")+
  scale_color_manual(name = "Sex", labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_fill_manual(name = "Sex",labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_linetype_manual(name = "Fit", values = c(1, 3))+
  guides(lty = guide_legend(override.aes = list(col = 'black')))+
  labs(x=varX,y=varY,title = "Origin buffer")+
  theme_light()+
  coord_cartesian(ylim=c(0,0.2))

plotD <- ggplot(Data,aes(x = LTS1.D,y = as.numeric(BicycleCommuting),color = sex, fill = sex)) + 
  stat_smooth(aes(linetype = "GAM"),method = "gam",method.args = list(family = "binomial"), formula = y~s(x), alpha = 0.2) +
  geom_rug(data = filter(Data, !BicycleCommuting),sides = "b",position = "jitter")+
  geom_rug(data = filter(Data, BicycleCommuting),sides = "t",position = "jitter")+
  scale_color_manual(name = "Sex", labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_fill_manual(name = "Sex",labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_linetype_manual(name = "Fit", values = c(1, 3))+
  guides(lty = guide_legend(override.aes = list(col = 'black')))+
  labs(x=varX,y=varY,title = "Destination buffer")+
  theme_light()+
  coord_cartesian(ylim=c(0,0.2))

plotR <- ggplot(Data,aes(x = LTS1,y = as.numeric(BicycleCommuting),color = sex, fill = sex)) + 
  stat_smooth(aes(linetype = "GAM"),method = "gam",method.args = list(family = "binomial"), formula = y~s(x), alpha = 0.2) +
  geom_rug(data = filter(Data, !BicycleCommuting),sides = "b",position = "jitter")+
  geom_rug(data = filter(Data, BicycleCommuting),sides = "t",position = "jitter")+
  scale_color_manual(name = "Sex", labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_fill_manual(name = "Sex",labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_linetype_manual(name = "Fit", values = c(1, 3))+
  guides(lty = guide_legend(override.aes = list(col = 'black')))+
  labs(x=varXR,y=varY,title = "Route buffer")+
  theme_light()+
  coord_cartesian(ylim=c(0,0.2))

plotlist = list(plotO,plotD,plotR)

grid.arrange(
  grobs = plotlist,
  layout_matrix = rbind(c(1,1,2,2),
                        c(NA,3,3,NA)))

##### LTS 2
varX <- "Proportion of LTS 2 roads"
varXR <- "Proportion of LTS 2 roads"

plotO <- ggplot(Data,aes(x = `LTS 2.O`,y = as.numeric(BicycleCommuting),color = sex, fill = sex)) + 
  stat_smooth(aes(linetype = "GAM"),method = "gam",method.args = list(family = "binomial"), formula = y~s(x), alpha = 0.2) +
  geom_rug(data = filter(Data, !BicycleCommuting),sides = "b",position = "jitter")+
  geom_rug(data = filter(Data, BicycleCommuting),sides = "t",position = "jitter")+
  scale_color_manual(name = "Sex", labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_fill_manual(name = "Sex",labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_linetype_manual(name = "Fit", values = c(1, 3))+
  guides(lty = guide_legend(override.aes = list(col = 'black')))+
  labs(x=varX,y=varY,title = "Origin buffer")+
  theme_light()

plotD <- ggplot(Data,aes(x = `LTS 2.D`,y = as.numeric(BicycleCommuting),color = sex, fill = sex)) + 
  stat_smooth(aes(linetype = "GAM"),method = "gam",method.args = list(family = "binomial"), formula = y~s(x), alpha = 0.2) +
  geom_rug(data = filter(Data, !BicycleCommuting),sides = "b",position = "jitter")+
  geom_rug(data = filter(Data, BicycleCommuting),sides = "t",position = "jitter")+
  scale_color_manual(name = "Sex", labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_fill_manual(name = "Sex",labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_linetype_manual(name = "Fit", values = c(1, 3))+
  guides(lty = guide_legend(override.aes = list(col = 'black')))+
  labs(x=varX,y=varY,title = "Destination buffer")+
  theme_light()

plotR <- ggplot(Data,aes(x = `LTS 2`,y = as.numeric(BicycleCommuting),color = sex, fill = sex)) + 
  stat_smooth(aes(linetype = "GAM"),method = "gam",method.args = list(family = "binomial"), formula = y~s(x), alpha = 0.2) +
  geom_rug(data = filter(Data, !BicycleCommuting),sides = "b",position = "jitter")+
  geom_rug(data = filter(Data, BicycleCommuting),sides = "t",position = "jitter")+
  scale_color_manual(name = "Sex", labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_fill_manual(name = "Sex",labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_linetype_manual(name = "Fit", values = c(1, 3))+
  guides(lty = guide_legend(override.aes = list(col = 'black')))+
  labs(x=varXR,y=varY,title = "Route buffer")+
  theme_light()

plotlist = list(plotO,plotD,plotR)

grid.arrange(
  grobs = plotlist,
  layout_matrix = rbind(c(1,1,2,2),
                        c(NA,3,3,NA)))


##### LTS 3
varX <- "Proportion of LTS 3 roads"
varXR <- "Proportion of LTS 3 roads"

plotO <- ggplot(Data,aes(x = `LTS 3.O`,y = as.numeric(BicycleCommuting),color = sex, fill = sex)) + 
  stat_smooth(aes(linetype = "GAM"),method = "gam",method.args = list(family = "binomial"), formula = y~s(x), alpha = 0.2) +
  geom_rug(data = filter(Data, !BicycleCommuting),sides = "b",position = "jitter")+
  geom_rug(data = filter(Data, BicycleCommuting),sides = "t",position = "jitter")+
  scale_color_manual(name = "Sex", labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_fill_manual(name = "Sex",labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_linetype_manual(name = "Fit", values = c(1, 3))+
  guides(lty = guide_legend(override.aes = list(col = 'black')))+
  labs(x=varX,y=varY,title = "Origin buffer")+
  theme_light()

plotD <- ggplot(Data,aes(x = `LTS 3.D`,y = as.numeric(BicycleCommuting),color = sex, fill = sex)) + 
  stat_smooth(aes(linetype = "GAM"),method = "gam",method.args = list(family = "binomial"), formula = y~s(x), alpha = 0.2) +
  geom_rug(data = filter(Data, !BicycleCommuting),sides = "b",position = "jitter")+
  geom_rug(data = filter(Data, BicycleCommuting),sides = "t",position = "jitter")+
  scale_color_manual(name = "Sex", labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_fill_manual(name = "Sex",labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_linetype_manual(name = "Fit", values = c(1, 3))+
  guides(lty = guide_legend(override.aes = list(col = 'black')))+
  labs(x=varX,y=varY,title = "Destination buffer")+
  theme_light()

plotR <- ggplot(Data,aes(x = `LTS 3`,y = as.numeric(BicycleCommuting),color = sex, fill = sex)) + 
  stat_smooth(aes(linetype = "GAM"),method = "gam",method.args = list(family = "binomial"), formula = y~s(x), alpha = 0.2) +
  geom_rug(data = filter(Data, !BicycleCommuting),sides = "b",position = "jitter")+
  geom_rug(data = filter(Data, BicycleCommuting),sides = "t",position = "jitter")+
  scale_color_manual(name = "Sex", labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_fill_manual(name = "Sex",labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_linetype_manual(name = "Fit", values = c(1, 3))+
  guides(lty = guide_legend(override.aes = list(col = 'black')))+
  labs(x=varXR,y=varY,title = "Route buffer")+
  theme_light()

plotlist = list(plotO,plotD,plotR)

grid.arrange(
  grobs = plotlist,
  layout_matrix = rbind(c(1,1,2,2),
                        c(NA,3,3,NA)))

##### LTS 4
varX <- "Proportion of LTS 4 roads"
varXR <- "Proportion of LTS 4 roads"

plotO <- ggplot(Data,aes(x = LTS4.O,y = as.numeric(BicycleCommuting),color = sex, fill = sex)) + 
  stat_smooth(aes(linetype = "GAM"),method = "gam",method.args = list(family = "binomial"), formula = y~s(x), alpha = 0.2) +
  geom_rug(data = filter(Data, !BicycleCommuting),sides = "b",position = "jitter")+
  geom_rug(data = filter(Data, BicycleCommuting),sides = "t",position = "jitter")+
  scale_color_manual(name = "Sex", labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_fill_manual(name = "Sex",labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_linetype_manual(name = "Fit", values = c(1, 3))+
  guides(lty = guide_legend(override.aes = list(col = 'black')))+
  labs(x=varX,y=varY,title = "Origin buffer")+
  theme_light()+
  coord_cartesian(ylim=c(0,0.2))

plotD <- ggplot(Data,aes(x = LTS4.D,y = as.numeric(BicycleCommuting),color = sex, fill = sex)) + 
  stat_smooth(aes(linetype = "GAM"),method = "gam",method.args = list(family = "binomial"), formula = y~s(x), alpha = 0.2) +
  geom_rug(data = filter(Data, !BicycleCommuting),sides = "b",position = "jitter")+
  geom_rug(data = filter(Data, BicycleCommuting),sides = "t",position = "jitter")+
  scale_color_manual(name = "Sex", labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_fill_manual(name = "Sex",labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_linetype_manual(name = "Fit", values = c(1, 3))+
  guides(lty = guide_legend(override.aes = list(col = 'black')))+
  labs(x=varX,y=varY,title = "Destination buffer")+
  theme_light()+
  coord_cartesian(ylim=c(0,0.2))

plotR <- ggplot(Data,aes(x = LTS4,y = as.numeric(BicycleCommuting),color = sex, fill = sex)) + 
  stat_smooth(aes(linetype = "GAM"),method = "gam",method.args = list(family = "binomial"), formula = y~s(x), alpha = 0.2) +
  geom_rug(data = filter(Data, !BicycleCommuting),sides = "b",position = "jitter")+
  geom_rug(data = filter(Data, BicycleCommuting),sides = "t",position = "jitter")+
  scale_color_manual(name = "Sex", labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_fill_manual(name = "Sex",labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_linetype_manual(name = "Fit", values = c(1, 3))+
  guides(lty = guide_legend(override.aes = list(col = 'black')))+
  labs(x=varXR,y=varY,title = "Route buffer")+
  theme_light()+
  coord_cartesian(ylim=c(0,0.2))

plotlist = list(plotO,plotD,plotR)

grid.arrange(
  grobs = plotlist,
  layout_matrix = rbind(c(1,1,2,2),
                        c(NA,3,3,NA)))

##### Average slope
varX <- "Average slope (%)"
varXR <- "Average slope (%)"

plotO <- ggplot(Data,aes(x = tan(Avgslope.O*pi/180)*100,y = as.numeric(BicycleCommuting),color = sex, fill = sex)) + 
  stat_smooth(aes(linetype = "GAM"),method = "gam",method.args = list(family = "binomial"), formula = y~s(x), alpha = 0.2) +
  geom_rug(data = filter(Data, !BicycleCommuting),sides = "b",position = "jitter")+
  geom_rug(data = filter(Data, BicycleCommuting),sides = "t",position = "jitter")+
  scale_color_manual(name = "Sex", labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_fill_manual(name = "Sex",labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_linetype_manual(name = "Fit", values = c(1, 3))+
  guides(lty = guide_legend(override.aes = list(col = 'black')))+
  labs(x=varX,y=varY,title = "Origin buffer")+
  theme_light()+
  coord_cartesian(ylim=c(0,0.2))

plotD <- ggplot(Data,aes(x = tan(Avgslope.D*pi/180)*100,y = as.numeric(BicycleCommuting),color = sex, fill = sex)) + 
  stat_smooth(aes(linetype = "GAM"),method = "gam",method.args = list(family = "binomial"), formula = y~s(x), alpha = 0.2) +
  geom_rug(data = filter(Data, !BicycleCommuting),sides = "b",position = "jitter")+
  geom_rug(data = filter(Data, BicycleCommuting),sides = "t",position = "jitter")+
  scale_color_manual(name = "Sex", labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_fill_manual(name = "Sex",labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_linetype_manual(name = "Fit", values = c(1, 3))+
  guides(lty = guide_legend(override.aes = list(col = 'black')))+
  labs(x=varX,y=varY,title = "Destination buffer")+
  theme_light()+
  coord_cartesian(ylim=c(0,0.2))

plotR <- ggplot(Data,aes(x = tan(Avgslope*pi/180)*100,y = as.numeric(BicycleCommuting),color = sex, fill = sex)) + 
  stat_smooth(aes(linetype = "GAM"),method = "gam",method.args = list(family = "binomial"), formula = y~s(x), alpha = 0.2) +
  geom_rug(data = filter(Data, !BicycleCommuting),sides = "b",position = "jitter")+
  geom_rug(data = filter(Data, BicycleCommuting),sides = "t",position = "jitter")+
  scale_color_manual(name = "Sex", labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_fill_manual(name = "Sex",labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_linetype_manual(name = "Fit", values = c(1, 3))+
  guides(lty = guide_legend(override.aes = list(col = 'black')))+
  labs(x=varXR,y=varY,title = "Route buffer")+
  theme_light()+
  coord_cartesian(ylim=c(0,0.2))

plotlist = list(plotO,plotD,plotR)

grid.arrange(
  grobs = plotlist,
  layout_matrix = rbind(c(1,1,2,2),
                        c(NA,3,3,NA)))

##### Entropy Index
varX <- "Entropy Index"

plotO <- ggplot(Data,aes(x = entropyIndex.O,y = as.numeric(BicycleCommuting),color = sex, fill = sex)) + 
  stat_smooth(aes(linetype = "GAM"),method = "gam",method.args = list(family = "binomial"), formula = y~s(x), alpha = 0.2) +
  geom_rug(data = filter(Data, !BicycleCommuting),sides = "b",position = "jitter")+
  geom_rug(data = filter(Data, BicycleCommuting),sides = "t",position = "jitter")+
  scale_color_manual(name = "Sex", labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_fill_manual(name = "Sex",labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_linetype_manual(name = "Fit", values = c(1, 3))+
  guides(lty = guide_legend(override.aes = list(col = 'black')))+
  labs(x=varX,y=varY,title = "Origin buffer")+
  theme_light() +
  coord_cartesian(ylim=c(0,0.2))

plotD <- ggplot(Data,aes(x = entropyIndex.D,y = as.numeric(BicycleCommuting),color = sex, fill = sex)) + 
  stat_smooth(aes(linetype = "GAM"),method = "gam",method.args = list(family = "binomial"), formula = y~s(x), alpha = 0.2) +
  geom_rug(data = filter(Data, !BicycleCommuting),sides = "b",position = "jitter")+
  geom_rug(data = filter(Data, BicycleCommuting),sides = "t",position = "jitter")+
  scale_color_manual(name = "Sex", labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_fill_manual(name = "Sex",labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_linetype_manual(name = "Fit", values = c(1, 3))+
  guides(lty = guide_legend(override.aes = list(col = 'black')))+
  labs(x=varX,y=varY,title = "Destination buffer")+
  theme_light() +
  coord_cartesian(ylim=c(0,0.2))

plotlist = list(plotO,plotD)

grid.arrange(
  grobs = plotlist,
  layout_matrix = rbind(c(1,1,2,2)))

#### TOI
varX <- "Transport opportunity index"

plotR <- ggplot(Data,aes(x = toi,y = as.numeric(BicycleCommuting),color = sex, fill = sex)) + 
  stat_smooth(aes(linetype = "GAM"),method = "gam",method.args = list(family = "binomial"), formula = y~s(x), alpha = 0.2) +
  geom_rug(data = filter(Data, !BicycleCommuting),sides = "b",position = "jitter")+
  geom_rug(data = filter(Data, BicycleCommuting),sides = "t",position = "jitter")+
  scale_color_manual(name = "Sex", labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_fill_manual(name = "Sex",labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_linetype_manual(name = "Fit", values = c(1, 3))+
  guides(lty = guide_legend(override.aes = list(col = 'black')))+
  labs(x=varX,y=varY,title = "")+
  theme_light()+
  coord_cartesian(ylim=c(0,0.2))

plotR

#### distance
varX <- "Commuting distance [km]"

plotR <- ggplot(Data,aes(x = distance,y = as.numeric(BicycleCommuting),color = sex, fill = sex)) + 
  stat_smooth(aes(linetype = "GAM"),method = "gam",method.args = list(family = "binomial"), formula = y~s(x), alpha = 0.2) +
  geom_rug(data = filter(Data, !BicycleCommuting),sides = "b",position = "jitter")+
  geom_rug(data = filter(Data, BicycleCommuting),sides = "t",position = "jitter")+
  scale_color_manual(name = "Sex", labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_fill_manual(name = "Sex",labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_linetype_manual(name = "Fit", values = c(1, 3))+
  guides(lty = guide_legend(override.aes = list(col = 'black')))+
  labs(x=varX,y=varY,title = "Route buffer")+
  theme_light()+
  coord_cartesian(ylim=c(0,0.2))

plotRZ <- ggplot(Data,aes(x = distance,y = as.numeric(BicycleCommuting),color = sex, fill = sex)) + 
  stat_smooth(aes(linetype = "GAM"),method = "gam",method.args = list(family = "binomial"), formula = y~s(x), alpha = 0.2) +
  geom_rug(data = filter(Data, !BicycleCommuting),sides = "b",position = "jitter")+
  geom_rug(data = filter(Data, BicycleCommuting),sides = "t",position = "jitter")+
  scale_color_manual(name = "Sex", labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_fill_manual(name = "Sex",labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_linetype_manual(name = "Fit", values = c(1, 3))+
  guides(lty = guide_legend(override.aes = list(col = 'black')))+
  labs(x=varX,y=varY,title = "Route buffer zoom-in")+
  theme_light()+
  coord_cartesian(xlim=c(0,10),ylim=c(0,0.2))

plotlist = list(plotR,plotRZ)

grid.arrange(grobs = plotlist,layout_matrix = rbind(c(1,1,2,2)))


#### GAM ####
library(mgcv)
library(voxel)
## Origen
fit.O <- gam(I(BicycleCommuting) ~ sex + s(age) + license + economic_activity + socioeconomic_status + factor(motorized_vehicles) +
               s(NAccidentes.O) + s(NSiTP.O) + s(NTM.O) + s(NCPark.O) + s(CiclR.O) + 
               s(`LTS 1`) + s(`LTS 2`) + s(`LTS 3`) + s(`LTS 4`) + 
               s(Avgslope.O) + s(distance),data = Data)

fit.O <- gam(I(BicycleCommuting) ~ sex + s(age,k=15) + license + economic_activity + socioeconomic_status + factor(motorized_vehicles) +
               s(NAccidentes.O) + s(NSiTP.O) + s(NTM.O,k=6) + s(NCPark.O) + s(CiclR.O,k=30) + 
               + s(LTS1.O) + s(LTS2.O) + s(LTS3.O) + s(LTS4.O) +
               s(Avgslope.O,k=20) + s(distance),data = Data)

fit.O1 <- gam(I(BicycleCommuting) ~ sex + s(age,k=15) + license + economic_activity + socioeconomic_status + factor(motorized_vehicles) +
                s(NAccidentes.O) + s(NSiTP.O) + s(NTM.O,k=6) + s(NCPark.O) + s(CiclR.O,k=30) + 
                + s(LTS1.O) + s(LTS4.O,k=15) + s(Avgslope.O,k=20) + s(distance),data = Data)

gam.check(fit.O1)

fit.O1 <- gam(BicycleCommuting ~ sex + s(age,k=15) + license + economic_activity + socioeconomic_status + factor(motorized_vehicles) +
                s(NAccidentes.O) + s(NSiTP.O) + s(NTM.O,k=6) + s(NCPark.O) + s(CiclR.O,k=30) + 
                + s(LTS1.O) + s(LTS4.O,k=15) + s(Avgslope.O,k=20) + s(distance),data = Data,family=binomial)


fit.O2 <- gam(BicycleCommuting ~ sex + s(age,k=15) + license + economic_activity + socioeconomic_status + factor(motorized_vehicles) +
                s(NAccidentes.O) + s(NSiTP.O) + s(NTM.O,k=6) + s(NCPark.O) + s(CiclR.O,k=30) + 
                + s(LTS1.O) + s(LTS4.O,k=15) + s(Avgslope.O,k=20) + s(distance),data = Data,family=binomial,method = "REML")


fit.O <- gam(I(BicycleCommuting) ~ sex + s(age) + license + economic_activity + socioeconomic_status + factor(motorized_vehicles) +
               s(NAccidentes.O),data = Data,family=binomial)
plot(fit.O,all.terms=T,residuals=T)

p <- plotGAM(fit.O1,smooth.cov = "age",rawOrFitted = "raw") 

ggplotGrob(p)


geom_rug(data = Data, aes_string(y = "Propensity", x = x ), alpha = 0.2)

vars <- c("BicycleCommuting","sex","")
purrr::map(vars, function(x){
  p <- plotGAM(fit.O1, smooth.cov = x) +
    geom_point(data = Data, aes_string(y = "Propensity", x = x ), alpha = 0.2) +
    geom_rug(data = Data, aes_string(y = "Propensity", x = x ), alpha = 0.2)
  g <- ggplotGrob(p)
})

library(mgcViz)
smfitO <- getViz(fit.O1)
plot(smfitO)

o <- plot( sm(smfitO, 1) )
o + l_fitLine(colour = "red") + l_rug(mapping = aes(x=x, y=y), alpha = 0.8) +
  l_ciLine(mul = 5, colour = "blue", linetype = 2) + 
  l_points(shape = 19, size = 1, alpha = 0.1) + theme_classic()


plot(fit.O1,shade=T,scale=F,trans = gtools::inv.logit)


fit.D1 <- gam(BicycleCommuting ~ sex + s(age,k=15) + license + economic_activity + socioeconomic_status + factor(motorized_vehicles) +
                s(NAccidentes.D) + s(NSiTP.D) + s(NTM.D,k=6) + s(NCPark.D) + s(CiclR.D,k=30) + 
                + s(LTS1.D) + s(LTS4.D,k=15) + s(Avgslope.D,k=20) + s(distance),data = Data,family=binomial)

fit.D2 <- gam(BicycleCommuting ~ sex + s(age,k=15) + license + economic_activity + socioeconomic_status + factor(motorized_vehicles) +
                + s(NTM.D,k=6) + 
                + s(LTS1.D) + s(LTS4.D,k=15) + s(Avgslope.D,k=20) + s(distance),data = Data,family=binomial)

Data2$trt=as.numeric(Data$sex)-1
Data2$y=as.numeric(Data$BicycleCommuting)

IV <- Information::create_infotables(data = Data2,y = "y",trt = "trt",bins = 10 )
View(IV$Summary)
