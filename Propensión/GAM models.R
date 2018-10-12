library(tidyverse)
library(mgcv)
library(mctest)

# Pruebas de multicolinealidad
myData <- Data %>% select(age,NAccidentes.O,NAccidentes.D,NAccidentes,
                          NSiTP.O,NSiTP.D,NSiTP,
                          NTM.O,NTM.D,NTM,toi,
                          NCPark.O,NCPark.D,NCPark,
                          CiclR.O,CiclR.D,CiclR,
                          LTS1.O,LTS1.D,LTS1,
                          LTS2.O,LTS2.D,LTS2,
                          LTS3.O,LTS3.D,LTS3,
                          LTS4.O,LTS4.D,LTS4,
                          entropyIndex.O,entropyIndex.D,
                          Avgslope.O,Avgslope.D,Avgslope,
                          distance)

DataO <- Data %>% select(age,NAccidentes.O,
                         NSiTP.O,
                         NTM.O,toi,
                         NCPark.O,
                         CiclR.O,
                         LTS1.O,
                         LTS2.O,
                         LTS3.O,
                         LTS4.O,
                         entropyIndex.O,
                         Avgslope.O,
                         distance)

DataO <- Data %>% select(age,NAccidentes.O,
                         NSiTP.O,
                         NTM.O,toi,
                         NCPark.O,
                         CiclR.O,
                         LTS1.O,
                         LTS4.O,
                         entropyIndex.O,
                         Avgslope.O,
                         distance)


DataO <- Data %>% select(age,NAccidentes.O,
                         NTM.O,toi,
                         NCPark.O,
                         CiclR.O,
                         LTS1.O,
                         LTS4.O,
                         entropyIndex.O,
                         Avgslope.O,
                         distance)

mctest::omcdiag(x = DataO,y = as.numeric(Data$BicycleCommuting))
mctest::imcdiag(x = DataO,y = as.numeric(Data$BicycleCommuting))

### Destination

DataD <- Data %>% select(age,NAccidentes.D,
                         NSiTP.D,
                         NTM.D,toi,
                         NCPark.D,
                         CiclR.D,
                         LTS1.D,
                         LTS2.D,
                         LTS3.D,
                         LTS4.D,
                         entropyIndex.D,
                         Avgslope.D,
                         distance)

DataD <- Data %>% select(age,NAccidentes.D,
                         NTM.D,toi,
                         NCPark.D,
                         CiclR.D,
                         LTS1.D,
                         LTS4.D,
                         entropyIndex.D,
                         Avgslope.D,
                         distance)

mctest::omcdiag(x = DataD,y = as.numeric(Data$BicycleCommuting))
mctest::imcdiag(x = DataD,y = as.numeric(Data$BicycleCommuting))


### Route

DataR <- Data %>% select(age,NAccidentes,
                         NSiTP,
                         NTM,toi,
                         NCPark,
                         CiclR,
                         LTS1,
                         LTS2,
                         LTS3,
                         LTS4,
                         Avgslope,
                         distance)

DataR <- Data %>% select(age,NAccidentes,
                         NSiTP,
                         NTM,toi,
                         NCPark,
                         CiclR,
                         LTS1,
                         LTS4,
                         Avgslope,
                         distance)

DataR <- Data %>% select(age,
                         NTM,toi,
                         CiclR,
                         LTS1,
                         LTS4,
                         Avgslope,
                         distance)

mctest::omcdiag(x = DataR,y = as.numeric(Data$BicycleCommuting))
mctest::imcdiag(x = DataR,y = as.numeric(Data$BicycleCommuting))

#### Male GAMs
DataM <- Data %>% filter(sex == "Hombre")
#Origin
fit.MO <- gam(BicycleCommuting ~ s(age,k=15) + license + economic_activity + socioeconomic_status + factor(motorized_vehicles) +
                s(NAccidentes.O) + toi + NTM.O + NCPark.O + CiclR.O + + s(LTS1.O,k=15) + s(LTS4.O) +
                s(entropyIndex.O) + s(Avgslope.O,k=40) + s(distance),data = DataM,family=binomial,method = "REML")

gam.check(fit.MO)

sum.MO <- summary(fit.MO)
plot(fit.MO,shade=T,scale=F)

#Destination
fit.MD <- gam(BicycleCommuting ~ s(age,k=15) + license + economic_activity + socioeconomic_status + factor(motorized_vehicles) +
                s(NAccidentes.D) + toi + NTM.D + NCPark.D + CiclR.D + + s(LTS1.D,k=20) + s(LTS4.D,k=20) +
                s(entropyIndex.D) + s(Avgslope.D,k=40) + s(distance),data = DataM,family=binomial,method = "REML")

gam.check(fit.MD)

sum.MD <- summary(fit.MD)
plot(fit.MD,shade=T,scale=F)

#Route
fit.MR <- gam(BicycleCommuting ~ s(age,k=15) + license + economic_activity + socioeconomic_status + factor(motorized_vehicles) +
                s(NAccidentes) + toi + NTM + CiclR + s(LTS1,k=20) + s(LTS4) +
                s(Avgslope,k=40) + s(distance),data = DataM,family=binomial,method = "REML")

gam.check(fit.MR)

sum.MR <- summary(fit.MR)
plot(fit.MR,shade=T,scale=F)

#### Female GAMs
DataF <- Data %>% filter(sex == "Mujer")

#Origin
fit.FO <- gam(BicycleCommuting ~ s(age,k=15) + license + economic_activity + socioeconomic_status + factor(motorized_vehicles) +
                s(NAccidentes.O) + toi + NTM.O + NCPark.O + CiclR.O + s(LTS1.O,k=15) + s(LTS4.O) +
                s(entropyIndex.O)+ s(Avgslope.O,k=40) + s(distance),data = DataF,family=binomial,method = "REML")

# fit.FO <- gam(BicycleCommuting ~ s(age,k=15) + economic_activity + socioeconomic_status + factor(motorized_vehicles) +
#                 s(NAccidentes.O) + toi + NTM.O + NCPark.O + CiclR.O + s(LTS1.O,k=15) + s(LTS4.O) +
#                 s(entropyIndex.O)+ s(Avgslope.O,k=40) + s(distance),data = DataF,family=binomial,method = "REML")


gam.check(fit.FO)

sum.FO <- summary(fit.FO)
plot(fit.FO,shade=T,scale=F)

#Destination
fit.FD <- gam(BicycleCommuting ~ s(age,k=15) + license + economic_activity + socioeconomic_status + factor(motorized_vehicles) +
                s(NAccidentes.D) + toi + NTM.D + NCPark.D + CiclR.D + s(LTS1.D,k=20) + s(LTS4.D,k=20) +
                s(entropyIndex.D,k=20)+ s(Avgslope.D,k=45) + s(distance),data = DataF,family=binomial,method = "REML")

gam.check(fit.FD)

sum.FD <- summary(fit.FD)
plot(fit.FD,shade=T,scale=F)

#Route
fit.FR <- gam(BicycleCommuting ~ s(age,k=15) + license + economic_activity + socioeconomic_status + factor(motorized_vehicles) +
                s(NAccidentes) + toi + NTM + CiclR + s(LTS1,k=20) + s(LTS4) +
                s(Avgslope,k=40) + s(distance),data = DataF,family=binomial,method = "REML")

gam.check(fit.FR)

sum.FR <- summary(fit.FR)
plot(fit.FR,shade=T,scale=F)

plot_object <- plot_gam(fit.MD,pred = "age",title="Smooth curve for age")+
  labs(x="Age",y="Log odds")+
  theme_light()
or_object1 <- or_gam(data = DataM, model = fit.MO, 
                    pred = "age", values = c(15, 20))
or_object2 <- or_gam(data = DataM, model = fit.MO, 
                     pred = "age", values = c(30, 35))

plot <- insert_or(plot_object, or_object1, or_yloc = 1.5,
                  values_xloc = 3, arrow_length = 2, 
                  arrow_col = "red")
plot <- insert_or(plot, or_object2, or_yloc = 1.5,
                  values_xloc = 3, arrow_length = 2, 
                  arrow_col = "red")
plot


plot_object <- plot_gam(fit.MO,pred = "LTS1.O",title="Smooth curve for Proportion of LTS1 roads at origin")+
  labs(x="Proportion of LTS1 roads",y="Log odds")+
  theme_light()
or_object1 <- or_gam(data = DataM, model = fit.MO, 
                     pred = "LTS1.O", values = c(0.3, 0.4))
or_object2 <- or_gam(data = DataM, model = fit.MO, 
                     pred = "LTS1.O", values = c(0.6, 0.7))

plot <- insert_or(plot_object, or_object1, or_yloc = 1.5,
                  values_xloc = 0.03, arrow_length = 0.02, 
                  arrow_col = "red")
plot <- insert_or(plot, or_object2, or_yloc = 2,
                  values_xloc = 0.03, arrow_length = 0.02, 
                  arrow_col = "red")
plot


plot_object <- plot_gam(fit.FD,pred = "Avgslope.D",title="Smooth curve Average slope at destination")+
  labs(x="Average slope (%)",y="Log odds")+
  theme_light()
or_object <- or_gam(data = DataM, model = fit.MO, 
                    pred = "Avgslope.D", values = c(3, 10))

plot <- insert_or(plot_object, or_object, or_yloc = 3,
                  values_xloc = 2, arrow_length = 1, 
                  arrow_col = "red")
plot

names <- c("Intercept","Type of driver license - Motorcycle","Type of driver license - Other","Occupation - Employed", "Occupation - Other",
           "Socioeconomic status - Middle", "Socioeconomic Status - High","Presence of motorized vehicles","Transport Opportunity Index",
           "Number of BRT stations","Number of bicycle parkings","Length of bikeways")
namesR <- c("Intercept","Type of driver license - Motorcycle","Type of driver license - Other","Occupation - Employed", "Occupation - Other",
           "Socioeconomic status - Middle", "Socioeconomic Status - High","Presence of motorized vehicles","Transport Opportunity Index",
           "Number of BRT stations","Length of bikeways")
OR.MO <- tibble(names=names,OR=exp(sum.MO$p.coeff[1:12]),LL=exp(sum.MO$p.coeff[1:12]-1.96*sum.MO$se[1:12]),UL=exp(sum.MO$p.coeff[1:12]+1.96*sum.MO$se[1:12]))
OR.MD <- tibble(names=names,OR=exp(sum.MD$p.coeff[1:12]),LL=exp(sum.MD$p.coeff[1:12]-1.96*sum.MD$se[1:12]),UL=exp(sum.MD$p.coeff[1:12]+1.96*sum.MD$se[1:12]))
OR.MR <- tibble(names=namesR,OR=exp(sum.MR$p.coeff[1:11]),LL=exp(sum.MR$p.coeff[1:11]-1.96*sum.MR$se[1:11]),UL=exp(sum.MR$p.coeff[1:11]+1.96*sum.MR$se[1:11]))
OR.FO <- tibble(names=names,OR=exp(sum.FO$p.coeff[1:12]),LL=exp(sum.FO$p.coeff[1:12]-1.96*sum.FO$se[1:12]),UL=exp(sum.FO$p.coeff[1:12]+1.96*sum.FO$se[1:12]))
OR.FD <- tibble(names=names,OR=exp(sum.FD$p.coeff[1:12]),LL=exp(sum.FD$p.coeff[1:12]-1.96*sum.FD$se[1:12]),UL=exp(sum.FD$p.coeff[1:12]+1.96*sum.FD$se[1:12]))
OR.FR <- tibble(names=namesR,OR=exp(sum.FR$p.coeff[1:11]),LL=exp(sum.FR$p.coeff[1:11]-1.96*sum.FR$se[1:11]),UL=exp(sum.FR$p.coeff[1:11]+1.96*sum.FR$se[1:11]))

plot.MO <- ggplot(OR.MO,aes(x=factor(names,names),y=OR,ymin=LL,ymax=UL))+
  geom_pointrange()+
  geom_hline(yintercept = 1, linetype=2)+
  coord_flip(ylim = c(0,3))+
  labs(x="Variables",y="Odds Ratio", title="Origin")+
  theme_light()

plot.MD <- ggplot(OR.MD,aes(x=factor(names,names),y=OR,ymin=LL,ymax=UL))+
  geom_pointrange()+
  geom_hline(yintercept = 1, linetype=2)+
  labs(x="Variables",y="Odds Ratio", title="Destination")+
  coord_flip(ylim = c(0,3))+
  theme_light()

plot.MR <- ggplot(OR.MR,aes(x=factor(names,names),y=OR,ymin=LL,ymax=UL))+
  geom_pointrange()+
  geom_hline(yintercept = 1, linetype=2)+
  labs(x="Variables",y="Odds Ratio", title="Route")+
  coord_flip(ylim = c(0,3))+
  theme_light()

plotlist = list(plot.MO,plot.MD,plot.MR)

grid.arrange(
  grobs = plotlist,
  layout_matrix = rbind(c(1,1,2,2),
                        c(NA,3,3,NA)))


plot.FO <- ggplot(OR.FO,aes(x=factor(names,names),y=OR,ymin=LL,ymax=UL))+
  geom_pointrange()+
  geom_hline(yintercept = 1, linetype=2)+
  coord_flip(ylim = c(0,3))+
  labs(x="Variables",y="Odds Ratio", title="Origin")+
  theme_light()

plot.FD <- ggplot(OR.FD,aes(x=factor(names,names),y=OR,ymin=LL,ymax=UL))+
  geom_pointrange()+
  geom_hline(yintercept = 1, linetype=2)+
  labs(x="Variables",y="Odds Ratio", title="Destination")+
  coord_flip(ylim = c(0,3))+
  theme_light()

plot.FR <- ggplot(OR.FR,aes(x=factor(names,names),y=OR,ymin=LL,ymax=UL))+
  geom_pointrange()+
  geom_hline(yintercept = 1, linetype=2)+
  labs(x="Variables",y="Odds Ratio", title="Route")+
  coord_flip(ylim = c(0,3))+
  theme_light()

plotlist = list(plot.FO,plot.FD,plot.FR)

grid.arrange(
  grobs = plotlist,
  layout_matrix = rbind(c(1,1,2,2),
                        c(NA,3,3,NA)))
