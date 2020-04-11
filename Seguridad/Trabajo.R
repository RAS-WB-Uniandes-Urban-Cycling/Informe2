#Preparación del entorno de trabajo----
rm(list=ls())
carpetaRAS="/Users/germancarvajal/OneDrive - Universidad de Los Andes/Uniandes - RAS - SDM"
gitRAS="/Users/germancarvajal/OneDrive - Universidad de Los Andes/proBikePolicies"
require(pacman)
pacman::p_load(gdata)
pacman::p_load(tidyverse)
pacman::p_load(lubridate)
pacman::p_load(sf)
pacman::p_load(dummies)
pacman::p_load(reshape2)
pacman::p_load(tmap)
pacman::p_load(ape)
pacman::p_load(lme4)
pacman::p_load(mgcv)
pacman::p_load(caret)
pacman::p_load(pROC)
pacman::p_load(MASS)
pacman::p_load(gamm4)
pacman::p_load(spatstat)
pacman::p_load(car)
pacman::p_load(oddsratio)
pacman::p_load(optimx)
pacman::p_load(ggpubr)
font_add("Helvetica Light",paste(gitRAS,"/Seguridad/Helvetica Light.ttf",sep=""))

#Leer bases de datos preprocesada----
load(paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/13. Level1.Rdata"))
load(paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/16. Level2.Rdata"))

#Transormación de variables a tipos adecuados
Level1<-AccidentesBiciTotal
rm(AccidentesBiciTotal)
names(Level1)[7:33]<-paste0(names(Level1),".L1")[7:33]

Level2<-Level2 %>%
  mutate(Year=as.factor(Year),UPlCodigo=as.factor(UPlCodigo)) %>% 
  mutate(Nonfatal=NULL,Fatal=NULL)
names(Level2)[3:11]<-paste0(names(Level2),".L2")[3:11]

Level3<-Level3 %>% 
  mutate(UPlCodigo=as.factor(UPlCodigo))
names(Level3)[2:22]<-paste0(names(Level3),".L3")[2:22]

#Integración en base de datos única
total<-left_join(Level1,Level2,by=c("UPlCodigo","Year"))
total$Year<-as.numeric(levels(total$Year)[as.numeric(total$Year)])
total<-left_join(total,Level3,by="UPlCodigo")

#Inclusión de coordenadas espaciales como variables de regresión
total<-data.frame(total,
                  as_data_frame(st_coordinates(Level1)) %>% transmute(Lon=X,Lat=Y)
                  ) %>% 
  st_set_geometry(.,st_geometry(Level1))

rm(Level1,Level2,Level3)

# Transformación de valores a miles de dólares (COP=3000)
total$Valor.D.L2<-(total$Valor.D.L2/3000)/1000
total$Valor.L3<-(total$Valor.L3/3000)/1000

# Transormación de kilómetros a millones de kilómetros al día
total$TraveledKm.D.L2<-(total$TraveledKm.D.L2/1e6)
total$TraveledKm.L3<-(total$TraveledKm.L3/1e6)

# Transformación del flujo a miles de vehiculos por hora
total$Flujo.L1<-total$Flujo.L1/1000

# Transformación de biciusuarios a miles
total$BICIUSURS.D.L2<-(total$BICIUSURS.D.L2/1000)
total$BICIUSURS.L3<-(total$BICIUSURS.L3/1000)

# Transformación de cicorutas a kilómetros
total$CicloRuta.D.L2<-total$CicloRuta.D.L2/1000
total$CicloRuta.L3<-total$CicloRuta.L3/1000

# Transformación del área de la UPZ a Km2
total$Area.L3<-total$Area.L3/1e6

#Estimación de modelos

#Modelo base sin efectos aleatorios
base<-glm(Gravedad2~1,
             data=total,
             family=binomial(link=logit))

#Modelo de sólo intercepto con efectos aleatorios por UPZ cómo base
modelbase<-gamm4(Gravedad2~1,
                data=total,
                family=binomial(link=logit),
                random=~(1|UPlCodigo),
                control = glmerControl(optimizer="optimx",
                                       check.conv.singular = .makeCC(action = "ignore",  tol = 1e-20), 
                                       optCtrl = list(method="nlminb",starttests=FALSE,kkt=FALSE)))

summary(modelbase$mer)

plot(fitted(modelbase$mer), modelbase$mer@resp$y)

#Test for the existance of random effects betwwen UPZ
2*(logLik(modelbase$mer)-logLik(base))
1-pchisq(q=as.numeric(2*(logLik(modelbase$mer)-logLik(base))),df=1)

#Modelo con efectos aleatorios por UPZ y por año dentro de UPZ
modelbase2<-gamm4(Gravedad2~1,
                 data=total,
                 family=binomial(link=logit),
                 random=~(1|UPlCodigo/Year),
                 control = glmerControl(optimizer="optimx",
                                        check.conv.singular = .makeCC(action = "ignore",  tol = 1e-20), 
                                        optCtrl = list(method="nlminb",starttests=FALSE,kkt=FALSE)))

summary(modelbase2$mer)

plot(fitted(modelbase2$mer), modelbase2$mer@resp$y)

#Test for the existance of random effects within UPZ by year
2*(logLik(modelbase2$mer)-logLik(modelbase$mer))
1-pchisq(q=as.numeric(2*(logLik(modelbase2$mer)-logLik(modelbase$mer))),df=1)

#Caterpillar plot over null model
u0 <- ranef(modelbase$mer, postVar = TRUE)
u0se <- sqrt(attr(u0[[1]], "postVar")[1, , ])
commid <- as.numeric(rownames(u0[[1]]))
u0tab <- cbind("commid" = commid, "u0" = u0[[1]], "u0se" = u0se)
colnames(u0tab)[2] <- "u0"
u0tab <- u0tab[order(u0tab$u0), ]
u0tab <- cbind(u0tab, c(1:dim(u0tab)[1]))
u0tab <- u0tab[order(u0tab$commid), ]
colnames(u0tab)[4] <- "u0rank"
plot(u0tab$u0rank, u0tab$u0, type = "n", xlab = "u_rank", ylab = "conditional modes of r.e. for comm_id:_cons", ylim = c(-.5, .5))
segments(u0tab$u0rank, u0tab$u0 - 1.96*u0tab$u0se, u0tab$u0rank, u0tab$u0 + 1.96*u0tab$u0se)
points(u0tab$u0rank, u0tab$u0, col = "blue")
abline(h = 0, col = "red")

#Modelo con efectos aleatorios por UPZ y por año dentro de UPZ, con todas las variables
model1<-gamm4(Gravedad2~1+Sexo.L1+Edad.L1+s(Hora.L1, k=30)+
                    BRT.L1+Bus.L1+Car.L1+Cargo.L1+Motorcycle.L1+
                    s(Slope.L1)+Ancho.L1+Carriles.L1+SITP.L1+
                    PrcntOficinas.D.L2+PrcntOficinas.L3+Valor.L3+
                    CicloRuta.D.L2+CicloRuta.L3+LIGTH.L3+INFORMATIVA.L3+PREVENTIVA.L3+TotalSeñales.L3+
                    SEMAFOROS.L3+HUECOS.L3+TraveledKm.D.L2+TraveledKm.L3+s(Lat,k=20)+s(Lon,k=20),
                  data=total,
                  family=binomial(link=logit),
                  random=~(1|UPlCodigo/Year),
                  verbose = TRUE,
                  start = list('theta'=0, coef(modelPQL$lme)),
                  control = glmerControl(optimizer="nloptwrap",
                                         check.conv.singular = .makeCC(action = "ignore",  tol = 1e-20), 
                                         optCtrl = list(method="nlminb",starttests=FALSE,kkt=FALSE)))

summary(model1$mer)

plot(fitted(modelbase2$mer), modelbase2$mer@resp$y)

coef(modelbase2$mer)

modelPQL<-gamm(Gravedad2~1+Sexo.L1+Edad.L1+
                 s(Hora.L1, k=30)+Slope.L1+
                 BRT.L1+Bus.L1+Car.L1+Cargo.L1+Motorcycle.L1+
                 SITP.L1+Ancho.L1+Carriles.L1+
                 TotalSeñales.L3+SEMAFOROS.L3+
                 LIGTH.L3+HUECOS.L3+
                 PrcntOficinas.D.L2+PrcntOficinas.L3+
                 Valor.D.L2+Valor.L3+
                 CicloRuta.D.L2+CicloRuta.L3+
                 TraveledKm.D.L2+TraveledKm.L3+
                 s(Lat,k=20)+s(Lon,k=20),
            data=total,
            family=binomial(link=logit),
            random=list(UPlCodigo=~1,Year=~1),
            control = lmeControl(sing.tol=1e-20),
            method = "REML")

summary(modelPQL$lme)
summary(modelPQL$gam)

gam.check(model$gam, type="response")

plot.gam(modelPQL$gam,las=1,ylab="Log-Odds of fatal accident", se=TRUE,shade=TRUE)


plot.gam(modelPQL$gam,
         las=1,
         main="Odds of a fatal bicyclists' collision", xlab = "Longitute", ylab = "Latitude",
         scheme=0,
         se=F, 
         shade=F,
         trans = function(x) exp(x))

df<-gam_to_df(modelPQL$gam, pred="Hora.L1") %>% 
  mutate(se_upr=exp(se_upr), se_lwr=exp(se_lwr), y=exp(y))

ggplot(df, aes_(~x, ~y)) + 
  geom_line(colour = "blue4", size = 1.1) + 
  geom_line(aes_(~x, ~se_upr), linetype = "dashed", colour = "black", size = 0.8, alpha=0.4) + 
  geom_line(aes_(~x, ~se_lwr), linetype = "dashed", colour = "black", size = 0.8, alpha=0.4) + 
  geom_ribbon(aes_(x = ~x, ymin = ~se_lwr, ymax = ~se_upr), fill = "grey", alpha = 0.4) + 
  ylab("Odds of a fatal bicyclists accident") + xlab("Time of occurence (hour)") + 
  cowplot::background_grid(major = "xy", minor = "none") +
  scale_x_continuous(breaks=seq(0,24,4),minor_breaks = seq(2,22,2))+
  theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+
  theme(legend.position = c(0.9, 0.125),legend.text=element_text(colour="black",size=10,family="Helvetica Light"),
        legend.title=element_text(colour="black",family="Helvetica Light"))
  
exp(cbind(coef(modelPQL$gam), confint(modelPQL$gam)))

predicted<-fitted(modelPQL$gam,type=response)
pROC::roc(modelPQL$lme$data$Gravedad2,predicted)
plot.roc(modelPQL$lme$data$Gravedad2,predicted)

confusionMatrix(reference = as.factor(modelPQL$lme$data$Gravedad2), as.factor(as.numeric(predicted>=0.04)), positive = "1")

plot(fitted(modelPQL$gam), residuals(modelPQL$gam))

#Over and undersampling al 30% con 5000 observaciones
set.seed(1)
n<-5000
p<-0.3
balanced <- rbind(total[total$Gravedad2==0,][sample(nrow(total[total$Gravedad2==0,]),n*(1-p),replace=TRUE),],
                  total[total$Gravedad2==1,][sample(nrow(total[total$Gravedad2==1,]),n*p,replace=TRUE),])
rm(n,p)

#Estimación del modelo (balanceado)
modelPQL_balanced<-gamm(Gravedad2~1+Sexo.L1+Edad.L1+
                          s(Hora.L1, k=30)+Slope.L1+
                          BRT.L1+Bus.L1+Car.L1+Cargo.L1+Motorcycle.L1+
                          SITP.L1+Ancho.L1+Carriles.L1+
                          TotalSeñales.L3+SEMAFOROS.L3+
                          LIGTH.L3+HUECOS.L3+
                          PrcntOficinas.D.L2+PrcntOficinas.L3+
                          Valor.D.L2+Valor.L3+
                          CicloRuta.D.L2+CicloRuta.L3+
                          TraveledKm.D.L2+TraveledKm.L3+
                          s(Lat,k=20)+s(Lon,k=20),
               data=balanced,
               family=binomial(link=logit),
               random=list(UPlCodigo=~1,Year=~1),
               control = lmeControl(sing.tol=1e-20),
               method = "REML")

summary(modelPQL_balanced$lme)
summary(modelPQL_balanced$gam)

exp(cbind(coef(modelPQL_balanced$gam), confint(modelPQL_balanced$gam)))

predicted<-fitted(model$gam,type=response)
roc(model$lme$data$Gravedad2,predicted)
plot.roc(model$lme$data$Gravedad2,predicted)

confusionMatrix(table(model$lme$data$Gravedad2,as.numeric(predicted>=0.5)))

plot(fitted(model$gam), residuals(model$gam))

plot.gam(model$gam,las=1,ylab="Log-Odds of Death", se=TRUE,shade=TRUE)

# Estimación del modelo con población como control de exposición

modelPQL2<-gamm(Gravedad2~1+Sexo.L1+Edad.L1+
                 s(Hora.L1, k=30)+Slope.L1+
                 BRT.L1+Bus.L1+Car.L1+Cargo.L1+Motorcycle.L1+
                 SITP.L1+Ancho.L1+Carriles.L1+
                 TotalSeñales.L3+SEMAFOROS.L3+
                 LIGTH.L3+HUECOS.L3+
                 PrcntOficinas.D.L2+PrcntOficinas.L3+
                 Valor.D.L2+Valor.L3+
                 CicloRuta.D.L2+CicloRuta.L3+
                 BICIUSURS.D.L2+BICIUSURS.L3+
                 s(Lat,k=20)+s(Lon,k=20),
               data=total,
               family=binomial(link=logit),
               random=list(UPlCodigo=~1,Year=~1),
               control = lmeControl(sing.tol=1e-20),
               method = "REML")

summary(modelPQL2$lme)
summary(modelPQL2$gam)

# Estimación del modelo sólo para mujeres

women<-total %>% filter(Sexo.L1==0)

model_women<-gamm(Gravedad2~1+Edad.L1+
                          s(Hora.L1, k=30)+Slope.L1+
                          BRT.L1+Bus.L1+Car.L1+Cargo.L1+Motorcycle.L1+
                          SITP.L1+Ancho.L1+Carriles.L1+
                          TotalSeñales.L3+SEMAFOROS.L3+
                          LIGTH.L3+HUECOS.L3+
                          PrcntOficinas.D.L2+PrcntOficinas.L3+
                          Valor.D.L2+Valor.L3+
                          CicloRuta.D.L2+CicloRuta.L3+
                          TraveledKm.D.L2+TraveledKm.L3+
                          s(Lat,k=20)+s(Lon,k=20),
                        data=women,
                        family=binomial(link=logit),
                        random=list(UPlCodigo=~1,Year=~1),
                        control = lmeControl(sing.tol=1e-20, singular.ok=TRUE, returnObject=TRUE, opt="optim",niterEM = 50,optimMethod = "L-BFGS-B"),
                        method = "REML",niterPQL = 1000)

summary(model_women$lme)
summary(model_women$gam)

#Estimación del modelo para los hombres

men<-total %>% filter(Sexo.L1==1)

model_men<-gamm(Gravedad2~1+Edad.L1+
                    s(Hora.L1, k=30)+Slope.L1+
                    BRT.L1+Bus.L1+Car.L1+Cargo.L1+Motorcycle.L1+
                    SITP.L1+Ancho.L1+Carriles.L1+
                    TotalSeñales.L3+SEMAFOROS.L3+
                    LIGTH.L3+HUECOS.L3+
                    PrcntOficinas.D.L2+PrcntOficinas.L3+
                    Valor.D.L2+Valor.L3+
                    CicloRuta.D.L2+CicloRuta.L3+
                    TraveledKm.D.L2+TraveledKm.L3+
                    s(Lat,k=20)+s(Lon,k=20),
                  data=men,
                  family=binomial(link=logit),
                  random=list(UPlCodigo=~1,Year=~1),
                  control = lmeControl(sing.tol=1e-20, singular.ok=TRUE, returnObject=TRUE, opt="optim"),
                  method = "REML",niterPQL = 200)

summary(model_men$lme)
summary(model_men$gam)

######## ESPECIFICACIONES ADICIONALES ####################

# Uso del Casco

modelPQL_casco<-gamm(Gravedad2~1+Sexo.L1+Edad.L1+
                 s(Hora.L1, k=30)+Slope.L1+
                 BRT.L1+Bus.L1+Car.L1+Cargo.L1+Motorcycle.L1+
                 SITP.L1+Ancho.L1+Carriles.L1+
                 TotalSeñales.L3+SEMAFOROS.L3+
                 LIGTH.L3+HUECOS.L3+
                 PrcntOficinas.D.L2+PrcntOficinas.L3+
                 Valor.D.L2+Valor.L3+
                 CicloRuta.D.L2+CicloRuta.L3+
                 TraveledKm.D.L2+TraveledKm.L3+
                 LLevaCasco.L1+
                 s(Lat,k=20)+s(Lon,k=20),
               data=total,
               family=binomial(link=logit),
               random=list(UPlCodigo=~1,Year=~1),
               control = lmeControl(sing.tol=1e-20),
               method = "REML")

summary(modelPQL_casco$lme)
summary(modelPQL_casco$gam)

# Antiguedad de la bicicleta

modelPQL_antique<-gamm(Gravedad2~1+Sexo.L1+Edad.L1+
                       s(Hora.L1, k=30)+Slope.L1+
                       BRT.L1+Bus.L1+Car.L1+Cargo.L1+Motorcycle.L1+
                       SITP.L1+Ancho.L1+Carriles.L1+
                       TotalSeñales.L3+SEMAFOROS.L3+
                       LIGTH.L3+HUECOS.L3+
                       PrcntOficinas.D.L2+PrcntOficinas.L3+
                       Valor.D.L2+Valor.L3+
                       CicloRuta.D.L2+CicloRuta.L3+
                       TraveledKm.D.L2+TraveledKm.L3+
                       AntiguedadBici.L1+
                       s(Lat,k=20)+s(Lon,k=20),
                     data=total,
                     family=binomial(link=logit),
                     random=list(UPlCodigo=~1,Year=~1),
                     control = lmeControl(sing.tol=1e-20),
                     method = "REML")

summary(modelPQL_antique$lme)
summary(modelPQL_antique$gam)

# Diseño de infraestructura

total$Diseno.L1<-ifelse(total$Diseno.L1=="Bikeway", 1,0)

modelPQL_diseno<-gamm(Gravedad2~1+Sexo.L1+Edad.L1+
                         s(Hora.L1, k=30)+Slope.L1+
                         BRT.L1+Bus.L1+Car.L1+Cargo.L1+Motorcycle.L1+
                         SITP.L1+Ancho.L1+Carriles.L1+
                         TotalSeñales.L3+SEMAFOROS.L3+
                         LIGTH.L3+HUECOS.L3+
                         PrcntOficinas.D.L2+PrcntOficinas.L3+
                         Valor.D.L2+Valor.L3+
                         CicloRuta.D.L2+CicloRuta.L3+
                         TraveledKm.D.L2+TraveledKm.L3+
                         Diseno.L1+
                         s(Lat,k=20)+s(Lon,k=20),
                       data=total,
                       family=binomial(link=logit),
                       random=list(UPlCodigo=~1, Year=~1),
                       control = lmeControl(sing.tol=1e-20, singular.ok=TRUE, returnObject=TRUE),
                       method = "REML")

summary(modelPQL_diseno$lme)
summary(modelPQL_diseno$gam)

# Weather

total$Clima.L1<-ifelse(total$Clima.L1=="Rain", 1, 0)

modelPQL_clima<-gamm(Gravedad2~1+Sexo.L1+Edad.L1+
                        s(Hora.L1, k=30)+Slope.L1+
                        BRT.L1+Bus.L1+Car.L1+Cargo.L1+Motorcycle.L1+
                        SITP.L1+Ancho.L1+Carriles.L1+
                        TotalSeñales.L3+SEMAFOROS.L3+
                        LIGTH.L3+HUECOS.L3+
                        PrcntOficinas.D.L2+PrcntOficinas.L3+
                        Valor.D.L2+Valor.L3+
                        CicloRuta.D.L2+CicloRuta.L3+
                        TraveledKm.D.L2+TraveledKm.L3+
                        Clima.L1+
                        s(Lat,k=20)+s(Lon,k=20),
                      data=total,
                      family=binomial(link=logit),
                      random=list(UPlCodigo=~1,Year=~1),
                      control = lmeControl(sing.tol=1e-20, singular.ok=TRUE, returnObject=TRUE),
                      method = "REML",verbosePQL = TRUE)

summary(modelPQL_clima$lme)
summary(modelPQL_clima$gam)

#Getis-Ord
data(getisord, package="spData")
xycoords <- cbind(xyz$x, xyz$y)
nb30 <- dnearneigh(xycoords, 0, 30)
G30 <- localG(xyz$val, nb2listw(nb30, style="B"))
G30[length(xyz$val)-136]
nb60 <- dnearneigh(xycoords, 0, 60)
G60 <- localG(xyz$val, nb2listw(nb60, style="B"))
G60[length(xyz$val)-136]
nb90 <- dnearneigh(xycoords, 0, 90)
G90 <- localG(xyz$val, nb2listw(nb90, style="B"))
G90[length(xyz$val)-136]
nb120 <- dnearneigh(xycoords, 0, 120)
G120 <- localG(xyz$val, nb2listw(nb120, style="B"))
G120[length(xyz$val)-136]
nb150 <- dnearneigh(xycoords, 0, 150)
G150 <- localG(xyz$val, nb2listw(nb150, style="B"))
G150[length(xyz$val)-136]
brks <- seq(-5,5,1)
cm.col <- cm.colors(length(brks)-1)
image(x, y, t(matrix(G30, nrow=16, ncol=16, byrow=TRUE)),
      breaks=brks, col=cm.col, asp=1)
text(xyz$x, xyz$y, round(G30, digits=1), cex=0.7)
polygon(c(195,225,225,195), c(195,195,225,225), lwd=2)
title(main=expression(paste("Values of the ", G[i], " statistic")))
G30s <- localG(xyz$val, nb2listw(include.self(nb30),
                                 style="B"))
cat("value according to Getis and Ord's eq. 14.2, p. 263 (1996)\n")
G30s[length(xyz$val)-136]
cat(paste("value given by Getis and Ord (1996), p. 267",
          "(division by n-1 rather than n \n in variance)\n"))
G30s[length(xyz$val)-136] *
  (sqrt(sum(scale(xyz$val, scale=FALSE)^2)/length(xyz$val)) /
     sqrt(var(xyz$val)))
image(x, y, t(matrix(G30s, nrow=16, ncol=16, byrow=TRUE)),
      breaks=brks, col=cm.col, asp=1)
text(xyz$x, xyz$y, round(G30s, digits=1), cex=0.7)
polygon(c(195,225,225,195), c(195,195,225,225), lwd=2)
title(main=expression(paste("Values of the ", G[i]^"*", " statistic")))


#Construcción de poligonos de Voronoi
UPZ<-st_read(paste0(carpetaRAS,"/BASES DE DATOS/Mapas de Referencia IDECA/MR1217.gdb"),layer='UPla', stringsAsFactors = FALSE) %>% 
  st_transform(crs=4326) %>% 
  st_transform(crs=3116)
UPZ<-UPZ[-c(1:3),]
hull<-st_union(UPZ)

d <- st_geometry(total %>% st_transform(3116))
d <- st_union(d)
v <- st_voronoi(d)
v <- st_intersection(st_cast(v), hull)
v<-st_sf(v) %>% st_transform(3116) %>% mutate(id=row_number())
h<-st_join(total[,"Gravedad2"] %>% st_transform(3116),v,left = TRUE)
h<- h %>% group_by(id) %>% summarise(d=sum(Gravedad2),n=n())
v<-st_join(v,h,left=TRUE)
plot(v[,"d"])

#Prueba de Moran's I para los polygonos de Voronoi utilizando el conteo de accidentes como variable
h<-as.matrix(st_distance(st_centroid(v),st_centroid(v)))
units(h)<-NULL
h.inv<-1/(1+h)
diag(h.inv)<-0
Moran.I(v$n,h.inv,na.rm = TRUE) #Prueba sobre ocurrencia de accidentes
Moran.I(v$d,h.inv,na.rm = TRUE) #Prueba sobre ocurrencia de muertes

#Análisis espacial----

#Transformación de SF a PPP
st_bbox(total %>% st_transform(3116))
w<-owin(c(985128,1006726),c(987937,1024043),unitname=c("metre","metres"))
obj<-data.frame(st_coordinates(total %>% filter(!is.na(Gravedad2)) %>% st_transform(3116))) %>% mutate(Gravedad=factor(ifelse((total %>% filter(!is.na(Gravedad2)))$Gravedad2==0,"Nonfatal","Fatal")))
pp<-ppp(obj$X,obj$Y,w,marks=obj$Gravedad,checkdup = FALSE,drop=TRUE)

summary(pp)
summary(split(pp)$Fatal)
summary(split(pp)$Nonfatal)

# Point distribution over fatal and non-fatal accidents
oldpar <- par(mar=rep(0,4), oma=rep(0,4))
plot(split(pp),chars=19,cex=0.3,use.marks = TRUE,which.marks=1,main="Accidents distribution 2011-2017",legend = FALSE)
par(oldpar)

#Función K de Ripley
K<-Kest(split(pp)$Fatal)
K
plot(K)
plot(envelope(split(pp)$Fatal,Kest),main="K")

K<-Kest(split(pp)$Nonfatal)
K
plot(K)
plot(envelope(split(pp)$Nonfatal,Kest),main="K")

#Quadrant analysis
Q <- quadratcount(split(pp)$Fatal, nx = 20, ny = 30)
den <- density(split(pp)$Fatal, sigma = 500)
oldpar <- par(mar=rep(0,4), oma=rep(0,4))
plot(den, main="",border=FALSE,col=colorRampPalette(c("aliceblue","yellow","red4")),valuesAreColours = FALSE)
plot(split(pp)$Fatal, cex = 0.1, pch = ".",cols="lightgray",legend = FALSE,border=FALSE,main="",add=TRUE)
plot(Q, add = TRUE, cex = 0.4,main="")
par(oldpar)

M<-quadrat.test(split(pp)$Fatal, nx = 20, ny = 30)
M
oldpar <- par(mar=rep(0,4), oma=rep(0,4))
plot(M, main="",cex=0.3)
par(oldpar)

fitPois<-ppm(unmark(pp),~polynom(x,y,2))
oldpar <- par(mar=rep(0,4), oma=rep(0,4))
diagnose.ppm(fitPois)
par(oldpar)

qqplot.ppm(fitPois, nsim = 39)

Fc <- Fest(pp)
Fc
oldpar <- par(mar=rep(0,4), oma=rep(0,4))
plot(Fc,main="")
par(oldpar)

Gc <- Gest(pp)
Gc
oldpar <- par(mar=rep(0,4), oma=rep(0,4))
plot(Gc,main="")
par(oldpar)
