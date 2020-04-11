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
pacman::p_load(car)
pacman::p_load(ggplot2)

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

############### VARIABLE DEPENDIENTE ##########################################################
#Totales----
sum(total$Gravedad2==1,na.rm=TRUE)
sum(total$Gravedad2==0,na.rm=TRUE)

############### LEVEL 1 - GENERAL CHARACTERISTICS ##########################################################

#Conteos por sexo----
table(total$Sexo.L1)
table(total$Sexo.L1,total$Gravedad2)
chisq.test(total$Sexo.L1,total$Gravedad2)

#Distribución de la edad----
h<-ifelse(5<=total$Edad.L1 & total$Edad.L1<=17,1,
          ifelse(18<=total$Edad.L1 & total$Edad.L1<=29,2,
                 ifelse(30<=total$Edad.L1 & total$Edad.L1<=49,3,
                        ifelse(50<=total$Edad.L1,4,NA))))
table(h)
table(h,total$Gravedad2)
chisq.test(h,total$Gravedad2)

#Licencia de conducción----
table(total$PortaLicencia.L1)
table(total$PortaLicencia.L1,total$Gravedad2)
chisq.test(total$PortaLicencia.L1,total$Gravedad2)

#Uso del casco----
table(total$LLevaCasco.L1)
table(total$LLevaCasco.L1,total$Gravedad2)
chisq.test(total$LLevaCasco.L1,total$Gravedad2)

#Antiguedad del vehiculo----
h<-ifelse(total$AntiguedadBici.L1<=2,1,
          ifelse(3<=total$AntiguedadBici.L1 & total$AntiguedadBici.L1<=5,2,
                 ifelse(6<=total$AntiguedadBici.L1 & total$AntiguedadBici.L1<=10,3,
                        ifelse(11<=total$AntiguedadBici.L1,4,NA))))
table(h)
table(h,total$Gravedad2)
chisq.test(h,total$Gravedad2)

#Weekday----
h<-ifelse(total$Dia.L1%in%c("Saturday","Sunday"),0,ifelse(total$Festivo.L1==1,0,1))
table(h)
table(h,total$Gravedad2)
chisq.test(h,total$Gravedad2)

#Hora de ocurrencia----
h<-ifelse((0+0/60)<=total$Hora.L1 &
            total$Hora.L1<=(6+0/60),1,
          ifelse((6+1/60)<=total$Hora.L1 &
                   total$Hora.L1<=(8+30/60),2,
                 ifelse((8+31/60)<=total$Hora.L1 &
                          total$Hora.L1<=(15+0/60),3,
                        ifelse((15+1/60)<=total$Hora.L1 &
                                 total$Hora.L1<=(19+30/60),4,
                               ifelse((19+31/60)<=total$Hora.L1 &
                                        total$Hora.L1<=(24+0/60),5,NA)))))
table(h)
table(h,total$Gravedad2)
chisq.test(h,total$Gravedad2)

#Condiciones climáticas
table(total$Clima.L1)
table(total$Clima.L1, total$Gravedad2)
chisq.test(total$Clima.L1, total$Gravedad2)

#Inclinación del terreno
h<-total %>% dplyr::select(Slope.L1,Gravedad2) %>% filter(!is.na(Gravedad2), !is.na(Slope.L1))
ggplot(data=h,aes(x=Slope.L1,y=Gravedad2))+
  stat_smooth() +
  geom_vline(xintercept=0.04)
h<-total$Slope.L1<=0.04
table(h)
table(h,total$Gravedad2)
chisq.test(h,total$Gravedad2)

#Diseño de la infraestructura----
table(total$Diseno.L1)
table(total$Diseno.L1,total$Gravedad2)
chisq.test(total$Diseno.L1,total$Gravedad2)

#Tipo de vehiculo involucrado en el accidente----
h<-total %>% dplyr::select (Gravedad2, BRT.L1, Bus.L1, Car.L1, Cargo.L1, Motorcycle.L1, Other.L1) %>% st_set_geometry(NULL) 
colSums(h[-1],na.rm = TRUE)
h %>% group_by(Gravedad2) %>% summarise_all(funs(sum(.,na.rm=TRUE))) %>% print(n=100)
rm(h)
chisq.test(total$Bus.L1,total$Gravedad2)
chisq.test(total$BRT.L1,total$Gravedad2)
chisq.test(total$Car.L1,total$Gravedad2)
chisq.test(total$Motorcycle.L1,total$Gravedad2)
chisq.test(total$Cargo.L1,total$Gravedad2)
chisq.test(total$Other.L1,total$Gravedad2)

############### LEVEL 1 - LTS ##########################################################

#Ancho de la via----
total %>% st_set_geometry(NULL) %>% 
  summarise(mean=mean(Ancho.L1,na.rm=TRUE),
            sd=sd(Ancho.L1,na.rm=TRUE)/sqrt(sum(!is.na(total$Ancho.L1)))) 
total %>% st_set_geometry(NULL) %>% 
  group_by(Gravedad2) %>% 
  summarise(mean=mean(Ancho.L1,na.rm=TRUE),
            sd=sd(Ancho.L1,na.rm=TRUE)/sqrt(sum(!is.na(total$Ancho.L1))))
t.test(Ancho.L1~Gravedad2,total,na.action="na.omit")

#Numero de carriles----
table(total$Carriles.L1)
table(total$Carriles.L1,total$Gravedad2)
chisq.test(total$Carriles.L1,total$Gravedad2)

#Ciloruta----
table(total$CicloRuta.L1)
table(total$CicloRuta.L1,total$Gravedad2)
chisq.test(total$CicloRuta.L1,total$Gravedad2)

#SITP----
table(total$SITP.L1)
table(total$SITP.L1,total$Gravedad2)
chisq.test(total$SITP.L1,total$Gravedad2)

#Velocidad de la via----
total %>% st_set_geometry(NULL) %>% 
  summarise(mean=mean(Velocidad.L1,na.rm=TRUE),
            sd=sd(Velocidad.L1,na.rm=TRUE)/sqrt(sum(!is.na(total$Velocidad.L1)))) 
total %>% st_set_geometry(NULL) %>% 
  group_by(Gravedad2) %>% 
  summarise(mean=mean(Velocidad.L1,na.rm=TRUE),
            sd=sd(Velocidad.L1,na.rm=TRUE)/sqrt(sum(!is.na(total$Velocidad.L1))))
t.test(Velocidad.L1~Gravedad2,total,na.action="na.omit")

#Congestion de la via----
total %>% st_set_geometry(NULL) %>% 
  summarise(mean=mean(Congestion.L1,na.rm=TRUE),
            sd=sd(Congestion.L1,na.rm=TRUE)/sqrt(sum(!is.na(total$Congestion.L1))))
total %>% st_set_geometry(NULL) %>% 
  group_by(Gravedad2) %>% 
  summarise(mean=mean(Congestion.L1,na.rm=TRUE),
            sd=sd(Congestion.L1,na.rm=TRUE)/sqrt(sum(!is.na(total$Congestion.L1))))

t.test(Congestion.L1~Gravedad2,total,na.action="na.omit")

#Densidad de la via----
total %>% st_set_geometry(NULL) %>% 
  summarise(mean=mean(Densidad.L1,na.rm=TRUE),
            sd=sd(Densidad.L1,na.rm=TRUE)/sqrt(sum(!is.na(total$Densidad.L1))))
total %>% st_set_geometry(NULL) %>% 
  group_by(Gravedad2) %>% 
  summarise(mean=mean(Densidad.L1,na.rm=TRUE),
            sd=sd(Densidad.L1,na.rm=TRUE)/sqrt(sum(!is.na(total$Densidad.L1))))
t.test(Densidad.L1~Gravedad2,total,na.action="na.omit")

#Flujo de la via----
total %>% st_set_geometry(NULL) %>% 
  summarise(mean=mean(Flujo.L1,na.rm=TRUE),
            sd=sd(Flujo.L1,na.rm=TRUE)/sqrt(sum(!is.na(total$Flujo.L1))))
total %>% st_set_geometry(NULL) %>% 
  group_by(Gravedad2) %>% 
  summarise(mean=mean(Flujo.L1,na.rm=TRUE),
            sd=sd(Flujo.L1,na.rm=TRUE)/sqrt(sum(!is.na(total$Flujo.L1))))
t.test(Flujo.L1~Gravedad2,total,na.action="na.omit")

#LTS----
table(total$LTS.L1)
table(total$LTS.L1,total$Gravedad2)
chisq.test(total$LTS.L1,total$Gravedad2)


############### LEVEL 2/3 - UPZ ##########################################################

#Entropy index----
h<-total %>% transmute(Gravedad2,X=Entropia.D.L2+Entropia.L3)
h %>% st_set_geometry(NULL) %>% 
  summarise(mean=mean(X,na.rm=TRUE),
            sd=sd(X,na.rm=TRUE)/sqrt(sum(!is.na(X)))) 
h %>% st_set_geometry(NULL) %>% 
  group_by(Gravedad2) %>% 
  summarise(mean=mean(X,na.rm=TRUE),
            sd=sd(X,na.rm=TRUE)/sqrt(sum(!is.na(X))))
t.test(X~Gravedad2,h,na.action="na.omit")

#Commercial use----
h<-total %>% transmute(Gravedad2,X=PrcntComercial.D.L2+PrcntComercial.L3)
h %>% st_set_geometry(NULL) %>% 
  summarise(mean=mean(X,na.rm=TRUE),
            sd=sd(X,na.rm=TRUE)/sqrt(sum(!is.na(X)))) 
h %>% st_set_geometry(NULL) %>% 
  group_by(Gravedad2) %>% 
  summarise(mean=mean(X,na.rm=TRUE),
            sd=sd(X,na.rm=TRUE)/sqrt(sum(!is.na(X))))
t.test(X~Gravedad2,h,na.action="na.omit")

#Office use----
h<-total %>% transmute(Gravedad2,X=PrcntOficinas.D.L2+PrcntOficinas.L3)
h %>% st_set_geometry(NULL) %>% 
  summarise(mean=mean(X,na.rm=TRUE),
            sd=sd(X,na.rm=TRUE)/sqrt(sum(!is.na(X)))) 
h %>% st_set_geometry(NULL) %>% 
  group_by(Gravedad2) %>% 
  summarise(mean=mean(X,na.rm=TRUE),
            sd=sd(X,na.rm=TRUE)/sqrt(sum(!is.na(X))))
t.test(X~Gravedad2,h,na.action="na.omit")

#Residential use----
h<-total %>% transmute(Gravedad2,X=PrcntResidencial.D.L2+PrcntResidencial.L3)
h %>% st_set_geometry(NULL) %>% 
  summarise(mean=mean(X,na.rm=TRUE),
            sd=sd(X,na.rm=TRUE)/sqrt(sum(!is.na(X)))) 
h %>% st_set_geometry(NULL) %>% 
  group_by(Gravedad2) %>% 
  summarise(mean=mean(X,na.rm=TRUE),
            sd=sd(X,na.rm=TRUE)/sqrt(sum(!is.na(X))))
t.test(X~Gravedad2,h,na.action="na.omit")

#Industrial use----
h<-total %>% transmute(Gravedad2,X=PrcntIndustrial.D.L2+PrcntIndustrial.L3)
h %>% st_set_geometry(NULL) %>% 
  summarise(mean=mean(X,na.rm=TRUE),
            sd=sd(X,na.rm=TRUE)/sqrt(sum(!is.na(X)))) 
h %>% st_set_geometry(NULL) %>% 
  group_by(Gravedad2) %>% 
  summarise(mean=mean(X,na.rm=TRUE),
            sd=sd(X,na.rm=TRUE)/sqrt(sum(!is.na(X))))
t.test(X~Gravedad2,h,na.action="na.omit")

#Bicycle pathway----
h<-total %>% transmute(Gravedad2,X=(CicloRuta.D.L2+CicloRuta.L3)/1000)
h %>% st_set_geometry(NULL) %>% 
  summarise(mean=mean(X,na.rm=TRUE),
            sd=sd(X,na.rm=TRUE)/sqrt(sum(!is.na(X)))) 
h %>% st_set_geometry(NULL) %>% 
  group_by(Gravedad2) %>% 
  summarise(mean=mean(X,na.rm=TRUE),
            sd=sd(X,na.rm=TRUE)/sqrt(sum(!is.na(X))))
t.test(X~Gravedad2,h,na.action="na.omit")

#Effective public space----
h<-total %>% transmute(Gravedad2,X=IUPIEPEFEC.L3)
h %>% st_set_geometry(NULL) %>% 
  summarise(mean=mean(X,na.rm=TRUE),
            sd=sd(X,na.rm=TRUE)/sqrt(sum(!is.na(X)))) 
h %>% st_set_geometry(NULL) %>% 
  group_by(Gravedad2) %>% 
  summarise(mean=mean(X,na.rm=TRUE),
            sd=sd(X,na.rm=TRUE)/sqrt(sum(!is.na(X))))
t.test(X~Gravedad2,h,na.action="na.omit")

#Traffic signals----
h<-total %>% transmute(Gravedad2,X=TotalSeñales.L3)
h %>% st_set_geometry(NULL) %>% 
  summarise(mean=mean(X,na.rm=TRUE),
            sd=sd(X,na.rm=TRUE)/sqrt(sum(!is.na(X)))) 
h %>% st_set_geometry(NULL) %>% 
  group_by(Gravedad2) %>% 
  summarise(mean=mean(X,na.rm=TRUE),
            sd=sd(X,na.rm=TRUE)/sqrt(sum(!is.na(X))))
t.test(X~Gravedad2,h,na.action="na.omit")

#Traffic lights----
h<-total %>% transmute(Gravedad2,X=SEMAFOROS.L3)
h %>% st_set_geometry(NULL) %>% 
  summarise(mean=mean(X,na.rm=TRUE),
            sd=sd(X,na.rm=TRUE)/sqrt(sum(!is.na(X)))) 
h %>% st_set_geometry(NULL) %>% 
  group_by(Gravedad2) %>% 
  summarise(mean=mean(X,na.rm=TRUE),
            sd=sd(X,na.rm=TRUE)/sqrt(sum(!is.na(X))))
t.test(X~Gravedad2,h,na.action="na.omit")

#Lighting conditions----
h<-total %>% transmute(Gravedad2,X=LIGTH.L3)
h %>% st_set_geometry(NULL) %>% 
  summarise(mean=mean(X,na.rm=TRUE),
            sd=sd(X,na.rm=TRUE)/sqrt(sum(!is.na(X)))) 
h %>% st_set_geometry(NULL) %>% 
  group_by(Gravedad2) %>% 
  summarise(mean=mean(X,na.rm=TRUE),
            sd=sd(X,na.rm=TRUE)/sqrt(sum(!is.na(X))))
t.test(X~Gravedad2,h,na.action="na.omit")

#Crowdness conditions----
h<-total %>% transmute(Gravedad2,X=CROWD.L3)
h %>% st_set_geometry(NULL) %>% 
  summarise(mean=mean(X,na.rm=TRUE),
            sd=sd(X,na.rm=TRUE)/sqrt(sum(!is.na(X)))) 
h %>% st_set_geometry(NULL) %>% 
  group_by(Gravedad2) %>% 
  summarise(mean=mean(X,na.rm=TRUE),
            sd=sd(X,na.rm=TRUE)/sqrt(sum(!is.na(X))))
t.test(X~Gravedad2,h,na.action="na.omit")

#Huecos conditions----
h<-total %>% transmute(Gravedad2,X=HUECOS.L3)
h %>% st_set_geometry(NULL) %>% 
  summarise(mean=mean(X,na.rm=TRUE),
            sd=sd(X,na.rm=TRUE)/sqrt(sum(!is.na(X)))) 
h %>% st_set_geometry(NULL) %>% 
  group_by(Gravedad2) %>% 
  summarise(mean=mean(X,na.rm=TRUE),
            sd=sd(X,na.rm=TRUE)/sqrt(sum(!is.na(X))))
t.test(X~Gravedad2,h,na.action="na.omit")

#Land value----
h<-total %>% transmute(Gravedad2,X=(Valor.D.L2+Valor.L3)/3000)
h %>% st_set_geometry(NULL) %>% 
  summarise(mean=mean(X,na.rm=TRUE),
            sd=sd(X,na.rm=TRUE)/sqrt(sum(!is.na(X)))) 
h %>% st_set_geometry(NULL) %>% 
  group_by(Gravedad2) %>% 
  summarise(mean=mean(X,na.rm=TRUE),
            sd=sd(X,na.rm=TRUE)/sqrt(sum(!is.na(X))))
t.test(X~Gravedad2,h,na.action="na.omit")

#UPZ area----
h<-total %>% transmute(Gravedad2,X=Area.L3/1000000)
h %>% st_set_geometry(NULL) %>% 
  summarise(mean=mean(X,na.rm=TRUE),
            sd=sd(X,na.rm=TRUE)/sqrt(sum(!is.na(X)))) 
h %>% st_set_geometry(NULL) %>% 
  group_by(Gravedad2) %>% 
  summarise(mean=mean(X,na.rm=TRUE),
            sd=sd(X,na.rm=TRUE)/sqrt(sum(!is.na(X))))
t.test(X~Gravedad2,h,na.action="na.omit")

#Daily traveled kilometers----
h<-total %>% transmute(Gravedad2,X=(TraveledKm.D.L2+TraveledKm.L3))
h %>% st_set_geometry(NULL) %>% 
  summarise(mean=mean(X,na.rm=TRUE),
            sd=sd(X,na.rm=TRUE)/sqrt(sum(!is.na(X)))) 
h %>% st_set_geometry(NULL) %>% 
  group_by(Gravedad2) %>% 
  summarise(mean=mean(X,na.rm=TRUE),
            sd=sd(X,na.rm=TRUE)/sqrt(sum(!is.na(X))))
t.test(X~Gravedad2,h,na.action="na.omit")

#Bicyclists population----
h<-total %>% transmute(Gravedad2,X=BICIUSURS.D.L2+BICIUSURS.L3)
h %>% st_set_geometry(NULL) %>% 
  summarise(mean=mean(X,na.rm=TRUE),
            sd=sd(X,na.rm=TRUE)/sqrt(sum(!is.na(X)))) 
h %>% st_set_geometry(NULL) %>% 
  group_by(Gravedad2) %>% 
  summarise(mean=mean(X,na.rm=TRUE),
            sd=sd(X,na.rm=TRUE)/sqrt(sum(!is.na(X))))
t.test(X~Gravedad2,h,na.action="na.omit")


############### DATOS FALTANTES ##########################################################
as.data.frame(colSums(is.na(total))/9950)
