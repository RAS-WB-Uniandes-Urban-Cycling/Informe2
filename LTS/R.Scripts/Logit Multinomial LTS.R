
#Ruta de los archivos

  #Alejandro

    #ruta_resultados <-"/Users/alejandropalacio/Documents/GitHub/proBikePolicies/LTS/Resultados/"

  #Marcelo

    ruta_resultados <-"C:/Users/marce/Documents/GitHub/proBikePolicies/LTS/Resultados/"
   
    
#Librerías a utilizar
library(nnet)
library(reshape2)
library(ggplot2)  
library(tidyverse)


#Se cargan los resultados
load(paste0(ruta_resultados,"Resultados_Clustering.Rdata"))
load(paste0(ruta_resultados, "Variables_LTS.Rdata"))

capa_LTS_PAM$clusters_PAM <-   as.numeric(capa_LTS_PAM$clusters_PAM)

capa_variables_LTS_model <-capa_LTS_PAM 
#capa_variables_LTS_model <-capa_LTS_PAM %>% filter((ZAT %in% c("DOCE DE OCTUBRE","CIUDAD SALITRE NOR-ORIENTAL","CIUDAD SALITRE SUR-ORIENTAL")))
#capa_variables_LTS <-capa_variables_LTS[-c(1,8)] %>% st_set_geometry(NULL)

#Se declara la regresion Logit Multinomial con un método de clustering elegido

logit_Multi<-multinom(capa_variables_LTS_model$clusters_PAM~Velocidad+Ancho+Carriles+CicloRuta+SITP+Congestion+Densidad+Flujo+
                      Velocidad:Ancho+Velocidad:Carriles+Velocidad:CicloRuta+Velocidad:SITP+Velocidad:Congestion+Velocidad:Densidad+Velocidad:Flujo+
                      Ancho:Carriles+Ancho:CicloRuta+Ancho:SITP+Ancho:Congestion+Ancho:Densidad+Ancho:Flujo+
                      Carriles:CicloRuta+Carriles:SITP+Carriles:Congestion+Carriles:Densidad+Carriles:Flujo+
                      CicloRuta:SITP+CicloRuta:Congestion+CicloRuta:Densidad+CicloRuta:Flujo+
                      SITP:Congestion+SITP:Densidad+SITP:Flujo+
                      Congestion:Densidad+Congestion:Flujo+
                      Densidad:Flujo, data=capa_variables_LTS_model)

logit_Multi<-multinom(capa_variables_LTS_model$clusters_PAM~Vprom+Trafico+Ancho+Carriles+CicloRuta+SITP+
                        Vprom:Trafico+Vprom:Ancho+Trafico:Ancho+Vprom:Carriles+Trafico:Carriles+
                        Ancho:Carriles+Vprom:CicloRuta+Trafico:CicloRuta+Ancho:CicloRuta+Carriles:CicloRuta+
                        Vprom:SITP+Trafico:SITP+Ancho:SITP+Carriles:SITP+CicloRuta:SITP, data=capa_variables_LTS_model)

logit_Multi<-multinom(capa_variables_LTS_model$clusters_PAM~Vprom+Trafico+Ancho+Carriles+CicloRuta+SITP, data=capa_variables_LTS_model)



#Resultados Logit Multinomial con base en LTS 1
summary(logit_Multi)

#Se revisa la significancia de las variables
summary(aov(logit_Multi))

#Se crea una matriz de fitted values para cada una de las observaciones
LTS_Estimado_Model<-(fitted(logit_Multi))
head(LTS_Estimado_Model)


#Modelo predictivo:

#Se genera un set de datos para input
expanded=expand.grid(Vprom=c(20,30,35,50),
                     Trafico=c(0.1,0.2,0.3,0.5),
                     Ancho=c(4,8,10,15),
                     Carriles=c(1,2,2,3),
                     CicloRuta=c(0,0,1),
                     SITP=c(0,1))

head(expanded)

#Se predice la probabilidad de que uno de los datos sea de un LTS determinado

predicted=predict(logit_Multi,expanded,type="probs")
predicted

predicted=predict(logit_Multi,capa_variables_LTS,type="probs")
predicted

Capa_Variables_Prediccion <- cbind(capa_variables_LTS,predicted)


#Se guardan los resultados

save(logit_Multi,file=paste0(ruta_resultados,"Modelo_Análisis_Estadístico_LTS.Rdata"))
save(Capa_Variables_Prediccion,file=paste0(ruta_resultados,"Capa_Predicción_LTS_Logit.Rdata"))
save(capa_variables_LTS_model,file=paste0(ruta_resultados,"Capa_Calculo_Logit.Rdata"))



#--------------------------------------------------------------------------------------------
#Pruebas

bp<-cbind(expanded,predicted)
head(bp)

bp2 = melt (bp,id.vars=c("Vprom","Trafico","Ancho","Carriles","CicloRuta","SITP"),value.name="Probabilidad")
head(bp2)

ggplot(bp2,aes(x=Vprom, y=Probabilidad, colour=CicloRuta))+geom_line()+facet_grid(variable~.,scales="free")

#Pruebas 2

LTS_sin_geo<-capa_LTS_PAM[-c(1,8)] %>% st_set_geometry(NULL)
bp<-cbind(LTS_sin_geo,probabilidadClusters)
head(bp)

bp2 = melt (bp,id.vars=c("Vprom","Trafico","Ancho","Carriles","CicloRuta","SITP"),value.name="Probabilidad")
head(bp2)

ggplot(bp2,aes(x=Vprom, y=Probabilidad, colour=CicloRuta))+geom_line()+facet_grid(variable~.,scales="free")

