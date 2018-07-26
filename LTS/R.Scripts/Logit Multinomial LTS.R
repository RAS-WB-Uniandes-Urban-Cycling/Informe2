
#Ruta de los archivos

  #Alejandro

    #ruta_resultados <-"/Users/alejandropalacio/Documents/GitHub/proBikePolicies/LTS/Resultados/"

  #Marcelo

    ruta_resultados <-"C:/Users/marce/Documents/GitHub/proBikePolicies/LTS/Resultados/"
    
#Se cargan los resultados
load(paste0(ruta_resultados,"Resultados_Clustering.Rdata"))
load(paste0(ruta_resultados, "Variables_LTS.Rdata"))

capa_variables_LTS_model <-capa_variables_LTS %>% filter(!(ZAT %in% c("DOCE DE OCTUBRE","CIUDAD SALITRE NOR-ORIENTAL","CIUDAD SALITRE SUR-ORIENTAL")))
capa_variables_LTS <-capa_variables_LTS[-c(1,8)] %>% st_set_geometry(NULL)

#Librerías a utilizar
library(nnet)
library(reshape2)
library(ggplot2)

#Se declara la regresion Logit Multinomial con un método de clustering elegido

#logit_Multi<-multinom(capa_LTS_Kmeans$clusters_Kmeans~Vprom+Trafico+Ancho+Carriles+CicloRuta+SITP, data=capa_LTS_Kmeans)
logit_Multi<-multinom(capa_LTS_PAM$clusters_PAM~Vprom+Trafico+Ancho+Carriles+CicloRuta+SITP, data=capa_LTS_PAM)

#Resultados Logit Multinomial con base en LTS 1
summary(logit_Multi)

#Se crea una matriz de fitted values para cada una de las observaciones
LTS_Estimado_Model<-as.matrix(fitted(logit_Multi))
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


#Se guardan los resultados

save(capa_variables_LTS,file=paste0(ruta_resultados,"Variables_LTS.Rdata"))

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

