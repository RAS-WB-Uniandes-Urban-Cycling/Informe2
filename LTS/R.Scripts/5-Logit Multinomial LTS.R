#Preparación del entorno de trabajo----

  #Ruta de los archivos
  
    ruta_resultados <-"/Users/alejandropalacio/Universidad de Los Andes/German Augusto Carvajal Murcia - UNIANDES - RAS - SDM/RESULTADOS/LTS/Bases de Datos/"

  #Librerías a utilizar
      
    library(nnet)
    library(reshape2)
    library(ggplot2)  
    library(tidyverse)
    library(mlogit)
    library(Hmisc)
  
  #Se cargan los resultados
      
    load(paste0(ruta_resultados, "5-Variables_LTS.Rdata"))
    load(paste0(ruta_resultados,"Capas Clustering LTS/Resultados_Clustering_Kennedy.Rdata"))
    
#Se almacena la capa_LTS_PAM en capa_variables_LTS_model    
    
    capa_LTS_PAM <- capa_LTS_PAM_Kennedy
    
    capa_LTS_PAM$clusters_PAM <-   as.numeric(capa_LTS_PAM$clusters_PAM)

    capa_variables_LTS_model <-capa_LTS_PAM 

#Se declara la regresion Logit Multinomial con un método de clustering elegido
    
    logit_Multi<-multinom(capa_variables_LTS_model$clusters_PAM~Velocidad+Ancho+Carriles+CicloRuta+SITP+Congestion+Densidad+Flujo+
                          Velocidad:Ancho+Velocidad:Carriles+Velocidad:CicloRuta+Velocidad:SITP+Velocidad:Congestion+Velocidad:Densidad+Velocidad:Flujo+Velocidad:Segregada+
                          Ancho:Carriles+Ancho:CicloRuta+Ancho:SITP+Ancho:Congestion+Ancho:Densidad+Ancho:Flujo+Ancho:Segregada+
                          Carriles:CicloRuta+Carriles:SITP+Carriles:Congestion+Carriles:Densidad+Carriles:Flujo+Carriles:Segregada+
                          CicloRuta:SITP+CicloRuta:Congestion+CicloRuta:Densidad+CicloRuta:Flujo+CicloRuta:Segregada+
                          SITP:Congestion+SITP:Densidad+SITP:Flujo+SITP:Segregada+
                          Congestion:Densidad+Congestion:Flujo+Congestion:Segregada+
                          Densidad:Flujo+Densidad:Segregada, data=capa_variables_LTS_model)
    
    
    logit_Multi<-multinom(capa_variables_LTS_model$clusters_PAM~Velocidad+Ancho+Carriles+CicloRuta+SITP+Congestion+Densidad+Flujo, data=capa_variables_LTS_model)

#Significancia de variables

    h <- summary(logit_Multi)
    
    h$coefficients
    h$standard.errors
    
    z <- h$coefficients/h$standard.errors
    
    p <- (1 - pnorm(abs(z), 0, 1)) * 2
    p

#Correlaciones entre variables

    rcorr(as.matrix(capa_variables_LTS_model[,c("Velocidad","Ancho","Carriles","Congestion","Densidad","Flujo","SITP","CicloRuta","Segregada")]),type="pearson")
    rcorr(as.matrix(capa_variables_LTS_model[,c("Velocidad","Ancho","Carriles","Congestion","Densidad","Flujo","SITP","CicloRuta","Segregada")]),type="spearman")

#Resultados Logit Multinomial con base en LTS 1

    summary(logit_Multi)

#Se revisa la significancia de las variables

    summary(aov(logit_Multi))

#Se crea una matriz de fitted values para cada una de las observaciones
    
    LTS_Estimado_Model<-(fitted(logit_Multi))
    head(LTS_Estimado_Model)

#Modelo predictivo:

    predicted=predict(logit_Multi,capa_variables_LTS,type="probs")
    predicted
    
    Capa_Variables_Prediccion_Kennedy <- cbind(capa_variables_LTS,predicted)

#Se guardan los resultados

    save(logit_Multi,file=paste0(ruta_resultados,"7-Modelo_Análisis_Estadístico_LTS.Rdata"))
    save(Capa_Variables_Prediccion_Kennedy,file=paste0(ruta_resultados,"Capa_Predicción_LTS_Logit_Kennedy.Rdata"))
    saveRDS(Capa_Variables_Prediccion,"LTS_Bogota.rds")
    save(capa_variables_LTS_model,file=paste0(ruta_resultados,"Capa_Calculo_Logit.Rdata"))
    

#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------

#Pruebas

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

    #capa_variables_LTS <-capa_variables_LTS[-c(1,8)] %>% st_set_geometry(NULL)

