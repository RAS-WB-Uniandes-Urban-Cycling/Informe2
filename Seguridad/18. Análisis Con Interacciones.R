# Limpiar espacio de trabajo----
rm(list=ls())
if(!is.null(dev.list())) dev.off()

# Preparación del entorno de trabajo----
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
pacman::p_load(magrittr)
pacman::p_load(pROC)
pacman::p_load(itsadug)
pacman::p_load(plyr)
pacman::p_load(lattice)
pacman::p_load(directlabels)
pacman::p_load(scales)
pacman::p_load(stringr)
pacman::p_load(spdep)
pacman::p_load(maptools)
pacman::p_load(lwgeom)
pacman::p_load(cartography)
pacman::p_load(extrafont)
pacman::p_load(showtext)
font_add("Arial", regular = "Arial.ttf",
         bold = "Arial Bold.ttf", italic = "Arial Italic.ttf", bolditalic = "Arial Bold Italic.ttf")
par(family = "Arial")

# Leer bases de datos preprocesada----
load(paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/13. Level1.Rdata"))
load(paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/16. Level2.Rdata"))

#Transormación de variables a tipos adecuados
Level1<-AccidentesBiciTotal
rm(AccidentesBiciTotal)
names(Level1)[7:33]<-paste0(names(Level1),".L1")[7:33]

Level2<-Level2 %>%
  dplyr::mutate(Year=as.factor(Year),UPlCodigo=as.factor(UPlCodigo)) %>% 
  dplyr::mutate(Nonfatal=NULL,Fatal=NULL)
names(Level2)[3:11]<-paste0(names(Level2),".L2")[3:11]

Level3<-Level3 %>% 
  dplyr::mutate(UPlCodigo=as.factor(UPlCodigo))
names(Level3)[2:22]<-paste0(names(Level3),".L3")[2:22]

#Integración en base de datos única
total<-dplyr::left_join(Level1,Level2,by=c("UPlCodigo","Year"))
total$Year<-as.numeric(levels(total$Year)[as.numeric(total$Year)])
total<-dplyr::left_join(total,Level3,by="UPlCodigo")

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

# Reducción a 2 niveles
total %<>% 
  dplyr::mutate(Entropia.L2 = Entropia.D.L2 + Entropia.L3,
         PrcntComercial.L2 = PrcntComercial.D.L2+PrcntComercial.L3,
         PrcntOficinas.L2 = PrcntOficinas.D.L2+PrcntOficinas.L3,
         PrcntResidencial.L2 = PrcntResidencial.D.L2+PrcntResidencial.L3,
         PrcntIndustrial.L2 = PrcntIndustrial.D.L2+PrcntIndustrial.L3,
         CicloRuta.L2 = (CicloRuta.D.L2+CicloRuta.L3),
         Valor.L2 = Valor.D.L2+Valor.L3,
         TraveledKm.L2 = (TraveledKm.D.L2+TraveledKm.L3),
         BICIUSURS.L2 = BICIUSURS.D.L2+BICIUSURS.L3,
         Vehiculo.L1 = factor(ifelse(BRT.L1==1, "BRT",
                              ifelse(Cargo.L1==1, "Cargo",
                              ifelse(Bus.L1==1, "Bus",
                              ifelse(Car.L1==1, "Car",
                              ifelse(Motorcycle.L1==1, "Moto",
                                     "Other"))))), 
                              levels = c("Car","BRT","Cargo","Bus","Moto","Other")),
         Dia.L1 = relevel(Dia.L1, ref = "Sunday"),
         Year = as.factor(Year),
         LTS.L1 = as.numeric(LTS.L1=="4"),
         Clima.L1 = factor(ifelse(Clima.L1=="Normal", "Normal", "Rain"), levels= c("Normal", "Rain")),
         Diseno.L1 = relevel(Diseno.L1, "Segment"))
 
##### Funciones ####################################

lr_test <- function(model1, model0, numvar) {
  chi.sq <- as.numeric(2*(logLik(model1)-logLik(model0)))
  cat("\n Chi.sq statistic: \n")
  cat(chi.sq)
  cat("\n p-value: \n")
  cat(1-pchisq(q=chi.sq, df=numvar))
  cat("\n   ")
}

evaluate_gam_perf <- function(obj_model) {
  w <- pROC::roc(obj_model$model$Gravedad2,fitted(obj_model,type=response), plot = T, print.auc = T)
  mt <- model_threshold(w)
  print(w)
  cat("", "The models' best threshold by F1 Score is:", mt, "", sep = "\n")
  print(confusionMatrix(reference = as.factor(obj_model$model$Gravedad2), as.factor(as.numeric(fitted(obj_model,type=response)>=mt)), positive = "1"))
}

extract_linear_odds <- function(obj_model) {
  exp(data.frame(coef = coef(obj_model)[!(str_detect(names(coef(obj_model)), "s\\(") | str_detect(names(coef(obj_model)), "te\\("))], 
                                      interval = data.frame(confint.default(obj_model))[!(str_detect(names(coef(obj_model)), "s\\(") | str_detect(names(coef(obj_model)), "te\\(")),]))
}

geo_effect_df <- function(model, hour, year = "2017") {
  data <- expand.grid(Lat = seq(min(total$Lat, na.rm=T), max(total$Lat, na.rm=T), length.out = 100),
              Lon = seq(min(total$Lon, na.rm=T), max(total$Lon, na.rm=T), length.out = 100)) %>% 
    mutate(Hora.L1 = hour,
           Sexo.L1 = 1,
           Dia.L1 = "Sunday",
           Vehiculo.L1 = "Car",
           Clima.L1 = "Normal",
           Diseno.L1 = "Segment",
           CicloRuta.L1 = 0,
           SITP.L1 = 0,
           LTS.L1 = 1,
           Edad.L1 = mean(total$Edad.L1, na.rm=T),
           Slope.L1 = mean(total$Slope.L1, na.rm=T),
           Ancho.L1 = mean(total$Ancho.L1, na.rm=T),
           Carriles.L1 = mean(total$Carriles.L1, na.rm=T),
           Velocidad.L1 = mean(total$Velocidad.L1, na.rm=T),
           AntiguedadBici.L1 = mean(total$AntiguedadBici.L1, na.rm=T),
           HUECOS.L3 = 0,
           INFORMATIVA.L3 = 0,
           PrcntOficinas.L2 = mean(total$PrcntOficinas.L2, na.rm=T),
           Valor.L2 = mean(total$Valor.L2, na.rm=T),
           SEMAFOROS.L3 = mean(total$SEMAFOROS.L3, na.rm=T),
           CicloRuta.L2 = mean(total$CicloRuta.L2, na.rm=T),
           LIGTH.L3 = mean(total$LIGTH.L3, na.rm=T),
           TraveledKm.L2 = mean(total$TraveledKm.L2, na.rm=T),
           BICIUSURS.L2 = mean(total$BICIUSURS.D.L2, na.rm=T),
           UPlCodigo = levels(total$UPlCodigo)[as.numeric(str_replace(names(sort(coef(model)[str_detect(names(coef(model)), "UPlCodigo\\)")], decreasing = T))[1], "s\\(UPlCodigo\\)\\.", ""))],
           Year = year) %>% 
    cbind(., predict(model, ., type = "terms", se.fit = T)) 
  
  data <- cbind(data, data %>% 
            dplyr::select(names(data)[str_detect(names(data),"fit") & (str_detect(names(data),"Lat") | str_detect(names(data), "Lon")) & !str_detect(names(data),"se\\.")]) %>% 
    transmute(fit = rowSums(.))) %>% 
    mutate(oddsRatio = exp(fit))
  
  return(data)
}

plotOneSmoothDF <- function(df, ylabs = "Odds of a fatal bicyclists' collision", xlabs = "x", min = 0, max = 24, minor = 2, major = 4) {
  ggplot(df, aes_(~x, ~y)) + 
    geom_line(colour = "blue4", size = 1.1) + 
    geom_line(aes_(~x, ~se_upr), linetype = "dashed", colour = "black", size = 0.8, alpha=0.4) + 
    geom_line(aes_(~x, ~se_lwr), linetype = "dashed", colour = "black", size = 0.8, alpha=0.4) + 
    geom_ribbon(aes_(x = ~x, ymin = ~se_lwr, ymax = ~se_upr), fill = "grey", alpha = 0.4) + 
    ylab(ylabs) + xlab(xlabs) + 
    cowplot::background_grid(major = "xy", minor = "none") +
    scale_x_continuous(breaks=seq(min,max,major),minor_breaks = seq(min + minor, max - minor, minor))+
    theme_minimal(base_family = "Arial")+theme(text=element_text(family="Arial",size=12,color="grey"))+
    theme(legend.position = c(0.9, 0.125),legend.text=element_text(colour="black",size=10,family="Arial"),
          legend.title=element_text(colour="black",family="Arial"))
}

plot_geo_effect <- function(model, map_hour, year = "2017") {
  
  geo_effect <- geo_effect_df(model, map_hour, year = year)
  
  direct.label(ggplot() + 
                 geom_point(data = total, aes(x = Lon, y = Lat), color = ifelse(total %>% pull(Gravedad2)==1, "red", "gray88"), size = 1e-4, show.legend = F, alpha = 0.5) +
                 stat_contour(data = geo_effect, aes(x = Lon, y = Lat, z = oddsRatio, colour = ..level..), 
                              show.legend = F, breaks = seq(0,3,0.25)) + 
                 scale_colour_gradient2(low = "green", mid = "lightskyblue", high = "red4", 
                                        midpoint = 1) +
                 xlab("Longitude") + ylab("Latitude") +
                 theme_minimal(),
               list("bottom.pieces", rot=90, cex=0.75, hjust = 1, vjust = 0.5,
                    fontface="plain", fontfamily="", alpha=1))
}

time_effect_df <- function(model, year = "2017") {
  data <- data.frame(Hora.L1 = seq(0,24,0.25),
             Lat = mean(total$Lat, na.rm = T),
             Lon = mean(total$Lon, na.rm = T),
             Sexo.L1 = 1,
             Dia.L1 = "Monday",
             Vehiculo.L1 = "Car",
             Clima.L1 = "Normal",
             Diseno.L1 = "Segment",
             CicloRuta.L1 = 0,
             SITP.L1 = 0,
             LTS.L1 = 1,
             Edad.L1 = mean(total$Edad.L1, na.rm=T),
             Slope.L1 = mean(total$Slope.L1, na.rm=T),
             Ancho.L1 = mean(total$Ancho.L1, na.rm=T),
             Carriles.L1 = mean(total$Carriles.L1, na.rm=T),
             Velocidad.L1 = mean(total$Velocidad.L1, na.rm=T),
             AntiguedadBici.L1 = mean(total$AntiguedadBici.L1, na.rm=T),
             HUECOS.L3 = 0,
             INFORMATIVA.L3 = 0,
             PrcntOficinas.L2 = mean(total$PrcntOficinas.L2, na.rm=T),
             Valor.L2 = mean(total$Valor.L2, na.rm=T),
             SEMAFOROS.L3 = mean(total$SEMAFOROS.L3, na.rm=T),
             CicloRuta.L2 = mean(total$CicloRuta.L2, na.rm=T),
             LIGTH.L3 = mean(total$LIGTH.L3, na.rm=T),
             TraveledKm.L2 = mean(total$TraveledKm.L2, na.rm=T),
             BICIUSURS.L2 = mean(total$BICIUSURS.D.L2, na.rm=T),
             UPlCodigo = levels(total$UPlCodigo)[as.numeric(str_replace(names(sort(coef(model)[str_detect(names(coef(model)), "UPlCodigo\\)")], decreasing = T))[1], "s\\(UPlCodigo\\)\\.", ""))],
             Year = year) %>% 
    cbind(., predict(model, ., type = "link", se.fit = TRUE)) %>%
    mutate(fit = NULL,
           se.fit_a = pmax(0, se.fit - as.numeric(summary(model)$se["(Intercept)"])))
  
  pred <- as.data.frame(predict(model, data, type = "terms", se.fit = TRUE)) 
  
  pred %<>% 
    dplyr::select(names(pred)[str_detect(names(pred),"fit") & str_detect(names(pred),"Hora") & !str_detect(names(pred),"se\\.")]) %>% 
    mutate(fit = rowSums(.))
  
  return(cbind(data, pred) %>% 
    mutate(x = Hora.L1,
            y = exp(fit),
            se_upr = exp(fit + se.fit_a),
            se_lwr = exp(fit - se.fit_a)))
}

lighting_effect_df <- function(model, year = "2017") {
  expand.grid(Hora.L1 = seq(0,24,1),
              LIGTH.L3 = seq(min(total$LIGTH.L3, na.rm=T),max(total$LIGTH.L3, na.rm=T),length.out = 100)) %>% 
    mutate(Lat = mean(total$Lat, na.rm = T),
           Lon = mean(total$Lon, na.rm = T),
           Sexo.L1 = 1,
           Dia.L1 = "Sunday",
           Vehiculo.L1 = "Car",
           Clima.L1 = "Normal",
           Diseno.L1 = "Segment",
           CicloRuta.L1 = 0,
           SITP.L1 = 0,
           LTS.L1 = 1,
           Edad.L1 = mean(total$Edad.L1, na.rm=T),
           Slope.L1 = mean(total$Slope.L1, na.rm=T),
           Ancho.L1 = mean(total$Ancho.L1, na.rm=T),
           Carriles.L1 = mean(total$Carriles.L1, na.rm=T),
           Velocidad.L1 = mean(total$Velocidad.L1, na.rm=T),
           AntiguedadBici.L1 = mean(total$AntiguedadBici.L1, na.rm=T),
           HUECOS.L3 = 0,
           INFORMATIVA.L3 = 0,
           PrcntOficinas.L2 = mean(total$PrcntOficinas.L2, na.rm=T),
           Valor.L2 = mean(total$Valor.L2, na.rm=T),
           SEMAFOROS.L3 = mean(total$SEMAFOROS.L3, na.rm=T),
           CicloRuta.L2 = mean(total$CicloRuta.L2, na.rm=T),
           TraveledKm.L2 = mean(total$TraveledKm.L2, na.rm=T),
           BICIUSURS.L2 = mean(total$BICIUSURS.D.L2, na.rm=T),
           UPlCodigo = levels(total$UPlCodigo)[as.numeric(str_replace(names(sort(coef(model)[str_detect(names(coef(model)), "UPlCodigo\\)")], decreasing = T))[1], "s\\(UPlCodigo\\)\\.", ""))],
           Year = year) %>% 
    mutate(predictedProba = as.numeric(predict.gam(model, ., type = "response")),
           oddsRatio = (predictedProba / (1-predictedProba)) * (1 / exp(as.numeric(coef(model)["(Intercept)"]))))
  
}

plot_light_effect <- function(df) {
  direct.label(ggplot() + 
                 stat_contour(data = df, aes(x = LIGTH.L3 * 100, y = Hora.L1, z = oddsRatio, colour = ..level..), 
                              show.legend = F, breaks = c(0,0.5,1,1.5,2,2.5,3,4,7,10,15,22)) + 
                 scale_colour_gradient2(low = "green", mid = "lightskyblue", high = "red4", 
                                        midpoint = 1) +
                 xlab("Lighting conditions (% based on SAFETIN scale)") + ylab("Time of occurrence (hour)") +
                 theme_minimal(),
               list("bottom.pieces", rot=0, cex=0.75, hjust = 1, vjust = 1,
                    fontface="plain", fontfamily="", alpha=1))
}

plotOneSmooth <- function(gam, var, ylabs = "Odds of a fatal bicyclists' collision", xlabs = "x", min = 0, max = 24, minor = 2, major = 4) {
  df<-gam_to_df(gam, pred=var) %>% 
      mutate(se_upr=exp(se_upr), se_lwr=exp(se_lwr), y=exp(y))
  plotOneSmoothDF(df, xlabs = xlabs, min = min, max = max, minor = minor, major = major)
}

save_to_eps <- function(graphName, obj, height = 6, width = 6.83) {
  setEPS()
  postscript(paste0(graphName, ".eps"), height = height, width = width,
             paper = "special", onefile = FALSE,
             horizontal = FALSE)
  showtext_begin()
  print(obj)
  showtext_end()
  dev.off()
  embed_fonts(paste0(graphName, ".eps"), outfile = paste0(graphName,"_embeded.eps"), options = "-dEPSCrop")
  file.remove(paste0(graphName, ".eps"))
}

save_to_pdf <- function(graphName, obj, height = 6, width = 6.83) {
  pdf(paste0(graphName, ".pdf"), height = height, width = width)
  showtext_begin()
  print(obj)
  showtext_end()
  dev.off()
  embed_fonts(paste0(graphName, ".pdf"), outfile = paste0(graphName,"_embeded.pdf"))
  file.remove(paste0(graphName, ".pdf"))
}

model_threshold <- function(w) {
  data_frame(Threshold=w$thresholds,Sensitivity=w$sensitivities,Specificity=w$specificities) %>% 
    mutate(F1_score = 2*Sensitivity*Specificity/(Sensitivity+Specificity)) %>%
    top_n(1, F1_score) %>% 
    pull(Threshold)
}

plot_distribution <- function(obj,window,mvi){
  plot(window, main = "", border = "gray45", col = "white", box = F, edge = 0.01)
  plot(mvi, main = "", col = "gray75", show.window = F, lwd = 0.2, add = T)
  plot(obj,chars=19,cex=0.4,show.window = F, use.marks = TRUE, which.marks=1,main="",legend = FALSE, frame.plot=T, bty="n", add = T)
}

plot_density <- function(obj, window, mvi){
  plot(obj*1e6, main="",border=FALSE,col=to.transparent(colorRampPalette(c("aliceblue","moccasin","red"))(50), fraction = 0.7),
       valuesAreColours = FALSE,  frame.plot=T, ribsep = 0.05, ribwid = 0.02)
  plot(window, main = "", border = "gray45", col = NULL, box = F, edge = 0.01, add = T)
  plot(mvi, main = "", col = "gray60", show.window = F, lwd = 0.2, add = T)
}

plot_quadrat <- function(obj,window,mvi){
  plot(1e6*intensity(obj, image=TRUE), dimyx=256, main=NULL, 
       las=1, col = to.transparent(colorRampPalette(c("aliceblue","moccasin","red"))(50), fraction = 0.7),
       ribsep = 0.05, ribwid = 0.02)
  plot(window, main = "", border = "gray45", col = NULL, box = F, edge = 0.01, add = T)
  plot(mvi, main = "", col = "gray60", show.window = F, lwd = 0.2, add = T)
  plot(obj, entries = ifelse(round(as.numeric(intensity(obj))*1e6,1)!=-1,"",""), cex = 0.4, main="", add = T, border = "gray60", lwd = 0.1, dx = 0, dy = 0, 
       vfont = NULL, show.tiles = F)
}

over_sampling <- function(df, p = 0.3) {
  n <- round(sum(1-df$Gravedad2, na.rm = T) / (1 - p), 0)
  n_new <- n - sum(1-df$Gravedad2, na.rm = T)
  
  return(rbind(df[df$Gravedad2==0,],
        df[df$Gravedad2==1,][sample(nrow(df[df$Gravedad2==1,]),n_new,replace=TRUE),]) %>% 
    as.data.frame())
}

##### Estimación de modelos ##################
# Modelo base sin efectos aleatorios----
nullModel<-glm(Gravedad2 ~ 1,
               data=total,
               family=binomial(link=logit))

summary(nullModel)

# Modelo de sólo intercepto con efectos aleatorios por UPZ----
upModel<-bam(Gravedad2 ~ 1 + s(UPlCodigo, bs = "re"),
               data=total,
               family=binomial(link=logit),
               control = list(nthreads = 4, irls.reg = 0.1, maxit = 50, mgcv.tol=1e-20, trace = T,
                              efs.tol=5e-1,  newton = list(conv.tol = 5e-1, maxHalf = 5)),
               method = "fREML")

summary(upModel)
gam.vcomp(upModel)
evaluate_gam_perf(upModel)
extract_linear_odds(upModel)
lr_test(upModel, nullModel, 1)

# Modelo de sólo intercepto con efectos aleatorios por UPZ y Year----
up_yeModel<-bam(Gravedad2 ~ 1 + s(UPlCodigo, bs = "re") + s(UPlCodigo, Year, bs = "re"),
                data=total,
                family=binomial(link=logit),
                control = list(nthreads = 4, irls.reg = 0.1, maxit = 50, mgcv.tol=1e-20, trace = T,
                               efs.tol=5e-1,  newton = list(conv.tol = 5e-1, maxHalf = 5)),
                method = "fREML")

summary(up_yeModel)
gam.vcomp(up_yeModel)
evaluate_gam_perf(up_yeModel)
extract_linear_odds(up_yeModel)
lr_test(up_yeModel, upModel, 1)

# Modelo con efectos aleatorios por UPZ y Year, con todas las variables (base)----
set.seed(123)
baseModel<-bam(Gravedad2 ~ 1 + Sexo.L1 + s(Edad.L1, k = 20) + Dia.L1 + Vehiculo.L1 + s(Slope.L1, k = 10) +
                 
              s(Ancho.L1, k = 4) + s(Carriles.L1, k = 4) + CicloRuta.L1 + SITP.L1 + 
              s(Velocidad.L1, k = 4) + LTS.L1 +
                 
              s(HUECOS.L3, k = 7) + s(INFORMATIVA.L3, k = 7) + s(PrcntOficinas.L2, k = 7) + 
              s(Valor.L2, k = 10) + s(SEMAFOROS.L3, k = 7) + s(CicloRuta.L2, k = 7) + 
                 
              s(LIGTH.L3, k = 8) + te(LIGTH.L3, Hora.L1, k = 8) + 
                 
              s(TraveledKm.L2, k = 7) + s(BICIUSURS.L2, k = 7) +
                 
              s(Hora.L1, k=7) + te(Lon, Hora.L1, k = 7) + te(Lat, Hora.L1, k = 7) +
              s(Lon, k = 7) + s(Lat, k = 7) + s(Lon, Lat, k = 7) + 
              s(UPlCodigo, bs = "re") + s(UPlCodigo, Year, bs = "re"),
            data=total,
            family=binomial(link=logit),
            control = list(nthreads = 4, irls.reg = 0.1, maxit = 50, mgcv.tol=1e-20, trace = T,
                           efs.tol=5e-1,  newton = list(conv.tol = 5e-1, maxHalf = 5)),
            method = "fREML")

set.seed(0)
gam.check(baseModel, rep = 100)
    
summary(baseModel)
gam.vcomp(baseModel)
evaluate_gam_perf(baseModel)
extract_linear_odds(baseModel)

save_to_pdf("/Users/germancarvajal/Desktop/edad", 
            plotOneSmooth(baseModel, "Edad.L1", xlabs = "Bicyclist' age (year)", min = 0, max = 90, minor = 2.5, major = 5),
            height = 5, width = 6.83)
save_to_pdf("/Users/germancarvajal/Desktop/slope", 
            plotOneSmooth(baseModel, "Slope.L1", xlabs = "Terrain slope (%)", min = 0, max = .5, minor = 0.025, major = 0.05) +
            xlim(0,0.11) + ylim(0,3),
            height = 5, width = 6.83)
save_to_pdf("/Users/germancarvajal/Desktop/huecos",
            plotOneSmooth(baseModel, "HUECOS.L3", xlabs = "Road potholes (count within UP)", min = 0, max = 650, minor = 50, major = 100),
            height = 5, width = 6.83)
save_to_pdf("/Users/germancarvajal/Desktop/lighting",
            plot_light_effect(lighting_effect_df(baseModel)),
            height = 5, width = 6.83)
save_to_pdf("/Users/germancarvajal/Desktop/traveledKm",
            plotOneSmooth(baseModel, "TraveledKm.L2", xlabs = "Traveled kilometers (million within UP)", min = 0, max = 1, minor = 0.05, major = 0.1),
            height = 5, width = 6.83)
save_to_pdf("/Users/germancarvajal/Desktop/biciusers",
            plotOneSmooth(baseModel, "BICIUSURS.L2", xlabs = "Byciclists x1000 (count within UP)", min = 0, max = 50, minor = 5, major = 10),
            height = 5, width = 6.83)
save_to_pdf("/Users/germancarvajal/Desktop/geo3",
            plot_geo_effect(baseModel, 3),
            height = 6.83, width = 4.8)
save_to_pdf("/Users/germancarvajal/Desktop/geo8",
            plot_geo_effect(baseModel, 8),
            height = 6.83, width = 4.8)
save_to_pdf("/Users/germancarvajal/Desktop/geo16",
            plot_geo_effect(baseModel, 16),
            height = 6.83, width = 4.8)
save_to_pdf("/Users/germancarvajal/Desktop/geo22",
            plot_geo_effect(baseModel, 22),
            height = 6.83, width = 4.8)
save_to_pdf("/Users/germancarvajal/Desktop/time",
            plotOneSmoothDF(time_effect_df(baseModel), xlabs = "Time of occurrence (hour)", min = 0, max = 24, minor = 2, major = 4),
            height = 5, width = 6.83)

# Modelo con efectos aleatorios por UPZ y Year, con todas las variables + AntiguedadBici----
set.seed(123)
antiguedadModel<-bam(Gravedad2 ~ 1 + Sexo.L1 + s(Edad.L1, k = 20) + Dia.L1 + Vehiculo.L1 + s(Slope.L1, k = 10) +
                 
                 s(AntiguedadBici.L1, k = 7) +
                 
                 s(Ancho.L1, k = 4) + s(Carriles.L1, k = 4) + CicloRuta.L1 + SITP.L1 + 
                 s(Velocidad.L1, k = 4) + LTS.L1 +
                 
                 s(HUECOS.L3, k = 7) + s(INFORMATIVA.L3, k = 7) + s(PrcntOficinas.L2, k = 7) + 
                 s(Valor.L2, k = 10) + s(SEMAFOROS.L3, k = 7) + s(CicloRuta.L2, k = 7) + 
                 
                 s(LIGTH.L3, k = 8) + te(LIGTH.L3, Hora.L1, k = 8) + 
                 
                 s(TraveledKm.L2, k = 7) + s(BICIUSURS.L2, k = 7) +
                 
                 s(Hora.L1, k=7) + te(Lon, Hora.L1, k = 7) + te(Lat, Hora.L1, k = 7) +
                 s(Lon, k = 7) + s(Lat, k = 7) + s(Lon, Lat, k = 7) + 
                 s(UPlCodigo, bs = "re") + s(UPlCodigo, Year, bs = "re"),
               data=total,
               family=binomial(link=logit),
               control = list(nthreads = 4, irls.reg = 0.1, maxit = 50, mgcv.tol=1e-20, trace = T,
                              efs.tol=5e-1,  newton = list(conv.tol = 5e-1, maxHalf = 5)),
               method = "fREML")

set.seed(0)
gam.check(antiguedadModel, rep = 100)

summary(antiguedadModel)
gam.vcomp(antiguedadModel)
evaluate_gam_perf(antiguedadModel)
extract_linear_odds(antiguedadModel)

save_to_pdf("/Users/germancarvajal/Desktop/antiguedad", 
            plotOneSmooth(antiguedadModel, "AntiguedadBici.L1", xlabs = "Byciclists x1000 (count within UP)", min = 0, max = 5, minor = 0.5, major = 1) +
            xlim(0,5) + ylim(0,3),
            height = 5, width = 6.83)

# Modelo con efectos aleatorios por UPZ y Year, con todas las variables + Clima.L1----
set.seed(123)
climaModel<-bam(Gravedad2 ~ 1 + Sexo.L1 + s(Edad.L1, k = 20) + Dia.L1 + Vehiculo.L1 + s(Slope.L1, k = 10) +
                       
                       Clima.L1 +
                       
                       s(Ancho.L1, k = 4) + s(Carriles.L1, k = 4) + CicloRuta.L1 + SITP.L1 + 
                       s(Velocidad.L1, k = 4) + LTS.L1 +
                       
                       s(HUECOS.L3, k = 7) + s(INFORMATIVA.L3, k = 7) + s(PrcntOficinas.L2, k = 7) + 
                       s(Valor.L2, k = 10) + s(SEMAFOROS.L3, k = 7) + s(CicloRuta.L2, k = 7) + 
                       
                       s(LIGTH.L3, k = 8) + te(LIGTH.L3, Hora.L1, k = 8) + 
                       
                       s(TraveledKm.L2, k = 7) + s(BICIUSURS.L2, k = 7) +
                       
                       s(Hora.L1, k=7) + te(Lon, Hora.L1, k = 7) + te(Lat, Hora.L1, k = 7) +
                       s(Lon, k = 7) + s(Lat, k = 7) + s(Lon, Lat, k = 7) + 
                       s(UPlCodigo, bs = "re") + s(UPlCodigo, Year, bs = "re"),
                     data=total,
                     family=binomial(link=logit),
                     control = list(nthreads = 4, irls.reg = 0.1, maxit = 50, mgcv.tol=1e-20, trace = T,
                                    efs.tol=5e-1,  newton = list(conv.tol = 5e-1, maxHalf = 5)),
                     method = "fREML")

set.seed(0)
gam.check(climaModel, rep = 100)

summary(climaModel)
gam.vcomp(climaModel)
evaluate_gam_perf(climaModel)
extract_linear_odds(climaModel)

# Modelo con efectos aleatorios por UPZ y Year, con todas las variables + Diseno.L1----
set.seed(123)
disenoModel<-bam(Gravedad2 ~ 1 + Sexo.L1 + s(Edad.L1, k = 20) + Dia.L1 + Vehiculo.L1 + s(Slope.L1, k = 10) +
                  
                  Diseno.L1 +
                  
                  s(Ancho.L1, k = 4) + s(Carriles.L1, k = 4) + CicloRuta.L1 + SITP.L1 + 
                  s(Velocidad.L1, k = 4) + LTS.L1 +
                  
                  s(HUECOS.L3, k = 7) + s(INFORMATIVA.L3, k = 7) + s(PrcntOficinas.L2, k = 7) + 
                  s(Valor.L2, k = 10) + s(SEMAFOROS.L3, k = 7) + s(CicloRuta.L2, k = 7) + 
                  
                  s(LIGTH.L3, k = 8) + te(LIGTH.L3, Hora.L1, k = 8) + 
                  
                  s(TraveledKm.L2, k = 7) + s(BICIUSURS.L2, k = 7) +
                  
                  s(Hora.L1, k=7) + te(Lon, Hora.L1, k = 7) + te(Lat, Hora.L1, k = 7) +
                  s(Lon, k = 7) + s(Lat, k = 7) + s(Lon, Lat, k = 7) + 
                  s(UPlCodigo, bs = "re") + s(UPlCodigo, Year, bs = "re"),
                data=total,
                family=binomial(link=logit),
                control = list(nthreads = 4, irls.reg = 0.1, maxit = 50, mgcv.tol=1e-20, trace = T,
                               efs.tol=5e-1,  newton = list(conv.tol = 5e-1, maxHalf = 5)),
                method = "fREML")

set.seed(0)
gam.check(disenoModel, rep = 100)

summary(disenoModel)
gam.vcomp(disenoModel)
evaluate_gam_perf(disenoModel)
extract_linear_odds(disenoModel)

# Modelo con efectos aleatorios por UPZ y Year, con todas las variables (Hombres)----
set.seed(123)
menModel<-bam(Gravedad2 ~ 1 + s(Edad.L1, k = 20) + Dia.L1 + Vehiculo.L1 + s(Slope.L1, k = 10) +
                   
                   s(Ancho.L1, k = 4) + s(Carriles.L1, k = 4) + CicloRuta.L1 + SITP.L1 + 
                   s(Velocidad.L1, k = 4) + LTS.L1 +
                   
                   s(HUECOS.L3, k = 7) + s(INFORMATIVA.L3, k = 7) + s(PrcntOficinas.L2, k = 7) + 
                   s(Valor.L2, k = 10) + s(SEMAFOROS.L3, k = 7) + s(CicloRuta.L2, k = 7) + 
                   
                   s(LIGTH.L3, k = 8) + te(LIGTH.L3, Hora.L1, k = 8) + 
                   
                   s(TraveledKm.L2, k = 7) + s(BICIUSURS.L2, k = 7) +
                   
                   s(Hora.L1, k=7) + te(Lon, Hora.L1, k = 7) + te(Lat, Hora.L1, k = 7) +
                   s(Lon, k = 7) + s(Lat, k = 7) + s(Lon, Lat, k = 7) + 
                   s(UPlCodigo, bs = "re") + s(UPlCodigo, Year, bs = "re"),
                 data=total %>% filter(Sexo.L1==1),
                 family=binomial(link=logit),
                 control = list(nthreads = 4, irls.reg = 0.1, maxit = 50, mgcv.tol=1e-20, trace = T,
                                efs.tol=5e-1,  newton = list(conv.tol = 5e-1, maxHalf = 5)),
                 method = "fREML")

set.seed(0)
gam.check(menModel, rep = 100)

summary(menModel)
gam.vcomp(menModel)
evaluate_gam_perf(menModel)
extract_linear_odds(menModel)

plot_geo_effect(menModel, 7)
plotOneSmooth(menModel, "Edad.L1", xlabs = "Bicyclist' age (year)", min = 0, max = 100, minor = 2.5, major = 5)
plotOneSmooth(menModel, "Slope.L1", xlabs = "Terrain slope (%)", min = 0, max = .5, minor = 0.025, major = 0.05) +
  xlim(0,0.1) + ylim(0,8)
plotOneSmooth(menModel, "HUECOS.L3", xlabs = "Road potholes (count within UP)", min = 0, max = 650, minor = 50, major = 100)
plotOneSmooth(menModel, "BICIUSURS.L2", xlabs = "Byciclists x1000 (count within UP)", min = 0, max = 50, minor = 5, major = 10)
plotOneSmoothDF(time_effect_df(menModel), xlabs = "Time of occurrence (hour)", min = 0, max = 24, minor = 2, major = 4)

# Modelo con efectos aleatorios por UPZ y Year, con todas las variables (Mujeres)----
set.seed(123)
womenModel<-bam(Gravedad2 ~ 1 + s(Edad.L1, k = 20) + Dia.L1 + Vehiculo.L1 + s(Slope.L1, k = 10) +
                  
                  s(Ancho.L1, k = 4) + s(Carriles.L1, k = 4) + CicloRuta.L1 + SITP.L1 + 
                  s(Velocidad.L1, k = 4) + LTS.L1 +
                  
                  s(HUECOS.L3, k = 7) + s(INFORMATIVA.L3, k = 7) + s(PrcntOficinas.L2, k = 7) + 
                  s(Valor.L2, k = 10) + s(SEMAFOROS.L3, k = 7) + s(CicloRuta.L2, k = 7) + 
                  
                  s(LIGTH.L3, k = 8) + te(LIGTH.L3, Hora.L1, k = 8) + 
                  
                  s(TraveledKm.L2, k = 7) + s(BICIUSURS.L2, k = 7) +
                  
                  s(Hora.L1, k=7) + te(Lon, Hora.L1, k = 7) + te(Lat, Hora.L1, k = 7) +
                  s(Lon, k = 7) + s(Lat, k = 7) + s(Lon, Lat, k = 7) + 
                  s(UPlCodigo, bs = "re") + s(UPlCodigo, Year, bs = "re"),
              data=total %>% filter(Sexo.L1==0),
              family=binomial(link=logit),
              control = list(nthreads = 4, irls.reg = 0.1, maxit = 50, mgcv.tol=1e-20, trace = T,
                             efs.tol=5e-1,  newton = list(conv.tol = 5e-1, maxHalf = 5)),
              method = "fREML")

set.seed(0)
gam.check(womenModel, rep = 100)

summary(womenModel)
gam.vcomp(womenModel)
evaluate_gam_perf(womenModel)
extract_linear_odds(womenModel)

plot_geo_effect(womenModel, 7)
plot_light_effect(lighting_effect_df(womenModel))
plotOneSmooth(womenModel, "Slope.L1", xlabs = "Terrain slope (%)", min = 0, max = .5, minor = 0.025, major = 0.05) +
  xlim(0,0.1) + ylim(0,8)
plotOneSmooth(womenModel, "HUECOS.L3", xlabs = "Road potholes (count within UP)", min = 0, max = 650, minor = 50, major = 100)

# Modelo con efectos aleatorios por UPZ y Year, con todas las variables (Balanceado)----

#Over and undersampling al 30% con 5000 observaciones

set.seed(1)
balanced_sample<-over_sampling(total %>% 
                                 st_set_geometry(NULL) %>% 
                                 dplyr::select(Gravedad2, Sexo.L1, Edad.L1, Dia.L1, Vehiculo.L1, Slope.L1, 
                                               Ancho.L1, Carriles.L1, CicloRuta.L1, SITP.L1, 
                                               Velocidad.L1, LTS.L1,
                                               HUECOS.L3, INFORMATIVA.L3, PrcntOficinas.L2,
                                               Valor.L2, SEMAFOROS.L3, CicloRuta.L2, 
                                               LIGTH.L3, 
                                               TraveledKm.L2, BICIUSURS.L2,
                                               Hora.L1, Lon, Lat, 
                                               UPlCodigo, Year) %>% 
                                 as.data.frame() %>% 
                                 drop_na(),
                                 p = 0.3)

set.seed(123)
balancedModel<-bam(Gravedad2 ~ 1 + Sexo.L1 + s(Edad.L1, k = 20) + Dia.L1 + Vehiculo.L1 + s(Slope.L1, k = 10) +
                  
                  s(Ancho.L1, k = 4) + s(Carriles.L1, k = 4) + CicloRuta.L1 + SITP.L1 + 
                  s(Velocidad.L1, k = 4) + LTS.L1 +
                  
                  s(HUECOS.L3, k = 7) + s(INFORMATIVA.L3, k = 7) + s(PrcntOficinas.L2, k = 7) + 
                  s(Valor.L2, k = 10) + s(SEMAFOROS.L3, k = 7) + s(CicloRuta.L2, k = 7) + 
                  
                  s(LIGTH.L3, k = 8) + te(LIGTH.L3, Hora.L1, k = 8) + 
                  
                  s(TraveledKm.L2, k = 7) + s(BICIUSURS.L2, k = 7) +
                  
                  s(Hora.L1, k=7) + te(Lon, Hora.L1, k = 7) + te(Lat, Hora.L1, k = 7) +
                  s(Lon, k = 7) + s(Lat, k = 7) + s(Lon, Lat, k = 7) + 
                  s(UPlCodigo, bs = "re") + s(UPlCodigo, Year, bs = "re"),
                data=balanced_sample,
                family=binomial(link=logit),
                control = list(nthreads = 4, irls.reg = 0.1, maxit = 50, mgcv.tol=1e-20, trace = T,
                               efs.tol=5e-1,  newton = list(conv.tol = 5e-1, maxHalf = 5)),
                method = "fREML")

set.seed(0)
gam.check(balancedModel, rep = 100)

summary(balancedModel)
gam.vcomp(balancedModel)
evaluate_gam_perf(balancedModel)
extract_linear_odds(balancedModel)

save_to_pdf("/Users/germancarvajal/Desktop/lanewidth", 
            plotOneSmooth(balancedModel, "Ancho.L1", xlabs = "Lane width (m)", min = 0, max = 8, minor = 0.5, major = 1),
            height = 5, width = 6.83)

save_to_pdf("/Users/germancarvajal/Desktop/lanes", 
            plotOneSmooth(balancedModel, "Carriles.L1", xlabs = "Number of lanes (count)", min = 0, max = 5, minor = 1, major = 1),
            height = 5, width = 6.83)

save_to_pdf("/Users/germancarvajal/Desktop/speed", 
            plotOneSmooth(balancedModel, "Velocidad.L1", xlabs = "Average motor vehicle's speed (Km/h)", min = 0, max = 70, minor = 5, major = 10),
            height = 5, width = 6.83)

save_to_pdf("/Users/germancarvajal/Desktop/officespace", 
            plotOneSmooth(balancedModel, "PrcntOficinas.L2", xlabs = "UPZ area used as office space (%)", min = 0, max = 0.7, minor = 0.1, major = 0.2) +
              scale_x_continuous(labels = scales::percent), 
            height = 5, width = 6.83)

##### Análisis espacial ----
spatial_analysis <- list("fatal"=list(), "nofatal" = list())

#Construcción de la ventana de observación
hull <- st_read(paste0(carpetaRAS,"/BASES DE DATOS/Mapas de Referencia IDECA/MR1217.gdb"),layer='UPla', stringsAsFactors = FALSE) %>% 
  st_make_valid(.) %>% 
  st_transform(crs=4326) %>% 
  left_join(total[c("Accidente", "UPlCodigo")] %>% 
              st_set_geometry(NULL) %>% 
              group_by(UPlCodigo) %>% 
              dplyr::summarise(n = sum(!is.na(Accidente))), by = "UPlCodigo") %>% 
  filter(n>=3) %>% 
  st_transform(crs=3116) %>% 
  st_union(.) %>% 
  as_Spatial(.)

#Contruccion de plano de referencia
spatial_analysis$mvi <- st_read(paste0(carpetaRAS,"/BASES DE DATOS/Mapas de Referencia IDECA/MR1217.gdb"),layer='MVI', stringsAsFactors = FALSE) %>% 
  st_make_valid(.) %>% 
  st_transform(crs=4326) %>% 
  filter(MVITCla==1) %>% 
  st_transform(crs=3116) %>% 
  st_union(.) %>% 
  as_Spatial(.) %>% 
  as.psp(.)

spatial_analysis$mvi$marks<-NULL
spatial_analysis$mvi$markformat<-NULL

#Transformación de SF a PPP
box <- st_bbox((total %>% st_transform(crs=3116)))
spatial_analysis$windowS <- owin(xrange = c(box[1],box[3]), yrange = c(box[2], box[4]), unitname = c("meter", "meters"))
spatial_analysis$window <- as.owin(W = hull)
unitname(spatial_analysis$window) <- c("meter","meters")
points <- data.frame(st_coordinates(total %>% filter(!is.na(Gravedad2)) %>% st_transform(3116))) %>% mutate(Gravedad=factor(ifelse((total %>% filter(!is.na(Gravedad2)))$Gravedad2==0,"Nonfatal","Fatal")))
spatial_analysis$ppS <- ppp(points$X, points$Y, spatial_analysis$windowS, marks = points$Gravedad, checkdup = FALSE, drop=TRUE)
spatial_analysis$pp <- ppp(points$X, points$Y, spatial_analysis$window, marks = points$Gravedad, checkdup = FALSE, drop=TRUE)

rm(box, points, hull)

# Analisis de accidentes fatales ----
# Point distribution over fatal accidents
save_to_pdf("/Users/germancarvajal/Desktop/fatal_dist", 
            plot_distribution(split(spatial_analysis$pp)$Fatal,spatial_analysis$window,spatial_analysis$mvi), 
            height = 6.83, width = 4.8)

#Función K de Ripley
spatial_analysis$fatal$k<-lohboot(split(spatial_analysis$ppS)$Fatal, fun = "Kest", nsim = 100, confidence = 0.95, type = 8)
save_to_pdf("/Users/germancarvajal/Desktop/fatal_k", 
            plot(spatial_analysis$fatal$k, main = ""), 
            height = 4.8, width = 4.8)

#Density analysis
spatial_analysis$fatal$sigma = bw.ppl(split(spatial_analysis$pp)$Fatal)
spatial_analysis$fatal$den <- density(split(spatial_analysis$pp)$Fatal, sigma = spatial_analysis$fatal$sigma)
save_to_pdf("/Users/germancarvajal/Desktop/fatal_density", 
            plot_density(spatial_analysis$fatal$den,spatial_analysis$window,spatial_analysis$mvi), 
            height = 6.83, width = 4.8)

# Quadrat analysis
spatial_analysis$fatal$nx <- round((spatial_analysis$windowS$xrange["xmax"]-spatial_analysis$windowS$xrange["xmin"])/(1.96*spatial_analysis$fatal$sigma))
spatial_analysis$fatal$ny <- round((spatial_analysis$windowS$yrange["ymax"]-spatial_analysis$windowS$yrange["ymin"])/(1.96*spatial_analysis$fatal$sigma))
spatial_analysis$fatal$Q <- quadratcount(split(spatial_analysis$pp)$Fatal, nx = spatial_analysis$fatal$nx, ny = spatial_analysis$fatal$ny)
save_to_pdf("/Users/germancarvajal/Desktop/fatal_quadrat", 
            plot_quadrat(spatial_analysis$fatal$Q,spatial_analysis$window,spatial_analysis$mvi), 
            height = 6.83, width = 4.8)

# Quadrat test
spatial_analysis$fatal$M<-quadrat.test(split(spatial_analysis$pp)$Fatal, nx = spatial_analysis$fatal$nx, ny = spatial_analysis$fatal$ny, 
                alternative = "clustered", method="MonteCarlo", nsim = 100)
spatial_analysis$fatal$M
plot(spatial_analysis$fatal$M, main="",cex=0.3, border = "gray60", lwd = 0.1)

# Analisis de accidentes no fatales ----
# Point distribution over fatal accidents
save_to_pdf("/Users/germancarvajal/Desktop/Nonfatal_dist", 
            plot_distribution(split(spatial_analysis$pp)$Nonfatal,spatial_analysis$window,spatial_analysis$mvi), 
            height = 6.83, width = 4.8)

#Función K de Ripley
spatial_analysis$nofatal$k<-lohboot(split(spatial_analysis$ppS)$Nonfatal, fun = "Kest", nsim = 100, confidence = 0.95, type = 8)
save_to_pdf("/Users/germancarvajal/Desktop/Nonfatal_k", 
            plot(spatial_analysis$nofatal$k, main = ""), 
            height = 4.8, width = 4.8)

#Density analysis
spatial_analysis$nofatal$sigma = bw.ppl(split(spatial_analysis$pp)$Nonfatal)
spatial_analysis$nofatal$den <- density(split(spatial_analysis$pp)$Nonfatal, sigma = spatial_analysis$nofatal$sigma)
save_to_pdf("/Users/germancarvajal/Desktop/Nonfatal_density", 
            plot_density(spatial_analysis$nofatal$den,spatial_analysis$window,spatial_analysis$mvi), 
            height = 6.83, width = 4.8)

# Quadrat analysis
spatial_analysis$nofatal$nx <- round((spatial_analysis$windowS$xrange["xmax"]-spatial_analysis$windowS$xrange["xmin"])/(1.96*spatial_analysis$nofatal$sigma))
spatial_analysis$nofatal$ny <- round((spatial_analysis$windowS$yrange["ymax"]-spatial_analysis$windowS$yrange["ymin"])/(1.96*spatial_analysis$nofatal$sigma))
spatial_analysis$nofatal$Q <- quadratcount(split(spatial_analysis$pp)$Nonfatal, nx = spatial_analysis$nofatal$nx, ny = spatial_analysis$nofatal$ny)
save_to_pdf("/Users/germancarvajal/Desktop/Nonfatal_quadrat", 
            plot_quadrat(spatial_analysis$nofatal$Q,spatial_analysis$window,spatial_analysis$mvi), 
            height = 6.83, width = 4.8)

# Quadrat test
spatial_analysis$nofatal$M<-quadrat.test(split(spatial_analysis$pp)$Nonfatal, nx = spatial_analysis$nofatal$nx, ny = spatial_analysis$nofatal$ny, 
                                       alternative = "clustered", method="MonteCarlo", nsim = 100)
spatial_analysis$nofatal$M
plot(spatial_analysis$nofatal$M, main="",cex=0.3, border = "gray60", lwd = 0.1)