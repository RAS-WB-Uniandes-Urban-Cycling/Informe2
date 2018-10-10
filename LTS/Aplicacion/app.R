
library(shiny)
library(leaflet)
library(tidyverse)
library(sf)
library(shinythemes)
library(shinyjs)
library(graphics)



#setwd("/Users/alejandropalacio/Desktop/Aplicacion/Data")
LTS <- readRDS("Data/LTS_Bogota.rds")
Intersecciones <-readRDS("Data/Intersecciones.rds")
Accidentes <- readRDS("Data/Accidentes_LTS_Bogota.rds")
capa_Origen_Propension <- readRDS("Data/Capa_Origen_Propension.rds")
capa_Destino_Propension <- readRDS("Data/Capa_Destino_Propension.rds")
capa_Ruta_Propension <- readRDS("Data/Capa_Ruta_Propension.rds")
capa_Camino_Propension <- readRDS("Data/Capa_Camino_Propension.rds")
UPZ_Accidentes <- readRDS("Data/UPZ.rds")



LTS <- LTS %>% st_as_sf() %>% mutate(Cluster=ifelse(X4>=0.5,1,ifelse(X3>=0.5,2,ifelse(X2>=0.5,3,4))))


# Define UI for application 
ui <- navbarPage("Mapas Interactivos",

  tabPanel("LTS Vias",
              
        div(class="outer",
               
            tags$head(includeCSS("Data//styles.css")),
           
            leafletOutput("MiniMap", height = "100%", width = "100%"),
    
            absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
            draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
            width = 330, height = "auto",
                  
            selectInput(inputId = "Localidad",label = "Seleccionar Localidad:",
            choices = c("ANTONIO NARIÑO","BARRIOS UNIDOS","BOSA","CANDELARIA","CHAPINERO","CIUDAD BOLIVAR","ENGATIVA","FONTIBON","KENNEDY","LOS MARTIRES","PUENTE ARANDA","RAFAEL URIBE URIBE",
            "SAN CRISTOBAL","SANTA FE","SUBA","SUMAPAZ","TEUSAQUILLO","TUNJUELITO", "USAQUEN","USME")),
      
            selectInput( inputId = "Variable", label = "Seleccionar Capa:",
            choices = c("Level of Traffic Stress","Ancho de la vía","Número de Carriles","Presencia CicloRuta","Presencia SITP","Velocidad","Congestión Vehicular","Densidad Vehicular","Flujo Vehicular")),
      
            plotOutput("Grafica",height = 250),
            helpText(" "),
            plotOutput("Grafica2",height = 250))
        )
    ),
  
  tabPanel("LTS Intersecciones",
      
        div(class="outer", 
            
            tags$head(includeCSS("Data//styles.css")),
            
            leafletOutput("MiniMap_2", height = "100%", width = "100%"),
            
            absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
            draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
            width = 330, height = "auto",
                          
            selectInput(inputId = "Localidad_2",label = "Seleccionar Localidad:",
            choices = c("ANTONIO NARIÑO","BARRIOS UNIDOS","BOSA","CANDELARIA","CHAPINERO","CIUDAD BOLIVAR","ENGATIVA","FONTIBON","KENNEDY","LOS MARTIRES","PUENTE ARANDA","RAFAEL URIBE URIBE",
            "SAN CRISTOBAL","SANTA FE","SUBA","SUMAPAZ","TEUSAQUILLO","TUNJUELITO", "USAQUEN","USME")),
            
            plotOutput("Grafica3",height = 250),
            helpText(" "),
            plotOutput("Grafica4",height = 250)
            
            )
 
       
    )       
  ),
  
  tabPanel("Accidentalidad",
           
           div(class="outer", 
               
               tags$head(includeCSS("Data//styles.css")),
               
               leafletOutput("MiniMap_3", height = "100%", width = "100%"),
               
               absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                             draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                             width = 330, height = "auto",
                             
                             selectInput(inputId = "Localidad_3",label = "Seleccionar Localidad:",
                                         choices = c("ANTONIO NARIÑO","BARRIOS UNIDOS","BOSA","CANDELARIA","CHAPINERO","CIUDAD BOLIVAR","ENGATIVA","FONTIBON","KENNEDY","LOS MARTIRES","PUENTE ARANDA","RAFAEL URIBE URIBE",
                                                     "SAN CRISTOBAL","SANTA FE","SUBA","SUMAPAZ","TEUSAQUILLO","TUNJUELITO", "USAQUEN","USME")),
                             
                             
                             checkboxInput("Muertes", "Accidentes Mortales", TRUE),
                             checkboxInput("NoMuertes", "Accidentes No Mortales", TRUE),
                             
                             helpText("Estadistica Localidad"),
                             plotOutput("Grafica5",height = 230),
                             helpText("Estadistica Bogotá"),
                             plotOutput("Grafica6",height = 230)
               )
               
               
           )       
  ),
  
  
  tabPanel("Buffers ODR Bici Usuarios",
           
           div(class="outer", 
               
               tags$head(includeCSS("Data//styles.css")),
               
               leafletOutput("MiniMap_4", height = "100%", width = "100%"),
               
               absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                             draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                             width = 330, height = "auto",
                             
                             selectInput(inputId = "Localidad_4",label = "Seleccionar Localidad:",
                                         choices = c("ANTONIO NARIÑO","BARRIOS UNIDOS","BOSA","CANDELARIA","CHAPINERO","CIUDAD BOLIVAR","ENGATIVA","FONTIBON","KENNEDY","LOS MARTIRES","PUENTE ARANDA","RAFAEL URIBE URIBE",
                                                     "SAN CRISTOBAL","SANTA FE","SUBA","SUMAPAZ","TEUSAQUILLO","TUNJUELITO", "USAQUEN","USME")),
                             
                             sliderInput("Cantidad", "Seleccionar la cantidad de viajes:",ticks = FALSE,animate = TRUE, min=1, max=1, value=1,step = 1),
                             checkboxInput("Buffers", "Mostrar Buffers", TRUE)
                
               )
               
               
           )       
  ),
  
  
  tabPanel("UPZ Accidentalidad",
           
           div(class="outer", 
               
               tags$head(includeCSS("Data//styles.css")),
               
               leafletOutput("MiniMap_5", height = "100%", width = "100%"),
               
               absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                             draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                             width = 330, height = "auto",
                             
                             
                             selectInput(inputId = "TipoAccidente",label = "Seleccionar Tipo Accidente:",
                                         choices = c("Accidentes","Accidentes Fatales","Accidentes No Fatales")),
                             
                             sliderInput("Ano", "Seleccionar el año:",ticks = FALSE,animate = TRUE, min=2011, max=2017, value=1,step = 1)
                             
               )
               
               
           )       
  )
  
  
  
  
  
)

server <- function(input, output,session) {
  
  output$MiniMap <- renderLeaflet({
    
    #a <- capa %>% filter(LocNombre==input$Localidad)
    a <- LTS %>% filter(LocNombre==input$Localidad)
    
    if (input$Variable=="Ancho de la vía") {
      
      pal <- colorNumeric("PuRd", a$Ancho)
      
      leaflet(a) %>% addTiles() %>% addProviderTiles("OpenStreetMap.BlackAndWhite")  %>%
        addPolylines(color =~pal(Ancho),opacity = 1,smoothFactor = 1) %>% addLegend("bottomleft", pal = pal, values = ~Ancho, title = "Ancho de la vía (m)",opacity = 1)
      
    }
    else if (input$Variable=="Número de Carriles") {
      
      pal <- colorFactor("Purples", a$Carriles)
      
      leaflet(a) %>% addTiles() %>% addProviderTiles("OpenStreetMap.BlackAndWhite")  %>%
        addPolylines(color =~pal(Carriles),opacity = 1,smoothFactor = 1) %>% addLegend("bottomleft", pal = pal, values = ~Carriles, title = "Número Carriles",opacity = 1)
      
    }
    
    else if (input$Variable=="Presencia CicloRuta") {
      
      pal <- colorFactor("Spectral", a$CicloRuta)
      
      leaflet(a) %>% addTiles() %>% addProviderTiles("OpenStreetMap.BlackAndWhite")  %>%
        addPolylines(color =~pal(CicloRuta),opacity = 1,smoothFactor = 1) %>% addLegend("bottomleft", pal = pal, values = ~CicloRuta, title = "Presencia de Cicloruta",opacity = 1)
      
    }
    
    else if (input$Variable=="Presencia SITP") {
      
      pal <- colorFactor("Spectral", a$SITP)
      
      leaflet(a) %>% addTiles() %>% addProviderTiles("OpenStreetMap.BlackAndWhite")  %>%
        addPolylines(color =~pal(SITP),opacity = 1,smoothFactor = 1) %>% addLegend("bottomleft", pal = pal, values = ~SITP, title = "Presencia SITP",opacity = 1)
      
    }
    
    else if(input$Variable=="Velocidad"){
    
            pal <- colorNumeric(palette = "YlOrRd", a$Velocidad)
            
            leaflet(a) %>% addTiles() %>% addProviderTiles("OpenStreetMap.BlackAndWhite")  %>%
              addPolylines(color =~pal(Velocidad),opacity = 1,smoothFactor = 1) %>% addLegend("bottomleft", pal = pal, values = ~Velocidad, title = "Velocidad (km/h)",opacity = 1)
            
    } 
    
    else if(input$Variable=="Congestión Vehicular"){
      
      pal <- colorNumeric(palette = "YlOrRd", a$Congestion)
      
      leaflet(a) %>% addTiles() %>% addProviderTiles("OpenStreetMap.BlackAndWhite")  %>%
        addPolylines(color =~pal(Congestion),opacity = 1,smoothFactor = 1) %>% addLegend("bottomleft", pal = pal, values = ~Congestion, title = "Congestión Vehicular",opacity = 1)
      
    } 
    
    else if(input$Variable=="Densidad Vehicular"){
      
      pal <- colorNumeric(palette = "YlOrRd", a$Densidad)
      
      leaflet(a) %>% addTiles() %>% addProviderTiles("OpenStreetMap.BlackAndWhite")  %>%
        addPolylines(color =~pal(Densidad),opacity = 1,smoothFactor = 1) %>% addLegend("bottomleft", pal = pal, values = ~Densidad, title = "Densidad Vehicular",opacity = 1)
      
    } 
    
    else if(input$Variable=="Flujo Vehicular"){
      
      pal <- colorNumeric(palette = "YlOrRd", a$Flujo)
      
      leaflet(a) %>% addTiles() %>% addProviderTiles("OpenStreetMap.BlackAndWhite")  %>%
        addPolylines(color =~pal(Flujo),opacity = 1,smoothFactor = 1) %>% addLegend("bottomleft", pal = pal, values = ~Flujo, title = "Flujo Vehicular",opacity = 1)
      
    } 
    else if (input$Variable=="Level of Traffic Stress") {
      
      pal <- colorFactor(c("lime green","dark green","orange","Red"), a$Cluster)
      
      leaflet(a) %>% addTiles() %>% addProviderTiles("OpenStreetMap.BlackAndWhite")  %>%
        addPolylines(color =~pal(Cluster),opacity = 1,smoothFactor = 1) %>% addLegend("bottomleft", pal = pal, values = ~Cluster, title = "LTS",opacity = 1)
    }
    
      
  })
  
  output$Grafica <- renderPlot({
    
    #a <- capa %>% filter(LocNombre==input$Localidad)
    a <- LTS %>% filter(LocNombre==input$Localidad)

    if (input$Variable=="Ancho de la vía") {
      
      boxplot(a$Ancho, col = "lightskyblue", main="Ancho de la vía", ylab="Ancho (m)")
      
    }
    else if (input$Variable=="Número de Carriles") {
      
      boxplot(a$Carriles, col = "lightskyblue", main="Número de Carriles", ylab="Carriles")
    }
    
    else if (input$Variable=="Presencia CicloRuta") {
      
      
      pie(table(a$CicloRuta),labels =table(a$CicloRuta) ,main="Presencia de Cicloruta",radius = 1,col = rainbow(4))
    }
    
    else if (input$Variable=="Presencia SITP") {
      
      pie(table(a$SITP),labels =table(a$SITP) ,main="Presencia de SITP",radius = 1,col = rainbow(4))
     
    }
    else if(input$Variable=="Velocidad"){
      
      boxplot(a$Velocidad, col = "lightskyblue", main="Velocidad", ylab="Velocidad (km/h)")
      
    } 
    else if(input$Variable=="Congestión Vehicular"){
      
      boxplot(a$Congestion, col = "lightskyblue", main="Congestión Vehicular", ylab="Congestión")
      
    } 
    else if(input$Variable=="Densidad Vehicular"){
      
      boxplot(a$Densidad, col = "lightskyblue", main="Densidad Vehicular", ylab="Densidad")
      
    } 
    else if(input$Variable=="Flujo Vehicular"){
      
      boxplot(a$Flujo, col = "lightskyblue", main="Flujo Vehicular", ylab="Flujo")
      
    } 
    else if (input$Variable=="Level of Traffic Stress") {
      
      pie(table(a$Cluster),labels =table(a$Cluster) ,main="Level of Traffic Stress",radius = 1,col =  c("lime green","dark green","orange","Red"))
      
    }
    
    
  })

    output$Grafica2 <- renderPlot({
      
      #a <- capa %>% filter(LocNombre==input$Localidad)
      
      a <- LTS %>% filter(LocNombre==input$Localidad)

      if (input$Variable=="Ancho de la vía") {
        
        hist(a$Ancho, col = "lightskyblue", xlab="Ancho",ylab="Frecuencia", main = "Histograma Ancho de la Vía") 
      
      }
      else if (input$Variable=="Número de Carriles") {
        
        hist(a$Carriles, col = "lightskyblue", xlab="Carriles",ylab="Frecuencia", main = "Histograma Número de Carriles") 
        
      }
      
      else if (input$Variable=="Presencia CicloRuta") {
        
        pie(table(a$CicloRuta),labels =c("No","Si") ,main="Presencia de Cicloruta",radius = 1,col = rainbow(4))
        
      }
      
      else if (input$Variable=="Presencia SITP") {
        
        pie(table(a$SITP),labels =c("No","Si") ,main="Presencia de SITP",radius = 1,col = rainbow(4))
      }
      else if(input$Variable=="Velocidad"){
        
        hist(a$Velocidad, col = "lightskyblue", xlab="Velocidad",ylab="Frecuencia", main = "Histograma Velocidad") 
      } 
      else if(input$Variable=="Congestión Vehicular"){
        
        hist(a$Congestion, col = "lightskyblue", xlab="Congestión",ylab="Frecuencia", main = "Histograma Congestion Vehicular") 
      } 
      else if(input$Variable=="Densidad Vehicular"){
        
        hist(a$Densidad, col = "lightskyblue", xlab="Densidad",ylab="Frecuencia", main = "Histograma Densidad Vehicular") 
      } 
      else if(input$Variable=="Flujo Vehicular"){
        
        hist(a$Flujo, col = "lightskyblue", xlab="Flujo",ylab="Frecuencia", main = "Histograma Flujo Vehicular") 
      }
      else if (input$Variable=="Level of Traffic Stress") {
        
        pie(table(a$Cluster),labels =c("LTS 1","LTS 2","LTS 3","LTS 4") ,main="Level of Traffic Stress",radius = 1,col = c("lime green","dark green","orange","Red"))
      }
    
  })
  
    output$MiniMap_2 <- renderLeaflet({
      
      a <- LTS %>% filter(LocNombre==input$Localidad_2)
      b <- Intersecciones %>% filter(LocNombre==input$Localidad_2) 
      pal <- colorFactor(c("lime green","dark green","orange","Red"), b$Cluster)
      
      leaflet(b) %>% addTiles() %>% addProviderTiles("OpenStreetMap.BlackAndWhite")  %>% addPolylines(data=a) %>% 
      addCircles(color =~pal(Cluster),opacity = 1)%>% addLegend("bottomleft", pal = pal, values = ~Cluster, title = "LTS",opacity = 1)
        
     
    })
    
    
    output$Grafica3 <- renderPlot({
      
      b <- Intersecciones %>% filter(LocNombre==input$Localidad_2) 
      pie(table(b$Cluster),labels =table(b$Cluster) ,main="Level of Traffic Stress",radius = 1,col =  c("lime green","dark green","orange","Red"))
      
    })

    
    output$Grafica4 <- renderPlot({
      
      b <- Intersecciones %>% filter(LocNombre==input$Localidad_2) 
      pie(table(b$Cluster),labels =c("LTS 1","LTS 2","LTS 3","LTS 4") ,main="Level of Traffic Stress",radius = 1,col =  c("lime green","dark green","orange","Red"))
      
    })
    
    
    output$MiniMap_3 <- renderLeaflet({
      
      a <- LTS %>% filter(LocNombre==input$Localidad_3)
      
      pal <- colorFactor(c("lime green","dark green","orange","Red"), a$Cluster)
      
      
      if(input$Muertes==TRUE & input$NoMuertes==FALSE ){
        
        b <- Accidentes %>% filter(LocNombre==input$Localidad_3,Gravedad2=="Dead")
        
        leaflet(a) %>% addTiles() %>% addProviderTiles("OpenStreetMap.BlackAndWhite")  %>%
          addPolylines(color =~pal(Cluster),opacity = 1,smoothFactor = 1) %>% addCircles(data=b)%>% addLegend("bottomleft", pal = pal, values = ~Cluster, title = "LTS",opacity = 1)
        
      }
      else if(input$Muertes==FALSE & input$NoMuertes==TRUE ){
        
        b <- Accidentes %>% filter(LocNombre==input$Localidad_3,Gravedad2=="Not Dead")
        
        leaflet(a) %>% addTiles() %>% addProviderTiles("OpenStreetMap.BlackAndWhite")  %>%
          addPolylines(color =~pal(Cluster),opacity = 1,smoothFactor = 1) %>% addCircles(data=b)%>% addLegend("bottomleft", pal = pal, values = ~Cluster, title = "LTS",opacity = 1)
        
      }
      else if(input$Muertes==TRUE & input$NoMuertes==TRUE ){
        
        b <- Accidentes %>% filter(LocNombre==input$Localidad_3)
        
        leaflet(a) %>% addTiles() %>% addProviderTiles("OpenStreetMap.BlackAndWhite")  %>%
          addPolylines(color =~pal(Cluster),opacity = 1,smoothFactor = 1) %>% addCircles(data=b)%>% addLegend("bottomleft", pal = pal, values = ~Cluster, title = "LTS",opacity = 1)
      }
      else{
        
        leaflet(a) %>% addTiles() %>% addProviderTiles("OpenStreetMap.BlackAndWhite")  %>%
          addPolylines(color =~pal(Cluster),opacity = 1,smoothFactor = 1) %>% addLegend("bottomleft", pal = pal, values = ~Cluster, title = "LTS",opacity = 1)
        
      }

    })
    
    output$Grafica5 <- renderPlot({
      
    
      if(input$Muertes==TRUE & input$NoMuertes==FALSE ){
        
        b <- Accidentes %>% filter(LocNombre==input$Localidad_3,Gravedad2=="Dead") %>% st_set_geometry(NULL)

        barplot(table(b$Cluster), main="Accidentes Mortales vs LTS", xlab="LTS", ylab = "Frecuencia",col = "lightskyblue")
      
      }
      else if(input$Muertes==FALSE & input$NoMuertes==TRUE ){
        
        b <- Accidentes %>% filter(LocNombre==input$Localidad_3,Gravedad2=="Not Dead")
        
        barplot(table(b$Cluster), main="Accidentes No Mortales vs LTS", xlab="LTS", ylab = "Frecuencia",col = "lightskyblue")
        
      }
      else if(input$Muertes==TRUE & input$NoMuertes==TRUE ){
        
        b <- Accidentes %>% filter(LocNombre==input$Localidad_3)
        
        barplot(table(b$Cluster), main="Accidentes vs LTS", xlab="LTS", ylab = "Frecuencia",col = "lightskyblue")
        
      }

      
    })
    
    
    output$Grafica6 <- renderPlot({
      
      if(input$Muertes==TRUE & input$NoMuertes==FALSE ){
        
        b <- Accidentes %>% filter(Gravedad2=="Dead") %>% st_set_geometry(NULL)
        
        barplot(table(b$Cluster), main="Accidentes Mortales vs LTS", xlab="LTS", ylab = "Frecuencia",col = "lightskyblue")
        
      }
      else if(input$Muertes==FALSE & input$NoMuertes==TRUE ){
        
        b <- Accidentes %>% filter(Gravedad2=="Not Dead")
        
        barplot(table(b$Cluster), main="Accidentes No Mortales vs LTS", xlab="LTS", ylab = "Frecuencia",col = "lightskyblue")
        
      }
      else if(input$Muertes==TRUE & input$NoMuertes==TRUE ){
        
        b <- Accidentes
        
        barplot(table(b$Cluster), main="Accidentes vs LTS", xlab="LTS", ylab = "Frecuencia",col = "lightskyblue")
        
      }
    })
    
    
    output$MiniMap_4 <- renderLeaflet({
      
      origen <- head(capa_Origen_Propension %>% filter(LocNombre==input$Localidad_4),input$Cantidad)
      destino <- head(capa_Destino_Propension %>% filter(LocNombre==input$Localidad_4),input$Cantidad)
      ruta <- head(capa_Ruta_Propension %>% filter(LocNombre==input$Localidad_4),input$Cantidad)
      camino <- head(capa_Camino_Propension %>% filter(LocNombre==input$Localidad_4),input$Cantidad)
      
      origen_labels <- paste0("<strong> Caracteristicas</strong> <br>Numero Accidentes: ", origen$NAccidentes, "<br>Numero Paraderos SITP: ",origen$NSiTP,
                            "<br>Numero Estaciones TM: ",origen$NTM,"<br>Numero Ciclo Parqueaderos: ",origen$NCPark,
                            "<br>Metros Cicloruta: ",round(origen$CiclR,0),"<br> Porcentaje LTS 1: ",round(origen$`LTS 1`,2),
                            "<br>Porcentaje LTS 2: ",round(origen$`LTS 2`,2),"<br>Porcentaje LTS 3: ",round(origen$`LTS 3`,2),
                            "<br>Porcentaje LTS 4: ",round(origen$`LTS 4`,2))
      
      destino_labels <- paste0("<strong> Caracteristicas</strong> <br>Numero Accidentes: ", destino$NAccidentes, "<br>Numero Paraderos SITP: ",destino$NSiTP,
                              "<br>Numero Estaciones TM: ",destino$NTM,"<br>Numero Ciclo Parqueaderos: ",destino$NCPark,
                              "<br>Metros Cicloruta: ",round(destino$CiclR,0),"<br> Porcentaje LTS 1: ",round(destino$`LTS 1`,2),
                              "<br>Porcentaje LTS 2: ",round(destino$`LTS 2`,2),"<br>Porcentaje LTS 3: ",round(destino$`LTS 3`,2),
                              "<br>Porcentaje LTS 4: ",round(destino$`LTS 4`,2))
      
      
      ruta_labels <- paste0("<strong> Caracteristicas</strong> <br>Numero Accidentes: ", ruta$NAccidentes, "<br>Numero Paraderos SITP: ",ruta$NSiTP,
                               "<br>Numero Estaciones TM: ",ruta$NTM,"<br>Numero Ciclo Parqueaderos: ",ruta$NCPark,
                               "<br>Metros Cicloruta: ",round(ruta$CiclR,0),"<br> Porcentaje LTS 1: ",round(ruta$`LTS 1`,2),
                               "<br>Porcentaje LTS 2: ",round(ruta$`LTS 2`,2),"<br>Porcentaje LTS 3: ",round(ruta$`LTS 3`,2),
                               "<br>Porcentaje LTS 4: ",round(ruta$`LTS 4`,2))

      if(input$Buffers==TRUE){
        leaflet() %>% addTiles() %>% addProviderTiles("OpenStreetMap.BlackAndWhite")   %>% addPolylines(data = camino,color = "Black")%>%  addPolygons(data = ruta,popup =ruta_labels) %>%
          addPolygons(data = origen, color = "Green",popup= origen_labels) %>% addPolygons(data = destino,color = "Red",popup= destino_labels)
      }else{
        leaflet() %>% addTiles() %>% addProviderTiles("OpenStreetMap.BlackAndWhite")   %>% addPolylines(data = camino,color = "Black")
      }
     
      
    })
    
    observe({
      updateSliderInput(session, "Cantidad", value = 1,
                        min = 1, max = nrow(capa_Origen_Propension %>% filter(LocNombre==input$Localidad_4)))
    })
    
    
    output$MiniMap_5 <- renderLeaflet({
      
      UPZ_Accidentes <- UPZ_Accidentes %>% filter(!(UPlNmbr %in% c("UPR RIO BLANCO","UPR RIO SUMAPAZ","CIUDAD USME","LA FLORA","MONTE BLANCO","PARQUE ENTRENUBES")))
      
      if(input$Ano==2011){
        
        if(input$TipoAccidente=="Accidentes"){
        
          pal <- colorNumeric(palette = "YlOrRd", UPZ_Accidentes$X2011)
          
          leaflet(UPZ_Accidentes) %>% addTiles() %>% addProviderTiles("OpenStreetMap.BlackAndWhite")  %>%
            addPolygons(color =~pal(X2011),fillOpacity = 0.7,smoothFactor = 1,stroke=FALSE) %>% addLegend("bottomleft", pal = pal, values = ~X2011, title = "Cantidad Accidentes",opacity = 1)
        }
        
        else if(input$TipoAccidente=="Accidentes Fatales"){
            
          pal <- colorNumeric(palette = "YlOrRd", UPZ_Accidentes$X2011_Dd)
              
          leaflet(UPZ_Accidentes) %>% addTiles() %>% addProviderTiles("OpenStreetMap.BlackAndWhite")  %>%
            addPolygons(color =~pal(X2011_Dd),fillOpacity = 0.7,smoothFactor = 1,stroke=FALSE) %>% addLegend("bottomleft", pal = pal, values = ~X2011_Dd, title = "Cantidad Accidentes",opacity = 1)
        }
          
        else{
            
          pal <- colorNumeric(palette = "YlOrRd", UPZ_Accidentes$X2011_ND)
            
          leaflet(UPZ_Accidentes) %>% addTiles() %>% addProviderTiles("OpenStreetMap.BlackAndWhite")  %>%
            addPolygons(color =~pal(X2011_ND),fillOpacity = 0.7,smoothFactor = 1,stroke=FALSE) %>% addLegend("bottomleft", pal = pal, values = ~X2011_ND, title = "Cantidad Accidentes",opacity = 1)
            
        }
          
          
      }
      
      else if(input$Ano==2012){
        
        if(input$TipoAccidente=="Accidentes"){
          
          pal <- colorNumeric(palette = "YlOrRd", UPZ_Accidentes$X2012)
          
          leaflet(UPZ_Accidentes) %>% addTiles() %>% addProviderTiles("OpenStreetMap.BlackAndWhite")  %>%
            addPolygons(color =~pal(X2012),fillOpacity = 0.7,smoothFactor = 1,stroke=FALSE) %>% addLegend("bottomleft", pal = pal, values = ~X2012, title = "Cantidad Accidentes",opacity = 1)
        }
        
        else if(input$TipoAccidente=="Accidentes Fatales"){
          
          pal <- colorNumeric(palette = "YlOrRd", UPZ_Accidentes$X2012_Dd)
          
          leaflet(UPZ_Accidentes) %>% addTiles() %>% addProviderTiles("OpenStreetMap.BlackAndWhite")  %>%
            addPolygons(color =~pal(X2012_Dd),fillOpacity = 0.7,smoothFactor = 1,stroke=FALSE) %>% addLegend("bottomleft", pal = pal, values = ~X2012_Dd, title = "Cantidad Accidentes",opacity = 1)
        }
        
        else{
          
          pal <- colorNumeric(palette = "YlOrRd", UPZ_Accidentes$X2012_ND)
          
          leaflet(UPZ_Accidentes) %>% addTiles() %>% addProviderTiles("OpenStreetMap.BlackAndWhite")  %>%
            addPolygons(color =~pal(X2012_ND),fillOpacity = 0.7,smoothFactor = 1,stroke=FALSE) %>% addLegend("bottomleft", pal = pal, values = ~X2012_ND, title = "Cantidad Accidentes",opacity = 1)
          
        }
        
        
      }
      
      else if(input$Ano==2013){
        
        if(input$TipoAccidente=="Accidentes"){
          
          pal <- colorNumeric(palette = "YlOrRd", UPZ_Accidentes$X2013)
          
          leaflet(UPZ_Accidentes) %>% addTiles() %>% addProviderTiles("OpenStreetMap.BlackAndWhite")  %>%
            addPolygons(color =~pal(X2013),fillOpacity = 0.7,smoothFactor = 1,stroke=FALSE) %>% addLegend("bottomleft", pal = pal, values = ~X2013, title = "Cantidad Accidentes",opacity = 1)
        }
        
        else if(input$TipoAccidente=="Accidentes Fatales"){
          
          pal <- colorNumeric(palette = "YlOrRd", UPZ_Accidentes$X2013_Dd)
          
          leaflet(UPZ_Accidentes) %>% addTiles() %>% addProviderTiles("OpenStreetMap.BlackAndWhite")  %>%
            addPolygons(color =~pal(X2013_Dd),fillOpacity = 0.7,smoothFactor = 1,stroke=FALSE) %>% addLegend("bottomleft", pal = pal, values = ~X2013_Dd, title = "Cantidad Accidentes",opacity = 1)
        }
        
        else{
          
          pal <- colorNumeric(palette = "YlOrRd", UPZ_Accidentes$X2013_ND)
          
          leaflet(UPZ_Accidentes) %>% addTiles() %>% addProviderTiles("OpenStreetMap.BlackAndWhite")  %>%
            addPolygons(color =~pal(X2013_ND),fillOpacity = 0.7,smoothFactor = 1,stroke=FALSE) %>% addLegend("bottomleft", pal = pal, values = ~X2013_ND, title = "Cantidad Accidentes",opacity = 1)
          
        }
        
        
      }
      
      else if(input$Ano==2014){
        
        if(input$TipoAccidente=="Accidentes"){
          
          pal <- colorNumeric(palette = "YlOrRd", UPZ_Accidentes$X2014)
          
          leaflet(UPZ_Accidentes) %>% addTiles() %>% addProviderTiles("OpenStreetMap.BlackAndWhite")  %>%
            addPolygons(color =~pal(X2014),fillOpacity = 0.7,smoothFactor = 1,stroke=FALSE) %>% addLegend("bottomleft", pal = pal, values = ~X2014, title = "Cantidad Accidentes",opacity = 1)
        }
        
        else if(input$TipoAccidente=="Accidentes Fatales"){
          
          pal <- colorNumeric(palette = "YlOrRd", UPZ_Accidentes$X2014_Dd)
          
          leaflet(UPZ_Accidentes) %>% addTiles() %>% addProviderTiles("OpenStreetMap.BlackAndWhite")  %>%
            addPolygons(color =~pal(X2014_Dd),fillOpacity = 0.7,smoothFactor = 1,stroke=FALSE) %>% addLegend("bottomleft", pal = pal, values = ~X2014_Dd, title = "Cantidad Accidentes",opacity = 1)
        }
        
        else{
          
          pal <- colorNumeric(palette = "YlOrRd", UPZ_Accidentes$X2014_ND)
          
          leaflet(UPZ_Accidentes) %>% addTiles() %>% addProviderTiles("OpenStreetMap.BlackAndWhite")  %>%
            addPolygons(color =~pal(X2014_ND),fillOpacity = 0.7,smoothFactor = 1,stroke=FALSE) %>% addLegend("bottomleft", pal = pal, values = ~X2014_ND, title = "Cantidad Accidentes",opacity = 1)
          
        }
        
        
      }
      
      else if(input$Ano==2015){
        
        if(input$TipoAccidente=="Accidentes"){
          
          pal <- colorNumeric(palette = "YlOrRd", UPZ_Accidentes$X2015)
          
          leaflet(UPZ_Accidentes) %>% addTiles() %>% addProviderTiles("OpenStreetMap.BlackAndWhite")  %>%
            addPolygons(color =~pal(X2015),fillOpacity = 0.7,smoothFactor = 1,stroke=FALSE) %>% addLegend("bottomleft", pal = pal, values = ~X2015, title = "Cantidad Accidentes",opacity = 1)
        }
        
        else if(input$TipoAccidente=="Accidentes Fatales"){
          
          pal <- colorNumeric(palette = "YlOrRd", UPZ_Accidentes$X2015_Dd)
          
          leaflet(UPZ_Accidentes) %>% addTiles() %>% addProviderTiles("OpenStreetMap.BlackAndWhite")  %>%
            addPolygons(color =~pal(X2015_Dd),fillOpacity = 0.7,smoothFactor = 1,stroke=FALSE) %>% addLegend("bottomleft", pal = pal, values = ~X2015_Dd, title = "Cantidad Accidentes",opacity = 1)
        }
        
        else{
          
          pal <- colorNumeric(palette = "YlOrRd", UPZ_Accidentes$X2015_ND)
          
          leaflet(UPZ_Accidentes) %>% addTiles() %>% addProviderTiles("OpenStreetMap.BlackAndWhite")  %>%
            addPolygons(color =~pal(X2015_ND),fillOpacity = 0.7,smoothFactor = 1,stroke=FALSE) %>% addLegend("bottomleft", pal = pal, values = ~X2015_ND, title = "Cantidad Accidentes",opacity = 1)
          
        }
        
        
      }
      
      else if(input$Ano==2016){
        
        if(input$TipoAccidente=="Accidentes"){
          
          pal <- colorNumeric(palette = "YlOrRd", UPZ_Accidentes$X2016)
          
          leaflet(UPZ_Accidentes) %>% addTiles() %>% addProviderTiles("OpenStreetMap.BlackAndWhite")  %>%
            addPolygons(color =~pal(X2016),fillOpacity = 0.7,smoothFactor = 1,stroke=FALSE) %>% addLegend("bottomleft", pal = pal, values = ~X2016, title = "Cantidad Accidentes",opacity = 1)
        }
        
        else if(input$TipoAccidente=="Accidentes Fatales"){
          
          pal <- colorNumeric(palette = "YlOrRd", UPZ_Accidentes$X2016_Dd)
          
          leaflet(UPZ_Accidentes) %>% addTiles() %>% addProviderTiles("OpenStreetMap.BlackAndWhite")  %>%
            addPolygons(color =~pal(X2016_Dd),fillOpacity = 0.7,smoothFactor = 1,stroke=FALSE) %>% addLegend("bottomleft", pal = pal, values = ~X2016_Dd, title = "Cantidad Accidentes",opacity = 1)
        }
        
        else{
          
          pal <- colorNumeric(palette = "YlOrRd", UPZ_Accidentes$X2016_ND)
          
          leaflet(UPZ_Accidentes) %>% addTiles() %>% addProviderTiles("OpenStreetMap.BlackAndWhite")  %>%
            addPolygons(color =~pal(X2016_ND),fillOpacity = 0.7,smoothFactor = 1,stroke=FALSE) %>% addLegend("bottomleft", pal = pal, values = ~X2016_ND, title = "Cantidad Accidentes",opacity = 1)
          
        }
        
        
      }
      
      else if(input$Ano==2017){
        
        if(input$TipoAccidente=="Accidentes"){
          
          pal <- colorNumeric(palette = "YlOrRd", UPZ_Accidentes$X2017)
          
          leaflet(UPZ_Accidentes) %>% addTiles() %>% addProviderTiles("OpenStreetMap.BlackAndWhite")  %>%
            addPolygons(color =~pal(X2017),fillOpacity = 0.7,smoothFactor = 1,stroke=FALSE) %>% addLegend("bottomleft", pal = pal, values = ~X2017, title = "Cantidad Accidentes",opacity = 1)
        }
        
        else if(input$TipoAccidente=="Accidentes Fatales"){
          
          pal <- colorNumeric(palette = "YlOrRd", UPZ_Accidentes$X2017_Dd)
          
          leaflet(UPZ_Accidentes) %>% addTiles() %>% addProviderTiles("OpenStreetMap.BlackAndWhite")  %>%
            addPolygons(color =~pal(X2017_Dd),fillOpacity = 0.7,smoothFactor = 1,stroke=FALSE) %>% addLegend("bottomleft", pal = pal, values = ~X2017_Dd, title = "Cantidad Accidentes",opacity = 1)
        }
        
        else{
          
          pal <- colorNumeric(palette = "YlOrRd", UPZ_Accidentes$X2017_ND)
          
          leaflet(UPZ_Accidentes) %>% addTiles() %>% addProviderTiles("OpenStreetMap.BlackAndWhite")  %>%
            addPolygons(color =~pal(X2017_ND),fillOpacity = 0.7,smoothFactor = 1,stroke=FALSE) %>% addLegend("bottomleft", pal = pal, values = ~X2017_ND, title = "Cantidad Accidentes",opacity = 1)
          
        }
        
        
      }
      
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

