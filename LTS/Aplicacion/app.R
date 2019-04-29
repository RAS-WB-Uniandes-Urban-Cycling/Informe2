
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
Accidentes <- readRDS("Data/Accidentes_LTS_Bogota_2.rds")
capa_Origen_Propension <- readRDS("Data/Capa_Origen_Propension.rds")
capa_Destino_Propension <- readRDS("Data/Capa_Destino_Propension.rds")
capa_Ruta_Propension <- readRDS("Data/Capa_Ruta_Propension.rds")
capa_Camino_Propension <- readRDS("Data/Capa_Camino_Propension.rds")
UPZ_Accidentes <- readRDS("Data/UPZ.rds")


Accidentes <- Accidentes %>% filter(format(Accidentes$Accidentes.Fecha,"%Y")=="2017")
LTS <- LTS %>% st_as_sf() %>% mutate(Cluster=ifelse(X4>=0.5,1,ifelse(X3>=0.5,2,ifelse(X2>=0.5,3,4))))


# Define UI for application 
ui <- navbarPage("Data-Driven LTS",

  tabPanel("Segment LTS",
              
        div(class="outer",
               
            tags$head(includeCSS("Data//styles.css")),
           
            leafletOutput("MiniMap", height = "100%", width = "100%"),
    
            absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
            draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
            width = 330, height = "auto",
                  
            selectInput(inputId = "Localidad",label = "Roadway network subset (Localidad)",
            choices = c("ANTONIO NARIÑO","BARRIOS UNIDOS","BOSA","CANDELARIA","CHAPINERO","CIUDAD BOLIVAR","ENGATIVA","FONTIBON","KENNEDY","LOS MARTIRES","PUENTE ARANDA","RAFAEL URIBE URIBE",
            "SAN CRISTOBAL","SANTA FE","SUBA","SUMAPAZ","TEUSAQUILLO","TUNJUELITO", "USAQUEN","USME")),
      
            selectInput( inputId = "Variable", label = "Layer",
            choices = c("Level of Traffic Stress","Roadway width","Number of lanes","Cycling infrastructure","Heavy vehicles","Vehicle speed","Congestion","Traffic density","Traffic flow")),
      
            plotOutput("Grafica",height = 250),
            helpText(" "),
            plotOutput("Grafica2",height = 250))
        )
    ),
  
  tabPanel("Intersection LTS",
      
        div(class="outer", 
            
            tags$head(includeCSS("Data//styles.css")),
            
            leafletOutput("MiniMap_2", height = "100%", width = "100%"),
            
            absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
            draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
            width = 330, height = "auto",
                          
            selectInput(inputId = "Localidad_2",label =  "Roadway network subset (Localidad)",
            choices = c("ANTONIO NARIÑO","BARRIOS UNIDOS","BOSA","CANDELARIA","CHAPINERO","CIUDAD BOLIVAR","ENGATIVA","FONTIBON","KENNEDY","LOS MARTIRES","PUENTE ARANDA","RAFAEL URIBE URIBE",
            "SAN CRISTOBAL","SANTA FE","SUBA","SUMAPAZ","TEUSAQUILLO","TUNJUELITO", "USAQUEN","USME")),
            
            plotOutput("Grafica3",height = 250),
            helpText(" "),
            plotOutput("Grafica4",height = 250)
            
            )
 
       
    )       
  ),
  
  tabPanel("Accidents Vs. LTS",
           
           div(class="outer", 
               
               tags$head(includeCSS("Data//styles.css")),
               
               leafletOutput("MiniMap_3", height = "100%", width = "100%"),
               
               absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                             draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                             width = 330, height = "auto",
                             
                             selectInput(inputId = "Localidad_3",label ="Roadway network subset (Localidad)",
                                         choices = c("ANTONIO NARIÑO","BARRIOS UNIDOS","BOSA","CANDELARIA","CHAPINERO","CIUDAD BOLIVAR","ENGATIVA","FONTIBON","KENNEDY","LOS MARTIRES","PUENTE ARANDA","RAFAEL URIBE URIBE",
                                                     "SAN CRISTOBAL","SANTA FE","SUBA","SUMAPAZ","TEUSAQUILLO","TUNJUELITO", "USAQUEN","USME")),
                             
                             
                             checkboxInput("Muertes", "Fatal Accidents", TRUE),
                             checkboxInput("NoMuertes", "Non-Fatal Accidents", TRUE),
                             
                             helpText(""),
                             plotOutput("Grafica5",height = 230),
                             helpText(""),
                             plotOutput("Grafica6",height = 230)
               )
               
               
           )       
  )
  
)

server <- function(input, output,session) {
  
  output$MiniMap <- renderLeaflet({
    
    #a <- capa %>% filter(LocNombre==input$Localidad)
    a <- LTS %>% filter(LocNombre==input$Localidad)
    
    if (input$Variable=="Roadway width") {
      
      pal <- colorNumeric("PuRd", a$Ancho)
      
      leaflet(a) %>% addTiles() %>% addProviderTiles("OpenStreetMap.BlackAndWhite")  %>%
        addPolylines(color =~pal(Ancho),opacity = 1,smoothFactor = 1) %>% addLegend("bottomleft", pal = pal, values = ~Ancho, title = "Roadway width (m)",opacity = 1)
      
    }
    else if (input$Variable=="Number of lanes") {
      
      pal <- colorFactor("Purples", a$Carriles)
      
      leaflet(a) %>% addTiles() %>% addProviderTiles("OpenStreetMap.BlackAndWhite")  %>%
        addPolylines(color =~pal(Carriles),opacity = 1,smoothFactor = 1) %>% addLegend("bottomleft", pal = pal, values = ~Carriles, title = "Number of lanes",opacity = 1)
      
    }
    
    else if (input$Variable=="Cycling infrastructure") {
      
      pal <- colorFactor("Spectral", a$CicloRuta)
      
      leaflet(a) %>% addTiles() %>% addProviderTiles("OpenStreetMap.BlackAndWhite")  %>%
        addPolylines(color =~pal(CicloRuta),opacity = 1,smoothFactor = 1) %>% addLegend("bottomleft", pal = pal, values = ~CicloRuta, title = "Cycling infrastructure",opacity = 1)
      
    }
    
    else if (input$Variable=="Heavy vehicles") {
      
      pal <- colorFactor("Spectral", a$SITP)
      
      leaflet(a) %>% addTiles() %>% addProviderTiles("OpenStreetMap.BlackAndWhite")  %>%
        addPolylines(color =~pal(SITP),opacity = 1,smoothFactor = 1) %>% addLegend("bottomleft", pal = pal, values = ~SITP, title = "Heavy vehicles",opacity = 1)
      
    }
    
    else if(input$Variable=="Vehicle speed"){
    
            pal <- colorNumeric(palette = "YlOrRd", a$Velocidad)
            
            leaflet(a) %>% addTiles() %>% addProviderTiles("OpenStreetMap.BlackAndWhite")  %>%
              addPolylines(color =~pal(Velocidad),opacity = 1,smoothFactor = 1) %>% addLegend("bottomleft", pal = pal, values = ~Velocidad, title = "Vehicle speed (km/h)",opacity = 1)
            
    } 
    
    else if(input$Variable=="Congestion"){
      
      pal <- colorNumeric(palette = "YlOrRd", a$Congestion)
      
      leaflet(a) %>% addTiles() %>% addProviderTiles("OpenStreetMap.BlackAndWhite")  %>%
        addPolylines(color =~pal(Congestion),opacity = 1,smoothFactor = 1) %>% addLegend("bottomleft", pal = pal, values = ~Congestion, title = "Congestion",opacity = 1)
      
    } 
    
    else if(input$Variable=="Traffic density"){
      
      pal <- colorNumeric(palette = "YlOrRd", a$Densidad)
      
      leaflet(a) %>% addTiles() %>% addProviderTiles("OpenStreetMap.BlackAndWhite")  %>%
        addPolylines(color =~pal(Densidad),opacity = 1,smoothFactor = 1) %>% addLegend("bottomleft", pal = pal, values = ~Densidad, title = "Traffic density (cars/h)",opacity = 1)
      
    } 
    
    else if(input$Variable=="Traffic flow"){
      
      pal <- colorNumeric(palette = "YlOrRd", a$Flujo)
      
      leaflet(a) %>% addTiles() %>% addProviderTiles("OpenStreetMap.BlackAndWhite")  %>%
        addPolylines(color =~pal(Flujo),opacity = 1,smoothFactor = 1) %>% addLegend("bottomleft", pal = pal, values = ~Flujo, title = "Traffic flow (cars/km)",opacity = 1)
      
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

    if (input$Variable=="Roadway width") {
      
      boxplot(a$Ancho, col = "lightskyblue", main="Roadway width", ylab="Roadway width (m)")
      
    }
    else if (input$Variable=="Number of lanes") {
      
      boxplot(a$Carriles, col = "lightskyblue", main="Number of lanes", ylab="Number of lanes")
    }
    
    else if (input$Variable=="Cycling infrastructure") {
      
      
      pie(table(a$CicloRuta),labels =table(a$CicloRuta) ,main="Cycling infrastructure",radius = 1,col = rainbow(4))
    }
    
    else if (input$Variable=="Heavy vehicles") {
      
      pie(table(a$SITP),labels =table(a$SITP) ,main="Heavy vehicles",radius = 1,col = rainbow(4))
     
    }
    else if(input$Variable=="Vehicle speed"){
      
      boxplot(a$Velocidad, col = "lightskyblue", main="Vehicle speed", ylab="Vehicle speed (km/h)")
      
    } 
    else if(input$Variable=="Congestion"){
      
      boxplot(a$Congestion, col = "lightskyblue", main="Congestion", ylab="Congestion")
      
    } 
    else if(input$Variable=="Traffic density"){
      
      boxplot(a$Densidad, col = "lightskyblue", main="Traffic density", ylab="Traffic density (cars/h)")
      
    } 
    else if(input$Variable=="Traffic flow"){
      
      boxplot(a$Flujo, col = "lightskyblue", main="Traffic flow", ylab="Traffic flow (cars/km)")
      
    } 
    else if (input$Variable=="Level of Traffic Stress") {
      
      pie(table(a$Cluster),labels =table(a$Cluster) ,main="Level of Traffic Stress",radius = 1,col =  c("lime green","dark green","orange","Red"))
      
    }
    
    
  })

    output$Grafica2 <- renderPlot({
      
      #a <- capa %>% filter(LocNombre==input$Localidad)
      
      a <- LTS %>% filter(LocNombre==input$Localidad)

      if (input$Variable=="Roadway width") {
        
        hist(a$Ancho, col = "lightskyblue", xlab="Roadway width (m)",ylab="Frequency", main = "Roadway width Histogram") 
      
      }
      else if (input$Variable=="Number of lanes") {
        
        hist(a$Carriles, col = "lightskyblue", xlab="Number of lanes",ylab="Frequency", main = "Number of lanes Histogram") 
        
      }
      
      else if (input$Variable=="Cycling infrastructure") {
        
        pie(table(a$CicloRuta),labels =c("No","Yes") ,main="Cycling infrastructure",radius = 1,col = rainbow(4))
        
      }
      
      else if (input$Variable=="Heavy vehicles") {
        
        pie(table(a$SITP),labels =c("No","Yes") ,main="Heavy vehicles",radius = 1,col = rainbow(4))
      }
      else if(input$Variable=="Vehicle speed"){
        
        hist(a$Velocidad, col = "lightskyblue", xlab="Vehicle speed",ylab="Frequency", main = "Vehicle speed Histogram") 
      } 
      else if(input$Variable=="Congestion"){
        
        hist(a$Congestion, col = "lightskyblue", xlab="Congestion",ylab="Frequency", main = "Congestion Histogram") 
      } 
      else if(input$Variable=="Traffic density"){
        
        hist(a$Densidad, col = "lightskyblue", xlab="Traffic density (cars/h)",ylab="Frequency", main = "Traffic density Histogram") 
      } 
      else if(input$Variable=="Traffic flow"){
        
        hist(a$Flujo, col = "lightskyblue", xlab="Traffic flow (cars/km)",ylab="Frequency", main = "Traffic flow Histogram") 
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

        barplot(table(b$Cluster), main="Fatal Accidents vs LTS (Localidad)", xlab="LTS", ylab = "Frequency",col = "lightskyblue")
      
      }
      else if(input$Muertes==FALSE & input$NoMuertes==TRUE ){
        
        b <- Accidentes %>% filter(LocNombre==input$Localidad_3,Gravedad2=="Not Dead")
        
        barplot(table(b$Cluster), main="Non-Fatal Accidents vs LTS (Localidad)", xlab="LTS", ylab = "Frequency",col = "lightskyblue")
        
      }
      else if(input$Muertes==TRUE & input$NoMuertes==TRUE ){
        
        b <- Accidentes %>% filter(LocNombre==input$Localidad_3)
        
        barplot(table(b$Cluster), main="Accidents vs LTS (Localidad)", xlab="LTS", ylab = "Frequency",col = "lightskyblue")
        
      }

      
    })
    
    
    output$Grafica6 <- renderPlot({
      
      if(input$Muertes==TRUE & input$NoMuertes==FALSE ){
        
        b <- Accidentes %>% filter(Gravedad2=="Dead") %>% st_set_geometry(NULL)
        
        barplot(table(b$Cluster), main="Fatal Accidents vs LTS (Bogotá)", xlab="LTS", ylab = "Frequency",col = "lightskyblue")
        
      }
      else if(input$Muertes==FALSE & input$NoMuertes==TRUE ){
        
        b <- Accidentes %>% filter(Gravedad2=="Not Dead")
        
        barplot(table(b$Cluster), main="Non-Fatal Accidents vs LTS (Bogotá)", xlab="LTS", ylab = "Frequency",col = "lightskyblue")
        
      }
      else if(input$Muertes==TRUE & input$NoMuertes==TRUE ){
        
        b <- Accidentes
        
        barplot(table(b$Cluster), main="Accidents vs LTS (Bogotá)", xlab="LTS", ylab = "Frequency",col = "lightskyblue")
        
      }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

