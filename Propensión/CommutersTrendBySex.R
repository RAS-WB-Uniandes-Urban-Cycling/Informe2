library(tidyverse)
library(readxl)

path <- "C:/Users/pa.uriza274/Universidad de Los Andes/German Augusto Carvajal Murcia - UNIANDES - RAS - SDM/RESULTADOS/PROPENSION/bases_de_datos/"

trend <- read_delim(paste0(path,"biciusuariostotal.csv"),delim = ";")
pop <- read_delim(paste0(path,"poblacionTOTAL.csv")delim = ";")

bu <- trend %>% left_join(pop) %>% mutate(BICIUSRS_POP=BICIUSRS/POPULATION*100000)

ggplot(bu,aes(x= ANO,y=BICIUSRS_POP)) +
  geom_smooth(aes(color=SEXO,fill=SEXO),method = 'gam', formula = y ~ s(x,k=5))+
  # geom_point(aes(color=SEXO), size=3)+
  geom_point(aes(shape=SEXO),colour="black",size = 2)+
  scale_x_continuous(breaks = 2005:2017)+
  scale_color_manual(values = c("red", "blue"))+
  scale_fill_manual(values = c("red", "blue"))+
  labs(x="Year",y="Number of Bicycle Commuters per 100,000 hab.",
       color="Sex",fill="Sex",shape="Sex",
       title="Number of bicycle commuters per 100,000 hab, Bogotá 2005-2017")+
  theme_light()

trend::mk.test(bu$BICIUSRSINT[bu$SEXO=="Female"])
trend::mk.test(bu$BICIUSRSINT[bu$SEXO=="Male"])
