#### paquetes
library(tidyverse)
library(gganimate)
library(cartogram)
library(sf)
library(stringr)
library(ggthemes)
library(viridis)
library(scales)
#### descargamos shape file original y le agregamos datos de las paso y elecciones
shp_comunas <- st_read("comunas.shp")

### PASO

paso_mtl <- read.delim("C://Users//user//Documents//PASO2019 - DATOS//Smart Matic//mesas_totales_agrp_politica.dsv",
                       sep = "|")

paso_vb <- read.delim("C://Users//user//Documents//PASO2019 - DATOS//Smart Matic//mesas_totales.dsv",
                      sep = "|")

postulaciones <- read.delim("C://Users//user//Documents//PASO2019 - DATOS//Smart Matic//descripcion_postulaciones.dsv",
                            sep = "|")


df_paso <- paso_mtl  %>% 
  filter(CODIGO_CATEGORIA ==1.000000e+11,
         CODIGO_DISTRITO == 1) %>% 
  group_by_at(vars(-c(CODIGO_AGRUPACION, VOTOS_AGRUPACION))) %>% 
  summarise(total_votos = sum(VOTOS_AGRUPACION)) %>% 
  left_join(paso_vb %>% 
              filter(CODIGO_CATEGORIA == 1.000000e+11,
                     CONTADOR == "VB",
                     CODIGO_DISTRITO == 1) %>% 
              select(CODIGO_MESA, vb=VALOR)) %>% 
  group_by(CODIGO_SECCION) %>% 
  summarise(total_votos_paso = sum(total_votos),
            vb_paso = sum(vb)) %>% 
  mutate(perc_vb_paso = vb_paso/total_votos_paso,
         COMUNAS = as.numeric(str_sub(CODIGO_SECCION, start = 3))) %>% 
  select(-CODIGO_SECCION)


### ahora con los datos de elecciones

elect <- read.delim("C://Users//user//Documents//elecciones//oficial//mesas_agrp_politicas.dsv",
                    sep = "|")
vb_elect <- read.delim("C://Users//user//Documents//elecciones//oficial//mesas_totales.dsv",
                       sep = "|")


df_elect <- elect  %>% 
  filter(CODIGO_CATEGORIA ==1.000000e+11,
         CODIGO_DISTRITO == 1) %>% 
  group_by_at(vars(-c(CODIGO_AGRUPACION, VOTOS_AGRUPACION, CODIGO_LISTA))) %>% 
  summarise(total_votos = sum(VOTOS_AGRUPACION)) %>% 
  left_join(vb_elect %>%
              filter(CODIGO_CATEGORIA == 1.000000e+11,
                    CONTADOR == "Voto blanco",
                    CODIGO_DISTRITO == 1) %>% 
              select(CODIGO_MESA, vb=VALOR)) %>% 
  group_by(CODIGO_SECCION) %>% 
  summarise(total_votos_elect = sum(total_votos),
            vb_elect = sum(vb)) %>% 
  mutate(perc_vb_elect = vb_elect/total_votos_elect,
         COMUNAS = as.numeric(str_sub(CODIGO_SECCION, start = 3))) %>% 
  select(-CODIGO_SECCION)


shp_comunas <- 
  shp_comunas %>% left_join(df_elect) %>% left_join(df_paso)


shp_comunas_2 <-  shp_comunas %>% 
  mutate(perc_vb_paso_2 = ((AREA * perc_vb_paso) %>% rescale(to = c(1,2)))^3,
         perc_vb_elect_2 = ((AREA * perc_vb_elect) %>% rescale(to = c(1,2)))^4)


# hacemos 3 graficos

plot1 <- ggplot(shp_comunas_2)+ geom_sf(fill = "red", color = "blue")+
  theme_map()


plot2 <- ggplot(cartogram_cont(shp_comunas_2, "perc_vb_paso_2"))+ 
  geom_sf(fill = "red", color = "blue")+
  theme_map()
  
plot3 <- ggplot(cartogram_cont(shp_comunas_2, "perc_vb_elect_2"))+
  geom_sf(fill = "red", color = "blue")+
  theme_map()


plot1
plot2
plot3

carto1 <- cartogram_cont(shp_comunas_2, "perc_vb_paso_2") %>% 
  select(everything(), value = perc_vb_paso, -perc_vb_paso, - perc_vb_elect) %>% 
  mutate(state = "PASO")

carto2 <- cartogram_cont(shp_comunas_2, "perc_vb_elect_2") %>% 
  select(everything(), value = perc_vb_elect, -perc_vb_paso, - perc_vb_elect) %>% 
  mutate(state = "DEFINITIVAS")

df_carto <- rbind(carto1, carto2)

ggplot(df_carto, aes(fill = perc_vb_elect_2))+
  geom_sf() +
  theme_map()+
  transition_states(states = state, 2, 1)
  
