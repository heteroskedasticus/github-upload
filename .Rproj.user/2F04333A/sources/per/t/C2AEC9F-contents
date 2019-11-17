library(tidyverse)
library(gganimate)
library(cartogram)
library(sf)
library(stringr)
library(ggthemes)
library(viridis)
library(scales)
library(readr)
library(janitor)
library(forcats)

Votaciones <- read_delim("Votaciones.csv", 
                         ";", escape_double = FALSE, locale = locale(encoding = "WINDOWS-1252"), 
                         trim_ws = TRUE) %>% 
  clean_names() %>% 
  mutate(perc_votos = suma_de_votos_en_mesa/suma_de_votantes_padron)

shp_comunas <- st_read("comunas.shp")


shp_comunas_2 <- left_join(shp_comunas, Votaciones, by= c("COMUNAS" = "comuna")) %>% 
  mutate(perc_votos_carto = case_when(tipo_de_votacion == "PASO"~ (((AREA * (1+perc_votos)^40))),
                                      TRUE~(((AREA * (1+perc_votos)^40)))))

### subseteamos en 6, transformamos y unimos

p1 <- shp_comunas_2 %>% 
  filter(tipo_de_votacion == "PASO") %>% 
  mutate (perc_votos = .8,
          iter = "CABA")

p2 <- shp_comunas_2 %>% 
  filter(tipo_de_votacion == "PASO",
         categoria == "Presidente") %>% 
  mutate(iter = "PASO") %>% 
  cartogram_cont("perc_votos_carto")


p3 <- shp_comunas_2 %>% 
  filter(tipo_de_votacion == "PASO",
         categoria != "Presidente") %>% 
  mutate(iter = "PASO") %>% 
  cartogram_cont("perc_votos_carto")

p4 <- shp_comunas_2 %>% 
  filter(tipo_de_votacion == "Definitivas",
         categoria == "Presidente") %>% 
  mutate(iter = "Definitivas") %>% 
  cartogram_cont("perc_votos_carto")

p5 <- shp_comunas_2 %>% 
  filter(tipo_de_votacion == "Definitivas",
         categoria != "Presidente") %>% 
  mutate(iter = "Definitivas") %>% 
  cartogram_cont("perc_votos_carto")

carto_df <- rbind(p1,
      p2,
      p3,
      p4,
      p5)


carto_df$categoria <- as.factor(carto_df$categoria)
carto_df$iter <- as.factor(carto_df$iter)
levels(carto_df$iter) <- c("CABA", "PASO", "Definitivas")



gplot <- ggplot(carto_df)+
  geom_sf(aes(fill= perc_votos))+
  facet_wrap(~categoria)+
  theme_map()+
  theme(legend.position = "none")+
  transition_states(states = iter,2,1)+
  ggtitle("{closest_state}")

anim_save("test.gif", gplot, width = 1000, height = 1000)



###tips


# anim <- ggplot(mtcars, aes(mpg, disp)) +
#   transition_states(gear, transition_length = 2, state_length = 1) +
#   enter_fade() +
#   exit_fade()
# 
# if (FALSE) {
#   # Explicitly animate using default (same as just printing the animation)
#   animate(anim)
#   
#   # Change duration and framerate
#   animate(anim, fps = 20, duration = 15)
#   
#   # Make the animation pause at the end and then rewind
#   animate(anim, nframes = 100, end_pause = 10, rewind = TRUE)
#   
#   # Use a different renderer
#   animate(anim, renderer = file_renderer('~/animation/'))[1:6]
# }
