# getwd()
setwd("/Users/alexvlima/Downloads//")

library(tidyverse)
library(readxl)

# rm(list = ls())
df <- read_excel(path = "report_variacao_vigente mpd.xlsx")
glimpse(df)

df %>% 
  filter(BAIRRO == "Moema") %>% 
  group_by(DORMS) %>% 
  summarise(media = mean(`PM2 Medio`, na.rm = T),
            maximo = max(`PM2 Medio`, na.rm = T))

df %>% 
  filter(BAIRRO == "Pinheiros", SUBTIPO %in% c("Apartamento","Studio")) %>% 
  group_by(DORMS) %>% 
  summarise(media = mean(`PM2 Medio`, na.rm = T),
            maximo = max(`PM2 Medio`, na.rm = T),
            minimo = min(`PM2 Medio`, na.rm = T),
            mediana = median(`PM2 Medio`, na.rm = T))

pinheiros <-
  df %>% 
  filter(BAIRRO == "Pinheiros", DORMS == 1, SUBTIPO %in% c("Apartamento","Studio")) %>% 
  arrange(desc(`PM2 Medio`))
unique(pinheiros$EMP)
# Tirar Metrocasa


