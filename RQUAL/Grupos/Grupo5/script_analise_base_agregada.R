#################
### DIRETORIO ###
#################

# getwd()
setwd("/Users/alexvlima/Library/Mobile Documents/com~apple~CloudDocs/ABR_Telecom_ESAQ/Grupos/Grupo5/Analise_2021/Dados/")

###################
### BIBLIOTECAS ###
###################

library(tidyverse)
library(brazilmaps)
library(sf)

################
### ARQUIVOS ###
################

# rm(list = ls())

# CLARO #

# Base Agregada #
temp_claro <- paste0("./CLARO/", list.files(path = "./CLARO/", pattern = "BASE_AGREGADA.*csv"))
base_agregada_claro <- lapply(temp_claro, read_csv2)

base_agregada_claro <- bind_rows(base_agregada_claro)
glimpse(base_agregada_claro)

rm(temp_claro)

# OI #

# Base Agregada #
temp_oi <- paste0("./OI/", list.files(path = "./OI/", pattern = "BASE_AGREGADA.*csv"))
base_agregada_oi <- lapply(temp_oi, read_csv2)

base_agregada_oi <- bind_rows(base_agregada_oi)
glimpse(base_agregada_oi)

rm(temp_oi)

# SKY #

# Base Agregada #
temp_sky <- paste0("./SKY/", list.files(path = "./SKY/", pattern = "BASE_AGREGADA.*csv"))
base_agregada_sky <- lapply(temp_sky, read_csv2)

base_agregada_sky <- bind_rows(base_agregada_sky)
glimpse(base_agregada_sky)

rm(temp_sky)

# TIM #

# Base Agregada #
temp_tim <- paste0("./TIM/", list.files(path = "./TIM/", pattern = "BASE_AGREGADA.*csv"))
base_agregada_tim <- lapply(temp_tim, read_csv2)

base_agregada_tim <- bind_rows(base_agregada_tim)
glimpse(base_agregada_tim)

rm(temp_tim)

# VIVO #

# Base Agregada #
temp_vivo <- paste0("./VIVO/", list.files(path = "./VIVO/", pattern = "BASE_AGREGADA.*csv"))
base_agregada_vivo <- lapply(temp_vivo, read_csv2)

base_agregada_vivo <- bind_rows(base_agregada_vivo)
glimpse(base_agregada_vivo)

rm(temp_vivo)

###############
### DATASET ###
###############

dfs = sapply(.GlobalEnv, is.data.frame) 
dfs

base_agregada <- do.call(bind_rows, mget(names(dfs)[dfs]))
glimpse(base_agregada)

rm(dfs)
rm(list = ls(pattern = "base_agregada_"))

base_agregada <- 
  base_agregada %>% 
  select(PRESTADORA, SERVICO, ID_MUNICIPIO, MES_ANO, AIND9A, BIND9A, AIND9B, BIND9B)

base_agregada <- 
  base_agregada %>% 
  mutate(PRESTADORA_ANONIMA = factor(PRESTADORA, 
                                     levels = c("CLARO","OI","SKY","TIM","VIVO"),
                                     labels = c("A","B","C","D","E")))

###############
### ANALISE ###
###############

# DADOS POR MES E PRESTADORA #

tabela1 <- 
  base_agregada %>%
  filter(is.na(SERVICO) == FALSE) %>%
  group_by(PRESTADORA_ANONIMA, SERVICO, MES_ANO) %>%
  dplyr::summarize(IND9A = sum(AIND9A, na.rm = T)/sum(BIND9A,na.rm = T) * 100,
                   IND9B = sum(AIND9B, na.rm = T)/sum(BIND9B,na.rm = T) * 100,
                   QTDE = n())

# write_csv2(tabela1, "tabela1.csv")
# rm(tabela1)

# MAPA #

base_agregada_gather <- 
  base_agregada %>% 
  filter(is.na(SERVICO) == FALSE) %>%
  select(PRESTADORA, ID_MUNICIPIO, SERVICO, AIND9A, BIND9A, AIND9B, BIND9B) %>%
  group_by(PRESTADORA, ID_MUNICIPIO, SERVICO) %>%
  dplyr::summarize(IND9A = sum(AIND9A, na.rm = T)/sum(BIND9A,na.rm = T) * 100,
                   IND9B = sum(AIND9B, na.rm = T)/sum(BIND9B,na.rm = T) * 100) %>%
  gather(key = "TIPO", value = "IND9", -PRESTADORA, -ID_MUNICIPIO, -SERVICO)

muni_map <- get_brmap("City") %>% 
  left_join(base_agregada_gather, c("City" = "ID_MUNICIPIO"))

map_uf <- get_brmap("State")

muni_map %>%
  filter(PRESTADORA == "CLARO", is.na(SERVICO) == FALSE) %>%
  ggplot() +
  geom_sf(data = map_uf, fill = "white") + 
  geom_sf(aes(fill = IND9), 
          # ajusta tamanho das linhas
          colour = "black", size = 0.1) +
  # muda escala de cores
  scale_fill_viridis_c(option = 2) +
  # tira sistema cartesiano
  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  facet_grid(rows = vars(TIPO), cols = vars(SERVICO))

muni_map %>%
  filter(PRESTADORA == "OI", is.na(SERVICO) == FALSE) %>%
  ggplot() +
  geom_sf(data = map_uf, fill = "white") + 
  geom_sf(aes(fill = IND9), 
          # ajusta tamanho das linhas
          colour = "black", size = 0.1) +
  # muda escala de cores
  scale_fill_viridis_c(option = 2) +
  # tira sistema cartesiano
  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  facet_grid(rows = vars(TIPO), cols = vars(SERVICO))

muni_map %>%
  filter(PRESTADORA == "SKY", is.na(SERVICO) == FALSE) %>%
  ggplot() +
  geom_sf(data = map_uf, fill = "white") + 
  geom_sf(aes(fill = IND9), 
          # ajusta tamanho das linhas
          colour = "black", size = 0.1) +
  # muda escala de cores
  scale_fill_viridis_c(option = 2) +
  # tira sistema cartesiano
  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  facet_grid(rows = vars(TIPO), cols = vars(SERVICO))

muni_map %>%
  filter(PRESTADORA == "TIM", is.na(SERVICO) == FALSE) %>%
  ggplot() +
  geom_sf(data = map_uf, fill = "white") + 
  geom_sf(aes(fill = IND9), 
          # ajusta tamanho das linhas
          colour = "black", size = 0.1) +
  # muda escala de cores
  scale_fill_viridis_c(option = 2) +
  # tira sistema cartesiano
  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  facet_grid(rows = vars(TIPO), cols = vars(SERVICO))

muni_map %>%
  filter(PRESTADORA == "VIVO", is.na(SERVICO) == FALSE) %>%
  ggplot() +
  geom_sf(data = map_uf, fill = "white") + 
  geom_sf(aes(fill = IND9), 
          # ajusta tamanho das linhas
          colour = "black", size = 0.1) +
  # muda escala de cores
  scale_fill_viridis_c(option = 2) +
  # tira sistema cartesiano
  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  facet_grid(rows = vars(TIPO), cols = vars(SERVICO))

rm(map_uf, muni_map, base_agregada_gather)
