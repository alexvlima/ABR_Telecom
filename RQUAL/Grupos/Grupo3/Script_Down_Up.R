#################
### DIRETORIO ###
#################

# getwd()
setwd("/Users/alexvlima/Library/Mobile Documents/com~apple~CloudDocs/ABR_Telecom_ESAQ/Grupos/Grupo3/Analise_2021/Analise_Grupo3/")

###################
### BIBLIOTECAS ###
###################

library(tidyverse)
library(brazilmaps)
library(sf)

################
### RAW DATA ###
################

# rm(list = ls())

# CLARO # 

down_up_claro <- read_delim("Dados/SCM_Down_Up_2020-12_CLARO.csv.gz", 
                            delim = ";")

down_up_claro <- 
  down_up_claro %>% filter(is.na(`Tipo de Descarte`) == TRUE) %>%
  select(deviceID, cod_mun = `Codigo IBGE`,  
         bwUp, bwDown, speedUp, speedDown) %>%
  mutate(Prestadora = "A")
  
glimpse(down_up_claro)

# OI MOVEL # 

down_up_oi_movel <- read_delim("Dados/SCM_Down_Up_2020-12_OI_MOVEL.csv.gz", 
                               delim = ";")

down_up_oi_movel <- 
  down_up_oi_movel %>% filter(is.na(`Tipo de Descarte`) == TRUE) %>%
  select(deviceID, cod_mun = `Codigo IBGE`,  
         bwUp, bwDown, speedUp, speedDown) %>%
  mutate(Prestadora = "B")

glimpse(down_up_oi_movel)

# OI # 

down_up_oi <- read_delim("Dados/SCM_Down_Up_2020-12_OI.csv.gz", 
                         delim = ";")

down_up_oi <- 
  down_up_oi %>% filter(is.na(`Tipo de Descarte`) == TRUE) %>%
  select(deviceID, cod_mun = `Codigo IBGE`,  
         bwUp, bwDown, speedUp, speedDown) %>%
  mutate(Prestadora = "B")

glimpse(down_up_oi)

# SKY # 

down_up_sky <- read_delim("Dados/SCM_Down_Up_2020-12_SKY.csv.gz", 
                          delim = ";", col_types = 
                          cols(deviceID = col_character(),
                          IP = col_character(),
                          location = col_double(),
                          macAddress = col_logical(),
                          manufacturer = col_character(),
                          modelName = col_logical(),
                          softwareVersion = col_character(),
                          bwUp = col_double(),
                          bwDown = col_double(),
                          dateTime = col_datetime(format = ""),
                          speedTestInterval = col_logical(),
                          speedTestCounter = col_logical(),
                          testPoint = col_character(),
                          `Tipo de Descarte` = col_character(),
                          CNPJ = col_character(),
                          `Codigo IBGE` = col_double(),
                          localTime = col_character()))

down_up_sky <- 
  down_up_sky %>% filter(is.na(`Tipo de Descarte`) == TRUE) %>%
  select(deviceID, cod_mun = `Codigo IBGE`,  
         bwUp, bwDown, speedUp, speedDown) %>%
  mutate(Prestadora = "C")

glimpse(down_up_sky)

# TIM # 

down_up_tim <- read_delim("Dados/SCM_Down_Up_2020-12_TIM.csv.gz", 
                          delim = ";")

down_up_tim <- 
  down_up_tim %>% filter(is.na(`Tipo de Descarte`) == TRUE) %>%
  select(deviceID, cod_mun = `Codigo IBGE`,  
         bwUp, bwDown, speedUp, speedDown) %>%
  mutate(Prestadora = "D")

glimpse(down_up_tim)

# VIVO # 

down_up_vivo <- read_delim("Dados/SCM_Down_Up_2020-12_VIVO.csv.gz", 
                           delim = ";")

down_up_vivo <- 
  down_up_vivo %>% filter(is.na(`Tipo de Descarte`) == TRUE) %>%
  select(deviceID, cod_mun = `Codigo IBGE`,  
         bwUp, bwDown, speedUp, speedDown) %>%
  mutate(Prestadora = "E")

glimpse(down_up_vivo)

###############
### DATASET ###
###############

df <- bind_rows(down_up_claro, 
                down_up_oi, 
                down_up_oi_movel, 
                down_up_sky, 
                down_up_tim, 
                down_up_vivo)
glimpse(df)

# rm(down_up_claro, down_up_oi, down_up_oi_movel, down_up_sky, down_up_tim, down_up_vivo)

#################
### INDICADOR ###
#################

df %>%
  group_by(Prestadora) %>%
  summarise(media_Down = mean(speedDown),
            media_Up = mean(speedUp),
            sd_Down = sd(speedDown),
            sd_Up = sd(speedUp),
            erro_Down = mean(speedDown) * 1.07 - mean(speedDown),
            erro_Up = mean(speedUp) * 1.07 - mean(speedUp)) %>%
  mutate(amostra_Down = (((1.96^2) * (sd_Down^2)) / (erro_Down^2)),
         amostra_Up = (((1.96^2) * (sd_Up^2)) / (erro_Up^2))) %>%
  select(-media_Down, -media_Up)
  
df <- 
  df %>%
  mutate(indDown = speedDown / bwDown, indUp = speedUp / bwUp) 

df <- 
  df %>%
  mutate(indDown100 = if_else(indDown >= 1, 1, 0),
         indUp100 = if_else(indUp >= 1, 1, 0),
         indDown80 = if_else(indDown >= 0.8, 1, 0),
         indUp80 = if_else(indUp >= 0.8, 1, 0),
         indDown60 = if_else(indDown >= 0.6, 1, 0),
         indUp60 = if_else(indUp >= 0.6, 1, 0),
         indDown40 = if_else(indDown >= 0.4, 1, 0),
         indUp40 = if_else(indUp >= 0.4, 1, 0))

base_munic <- 
  df %>% 
  group_by(cod_mun, Prestadora) %>% 
  summarise(indDown100 = mean(indDown100),
            indDown80 = mean(indDown80),
            indDown60 = mean(indDown60),
            indDown40 = mean(indDown40), 
            indUp100 = mean(indUp100),
            indUp80 = mean(indUp80),
            indUp60 = mean(indUp60),
            indUp40 = mean(indUp40))

glimpse(base_munic)

base_munic$Prestadora <- as.factor(base_munic$Prestadora)

#####################
### ANALISE - CDF ###
#####################

base_munic %>%
  filter(indDown100 >= 0) %>%
  ggplot(aes(indDown100, group = Prestadora, color = Prestadora)) + 
  stat_ecdf(size = 1) + 
  scale_x_continuous(name = "", labels = scales::percent) + 
  scale_y_continuous(name = "cdf") +
  ggtitle("Indicador 100% Download") + 
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)) 

base_munic %>%
  filter(indDown80 >= 0) %>%
  ggplot(aes(indDown80, group = Prestadora, color = Prestadora)) + 
  stat_ecdf(size = 1) + 
  scale_x_continuous(name = "", labels = scales::percent) + 
  scale_y_continuous(name = "cdf") +
  ggtitle("Indicador 80% Download") + 
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)) 

base_munic %>%
  filter(indDown60 >= 0) %>%
  ggplot(aes(indDown60, group = Prestadora, color = Prestadora)) + 
  stat_ecdf(size = 1) + 
  scale_x_continuous(name = "", labels = scales::percent) + 
  scale_y_continuous(name = "cdf") +
  ggtitle("Indicador 60% Download") + 
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)) 

base_munic %>%
  filter(indDown40 >= 0) %>%
  ggplot(aes(indDown40, group = Prestadora, color = Prestadora)) + 
  stat_ecdf(size = 1) + 
  scale_x_continuous(name = "", labels = scales::percent) + 
  scale_y_continuous(name = "cdf") +
  ggtitle("Indicador 40% Download") + 
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5))

base_munic %>%
  filter(indUp100 >= 0) %>%
  ggplot(aes(indUp100, group = Prestadora, color = Prestadora)) + 
  stat_ecdf(size = 1) + 
  scale_x_continuous(name = "", labels = scales::percent) + 
  scale_y_continuous(name = "cdf") +
  ggtitle("Indicador 100% Upload") + 
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)) 

base_munic %>%
  filter(indUp80 >= 0) %>%
  ggplot(aes(indUp80, group = Prestadora, color = Prestadora)) + 
  stat_ecdf(size = 1) + 
  scale_x_continuous(name = "", labels = scales::percent) + 
  scale_y_continuous(name = "cdf") +
  ggtitle("Indicador 80% Upload") + 
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)) 

base_munic %>%
  filter(indUp60 >= 0) %>%
  ggplot(aes(indUp60, group = Prestadora, color = Prestadora)) + 
  stat_ecdf(size = 1) + 
  scale_x_continuous(name = "", labels = scales::percent) + 
  scale_y_continuous(name = "cdf") +
  ggtitle("Indicador 60% Upload") + 
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)) 

base_munic %>%
  filter(indUp40 >= 0) %>%
  ggplot(aes(indUp40, group = Prestadora, color = Prestadora)) + 
  stat_ecdf(size = 1) + 
  scale_x_continuous(name = "", labels = scales::percent) + 
  scale_y_continuous(name = "cdf") +
  ggtitle("Indicador 40% Upload") + 
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5))

### MAPA ###

muni_map <- get_brmap("City") %>% 
  left_join(base_munic, c("City" = "cod_mun"))

map_uf <- get_brmap("State")

muni_map %>%
  filter(Prestadora == "A") %>%
  gather(key = "indicador", value = "valor", 
         -nome, -City, -State, -MicroRegion, -MesoRegion,-Region,-Prestadora,-geometry) %>%
  filter(str_detect(indicador, "indDown")) %>%
  ggplot() +
  geom_sf(data = map_uf, fill = "white") + 
  geom_sf(aes(fill = valor), 
          # ajusta tamanho das linhas
          colour = "black", size = 0.1) +
  # muda escala de cores
  scale_fill_viridis_c(option = 2, direction = 1, labels = scales::percent) +
  # tira sistema cartesiano
  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  facet_wrap(~factor(indicador, 
                     levels = c("indDown100","indDown80","indDown60","indDown40")))

muni_map %>%
  filter(Prestadora == "B") %>%
  gather(key = "indicador", value = "valor", 
         -nome, -City, -State, -MicroRegion, -MesoRegion,-Region,-Prestadora,-geometry) %>%
  filter(str_detect(indicador, "indDown")) %>%
  ggplot() +
  geom_sf(data = map_uf, fill = "white") + 
  geom_sf(aes(fill = valor), 
          # ajusta tamanho das linhas
          colour = "black", size = 0.1) +
  # muda escala de cores
  scale_fill_viridis_c(option = 2, direction = 1, labels = scales::percent) +
  # tira sistema cartesiano
  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  facet_wrap(~factor(indicador, 
                     levels = c("indDown100","indDown80","indDown60","indDown40")))

muni_map %>%
  filter(Prestadora == "C") %>%
  gather(key = "indicador", value = "valor", 
         -nome, -City, -State, -MicroRegion, -MesoRegion,-Region,-Prestadora,-geometry) %>%
  filter(str_detect(indicador, "indDown")) %>%
  ggplot() +
  geom_sf(data = map_uf, fill = "white") + 
  geom_sf(aes(fill = valor), 
          # ajusta tamanho das linhas
          colour = "black", size = 0.1) +
  # muda escala de cores
  scale_fill_viridis_c(option = 2, direction = 1, labels = scales::percent) +
  # tira sistema cartesiano
  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  facet_wrap(~factor(indicador, 
                     levels = c("indDown100","indDown80","indDown60","indDown40")))

muni_map %>%
  filter(Prestadora == "D") %>%
  gather(key = "indicador", value = "valor", 
         -nome, -City, -State, -MicroRegion, -MesoRegion,-Region,-Prestadora,-geometry) %>%
  filter(str_detect(indicador, "indDown")) %>%
  ggplot() +
  geom_sf(data = map_uf, fill = "white") + 
  geom_sf(aes(fill = valor), 
          # ajusta tamanho das linhas
          colour = "black", size = 0.1) +
  # muda escala de cores
  scale_fill_viridis_c(option = 2, direction = 1, labels = scales::percent) +
  # tira sistema cartesiano
  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  facet_wrap(~factor(indicador, 
                     levels = c("indDown100","indDown80","indDown60","indDown40")))

muni_map %>%
  filter(Prestadora == "E") %>%
  gather(key = "indicador", value = "valor", 
         -nome, -City, -State, -MicroRegion, -MesoRegion,-Region,-Prestadora,-geometry) %>%
  filter(str_detect(indicador, "indDown")) %>%
  ggplot() +
  geom_sf(data = map_uf, fill = "white") + 
  geom_sf(aes(fill = valor), 
          # ajusta tamanho das linhas
          colour = "black", size = 0.1) +
  # muda escala de cores
  scale_fill_viridis_c(option = 2, direction = 1, labels = scales::percent) +
  # tira sistema cartesiano
  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  facet_wrap(~factor(indicador, 
                     levels = c("indDown100","indDown80","indDown60","indDown40")))

muni_map %>%
  filter(Prestadora == "F") %>%
  gather(key = "indicador", value = "valor", 
         -nome, -City, -State, -MicroRegion, -MesoRegion,-Region,-Prestadora,-geometry) %>%
  filter(str_detect(indicador, "indDown")) %>%
  ggplot() +
  geom_sf(data = map_uf, fill = "white") + 
  geom_sf(aes(fill = valor), 
          # ajusta tamanho das linhas
          colour = "black", size = 0.1) +
  # muda escala de cores
  scale_fill_viridis_c(option = 2, direction = 1, labels = scales::percent) +
  # tira sistema cartesiano
  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  facet_wrap(~factor(indicador, 
                     levels = c("indDown100","indDown80","indDown60","indDown40")))

muni_map %>%
  filter(Prestadora == "A") %>%
  gather(key = "indicador", value = "valor", 
         -nome, -City, -State, -MicroRegion, -MesoRegion,-Region,-Prestadora,-geometry) %>%
  filter(str_detect(indicador, "indUp")) %>%
  ggplot() +
  geom_sf(data = map_uf, fill = "white") + 
  geom_sf(aes(fill = valor), 
          # ajusta tamanho das linhas
          colour = "black", size = 0.1) +
  # muda escala de cores
  scale_fill_viridis_c(option = 2, direction = 1, labels = scales::percent) +
  # tira sistema cartesiano
  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  facet_wrap(~factor(indicador, 
                     levels = c("indUp100","indUp80","indUp60","indUp40")))

muni_map %>%
  filter(Prestadora == "B") %>%
  gather(key = "indicador", value = "valor", 
         -nome, -City, -State, -MicroRegion, -MesoRegion,-Region,-Prestadora,-geometry) %>%
  filter(str_detect(indicador, "indUp")) %>%
  ggplot() +
  geom_sf(data = map_uf, fill = "white") + 
  geom_sf(aes(fill = valor), 
          # ajusta tamanho das linhas
          colour = "black", size = 0.1) +
  # muda escala de cores
  scale_fill_viridis_c(option = 2, direction = 1, labels = scales::percent) +
  # tira sistema cartesiano
  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  facet_wrap(~factor(indicador, 
                     levels = c("indUp100","indUp80","indUp60","indUp40")))

muni_map %>%
  filter(Prestadora == "C") %>%
  gather(key = "indicador", value = "valor", 
         -nome, -City, -State, -MicroRegion, -MesoRegion,-Region,-Prestadora,-geometry) %>%
  filter(str_detect(indicador, "indUp")) %>%
  ggplot() +
  geom_sf(data = map_uf, fill = "white") + 
  geom_sf(aes(fill = valor), 
          # ajusta tamanho das linhas
          colour = "black", size = 0.1) +
  # muda escala de cores
  scale_fill_viridis_c(option = 2, direction = 1, labels = scales::percent) +
  # tira sistema cartesiano
  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  facet_wrap(~factor(indicador, 
                     levels = c("indUp100","indUp80","indUp60","indUp40")))

muni_map %>%
  filter(Prestadora == "D") %>%
  gather(key = "indicador", value = "valor", 
         -nome, -City, -State, -MicroRegion, -MesoRegion,-Region,-Prestadora,-geometry) %>%
  filter(str_detect(indicador, "indUp")) %>%
  ggplot() +
  geom_sf(data = map_uf, fill = "white") + 
  geom_sf(aes(fill = valor), 
          # ajusta tamanho das linhas
          colour = "black", size = 0.1) +
  # muda escala de cores
  scale_fill_viridis_c(option = 2, direction = 1, labels = scales::percent) +
  # tira sistema cartesiano
  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  facet_wrap(~factor(indicador, 
                     levels = c("indUp100","indUp80","indUp60","indUp40")))

muni_map %>%
  filter(Prestadora == "E") %>%
  gather(key = "indicador", value = "valor", 
         -nome, -City, -State, -MicroRegion, -MesoRegion,-Region,-Prestadora,-geometry) %>%
  filter(str_detect(indicador, "indUp")) %>%
  ggplot() +
  geom_sf(data = map_uf, fill = "white") + 
  geom_sf(aes(fill = valor), 
          # ajusta tamanho das linhas
          colour = "black", size = 0.1) +
  # muda escala de cores
  scale_fill_viridis_c(option = 2, direction = 1, labels = scales::percent) +
  # tira sistema cartesiano
  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  facet_wrap(~factor(indicador, 
                     levels = c("indUp100","indUp80","indUp60","indUp40")))

muni_map %>%
  filter(Prestadora == "F") %>%
  gather(key = "indicador", value = "valor", 
         -nome, -City, -State, -MicroRegion, -MesoRegion,-Region,-Prestadora,-geometry) %>%
  filter(str_detect(indicador, "indUp")) %>%
  ggplot() +
  geom_sf(data = map_uf, fill = "white") + 
  geom_sf(aes(fill = valor), 
          # ajusta tamanho das linhas
          colour = "black", size = 0.1) +
  # muda escala de cores
  scale_fill_viridis_c(option = 2, direction = 1, labels = scales::percent) +
  # tira sistema cartesiano
  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  facet_wrap(~factor(indicador, 
                     levels = c("indUp100","indUp80","indUp60","indUp40")))
