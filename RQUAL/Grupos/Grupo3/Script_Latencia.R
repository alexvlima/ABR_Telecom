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

latencia_claro <- read_delim("Dados/SCM_Latencia_2020-12_CLARO.csv.gz", 
                            delim = ";")

latencia_claro <-
  latencia_claro %>% filter(is.na(`Tipo de Descarte`) == TRUE) %>%
  select(deviceID, cod_mun = `codigo IBGE`,  latencia = avgRTT) %>%
  mutate(Prestadora = "A")

latencia_claro$latencia <- as.numeric(latencia_claro$latencia)
glimpse(latencia_claro)

# OI MOVEL # 

latencia_oi_movel <- read_delim("Dados/SCM_Latencia_2020-12_OI_MOVEL.csv.gz", 
                             delim = ";")

latencia_oi_movel <- 
  latencia_oi_movel %>% filter(is.na(`Tipo de Descarte`) == TRUE) %>%
  select(deviceID, cod_mun = `codigo IBGE`,  latencia = avgRTT) %>%
  mutate(Prestadora = "B")

glimpse(latencia_oi_movel)

# OI # 

latencia_oi <- read_delim("Dados/SCM_Latencia_2020-12_OI.csv.gz", 
                         delim = ";")

latencia_oi <- 
  latencia_oi %>% filter(is.na(`Tipo de Descarte`) == TRUE) %>%
  select(deviceID, cod_mun = `codigo IBGE`, latencia = avgRTT) %>%
  mutate(Prestadora = "B")

glimpse(latencia_oi)

# SKY # 

latencia_sky <- read_delim("Dados/SCM_Latencia_2020-12_SKY.csv.gz", 
                          delim = ";")

latencia_sky <- 
  latencia_sky %>% filter(is.na(`Tipo de Descarte`) == TRUE) %>%
  select(deviceID, cod_mun = `codigo IBGE`, latencia = avgRTT) %>%
  mutate(Prestadora = "C")

glimpse(latencia_sky)

# TIM # 

latencia_tim <- read_delim("Dados/SCM_Latencia_2020-12_TIM.csv.gz", 
                          delim = ";")

latencia_tim <- 
  latencia_tim %>% filter(is.na(`Tipo de Descarte`) == TRUE) %>%
  select(deviceID, cod_mun = `codigo IBGE`, latencia = avgRTT) %>%
  mutate(Prestadora = "D")

glimpse(latencia_tim)

# VIVO # 

latencia_vivo <- read_delim("Dados/SCM_Latencia_2020-12_VIVO.csv.gz", 
                           delim = ";")

latencia_vivo <- 
  latencia_vivo %>% filter(is.na(`Tipo de Descarte`) == TRUE) %>%
  select(deviceID, cod_mun = `codigo IBGE`, latencia = avgRTT) %>%
  mutate(Prestadora = "E")

glimpse(latencia_vivo)

###############
### DATASET ###
###############

df <- bind_rows(latencia_claro, 
                latencia_oi_movel, 
                latencia_oi, 
                latencia_sky, 
                latencia_tim, 
                latencia_vivo)
glimpse(df)

# rm(latencia_claro, latencia_oi, latencia_oi_movel, latencia_sky, latencia_tim, latencia_vivo)

#################
### INDICADOR ###
#################

df <- 
  df %>%
  mutate(latencia_50 = if_else(latencia <= 50, 1, 0),
         latencia_65 = if_else(latencia <= 65, 1, 0),
         latencia_80 = if_else(latencia <= 80, 1, 0),
         latencia_100 = if_else(latencia <= 100, 1, 0))

base_munic <- 
  df %>% 
  group_by(cod_mun, Prestadora) %>% 
  summarise(latencia_50 = mean(latencia_50),
            latencia_65 = mean(latencia_65),
            latencia_80 = mean(latencia_80),
            latencia_100 = mean(latencia_100))

glimpse(base_munic)

base_munic$Prestadora <- as.factor(base_munic$Prestadora)

#####################
### ANALISE - CDF ###
#####################

base_munic %>%
  ggplot(aes(latencia_50, group = Prestadora, color = Prestadora)) + 
  stat_ecdf(size = 1) + 
  scale_x_continuous(name = "", labels = scales::percent) + 
  scale_y_continuous(name = "cdf") +
  ggtitle("Indicador Latência 50ms") + 
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)) 

base_munic %>%
  ggplot(aes(latencia_65, group = Prestadora, color = Prestadora)) + 
  stat_ecdf(size = 1) + 
  scale_x_continuous(name = "", labels = scales::percent) + 
  scale_y_continuous(name = "cdf") +
  ggtitle("Indicador Latência 65ms") + 
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)) 

base_munic %>%
  ggplot(aes(latencia_80, group = Prestadora, color = Prestadora)) + 
  stat_ecdf(size = 1) + 
  scale_x_continuous(name = "", labels = scales::percent) + 
  scale_y_continuous(name = "cdf") +
  ggtitle("Indicador Latência 80ms") + 
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)) 

base_munic %>%
  ggplot(aes(latencia_100, group = Prestadora, color = Prestadora)) + 
  stat_ecdf(size = 1) + 
  scale_x_continuous(name = "", breaks = c(0,0.25,0.5,0.75,1), labels = scales::percent) + 
  scale_y_continuous(name = "cdf") +
  ggtitle("Indicador Latência 100ms") + 
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)) 


