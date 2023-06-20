#################
### DIRETORIO ###
#################

# getwd()
setwd("/Users/alexvlima/Downloads/Analise_Grupo3/")

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

jitter_claro <- read_delim("Dados/SCM_Jitter_2020-12_CLARO.csv.gz", 
                             delim = ";")

jitter_claro <- 
  jitter_claro %>% filter(is.na(`Tipo de Descarte`) == TRUE) %>%
  select(deviceID, cod_mun = `Codigo IBGE`,  jitter = Jitter) %>%
  mutate(Prestadora = "A")

jitter_claro$jitter <- as.numeric(jitter_claro$jitter)

glimpse(jitter_claro)

# OI MOVEL # 

jitter_oi_movel <- read_delim("Dados/SCM_Jitter_2020-12_OI_MOVEL.csv.gz", 
                                delim = ";")

jitter_oi_movel <- 
  jitter_oi_movel %>% filter(is.na(`Tipo de Descarte`) == TRUE) %>%
  select(deviceID, cod_mun = `Codigo IBGE`,  jitter = Jitter) %>%
  mutate(Prestadora = "B")

glimpse(jitter_oi_movel)

# OI # 

jitter_oi <- read_delim("Dados/SCM_Jitter_2020-12_OI.csv.gz", 
                          delim = ";")

jitter_oi <- 
  jitter_oi %>% filter(is.na(`Tipo de Descarte`) == TRUE) %>%
  select(deviceID, cod_mun = `Codigo IBGE`,  jitter = Jitter) %>%
  mutate(Prestadora = "B")

glimpse(jitter_oi)

# SKY # 

jitter_sky <- read_delim("Dados/SCM_Jitter_2020-12_SKY.csv.gz", 
                           delim = ";")

jitter_sky <- 
  jitter_sky %>% filter(is.na(`Tipo de Descarte`) == TRUE) %>%
  select(deviceID, cod_mun = `Codigo IBGE`,  jitter = Jitter) %>%
  mutate(Prestadora = "C")

glimpse(jitter_sky)

# TIM # 

jitter_tim <- read_delim("Dados/SCM_Jitter_2020-12_TIM.csv.gz", 
                           delim = ";")

jitter_tim <- 
  jitter_tim %>% filter(is.na(`Tipo de Descarte`) == TRUE) %>%
  select(deviceID, cod_mun = `Codigo IBGE`,  jitter = Jitter) %>%
  mutate(Prestadora = "D")

glimpse(jitter_tim)

# VIVO # 

jitter_vivo <- read_delim("Dados/SCM_Jitter_2020-12_VIVO.csv.gz", 
                            delim = ";")

jitter_vivo <- 
  jitter_vivo %>% filter(is.na(`Tipo de Descarte`) == TRUE) %>%
  select(deviceID, cod_mun = `Codigo IBGE`,  jitter = Jitter) %>%
  mutate(Prestadora = "E")

glimpse(jitter_vivo)

###############
### DATASET ###
###############

df <- bind_rows(jitter_claro, 
                jitter_oi_movel, 
                jitter_oi, 
                jitter_sky, 
                jitter_tim, 
                jitter_vivo)
glimpse(df)

# rm(jitter_claro, jitter_oi, jitter_oi_movel, jitter_sky, jitter_tim, jitter_vivo)

#################
### INDICADOR ###
#################

df <- 
  df %>%
  mutate(jitter_30 = if_else(jitter <= 30, 1, 0),
         jitter_40 = if_else(jitter <= 40, 1, 0),
         jitter_50 = if_else(jitter <= 50, 1, 0))

base_munic <- 
  df %>% 
  group_by(cod_mun, Prestadora) %>% 
  summarise(jitter_30 = mean(jitter_30),
            jitter_40 = mean(jitter_40),
            jitter_50 = mean(jitter_50))

glimpse(base_munic)

base_munic$Prestadora <- as.factor(base_munic$Prestadora)

#####################
### ANALISE - CDF ###
#####################

base_munic %>%
  ggplot(aes(jitter_30, group = Prestadora, color = Prestadora)) + 
  stat_ecdf(size = 1) + 
  scale_x_continuous(name = "", labels = scales::percent) + 
  scale_y_continuous(name = "cdf") +
  ggtitle("Indicador Jitter 30ms") + 
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)) 

base_munic %>%
  ggplot(aes(jitter_40, group = Prestadora, color = Prestadora)) + 
  stat_ecdf(size = 1) + 
  scale_x_continuous(name = "", labels = scales::percent) + 
  scale_y_continuous(name = "cdf") +
  ggtitle("Indicador Jitter 40ms") + 
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)) 

base_munic %>%
  ggplot(aes(jitter_50, group = Prestadora, color = Prestadora)) + 
  stat_ecdf(size = 1) + 
  scale_x_continuous(name = "", breaks = c(0.0,0.25,0.50,0.75,1.0), labels = scales::percent) + 
  scale_y_continuous(name = "cdf") +
  ggtitle("Indicador Jitter 50ms") + 
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)) 

