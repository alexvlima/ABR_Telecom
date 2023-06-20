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

perda_claro <- read_delim("Dados/SCM_Perda_Pacotes_2020-12_CLARO.csv.gz", 
                             delim = ";")

perda_claro <- 
  perda_claro %>% filter(is.na(`Tipo de Descarte`) == TRUE) %>%
  select(deviceID, cod_mun = `Codigo IBGE`,  
         perda_failure = `Packet Loss Failure`, perda_successes = `Packet Loss Successes`) %>%
  mutate(Prestadora = "A")

glimpse(perda_claro)

# OI MOVEL # 

perda_oi_movel <- read_delim("Dados/SCM_Perda_Pacotes_2020-12_OI_MOVEL.csv.gz", 
                                delim = ";")

perda_oi_movel <- 
  perda_oi_movel %>% filter(is.na(`Tipo de Descarte`) == TRUE) %>%
  select(deviceID, cod_mun = `Codigo IBGE`,  
         perda_failure = `Packet Loss Failure`, perda_successes = `Packet Loss Successes`) %>%
  mutate(Prestadora = "B")

glimpse(perda_oi_movel)

# OI # 

perda_oi <- read_delim("Dados/SCM_Perda_Pacotes_2020-12_OI.csv.gz", 
                          delim = ";")

perda_oi <- 
  perda_oi %>% filter(is.na(`Tipo de Descarte`) == TRUE) %>%
  select(deviceID, cod_mun = `Codigo IBGE`,  
         perda_failure = `Packet Loss Failure`, perda_successes = `Packet Loss Successes`) %>%
  mutate(Prestadora = "B")

glimpse(perda_oi)

# SKY # 

perda_sky <- read_delim("Dados/SCM_Perda_Pacotes_2020-12_SKY.csv.gz", 
                           delim = ";")

perda_sky <- 
  perda_sky %>% filter(is.na(`Tipo de Descarte`) == TRUE) %>%
  select(deviceID, cod_mun = `Codigo IBGE`,  
         perda_failure = `Packet Loss Failure`, perda_successes = `Packet Loss Successes`) %>%
  mutate(Prestadora = "C")

glimpse(perda_sky)

# TIM # 

perda_tim <- read_delim("Dados/SCM_Perda_Pacotes_2020-12_TIM.csv.gz", 
                           delim = ";")

perda_tim <- 
  perda_tim %>% filter(is.na(`Tipo de Descarte`) == TRUE) %>%
  select(deviceID, cod_mun = `Codigo IBGE`,  
         perda_failure = `Packet Loss Failure`, perda_successes = `Packet Loss Successes`) %>%
  mutate(Prestadora = "D")

glimpse(perda_tim)

# VIVO # 

perda_vivo <- read_delim("Dados/SCM_Perda_Pacotes_2020-12_VIVO.csv.gz", 
                            delim = ";")

perda_vivo <- 
  perda_vivo %>% filter(is.na(`Tipo de Descarte`) == TRUE) %>%
  select(deviceID, cod_mun = `Codigo IBGE`,  
         perda_failure = `Packet Loss Failure`, perda_successes = `Packet Loss Successes`) %>%
  mutate(Prestadora = "E")

glimpse(perda_vivo)

###############
### DATASET ###
###############

df <- bind_rows(perda_claro, 
                perda_oi_movel, 
                perda_oi, 
                perda_sky, 
                perda_tim, 
                perda_vivo)
glimpse(df)

# rm(perda_claro, perda_oi, perda_oi_movel, perda_sky, perda_tim, perda_vivo)

#################
### INDICADOR ###
#################

df <- 
  df %>%
  mutate(perda = perda_failure / perda_successes) 

df <- 
  df %>%
  mutate(perda_1 = if_else(perda <= 1, 1, 0),
         perda_1.5 = if_else(perda <= 1.5, 1, 0),
         perda_2 = if_else(perda <= 2, 1, 0))

base_munic <- 
  df %>% 
  group_by(cod_mun, Prestadora) %>% 
  summarise(perda_1 = mean(perda_1),
            perda_1.5 = mean(perda_1.5),
            perda_2 = mean(perda_2))

glimpse(base_munic)

base_munic$Prestadora <- as.factor(base_munic$Prestadora)

#####################
### ANALISE - CDF ###
#####################

base_munic %>%
  ggplot(aes(perda_1, group = Prestadora, color = Prestadora)) + 
  stat_ecdf(size = 1) + 
  scale_x_continuous(name = "", labels = scales::percent) + 
  scale_y_continuous(name = "cdf") +
  ggtitle("Indicador Perda 1%") + 
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)) 

base_munic %>%
  ggplot(aes(perda_1.5, group = Prestadora, color = Prestadora)) + 
  stat_ecdf(size = 1) + 
  scale_x_continuous(name = "", labels = scales::percent) + 
  scale_y_continuous(name = "cdf") +
  ggtitle("Indicador Perda 1.5%") + 
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)) 

base_munic %>%
  ggplot(aes(perda_2, group = Prestadora, color = Prestadora)) + 
  stat_ecdf(size = 1) + 
  scale_x_continuous(name = "", labels = scales::percent) + 
  scale_y_continuous(name = "cdf") +
  ggtitle("Indicador Perda 2%") + 
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)) 

