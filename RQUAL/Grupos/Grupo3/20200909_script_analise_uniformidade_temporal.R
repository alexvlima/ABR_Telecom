#################
### DIRETORIO ###
#################

getwd()
setwd("/Users/alexvlima/Desktop/Desktop - Alexandre’s MacBook Pro/ABR_Telecom_ESAQ/Grupo3/")

###################
### BIBLIOTECAS ###
###################

library(tidyverse)
library(lubridate)

################
### ANALISES ###
################

list.files()

### CLARO ###

df_claro <- read_csv2("./SCM/SCM_Down_Up_2020-06_CLARO.csv.gz")
glimpse(df_claro)

# sum(is.na(df_claro$location))
# table(df_claro$Tipo.de.Descarte)

qtde_municipios <- 
  df_claro %>%
  filter(is.na(location) == FALSE, is.na(`Tipo de Descarte`) == TRUE) %>%
  group_by(COD_IBGE = `Codigo IBGE`, DIA = lubridate::day(dateTime)) %>%
  summarize(QTDE = n())

write_csv2(qtde_municipios, "distribuicao_medidas_claro.csv")

rm(df_claro, qtde_municipios)

### VIVO ###

df_vivo <- read_csv2("./SCM/SCM_Down_Up_2020-06_VIVO.csv.gz")
glimpse(df_vivo)

qtde_municipios <- 
  df_vivo %>%
  filter(is.na(location) == FALSE, is.na(`Tipo de Descarte`) == TRUE) %>%
  group_by(COD_IBGE = `Codigo IBGE`, DIA = lubridate::day(dateTime)) %>%
  summarize(QTDE = n())

write_csv2(qtde_municipios, "distribuicao_medidas_vivo.csv")

rm(df_vivo, qtde_municipios)

### TIM ###

df_tim <- read_csv2("./SCM/SCM_Down_Up_2020-06_TIM.csv.gz")
glimpse(df_tim)

qtde_municipios <- 
  df_tim %>%
  filter(is.na(location) == FALSE, is.na(`Tipo de Descarte`) == TRUE) %>%
  group_by(COD_IBGE = `Codigo IBGE`, DIA = lubridate::day(dateTime)) %>%
  summarize(QTDE = n())

write_csv2(qtde_municipios, "distribuicao_medidas_tim.csv")

rm(df_tim, qtde_municipios)

### OI ###

df_oi <- read_csv2("./SCM/SCM_Down_Up_2020-06_OI.csv.gz")
glimpse(df_oi)

qtde_municipios <- 
  df_oi %>%
  filter(is.na(location) == FALSE, is.na(`Tipo de Descarte`) == TRUE) %>%
  group_by(COD_IBGE = `Codigo IBGE`, DIA = lubridate::day(dateTime)) %>%
  summarize(QTDE = n())

write_csv2(qtde_municipios, "distribuicao_medidas_oi.csv")

rm(df_oi, qtde_municipios)

df_oi_movel <- read_csv2("./SCM/SCM_Down_Up_2020-06_OI_MOVEL.csv.gz")
glimpse(df_oi_movel)

qtde_municipios <- 
  df_oi_movel %>%
  filter(is.na(location) == FALSE, is.na(`Tipo de Descarte`) == TRUE) %>%
  group_by(COD_IBGE = `Codigo IBGE`, DIA = lubridate::day(dateTime)) %>%
  summarize(QTDE = n())

write_csv2(qtde_municipios, "distribuicao_medidas_oi.csv")

rm(df_oi_movel, qtde_municipios)

### SKY ###

df_sky <- read_csv2("./SCM/SCM_Down_Up_2020-06_SKY.csv.gz")
glimpse(df_sky)

qtde_municipios <- 
  df_sky %>%
  filter(is.na(location) == FALSE, is.na(`Tipo de Descarte`) == TRUE) %>%
  group_by(COD_IBGE = `Codigo IBGE`, DIA = lubridate::day(dateTime)) %>%
  summarize(QTDE = n())

write_csv2(qtde_municipios, "distribuicao_medidas_sky.csv")

rm(df_sky, qtde_municipios)

