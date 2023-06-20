#################
### DIRETORIO ###
#################

getwd()
setwd("/Users/alexvlima/Desktop/ABR_Telecom_ESAQ/")

###################
### BIBLIOTECAS ###
###################

library(readxl)
library(tidyverse)

################
### ARQUIVOS ###
################

rm(list = ls())
list.files()

df_oi1 <- read_excel("TUPs Oi Coletivos Fique Ligado-Dez2019 e Mar2020.xlsx", sheet = 1)
df_oi2 <- read_excel("TUPs Oi Coletivos Fique Ligado-Dez2019 e Mar2020.xlsx", sheet = 2)
df_claro <- read_excel("TUP_CLaro.xlsx")
df_vivo <- read_excel("Inf7-VIVO.xlsx")

###############
### AJUSTES ###
###############

glimpse(df_oi1)
glimpse(df_oi2)
glimpse(df_claro)
glimpse(df_vivo)

# NOMES DAS COLUNAS #

nomes <- c("num_telefone", "DDD", "ult_comunicacao", "status", "cod_IBGE", "UF")

df_oi1 <- df_oi1 %>% select(-`Data Carga arquivo no Fique Ligado`)
colnames(df_oi1) <- nomes

df_oi2 <- df_oi2 %>% select(-`Data Carga arquivo no Fique Ligado`)
colnames(df_oi2) <- nomes

df_claro <- df_claro %>% select(-`Data Carga arquivo`)
colnames(df_claro) <- nomes

df_vivo <- df_vivo %>% select(-`Data do arquivo`,-Município)
colnames(df_vivo) <- nomes

# STATUS DO TUP #

ativo <- c(0, 8, 13, 14)
inativo <- c(2, 5, 9, 15)
transitorio <- c(3, 7)

df_total <- rbind(df_oi1, df_oi2, df_claro, df_vivo)
glimpse(df_total)

tab1 <- table(df_total$status)
prop.table(tab1)

df_total <- df_total %>% filter(status %in% c(ativo, inativo, transitorio))
df_total$status_2 <- if_else(df_total$status %in% ativo, 1,
                             if_else(df_total$status %in% inativo, 2, 3))

###############
### TABELAS ###
###############

# 1 - ATIVO
# 2 - INATIVO
# 3 - TRANSITORIO

tab2 <- table(df_total$status_2)
prop.table(tab2)

# QUANTIDADE DE STATUS POR DDD #
resultado <- 
  df_total %>%
  mutate(DDD_Telefone = paste0(df_total$DDD,df_total$num_telefone)) %>%
  group_by(DDD_Telefone) %>%
  count(status_2)
glimpse(resultado)

# AJUSTES PARA COLOCAR A BASE PARA EXPORTAR #
resultado_spread <- resultado %>% spread(status_2, n)
colnames(resultado_spread) <- c("DDD_Telefone", "Ativo", "Inativo", "Transitorio")
glimpse(resultado_spread)

# COLOCAR 0 QUANDO NÃO HÁ INFORMAÇÃO #
resultado_spread[is.na(resultado_spread)] <- 0

# QUANTIDADE DE DDD #
DDD <- substr(resultado_spread$DDD_Telefone,1,2)
length(unique(DDD))

# EXPORTAR BASE #
write_csv2(resultado_spread, "resultado.csv")
