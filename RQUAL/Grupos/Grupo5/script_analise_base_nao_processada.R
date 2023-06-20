#################
### DIRETORIO ###
#################

# getwd()
setwd("/Users/alexvlima/Library/Mobile Documents/com~apple~CloudDocs/ABR_Telecom_ESAQ/Grupos/Grupo5/Analise_2021/Dados/")

###################
### BIBLIOTECAS ###
###################

library(tidyverse)

################
### ARQUIVOS ###
################

# CLARO #

# Base Nao Processada #
temp_claro <- paste0("./CLARO/", list.files(path = "./CLARO/", pattern = "NAO_PROCESSA.*csv"))
base_nao_processada_claro <- lapply(temp_claro, read_csv2)

mes_ano_arquivo <- str_extract_all(string = temp_claro, pattern = "[[:digit:]]+")

base_nao_processada_claro[[1]]$MES_ANO <- mes_ano_arquivo[[1]][[2]]
base_nao_processada_claro[[2]]$MES_ANO <- mes_ano_arquivo[[2]][[2]]
base_nao_processada_claro[[3]]$MES_ANO <- mes_ano_arquivo[[3]][[2]]
base_nao_processada_claro[[4]]$MES_ANO <- mes_ano_arquivo[[4]][[2]]
base_nao_processada_claro[[5]]$MES_ANO <- mes_ano_arquivo[[5]][[2]]

base_nao_processada_claro <- bind_rows(base_nao_processada_claro)

base_nao_processada_claro$PRESTADORA <- "CLARO"

glimpse(base_nao_processada_claro)

rm(temp_claro, mes_ano_arquivo)

# OI #

# Base Nao Processada #
temp_oi <- paste0("./OI/", list.files(path = "./OI/", pattern = "NAO_PROCESSA.*csv"))
base_nao_processada_oi <- lapply(temp_oi, read_csv2)

mes_ano_arquivo <- str_extract_all(string = temp_oi, pattern = "[[:digit:]]+")

base_nao_processada_oi[[1]]$MES_ANO <- mes_ano_arquivo[[1]][[2]]
base_nao_processada_oi[[2]]$MES_ANO <- mes_ano_arquivo[[2]][[2]]
base_nao_processada_oi[[3]]$MES_ANO <- mes_ano_arquivo[[3]][[2]]
base_nao_processada_oi[[4]]$MES_ANO <- mes_ano_arquivo[[4]][[2]]
base_nao_processada_oi[[5]]$MES_ANO <- mes_ano_arquivo[[5]][[2]]

base_nao_processada_oi[[1]]$CODIGO_DE_ACESSO_ID_DO_ASSINANTE <- as.numeric(base_nao_processada_oi[[1]]$CODIGO_DE_ACESSO_ID_DO_ASSINANTE)
base_nao_processada_oi[[2]]$CODIGO_DE_ACESSO_ID_DO_ASSINANTE <- as.numeric(base_nao_processada_oi[[2]]$CODIGO_DE_ACESSO_ID_DO_ASSINANTE)
base_nao_processada_oi[[3]]$CODIGO_DE_ACESSO_ID_DO_ASSINANTE <- as.numeric(base_nao_processada_oi[[3]]$CODIGO_DE_ACESSO_ID_DO_ASSINANTE)
base_nao_processada_oi[[4]]$CODIGO_DE_ACESSO_ID_DO_ASSINANTE <- as.numeric(base_nao_processada_oi[[4]]$CODIGO_DE_ACESSO_ID_DO_ASSINANTE)
base_nao_processada_oi[[5]]$CODIGO_DE_ACESSO_ID_DO_ASSINANTE <- as.numeric(base_nao_processada_oi[[5]]$CODIGO_DE_ACESSO_ID_DO_ASSINANTE)

base_nao_processada_oi <- bind_rows(base_nao_processada_oi)
glimpse(base_nao_processada_oi)

rm(temp_oi, mes_ano_arquivo)

# SKY #

# Base Nao Processada #
temp_sky <- paste0("./SKY/", list.files(path = "./SKY/", pattern = "NAO_PROCESSA.*csv"))
base_nao_processada_sky <- lapply(temp_sky, read_csv2)

mes_ano_arquivo <- str_extract_all(string = temp_sky, pattern = "[[:digit:]]+")

base_nao_processada_sky[[1]]$MES_ANO <- mes_ano_arquivo[[1]][[2]]
base_nao_processada_sky[[2]]$MES_ANO <- mes_ano_arquivo[[2]][[2]]
base_nao_processada_sky[[3]]$MES_ANO <- mes_ano_arquivo[[3]][[2]]
base_nao_processada_sky[[4]]$MES_ANO <- mes_ano_arquivo[[4]][[2]]
base_nao_processada_sky[[5]]$MES_ANO <- mes_ano_arquivo[[5]][[2]]

base_nao_processada_sky <- bind_rows(base_nao_processada_sky)
base_nao_processada_sky$PRESTADORA <- "SKY"

glimpse(base_nao_processada_sky)

rm(temp_sky, mes_ano_arquivo)

# TIM #

# Base Nao Processada #
temp_tim <- paste0("./TIM/", list.files(path = "./TIM/", pattern = "NAO_PROCESSA.*csv"))
base_nao_processada_tim <- lapply(temp_tim, read_csv2)

mes_ano_arquivo <- str_extract_all(string = temp_tim, pattern = "[[:digit:]]+")

base_nao_processada_tim[[1]]$MES_ANO <- mes_ano_arquivo[[1]][[2]]
base_nao_processada_tim[[2]]$MES_ANO <- mes_ano_arquivo[[2]][[2]]
base_nao_processada_tim[[3]]$MES_ANO <- mes_ano_arquivo[[3]][[2]]
base_nao_processada_tim[[4]]$MES_ANO <- mes_ano_arquivo[[4]][[2]]
base_nao_processada_tim[[5]]$MES_ANO <- mes_ano_arquivo[[5]][[2]]

base_nao_processada_tim[[1]]$CODIGO_DE_ACESSO_ID_DO_ASSINANTE <- as.numeric(base_nao_processada_tim[[1]]$CODIGO_DE_ACESSO_ID_DO_ASSINANTE)
base_nao_processada_tim[[2]]$CODIGO_DE_ACESSO_ID_DO_ASSINANTE <- as.numeric(base_nao_processada_tim[[2]]$CODIGO_DE_ACESSO_ID_DO_ASSINANTE)
base_nao_processada_tim[[3]]$CODIGO_DE_ACESSO_ID_DO_ASSINANTE <- as.numeric(base_nao_processada_tim[[3]]$CODIGO_DE_ACESSO_ID_DO_ASSINANTE)
base_nao_processada_tim[[4]]$CODIGO_DE_ACESSO_ID_DO_ASSINANTE <- as.numeric(base_nao_processada_tim[[4]]$CODIGO_DE_ACESSO_ID_DO_ASSINANTE)
base_nao_processada_tim[[5]]$CODIGO_DE_ACESSO_ID_DO_ASSINANTE <- as.numeric(base_nao_processada_tim[[5]]$CODIGO_DE_ACESSO_ID_DO_ASSINANTE)

base_nao_processada_tim <- bind_rows(base_nao_processada_tim)
glimpse(base_nao_processada_tim)

rm(temp_tim, mes_ano_arquivo)

# VIVO #

# Base Nao Processada #
temp_vivo <- paste0("./VIVO/", list.files(path = "./VIVO/", pattern = "NAO_PROCESSA.*csv"))
base_nao_processada_vivo <- lapply(temp_vivo, read_csv2)

mes_ano_arquivo <- str_extract_all(string = temp_vivo, pattern = "[[:digit:]]+")

base_nao_processada_vivo[[1]]$MES_ANO <- mes_ano_arquivo[[1]][[2]]
base_nao_processada_vivo[[2]]$MES_ANO <- mes_ano_arquivo[[2]][[2]]
base_nao_processada_vivo[[3]]$MES_ANO <- mes_ano_arquivo[[3]][[2]]
base_nao_processada_vivo[[4]]$MES_ANO <- mes_ano_arquivo[[4]][[2]]
base_nao_processada_vivo[[5]]$MES_ANO <- mes_ano_arquivo[[5]][[2]]

base_nao_processada_vivo <- bind_rows(base_nao_processada_vivo)
glimpse(base_nao_processada_vivo)

rm(temp_vivo, mes_ano_arquivo)

###############
### DATASET ###
###############

# AJUSTE NA VARIAVEL ID_DA_SOLICITACAO_PROTOCOLO #

# base_nao_processada_claro$ID_DA_SOLICITACAO_PROTOCOLO <- as.character(base_nao_processada_claro$ID_DA_SOLICITACAO_PROTOCOLO)
# base_nao_processada_oi$ID_DA_SOLICITACAO_PROTOCOLO <- as.character(base_nao_processada_oi$ID_DA_SOLICITACAO_PROTOCOLO)
# base_nao_processada_sky$ID_DA_SOLICITACAO_PROTOCOLO <- as.character(base_nao_processada_sky$ID_DA_SOLICITACAO_PROTOCOLO)
# base_nao_processada_vivo$ID_DA_SOLICITACAO_PROTOCOLO <- as.character(base_nao_processada_vivo$ID_DA_SOLICITACAO_PROTOCOLO)

base_nao_processada_claro <- 
  base_nao_processada_claro %>%
  select(PRESTADORA, MES_ANO, COD_PRE)

base_nao_processada_oi <- 
  base_nao_processada_oi %>%
  select(PRESTADORA, MES_ANO, COD_PRE)

base_nao_processada_sky <- 
  base_nao_processada_sky %>%
  select(PRESTADORA, MES_ANO, COD_PRE)

base_nao_processada_tim <- 
  base_nao_processada_tim %>%
  select(PRESTADORA, MES_ANO, COD_PRE)

base_nao_processada_vivo <- 
  base_nao_processada_vivo %>%
  select(PRESTADORA, MES_ANO, COD_PRE)

dfs = sapply(.GlobalEnv, is.data.frame) 
dfs

base_nao_processada <- do.call(bind_rows, mget(names(dfs)[dfs]))
glimpse(base_nao_processada)

rm(dfs)
rm(list = ls(pattern = "base_nao_processada_"))

###############
### ANALISE ###
###############

tabela2 <- 
  base_nao_processada %>%
  group_by(PRESTADORA, MES_ANO, COD_PRE) %>%
  dplyr::summarise(N = n()) %>%
  arrange(PRESTADORA, MES_ANO)

# write_csv2(tabela2, "tabela2.csv")
# rm(tabela2)

