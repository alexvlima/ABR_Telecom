#################
### DIRETORIO ###
#################

# getwd()
setwd("/Users/alexvlima/Library/Mobile Documents/com~apple~CloudDocs/ABR_Telecom_ESAQ/Grupos/Grupo5/Analise_2021/Dados/")

###################
### BIBLIOTECAS ###
###################

library(tidyverse)
library(stringr)
library(brazilmaps)
library(sf)

################
### ARQUIVOS ###
################

# rm(list = ls())

# CLARO #

# Base Processada #
temp_claro <- paste0("./CLARO/", list.files(path = "./CLARO/", pattern = "BASE_PROCESSADA.*csv"))
base_processada_claro <- lapply(temp_claro, read_csv2)

mes_ano_arquivo <- str_extract_all(string = temp_claro, pattern = "[[:digit:]]+")

base_processada_claro[[1]]$MES_ANO <- mes_ano_arquivo[[1]][[2]]
base_processada_claro[[2]]$MES_ANO <- mes_ano_arquivo[[2]][[2]]
base_processada_claro[[3]]$MES_ANO <- mes_ano_arquivo[[3]][[2]]
base_processada_claro[[4]]$MES_ANO <- mes_ano_arquivo[[4]][[2]]
base_processada_claro[[5]]$MES_ANO <- mes_ano_arquivo[[5]][[2]]

base_processada_claro <- bind_rows(base_processada_claro)
glimpse(base_processada_claro)

rm(temp_claro, mes_ano_arquivo)

# OI #

# Base Processada #
temp_oi <- paste0("./OI/", list.files(path = "./OI/", pattern = "BASE_PROCESSADA.*csv"))
base_processada_oi <- lapply(temp_oi, read_csv2)

mes_ano_arquivo <- str_extract_all(string = temp_oi, pattern = "[[:digit:]]+")

base_processada_oi[[1]]$MES_ANO <- mes_ano_arquivo[[1]][[2]]
base_processada_oi[[2]]$MES_ANO <- mes_ano_arquivo[[2]][[2]]
base_processada_oi[[3]]$MES_ANO <- mes_ano_arquivo[[3]][[2]]
base_processada_oi[[4]]$MES_ANO <- mes_ano_arquivo[[4]][[2]]
base_processada_oi[[5]]$MES_ANO <- mes_ano_arquivo[[5]][[2]]

base_processada_oi[[1]]$CODIGO_DE_ACESSO_ID_DO_ASSI <- as.numeric(base_processada_oi[[1]]$CODIGO_DE_ACESSO_ID_DO_ASSI)
base_processada_oi[[2]]$CODIGO_DE_ACESSO_ID_DO_ASSI <- as.numeric(base_processada_oi[[2]]$CODIGO_DE_ACESSO_ID_DO_ASSI)
base_processada_oi[[3]]$CODIGO_DE_ACESSO_ID_DO_ASSI <- as.numeric(base_processada_oi[[3]]$CODIGO_DE_ACESSO_ID_DO_ASSI)
base_processada_oi[[4]]$CODIGO_DE_ACESSO_ID_DO_ASSI <- as.numeric(base_processada_oi[[4]]$CODIGO_DE_ACESSO_ID_DO_ASSI)
base_processada_oi[[5]]$CODIGO_DE_ACESSO_ID_DO_ASSI <- as.numeric(base_processada_oi[[5]]$CODIGO_DE_ACESSO_ID_DO_ASSI)

base_processada_oi <- bind_rows(base_processada_oi)
glimpse(base_processada_oi)

rm(temp_oi, mes_ano_arquivo)

# SKY #

# Base Processada #
temp_sky <- paste0("./SKY/", list.files(path = "./SKY/", pattern = "BASE_PROCESSADA.*csv"))
base_processada_sky <- lapply(temp_sky, read_csv2)

mes_ano_arquivo <- str_extract_all(string = temp_sky, pattern = "[[:digit:]]+")

base_processada_sky[[1]]$MES_ANO <- mes_ano_arquivo[[1]][[2]]
base_processada_sky[[2]]$MES_ANO <- mes_ano_arquivo[[2]][[2]]
base_processada_sky[[3]]$MES_ANO <- mes_ano_arquivo[[3]][[2]]
base_processada_sky[[4]]$MES_ANO <- mes_ano_arquivo[[4]][[2]]
base_processada_sky[[5]]$MES_ANO <- mes_ano_arquivo[[5]][[2]]

base_processada_sky[[1]]$CODIGO_DE_MEDICAO_IND9B <- as.character(base_processada_sky[[1]]$CODIGO_DE_MEDICAO_IND9B)
base_processada_sky[[2]]$CODIGO_DE_MEDICAO_IND9B <- as.character(base_processada_sky[[2]]$CODIGO_DE_MEDICAO_IND9B)
base_processada_sky[[3]]$CODIGO_DE_MEDICAO_IND9B <- as.character(base_processada_sky[[3]]$CODIGO_DE_MEDICAO_IND9B)
base_processada_sky[[4]]$CODIGO_DE_MEDICAO_IND9B <- as.character(base_processada_sky[[4]]$CODIGO_DE_MEDICAO_IND9B)
base_processada_sky[[5]]$CODIGO_DE_MEDICAO_IND9B <- as.character(base_processada_sky[[5]]$CODIGO_DE_MEDICAO_IND9B)

base_processada_sky[[1]]$CODIGO_DE_MEDICAO_IND9A <- as.character(base_processada_sky[[1]]$CODIGO_DE_MEDICAO_IND9A)
base_processada_sky[[2]]$CODIGO_DE_MEDICAO_IND9A <- as.character(base_processada_sky[[2]]$CODIGO_DE_MEDICAO_IND9A)
base_processada_sky[[3]]$CODIGO_DE_MEDICAO_IND9A <- as.character(base_processada_sky[[3]]$CODIGO_DE_MEDICAO_IND9A)
base_processada_sky[[4]]$CODIGO_DE_MEDICAO_IND9A <- as.character(base_processada_sky[[4]]$CODIGO_DE_MEDICAO_IND9A)
base_processada_sky[[5]]$CODIGO_DE_MEDICAO_IND9A <- as.character(base_processada_sky[[5]]$CODIGO_DE_MEDICAO_IND9A)

base_processada_sky <- bind_rows(base_processada_sky)
glimpse(base_processada_sky)

rm(temp_sky, mes_ano_arquivo)

# TIM #

# Base Processada #
temp_tim <- paste0("./TIM/", list.files(path = "./TIM/", pattern = "BASE_PROCESSADA.*csv"))
base_processada_tim <- lapply(temp_tim, read_csv2)

mes_ano_arquivo <- str_extract_all(string = temp_tim, pattern = "[[:digit:]]+")

base_processada_tim[[1]]$MES_ANO <- mes_ano_arquivo[[1]][[2]]
base_processada_tim[[2]]$MES_ANO <- mes_ano_arquivo[[2]][[2]]
base_processada_tim[[3]]$MES_ANO <- mes_ano_arquivo[[3]][[2]]
base_processada_tim[[4]]$MES_ANO <- mes_ano_arquivo[[4]][[2]]
base_processada_tim[[5]]$MES_ANO <- mes_ano_arquivo[[5]][[2]]

base_processada_tim[[1]]$CODIGO_DE_ACESSO_ID_DO_ASSI <- as.numeric(base_processada_tim[[1]]$CODIGO_DE_ACESSO_ID_DO_ASSI)
base_processada_tim[[2]]$CODIGO_DE_ACESSO_ID_DO_ASSI <- as.numeric(base_processada_tim[[2]]$CODIGO_DE_ACESSO_ID_DO_ASSI)
base_processada_tim[[3]]$CODIGO_DE_ACESSO_ID_DO_ASSI <- as.numeric(base_processada_tim[[3]]$CODIGO_DE_ACESSO_ID_DO_ASSI)
base_processada_tim[[4]]$CODIGO_DE_ACESSO_ID_DO_ASSI <- as.numeric(base_processada_tim[[4]]$CODIGO_DE_ACESSO_ID_DO_ASSI)
base_processada_tim[[5]]$CODIGO_DE_ACESSO_ID_DO_ASSI <- as.numeric(base_processada_tim[[5]]$CODIGO_DE_ACESSO_ID_DO_ASSI)

base_processada_tim <- bind_rows(base_processada_tim)
glimpse(base_processada_tim)

rm(temp_tim, mes_ano_arquivo)

# VIVO #

# Base Processada #
temp_vivo <- paste0("./VIVO/", list.files(path = "./VIVO/", pattern = "BASE_PROCESSADA.*csv"))
base_processada_vivo <- lapply(temp_vivo, read_csv2)

mes_ano_arquivo <- str_extract_all(string = temp_vivo, pattern = "[[:digit:]]+")

base_processada_vivo[[1]]$MES_ANO <- mes_ano_arquivo[[1]][[2]]
base_processada_vivo[[2]]$MES_ANO <- mes_ano_arquivo[[2]][[2]]
base_processada_vivo[[3]]$MES_ANO <- mes_ano_arquivo[[3]][[2]]
base_processada_vivo[[4]]$MES_ANO <- mes_ano_arquivo[[4]][[2]]
base_processada_vivo[[5]]$MES_ANO <- mes_ano_arquivo[[5]][[2]]

base_processada_vivo <- bind_rows(base_processada_vivo)
glimpse(base_processada_vivo)

rm(temp_vivo, mes_ano_arquivo)

###############
### DATASET ###
###############

base_processada_claro <- 
  base_processada_claro %>% 
  select(PRESTADORA, MES_ANO, 
         ID_MUNICIPIO, SERVICO, CATEGORIA_DO_CLIENTE, 
         TIPO_DE_VISITA_TECNICA, FECHAMENTO, TURNO_DO_PERIODO_AGENDADO,
         CODIGO_DE_MEDICAO_IND9A, CODIGO_DE_MEDICAO_IND9B)

base_processada_oi <- 
  base_processada_oi %>% 
  select(PRESTADORA, MES_ANO,
         ID_MUNICIPIO, SERVICO, CATEGORIA_DO_CLIENTE, 
         TIPO_DE_VISITA_TECNICA, FECHAMENTO, TURNO_DO_PERIODO_AGENDADO,
         CODIGO_DE_MEDICAO_IND9A, CODIGO_DE_MEDICAO_IND9B)

base_processada_sky <- 
  base_processada_sky %>% 
  select(PRESTADORA, MES_ANO,
         ID_MUNICIPIO, SERVICO, CATEGORIA_DO_CLIENTE, 
         TIPO_DE_VISITA_TECNICA, FECHAMENTO, TURNO_DO_PERIODO_AGENDADO,
         CODIGO_DE_MEDICAO_IND9A, CODIGO_DE_MEDICAO_IND9B)

base_processada_tim <- 
  base_processada_tim %>% 
  select(PRESTADORA, MES_ANO,
         ID_MUNICIPIO, SERVICO, CATEGORIA_DO_CLIENTE, 
         TIPO_DE_VISITA_TECNICA, FECHAMENTO, TURNO_DO_PERIODO_AGENDADO,
         CODIGO_DE_MEDICAO_IND9A, CODIGO_DE_MEDICAO_IND9B)

base_processada_vivo <- 
  base_processada_vivo %>% 
  select(PRESTADORA, MES_ANO,
         ID_MUNICIPIO, SERVICO, CATEGORIA_DO_CLIENTE, 
         TIPO_DE_VISITA_TECNICA, FECHAMENTO, TURNO_DO_PERIODO_AGENDADO,
         CODIGO_DE_MEDICAO_IND9A, CODIGO_DE_MEDICAO_IND9B)

dfs = sapply(.GlobalEnv, is.data.frame) 
dfs

base_processada <- do.call(bind_rows, mget(names(dfs)[dfs]))
glimpse(base_processada)

rm(dfs)
rm(list = ls(pattern = "base_processada_"))

###############
### ANALISE ###
###############

base_processada <- 
  base_processada %>% 
  mutate(PRESTADORA_ANONIMA = factor(PRESTADORA, 
                                     levels = c("CLARO","OI","SKY","TIM","VIVO"),
                                     labels = c("A","B","C","D","E")))

Encoding(base_processada$CATEGORIA_DO_CLIENTE) <- "latin1"
Encoding(base_processada$TIPO_DE_VISITA_TECNICA) <- "latin1"

base_processada$MES_ANO <- 
  factor(base_processada$MES_ANO, 
         levels = c("082020","092020","102020","112020","122020"),
         labels = c("Ago", "Set", "Out", "Nov", "Dez")) 

unique(base_processada$CATEGORIA_DO_CLIENTE)

base_processada$CATEGORIA_DO_CLIENTE <-
  ifelse(base_processada$CATEGORIA_DO_CLIENTE %in% c("RED","WHITE","BLUE","PME SLA"), NA,
       ifelse(base_processada == "RESIDÃƒÅ NCIAL", "RESIDENCIAL",
              ifelse(base_processada == "NÃƒO RESIDENCIAL", "NÃO RESIDENCIAL", 
                     base_processada$CATEGORIA_DO_CLIENTE)))

base_processada %>%
  filter(is.na(CATEGORIA_DO_CLIENTE) == FALSE) %>%
  mutate(CATEGORIA_DO_CLIENTE = factor(CATEGORIA_DO_CLIENTE)) %>%
  group_by(CATEGORIA_DO_CLIENTE) %>%
  summarise(N = n()) %>%
  mutate(prop = round(N/sum(N), digits = 2)) %>%
  ggplot(aes(x = fct_rev(CATEGORIA_DO_CLIENTE), y = prop, label = N)) + 
  geom_col(fill = "steelblue") + 
  geom_text(position = position_dodge(width = 0.9), vjust = -0.5, size = 2.5) + 
  geom_text(aes(label = scales::percent(x = prop, accuracy = 1), 
                y = prop), 
            position = position_dodge(width = 0.9), vjust = 1, size = 2.5) + 
  scale_y_continuous(name = "", labels=scales::percent) +
  xlab("CATEGORIA DO CLIENTE") + 
  theme_minimal()

base_processada %>%
  filter(is.na(TIPO_DE_VISITA_TECNICA) == FALSE) %>%
  mutate(TIPO_DE_VISITA_TECNICA = factor(TIPO_DE_VISITA_TECNICA, 
                levels = c("INSTALAÇÃO", "REPARO", "MUDANÇA DE ENDEREÇO"))) %>%
  group_by(TIPO_DE_VISITA_TECNICA) %>%
  summarise(N = n()) %>%
  mutate(prop = round(N/sum(N), digits = 2)) %>%
  ggplot(aes(x = TIPO_DE_VISITA_TECNICA, y = prop, label = N)) + 
  geom_col(fill = "steelblue") + 
  geom_text(position = position_dodge(width = 0.9), vjust = -0.5, size = 2.5) + 
  geom_text(aes(label = scales::percent(x = prop, accuracy = 1), 
                y = prop), 
            position = position_dodge(width = 0.9), vjust = 1, size = 2.5) + 
  scale_y_continuous(name = "", labels=scales::percent) +
  xlab("TIPO DE VISITA TÉCNICA") + 
  theme_minimal()

base_processada %>%
  filter(is.na(FECHAMENTO) == FALSE) %>%
  group_by(PRESTADORA_ANONIMA, MES_ANO, FECHAMENTO) %>%
  summarise(N = n()) %>%
  mutate(prop = round(N/sum(N), digits = 2)) %>%
  ggplot(aes(x = MES_ANO, y = N, fill = FECHAMENTO, label = N)) + 
  geom_col(position = "dodge") + 
  geom_text(position = position_dodge(width = 0.9), vjust = -0.5, size = 2.5) + 
  geom_text(aes(label = scales::percent(x = prop, accuracy = 1), 
                y = prop), 
            position = position_dodge(width = 0.9), vjust = 1, size = 2.5) + 
  xlab("") +
  ylab("") + 
  theme_light() + 
  facet_wrap(~PRESTADORA_ANONIMA, scales = "free")

tabela3 <- 
  base_processada %>%
  group_by(PRESTADORA_ANONIMA, MES_ANO, 
           CODIGO_DE_MEDICAO_IND9A, CODIGO_DE_MEDICAO_IND9B) %>%
  summarise(N = n())

# write_csv2(tabela3, "tabela3.csv")
# rm(tabela3)

# MAPA #

base_processada_gather <-
  base_processada %>%
  select(PRESTADORA, ID_MUNICIPIO, SERVICO, 
         CODIGO_DE_MEDICAO_IND9A, CODIGO_DE_MEDICAO_IND9B) %>%
  mutate(IND9A = ifelse(CODIGO_DE_MEDICAO_IND9A == "00", 0, 1),
         IND9B = ifelse(CODIGO_DE_MEDICAO_IND9B == "00", 0, 1)) %>%
  select(-CODIGO_DE_MEDICAO_IND9A, -CODIGO_DE_MEDICAO_IND9B) %>%
  group_by(PRESTADORA, ID_MUNICIPIO, SERVICO) %>%
  dplyr::summarize(IND9A = mean(IND9A, na.rm = T),
                   IND9B = mean(IND9B, na.rm = T)) %>%
  gather(key = "TIPO", value = "DESCARTE", -PRESTADORA, -ID_MUNICIPIO, -SERVICO)
  
muni_map <- get_brmap("City") %>% 
  left_join(base_processada_gather, c("City" = "ID_MUNICIPIO"))

map_uf <- get_brmap("State")

muni_map %>%
  filter(PRESTADORA == "CLARO", is.na(SERVICO) == FALSE) %>%
  ggplot() +
  geom_sf(data = map_uf, fill = "white") + 
  geom_sf(aes(fill = DESCARTE), 
          # ajusta tamanho das linhas
          colour = "black", size = 0.1) +
  # muda escala de cores
  scale_fill_viridis_c(option = 2, direction = -1, labels = scales::percent) +
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
  geom_sf(aes(fill = DESCARTE), 
          # ajusta tamanho das linhas
          colour = "black", size = 0.1) +
  # muda escala de cores
  scale_fill_viridis_c(option = 2, direction = -1, labels = scales::percent) +
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
  geom_sf(aes(fill = DESCARTE), 
          # ajusta tamanho das linhas
          colour = "black", size = 0.1) +
  # muda escala de cores
  scale_fill_viridis_c(option = 2, direction = -1, labels = scales::percent) +
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
  geom_sf(aes(fill = DESCARTE), 
          # ajusta tamanho das linhas
          colour = "black", size = 0.1) +
  # muda escala de cores
  scale_fill_viridis_c(option = 2, direction = -1, labels = scales::percent) +
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
  geom_sf(aes(fill = DESCARTE), 
          # ajusta tamanho das linhas
          colour = "black", size = 0.1) +
  # muda escala de cores
  scale_fill_viridis_c(option = 2, direction = -1, labels = scales::percent) +
  # tira sistema cartesiano
  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  facet_grid(rows = vars(TIPO), cols = vars(SERVICO))
