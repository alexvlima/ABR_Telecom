#################
### DIRETORIO ###
#################

# getwd()
setwd("/Users/alexvlima/Library/Mobile Documents/com~apple~CloudDocs/ABR_Telecom_ESAQ/Grupos/Grupo2/Estabilizacao_p/")

#################
### LIBRARIES ###
#################

library(tidyverse)

###############
### DATASET ###
###############

# rm(list = ls())
temp <- paste0("./Dados/Rawdata/", list.files(path = "./Dados/Rawdata/", pattern = "*.*csv"))
base_resultados <- lapply(temp, read_csv2, locale(encoding = "UTF-8"), col_names = TRUE, col_types = NULL)

rm(temp)

base_resultados <- lapply(base_resultados, function(x) select(x, 
                                                              PRESTADORA,
                                                              CODIGO_IBGE,
                                                              TECNOLOGIA,
                                                              VELOCIDADE_UPLOAD,
                                                              VELOCIDADE_DOWNLOAD,
                                                              LATENCIA_MEDIA,
                                                              LATENCIA_MINIMA,
                                                              LATENCIA_MAXIMA,
                                                              VARIACAO_DA_LATENCIA_JITTER,
                                                              PERDA_DE_PACOTES,
                                                              CM_IND4_UL,
                                                              CM_IND4_DL,
                                                              CM_IND5,
                                                              CM_IND6,
                                                              CM_IND7))

 base_resultados <- 
  base_resultados %>%
  map(~mutate(.,
              VARIACAO_DA_LATENCIA_JITTER = as.numeric(.$VARIACAO_DA_LATENCIA_JITTER),
              VELOCIDADE_UPLOAD = as.numeric(.$VELOCIDADE_UPLOAD),
              VELOCIDADE_DOWNLOAD = as.numeric(.$VELOCIDADE_DOWNLOAD),
              PERDA_DE_PACOTES = as.numeric(.$PERDA_DE_PACOTES))) 
  
base_resultados <- bind_rows(base_resultados)
glimpse(base_resultados)

###############
### FILTROS ###
###############

# sum(is.na(base_resultados$CODIGO_IBGE)==TRUE)

base_resultados <-
  base_resultados %>%
  filter(is.na(CODIGO_IBGE) == FALSE)

glimpse(base_resultados)

saveRDS(base_resultados, "Base_SMP.rds")
# base_resultados <- read_rds("Base_SMP.rds")

municipios_1000acessos <- 
  base_resultados %>%
  group_by(PRESTADORA, CODIGO_IBGE) %>%
  summarise(N = n()) %>%
  mutate(chave = paste0(PRESTADORA,CODIGO_IBGE)) %>%
  filter(N >= 1000)

#############
### IND 4 ###
#############

base_smp_ind4 <- 
  base_resultados %>%
  filter(paste0(PRESTADORA,CODIGO_IBGE) %in% municipios_1000acessos$chave) %>%
  filter(CM_IND4_UL=="00-VALIDA",CM_IND4_DL=="00-VALIDA") %>%
  select(PRESTADORA, CODIGO_IBGE, TECNOLOGIA, VELOCIDADE_DOWNLOAD, VELOCIDADE_UPLOAD, CM_IND4_UL, CM_IND4_DL)

base_smp_ind4 <- 
  base_smp_ind4 %>%
  mutate(IND4_DOWN = ifelse(TECNOLOGIA == "3G" & VELOCIDADE_DOWNLOAD >= 1500,1,
                            ifelse(TECNOLOGIA == "4G" & VELOCIDADE_DOWNLOAD >= 5000,1,0)),
         IND4_UP =   ifelse(TECNOLOGIA == "3G" & VELOCIDADE_DOWNLOAD >= 500,1,
                                 ifelse(TECNOLOGIA == "4G" & VELOCIDADE_DOWNLOAD >= 1500,1,0))) %>%
  select(PRESTADORA, CODIGO_IBGE, IND4_DOWN, IND4_UP)

base_smp_ind4 %>%
  mutate(IND4 = (IND4_DOWN + IND4_UP)/2) %>%
  group_by(PRESTADORA, CODIGO_IBGE) %>%
  summarise(IND4 = mean(IND4),
            N = n()) %>%
  ggplot(aes(x = N, y = IND4)) + 
  geom_line() +
  scale_y_continuous("IND4", labels = scales::percent) + 
  theme_minimal() +
  facet_wrap(~PRESTADORA)

base_smp_ind4 %>%
  mutate(IND4 = (IND4_DOWN + IND4_UP)/2) %>%
  group_by(PRESTADORA, CODIGO_IBGE) %>%
  summarise(IND4 = mean(IND4),
            N = n()) %>%
  filter(N >= 100) %>%
  group_by(PRESTADORA) %>%
  summarise(IND4 = mean(IND4)) %>%
  summarise(IND4 = mean(IND4))
  
# PRESTADORA  IND4
# 1 CLARO      0.940
# 2 OI         0.850
# 3 TIM        0.925
# 4 VIVO       0.999

# IND4
# 1 0.928

rm(base_smp_ind4)

#############
### IND 5 ###
#############

base_smp_ind5 <- 
  base_resultados %>%
  filter(paste0(PRESTADORA,CODIGO_IBGE) %in% municipios_1000acessos$chave) %>%
  filter(CM_IND5=="00-VALIDA") %>%
  select(PRESTADORA, CODIGO_IBGE, TECNOLOGIA, LATENCIA_MEDIA) %>%
  mutate(IND5 = ifelse(TECNOLOGIA == "3G" & LATENCIA_MEDIA <= 200000,1,
                            ifelse(TECNOLOGIA == "4G" & LATENCIA_MEDIA <= 10000,1,0))) %>%
  select(PRESTADORA, CODIGO_IBGE, IND5)
  
base_smp_ind5 %>% 
  group_by(PRESTADORA, CODIGO_IBGE) %>%
  summarise(IND5 = mean(IND5),
            N = n()) %>%
  filter(N >= 100) %>%
  group_by(PRESTADORA) %>%
  summarise(IND5 = mean(IND5)) %>%
  summarise(IND5 = mean(IND5))

# PRESTADORA        IND5
# 1 CLARO      0.00000102 
# 2 OI         0.000000388
# 3 TIM        0          
# 4 VIVO       0.0000544 

# IND5
# 1 0.0000140

rm(base_smp_ind5)

#############
### IND 6 ###
#############

base_smp_ind6 <- 
  base_resultados %>%
  filter(paste0(PRESTADORA,CODIGO_IBGE) %in% municipios_1000acessos$chave) %>%
  filter(CM_IND6=="00-VALIDA") %>%
  select(PRESTADORA, CODIGO_IBGE, TECNOLOGIA, JITTER=VARIACAO_DA_LATENCIA_JITTER) %>%
  mutate(IND6 = ifelse(TECNOLOGIA == "3G" & JITTER <= 40,1,
                       ifelse(TECNOLOGIA == "4G" & JITTER <= 25,1,0))) %>%
  select(PRESTADORA, CODIGO_IBGE, IND6)

base_smp_ind6 %>% 
  filter(PRESTADORA != "CLARO") %>%
  group_by(PRESTADORA, CODIGO_IBGE) %>%
  summarise(IND6 = mean(IND6),
            N = n()) %>%
  filter(N >= 100) %>%
  group_by(PRESTADORA) %>%
  summarise(IND6 = mean(IND6)) %>%
  summarise(IND6 = mean(IND6))

# PRESTADORA    IND6
# 1 CLARO      0.00168
# 2 OI         0.555  
# 3 TIM        0.961  
# 4 VIVO       0.944 

# IND6
# 1 0.616

rm(base_smp_ind6)

#############
### IND 7 ###
#############

base_smp_ind7 <- 
  base_resultados %>%
  filter(paste0(PRESTADORA,CODIGO_IBGE) %in% municipios_1000acessos$chave) %>%
  filter(CM_IND7=="00-VALIDA") %>%
  select(PRESTADORA, CODIGO_IBGE, TECNOLOGIA, PERDA_DE_PACOTES) %>%
  mutate(IND7 = ifelse(TECNOLOGIA == "3G" & PERDA_DE_PACOTES <= 2,1,
                       ifelse(TECNOLOGIA == "4G" & PERDA_DE_PACOTES <= 2,1,0))) %>%
  select(PRESTADORA, CODIGO_IBGE, IND7)

base_smp_ind7 %>% 
  group_by(PRESTADORA, CODIGO_IBGE) %>%
  summarise(IND7 = mean(IND7),
            N = n()) %>%
  filter(N >= 100) %>%
  group_by(PRESTADORA) %>%
  summarise(IND7 = mean(IND7)) %>%
  summarise(IND7 = mean(IND7))

# PRESTADORA  IND7
# 1 CLARO      0.804
# 2 OI         0.964
# 3 TIM        0.966
# 4 VIVO       0.931

# IND7
# 1 0.916

rm(base_smp_ind7)

#####################
### ESTABILIZACAO ###
#####################

base_smp_ind4 <- 
  base_resultados %>%
  filter(paste0(PRESTADORA,CODIGO_IBGE) %in% municipios_1000acessos$chave) %>%
  filter(CM_IND4_UL=="00-VALIDA",CM_IND4_DL=="00-VALIDA") %>%
  select(PRESTADORA, CODIGO_IBGE, TECNOLOGIA, VELOCIDADE_DOWNLOAD, VELOCIDADE_UPLOAD, CM_IND4_UL, CM_IND4_DL) %>%
  mutate(IND4_DOWN = ifelse(TECNOLOGIA == "3G" & VELOCIDADE_DOWNLOAD >= 1500,1,
                            ifelse(TECNOLOGIA == "4G" & VELOCIDADE_DOWNLOAD >= 5000,1,0)),
         IND4_UP =   ifelse(TECNOLOGIA == "3G" & VELOCIDADE_DOWNLOAD >= 500,1,
                            ifelse(TECNOLOGIA == "4G" & VELOCIDADE_DOWNLOAD >= 1500,1,0))) %>%
  select(PRESTADORA, CODIGO_IBGE, IND4_DOWN, IND4_UP) %>%
  mutate(IND4 = (IND4_DOWN + IND4_UP)/2)

amostras <- seq(from = 10, to = 1000, by = 10)

resultados <- data.frame(amostra = numeric(0),
                         prestadora = character(0),
                         IND4 = numeric(0))

for(i in 1:length(amostras)) { 
  
  temp <- 
    base_smp_ind4 %>%
    filter(CODIGO_IBGE == 2927408) %>%
    group_by(PRESTADORA) %>%
    sample_n(size = amostras[i]) %>%
    group_by(amostra = amostras[i], prestadora = PRESTADORA) %>%
    summarise(IND4 = mean(IND4)) 
  
  resultados <- bind_rows(resultados, temp)
  
}

rm(i, temp, amostras)  

resultados %>%
  ggplot(aes(x = amostra, y = IND4)) +
  geom_line() +
  scale_y_continuous("IND4", labels = scales::percent) + 
  theme_minimal() +
  facet_wrap(~prestadora)
  
# base_resultados %>%
  # filter(CODIGO_IBGE == 3550308, CM_IND5 == "00-VALIDA") %>%
  # select(LATENCIA_MEDIA)
