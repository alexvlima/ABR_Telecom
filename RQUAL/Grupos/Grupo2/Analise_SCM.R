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
temp <- paste0("./Dados/AppEAQ/Claro/", list.files(path = "./Dados/AppEAQ/Claro/", pattern = "*.*csv"))
base_resultados_claro <- lapply(temp, read_csv2, locale(encoding = "UTF-8"), col_names = TRUE, col_types = NULL)

rm(temp)

base_resultados_claro <- lapply(base_resultados_claro, function(x) select(x, 
                                                              deviceid,
                                                              isp,
                                                              download_state,
                                                              upload_state,
                                                              wan_mode,
                                                              mac_address,
                                                              `download_rate(bps)`,
                                                              `upload_rate(bps)`,
                                                              state_latency_jitter,
                                                              `latency(ms)`,
                                                              `jitter(ms)`,
                                                              state_packet_loss,
                                                              `packet_loss_percent(%)`,
                                                              municipio,
                                                              UF = uf))

base_resultados_claro <-
  base_resultados_claro %>%
  map(., ~mutate(.x, `latency(ms)` = as.numeric(gsub(pattern = ",",
                                                     replacement = ".",
                                                    `latency(ms)`)),
                 `jitter(ms)` = as.numeric(gsub(pattern = ",",
                                           replacement = ".",
                                           `jitter(ms)`)),
                 `packet_loss_percent(%)` = as.numeric(gsub(pattern = ",",
                                                       replacement = ".",
                                                       `packet_loss_percent(%)`))))

base_resultados_claro <- bind_rows(base_resultados_claro)

glimpse(base_resultados_claro)

temp <- paste0("./Dados/AppEAQ/Oi/", list.files(path = "./Dados/AppEAQ/Oi/", pattern = "*.*csv"))
base_resultados_oi <- lapply(temp, read_csv2, locale(encoding = "UTF-8"), col_names = TRUE, col_types = NULL)

rm(temp)

base_resultados_oi <- lapply(base_resultados_oi, function(x) select(x, 
                                                                    deviceid,
                                                                    isp,
                                                                    download_state,
                                                                    upload_state,
                                                                    wan_mode,
                                                                    mac_address,
                                                                    `download_rate(bps)`,
                                                                    `upload_rate(bps)`,
                                                                    state_latency_jitter,
                                                                    `latency(ms)`,
                                                                    `jitter(ms)`,
                                                                    state_packet_loss,
                                                                    `packet_loss_percent(%)`,
                                                                    municipio,
                                                                    UF = uf))

base_resultados_oi <-
  base_resultados_oi %>%
  map(., ~mutate(.x,
                 `latency(ms)` = as.numeric(gsub(pattern = ",",
                                                     replacement = ".",
                                                     `latency(ms)`)),
                 `jitter(ms)` = as.numeric(gsub(pattern = ",",
                                                replacement = ".",
                                                `jitter(ms)`)),
                 `packet_loss_percent(%)` = as.numeric(gsub(pattern = ",",
                                                            replacement = ".",
                                                            `packet_loss_percent(%)`))))

base_resultados_oi <- bind_rows(base_resultados_oi)

glimpse(base_resultados_oi)

temp <- paste0("./Dados/AppEAQ/Tim/", list.files(path = "./Dados/AppEAQ/Tim/", pattern = "*.*csv"))
base_resultados_tim <- lapply(temp, read_csv2, locale(encoding = "UTF-8"), col_names = TRUE, col_types = NULL)

rm(temp)

base_resultados_tim <- lapply(base_resultados_tim, function(x) select(x, 
                                                                    deviceid,
                                                                    isp,
                                                                    download_state,
                                                                    upload_state,
                                                                    wan_mode,
                                                                    mac_address,
                                                                    `download_rate(bps)`,
                                                                    `upload_rate(bps)`,
                                                                    state_latency_jitter,
                                                                    `latency(ms)`,
                                                                    `jitter(ms)`,
                                                                    state_packet_loss,
                                                                    `packet_loss_percent(%)`,
                                                                    municipio,
                                                                    UF = uf))

base_resultados_tim <-
  base_resultados_tim %>%
  map(., ~mutate(.x,
                 `latency(ms)` = as.numeric(gsub(pattern = ",",
                                                 replacement = ".",
                                                 `latency(ms)`)),
                 `jitter(ms)` = as.numeric(gsub(pattern = ",",
                                                replacement = ".",
                                                `jitter(ms)`)),
                 `packet_loss_percent(%)` = as.numeric(gsub(pattern = ",",
                                                            replacement = ".",
                                                            `packet_loss_percent(%)`))))

base_resultados_tim <- bind_rows(base_resultados_tim)

glimpse(base_resultados_tim)

temp <- paste0("./Dados/AppEAQ/Vivo/", list.files(path = "./Dados/AppEAQ/Vivo/", pattern = "*.*csv"))
base_resultados_vivo <- lapply(temp, read_csv2, locale(encoding = "UTF-8"), col_names = TRUE, col_types = NULL)

rm(temp)

base_resultados_vivo <- lapply(base_resultados_vivo, function(x) select(x, 
                                                                      deviceid,
                                                                      isp,
                                                                      download_state,
                                                                      upload_state,
                                                                      wan_mode,
                                                                      mac_address,
                                                                      `download_rate(bps)`,
                                                                      `upload_rate(bps)`,
                                                                      state_latency_jitter,
                                                                      `latency(ms)`,
                                                                      `jitter(ms)`,
                                                                      state_packet_loss,
                                                                      `packet_loss_percent(%)`,
                                                                      municipio,
                                                                      UF = uf))

base_resultados_vivo <-
  base_resultados_vivo %>%
  map(., ~mutate(.x,
                 `latency(ms)` = as.numeric(gsub(pattern = ",",
                                                 replacement = ".",
                                                 `latency(ms)`)),
                 `jitter(ms)` = as.numeric(gsub(pattern = ",",
                                                replacement = ".",
                                                `jitter(ms)`)),
                 `packet_loss_percent(%)` = as.numeric(gsub(pattern = ",",
                                                            replacement = ".",
                                                            `packet_loss_percent(%)`))))

base_resultados_vivo <- bind_rows(base_resultados_vivo)

glimpse(base_resultados_vivo)

base_resultados <- 
  base_resultados_claro %>% 
  bind_rows(base_resultados_oi) %>%
  bind_rows(base_resultados_tim) %>%
  bind_rows(base_resultados_vivo)

rm(list = ls(pattern = "base_resultados_"))  

###############
### FILTROS ###
###############

# sum(is.na(base_resultados$CODIGO_IBGE)==TRUE)

base_resultados <-
  base_resultados %>%
  filter(is.na(municipio) == FALSE)

glimpse(base_resultados)

saveRDS(base_resultados, "Base_SCM.rds")
# base_resultados <- read_rds("Base_SCM.rds")

municipios_900acessos <- 
  base_resultados %>%
  group_by(isp,municipio,UF) %>%
  summarise(N = n()) %>%
  mutate(chave = paste0(isp,municipio,UF)) %>%
  filter(N >= 900)

#############
### IND 4 ###
#############

# unique(base_resultados$download_state)

base_scm_ind4 <- 
  base_resultados %>%
  filter(paste0(isp,municipio,UF) %in% municipios_900acessos$chave) %>%
  filter(download_state=="Completed",upload_state=="Completed") %>%
  select(isp,municipio,UF,`download_rate(bps)`,`upload_rate(bps)`)

base_scm_ind4 <- 
  base_scm_ind4 %>%
  mutate(IND4_DOWN = ifelse(`download_rate(bps)` >= 10000000,1,0),
         IND4_UP =   ifelse(`upload_rate(bps)` >= 2000000,1,0)) %>%
  select(PRESTADORA=isp, MUNICIPIO=municipio, UF, IND4_DOWN, IND4_UP)

base_scm_ind4 %>%
  mutate(IND4 = (IND4_DOWN + IND4_UP)/2) %>%
  group_by(PRESTADORA, MUNICIPIO, UF) %>%
  summarise(IND4 = mean(IND4),
            N = n()) %>%
  ggplot(aes(x = N, y = IND4)) + 
  geom_line() +
  scale_y_continuous("IND4", labels = scales::percent) + 
  theme_minimal() +
  facet_wrap(~PRESTADORA)

base_scm_ind4 %>%
  mutate(IND4 = (IND4_DOWN + IND4_UP)/2) %>%
  group_by(PRESTADORA, MUNICIPIO, UF) %>%
  summarise(IND4 = mean(IND4),
            N = n()) %>%
  filter(N >= 100) %>%
  group_by(PRESTADORA) %>%
  summarise(IND4 = mean(IND4)) %>%
  summarise(IND4 = mean(IND4))
  
# PRESTADORA  IND4
# 1 claro      0.934
# 2 oi         0.839
# 3 tim        0.874  
# 4 vivo       0.906

# IND4
# 1 0.888

rm(base_scm_ind4)

#############
### IND 5 ###
#############

base_scm_ind5 <- 
  base_resultados %>%
  filter(paste0(isp,municipio,UF) %in% municipios_900acessos$chave) %>%
  filter(state_latency_jitter=="Completed") %>%
  select(PRESTADORA=isp, MUNICIPIO=municipio, UF, `latency(ms)`) %>%
  mutate(IND5 = ifelse(`latency(ms)` <= 80,1,0)) %>%
  select(PRESTADORA, MUNICIPIO, UF, IND5)
  
base_scm_ind5 %>% 
  group_by(PRESTADORA, MUNICIPIO, UF) %>%
  summarise(IND5 = mean(IND5),
            N = n()) %>%
  filter(N >= 100) %>%
  group_by(PRESTADORA) %>%
  summarise(IND5 = mean(IND5)) %>%
  summarise(IND5 = mean(IND5))

# PRESTADORA        IND5
# 1 claro      0.879
# 2 oi         0.797
# 3 tim        0.899
# 4 vivo       0.917 

# IND5
# 1 0.873

rm(base_scm_ind5)

#############
### IND 6 ###
#############

base_scm_ind6 <- 
  base_resultados %>%
  filter(paste0(isp,municipio,UF) %in% municipios_900acessos$chave) %>%
  filter(state_latency_jitter=="Completed") %>%
  select(PRESTADORA=isp, MUNICIPIO=municipio, UF, `jitter(ms)`) %>%
  mutate(IND6 = ifelse(`jitter(ms)` <= 40,1,0)) %>%
  select(PRESTADORA, MUNICIPIO, UF, IND6)

base_scm_ind6 %>% 
  group_by(PRESTADORA, MUNICIPIO, UF) %>%
  summarise(IND6 = mean(IND6, na.rm = T),
            N = n()) %>%
  filter(N >= 100) %>%
  group_by(PRESTADORA) %>%
  summarise(IND6 = mean(IND6)) %>%
  summarise(IND6 = mean(IND6))

# PRESTADORA    IND6
# 1 claro      0.926
# 2 oi         0.916
# 3 tim        0.953
# 4 vivo       0.915

# IND6
# 1 0.928

rm(base_scm_ind6)

#############
### IND 7 ###
#############

base_scm_ind7 <- 
  base_resultados %>%
  filter(paste0(isp,municipio,UF) %in% municipios_900acessos$chave) %>%
  filter(state_packet_loss=="Completed") %>%
  select(PRESTADORA=isp, MUNICIPIO=municipio, UF, `packet_loss_percent(%)`) %>%
  mutate(IND7 = ifelse(`packet_loss_percent(%)` <= 2,1,0)) %>%
  select(PRESTADORA, MUNICIPIO, UF, IND7)

base_scm_ind7 %>% 
  group_by(PRESTADORA, MUNICIPIO, UF) %>%
  summarise(IND7 = mean(IND7, na.rm = T),
            N = n()) %>%
  filter(N >= 100) %>%
  group_by(PRESTADORA) %>%
  summarise(IND7 = mean(IND7)) %>%
  summarise(IND7 = mean(IND7))

# PRESTADORA  IND7
# 1 claro      0.960
# 2 oi         0.946
# 3 tim        0.967
# 4 vivo       0.960

# IND7
# 1 0.958

rm(base_scm_ind7)


