#################
### DIRETORIO ###
#################

# getwd()
setwd("/Users/alexvlima/Library/Mobile Documents/com~apple~CloudDocs/ABR_Telecom_ESAQ/Grupos/Grupo2/Estabilizacao_p/")

#################
### LIBRARIES ###
#################

library(tidyverse)
library(scales)

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

base_resultados <-
  base_resultados %>%
  filter(is.na(municipio) == FALSE,
         wan_mode == "WiFi",
         download_state=="Completed",upload_state=="Completed")

municipios_900acessos <- 
  base_resultados %>%
  group_by(isp,municipio,UF) %>%
  summarise(N = n()) %>%
  mutate(chave = paste0(isp,municipio,UF)) %>%
  filter(N >= 900)

###############
### ANALISE ###
###############

base_scm_ind4 <- 
  base_resultados %>%
  filter(paste0(isp,municipio,UF) %in% municipios_900acessos$chave) %>%
  filter(download_state=="Completed",upload_state=="Completed") %>%
  select(deviceid, isp,municipio,UF,`download_rate(bps)`,`upload_rate(bps)`)

base_scm_ind4 %>%
  group_by(isp, deviceid) %>%
  summarise(N = n(),
            Decil_1 = quantile(`download_rate(bps)`, probs = 0.1),
            Decil_2 = quantile(`download_rate(bps)`, probs = 0.2),
            Decil_3 = quantile(`download_rate(bps)`, probs = 0.3),
            Decil_4 = quantile(`download_rate(bps)`, probs = 0.4),
            Decil_5 = quantile(`download_rate(bps)`, probs = 0.5),
            Decil_6 = quantile(`download_rate(bps)`, probs = 0.6),
            Decil_7 = quantile(`download_rate(bps)`, probs = 0.7),
            Decil_8 = quantile(`download_rate(bps)`, probs = 0.8),
            Decil_9 = quantile(`download_rate(bps)`, probs = 0.9),
            Quartil_1 = quantile(`download_rate(bps)`, probs = 0.25),
            Quartil_2 = quantile(`download_rate(bps)`, probs = 0.5),
            Quartil_3 = quantile(`download_rate(bps)`, probs = 0.75)) %>%
  filter(N >= 10) %>%
  mutate(Votos = ifelse(Decil_1 >= 10000000,1,0) + ifelse(Decil_2 >= 10000000,1,0) + 
           ifelse(Decil_3 >= 10000000,1,0) + ifelse(Decil_4 >= 10000000,1,0) +
           ifelse(Decil_5 >= 10000000,1,0) + ifelse(Decil_6 >= 10000000,1,0) + 
           ifelse(Decil_7 >= 10000000,1,0) + ifelse(Decil_8 >= 10000000,1,0) +
           ifelse(Decil_9 >= 10000000,1,0)) %>%
  group_by(Prestadora = isp) %>%
  ggplot(aes(x = Votos/9, color = Prestadora)) +
  geom_density() + 
  scale_y_continuous(name = "Percentagem", labels=percent, breaks = c(seq(0,1,by = 0.1))) +
  scale_x_continuous(labels=percent, breaks = c(seq(0,1,by = 0.1))) + 
  ggtitle("Função de Densidade") + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")


base_scm_ind4 %>%
  group_by(isp, deviceid) %>%
  summarise(N = n(),
            Decil_1 = quantile(`download_rate(bps)`, probs = 0.1),
            Decil_2 = quantile(`download_rate(bps)`, probs = 0.2),
            Decil_3 = quantile(`download_rate(bps)`, probs = 0.3),
            Decil_4 = quantile(`download_rate(bps)`, probs = 0.4),
            Decil_5 = quantile(`download_rate(bps)`, probs = 0.5),
            Decil_6 = quantile(`download_rate(bps)`, probs = 0.6),
            Decil_7 = quantile(`download_rate(bps)`, probs = 0.7),
            Decil_8 = quantile(`download_rate(bps)`, probs = 0.8),
            Decil_9 = quantile(`download_rate(bps)`, probs = 0.9),
            Quartil_1 = quantile(`download_rate(bps)`, probs = 0.25),
            Quartil_2 = quantile(`download_rate(bps)`, probs = 0.5),
            Quartil_3 = quantile(`download_rate(bps)`, probs = 0.75)) %>%
  filter(N >= 3) %>%
  mutate(Votos = ifelse(Quartil_1 >= 10000000,1,0) + 
           ifelse(Quartil_2 >= 10000000,1,0) + 
           ifelse(Quartil_3 >= 10000000,1,0)) %>%
  group_by(Prestadora = isp) %>%
  ggplot(aes(x = Votos/3, color = Prestadora)) +
  geom_density() + 
  scale_y_continuous(name = "Percentagem", labels=percent, breaks = c(seq(0,1,by = 0.1))) +
  scale_x_continuous(labels=percent, breaks = c(seq(0,1,by = 0.1))) + 
  ggtitle("Função de Densidade") + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

# Brasília #

base_scm_ind4 %>%
  filter(UF == "DF") %>%
  group_by(isp, deviceid) %>%
  summarise(N = n(),
            Decil_1 = quantile(`download_rate(bps)`, probs = 0.1),
            Decil_2 = quantile(`download_rate(bps)`, probs = 0.2),
            Decil_3 = quantile(`download_rate(bps)`, probs = 0.3),
            Decil_4 = quantile(`download_rate(bps)`, probs = 0.4),
            Decil_5 = quantile(`download_rate(bps)`, probs = 0.5),
            Decil_6 = quantile(`download_rate(bps)`, probs = 0.6),
            Decil_7 = quantile(`download_rate(bps)`, probs = 0.7),
            Decil_8 = quantile(`download_rate(bps)`, probs = 0.8),
            Decil_9 = quantile(`download_rate(bps)`, probs = 0.9),
            Quartil_1 = quantile(`download_rate(bps)`, probs = 0.25),
            Quartil_2 = quantile(`download_rate(bps)`, probs = 0.5),
            Quartil_3 = quantile(`download_rate(bps)`, probs = 0.75)) %>%
  filter(N >= 10) %>%
  mutate(Votos = ifelse(Decil_1 >= 10000000,1,0) + ifelse(Decil_2 >= 10000000,1,0) + 
                 ifelse(Decil_3 >= 10000000,1,0) + ifelse(Decil_4 >= 10000000,1,0) +
                 ifelse(Decil_5 >= 10000000,1,0) + ifelse(Decil_6 >= 10000000,1,0) + 
                 ifelse(Decil_7 >= 10000000,1,0) + ifelse(Decil_8 >= 10000000,1,0) +
                 ifelse(Decil_9 >= 10000000,1,0)) %>%
  group_by(Prestadora = isp) %>%
  summarise(N = n(),
            Media = mean(Votos/9),
            DP = sd(Votos/9)) %>%
  filter(N >= 102)

base_scm_ind4 %>%
  filter(UF == "DF") %>%
  group_by(isp, deviceid) %>%
  summarise(N = n(),
            Decil_1 = quantile(`download_rate(bps)`, probs = 0.1),
            Decil_2 = quantile(`download_rate(bps)`, probs = 0.2),
            Decil_3 = quantile(`download_rate(bps)`, probs = 0.3),
            Decil_4 = quantile(`download_rate(bps)`, probs = 0.4),
            Decil_5 = quantile(`download_rate(bps)`, probs = 0.5),
            Decil_6 = quantile(`download_rate(bps)`, probs = 0.6),
            Decil_7 = quantile(`download_rate(bps)`, probs = 0.7),
            Decil_8 = quantile(`download_rate(bps)`, probs = 0.8),
            Decil_9 = quantile(`download_rate(bps)`, probs = 0.9),
            Quartil_1 = quantile(`download_rate(bps)`, probs = 0.25),
            Quartil_2 = quantile(`download_rate(bps)`, probs = 0.5),
            Quartil_3 = quantile(`download_rate(bps)`, probs = 0.75)) %>%
  filter(N >= 3) %>%
  mutate(Votos = ifelse(Quartil_1 >= 10000000,1,0) + 
                 ifelse(Quartil_2 >= 10000000,1,0) + 
                 ifelse(Quartil_3 >= 10000000,1,0)) %>%
  group_by(Prestadora = isp) %>%
  summarise(N = n(),
            Mediana = median(Votos/3),
            Media = mean(Votos/3),
            DP = sd(Votos/3)) %>% 
  filter(N >= 102)

# Anapolis #

base_scm_ind4 %>%
  filter(UF == "GO", municipio == "Anápolis") %>%
  group_by(isp, deviceid) %>%
  summarise(N = n(),
            Decil_1 = quantile(`download_rate(bps)`, probs = 0.1),
            Decil_2 = quantile(`download_rate(bps)`, probs = 0.2),
            Decil_3 = quantile(`download_rate(bps)`, probs = 0.3),
            Decil_4 = quantile(`download_rate(bps)`, probs = 0.4),
            Decil_5 = quantile(`download_rate(bps)`, probs = 0.5),
            Decil_6 = quantile(`download_rate(bps)`, probs = 0.6),
            Decil_7 = quantile(`download_rate(bps)`, probs = 0.7),
            Decil_8 = quantile(`download_rate(bps)`, probs = 0.8),
            Decil_9 = quantile(`download_rate(bps)`, probs = 0.9),
            Quartil_1 = quantile(`download_rate(bps)`, probs = 0.25),
            Quartil_2 = quantile(`download_rate(bps)`, probs = 0.5),
            Quartil_3 = quantile(`download_rate(bps)`, probs = 0.75)) %>%
  filter(N >= 10) %>%
  mutate(Votos = ifelse(Decil_1 >= 10000000,1,0) + ifelse(Decil_2 >= 10000000,1,0) + 
           ifelse(Decil_3 >= 10000000,1,0) + ifelse(Decil_4 >= 10000000,1,0) +
           ifelse(Decil_5 >= 10000000,1,0) + ifelse(Decil_6 >= 10000000,1,0) + 
           ifelse(Decil_7 >= 10000000,1,0) + ifelse(Decil_8 >= 10000000,1,0) +
           ifelse(Decil_9 >= 10000000,1,0)) %>%
  group_by(Prestadora = isp) %>%
  summarise(N = n(),
            Media = mean(Votos/9),
            DP = sd(Votos/9)) %>%
  filter(N >= 102)

base_scm_ind4 %>%
  filter(UF == "GO", municipio == "Anápolis") %>%
  group_by(isp, deviceid) %>%
  summarise(N = n(),
            Decil_1 = quantile(`download_rate(bps)`, probs = 0.1),
            Decil_2 = quantile(`download_rate(bps)`, probs = 0.2),
            Decil_3 = quantile(`download_rate(bps)`, probs = 0.3),
            Decil_4 = quantile(`download_rate(bps)`, probs = 0.4),
            Decil_5 = quantile(`download_rate(bps)`, probs = 0.5),
            Decil_6 = quantile(`download_rate(bps)`, probs = 0.6),
            Decil_7 = quantile(`download_rate(bps)`, probs = 0.7),
            Decil_8 = quantile(`download_rate(bps)`, probs = 0.8),
            Decil_9 = quantile(`download_rate(bps)`, probs = 0.9),
            Quartil_1 = quantile(`download_rate(bps)`, probs = 0.25),
            Quartil_2 = quantile(`download_rate(bps)`, probs = 0.5),
            Quartil_3 = quantile(`download_rate(bps)`, probs = 0.75)) %>%
  filter(N >= 3) %>%
  mutate(Votos = ifelse(Quartil_1 >= 10000000,1,0) + 
           ifelse(Quartil_2 >= 10000000,1,0) + 
           ifelse(Quartil_3 >= 10000000,1,0)) %>%
  group_by(Prestadora = isp) %>%
  summarise(N = n(),
            Media = mean(Votos/3),
            DP = sd(Votos/3)) %>% 
  filter(N >= 102)

# Santa Maria #

base_scm_ind4 %>%
  filter(UF == "RS", municipio == "Santa Maria") %>%
  group_by(isp, deviceid) %>%
  summarise(N = n(),
            Decil_1 = quantile(`download_rate(bps)`, probs = 0.1),
            Decil_2 = quantile(`download_rate(bps)`, probs = 0.2),
            Decil_3 = quantile(`download_rate(bps)`, probs = 0.3),
            Decil_4 = quantile(`download_rate(bps)`, probs = 0.4),
            Decil_5 = quantile(`download_rate(bps)`, probs = 0.5),
            Decil_6 = quantile(`download_rate(bps)`, probs = 0.6),
            Decil_7 = quantile(`download_rate(bps)`, probs = 0.7),
            Decil_8 = quantile(`download_rate(bps)`, probs = 0.8),
            Decil_9 = quantile(`download_rate(bps)`, probs = 0.9),
            Quartil_1 = quantile(`download_rate(bps)`, probs = 0.25),
            Quartil_2 = quantile(`download_rate(bps)`, probs = 0.5),
            Quartil_3 = quantile(`download_rate(bps)`, probs = 0.75)) %>%
  filter(N >= 10) %>%
  mutate(Votos = ifelse(Decil_1 >= 10000000,1,0) + ifelse(Decil_2 >= 10000000,1,0) + 
           ifelse(Decil_3 >= 10000000,1,0) + ifelse(Decil_4 >= 10000000,1,0) +
           ifelse(Decil_5 >= 10000000,1,0) + ifelse(Decil_6 >= 10000000,1,0) + 
           ifelse(Decil_7 >= 10000000,1,0) + ifelse(Decil_8 >= 10000000,1,0) +
           ifelse(Decil_9 >= 10000000,1,0)) %>%
  group_by(Prestadora = isp) %>%
  summarise(N = n(),
            Media = mean(Votos/9),
            DP = sd(Votos/9)) %>%
  filter(N >= 102)

base_scm_ind4 %>%
  filter(UF == "RS", municipio == "Santa Maria") %>%
  group_by(isp, deviceid) %>%
  summarise(N = n(),
            Decil_1 = quantile(`download_rate(bps)`, probs = 0.1),
            Decil_2 = quantile(`download_rate(bps)`, probs = 0.2),
            Decil_3 = quantile(`download_rate(bps)`, probs = 0.3),
            Decil_4 = quantile(`download_rate(bps)`, probs = 0.4),
            Decil_5 = quantile(`download_rate(bps)`, probs = 0.5),
            Decil_6 = quantile(`download_rate(bps)`, probs = 0.6),
            Decil_7 = quantile(`download_rate(bps)`, probs = 0.7),
            Decil_8 = quantile(`download_rate(bps)`, probs = 0.8),
            Decil_9 = quantile(`download_rate(bps)`, probs = 0.9),
            Quartil_1 = quantile(`download_rate(bps)`, probs = 0.25),
            Quartil_2 = quantile(`download_rate(bps)`, probs = 0.5),
            Quartil_3 = quantile(`download_rate(bps)`, probs = 0.75)) %>%
  filter(N >= 3) %>%
  mutate(Votos = ifelse(Quartil_1 >= 10000000,1,0) + 
           ifelse(Quartil_2 >= 10000000,1,0) + 
           ifelse(Quartil_3 >= 10000000,1,0)) %>%
  group_by(Prestadora = isp) %>%
  summarise(N = n(),
            Media = mean(Votos/3),
            DP = sd(Votos/3)) %>% 
  filter(N >= 102) 

# Porto Velho #

base_scm_ind4 %>%
  filter(UF == "RO", municipio == "Porto Velho") %>%
  group_by(isp, deviceid) %>%
  summarise(N = n(),
            Decil_1 = quantile(`download_rate(bps)`, probs = 0.1),
            Decil_2 = quantile(`download_rate(bps)`, probs = 0.2),
            Decil_3 = quantile(`download_rate(bps)`, probs = 0.3),
            Decil_4 = quantile(`download_rate(bps)`, probs = 0.4),
            Decil_5 = quantile(`download_rate(bps)`, probs = 0.5),
            Decil_6 = quantile(`download_rate(bps)`, probs = 0.6),
            Decil_7 = quantile(`download_rate(bps)`, probs = 0.7),
            Decil_8 = quantile(`download_rate(bps)`, probs = 0.8),
            Decil_9 = quantile(`download_rate(bps)`, probs = 0.9),
            Quartil_1 = quantile(`download_rate(bps)`, probs = 0.25),
            Quartil_2 = quantile(`download_rate(bps)`, probs = 0.5),
            Quartil_3 = quantile(`download_rate(bps)`, probs = 0.75)) %>%
  filter(N >= 10) %>%
  mutate(Votos = ifelse(Decil_1 >= 10000000,1,0) + ifelse(Decil_2 >= 10000000,1,0) + 
           ifelse(Decil_3 >= 10000000,1,0) + ifelse(Decil_4 >= 10000000,1,0) +
           ifelse(Decil_5 >= 10000000,1,0) + ifelse(Decil_6 >= 10000000,1,0) + 
           ifelse(Decil_7 >= 10000000,1,0) + ifelse(Decil_8 >= 10000000,1,0) +
           ifelse(Decil_9 >= 10000000,1,0)) %>%
  group_by(Prestadora = isp) %>%
  summarise(N = n(),
            Media = mean(Votos/9),
            DP = sd(Votos/9)) %>%
  filter(N >= 102)

base_scm_ind4 %>%
  filter(UF == "RO", municipio == "Porto Velho") %>%
  group_by(isp, deviceid) %>%
  summarise(N = n(),
            Decil_1 = quantile(`download_rate(bps)`, probs = 0.1),
            Decil_2 = quantile(`download_rate(bps)`, probs = 0.2),
            Decil_3 = quantile(`download_rate(bps)`, probs = 0.3),
            Decil_4 = quantile(`download_rate(bps)`, probs = 0.4),
            Decil_5 = quantile(`download_rate(bps)`, probs = 0.5),
            Decil_6 = quantile(`download_rate(bps)`, probs = 0.6),
            Decil_7 = quantile(`download_rate(bps)`, probs = 0.7),
            Decil_8 = quantile(`download_rate(bps)`, probs = 0.8),
            Decil_9 = quantile(`download_rate(bps)`, probs = 0.9),
            Quartil_1 = quantile(`download_rate(bps)`, probs = 0.25),
            Quartil_2 = quantile(`download_rate(bps)`, probs = 0.5),
            Quartil_3 = quantile(`download_rate(bps)`, probs = 0.75)) %>%
  filter(N >= 3) %>%
  mutate(Votos = ifelse(Quartil_1 >= 10000000,1,0) + 
           ifelse(Quartil_2 >= 10000000,1,0) + 
           ifelse(Quartil_3 >= 10000000,1,0)) %>%
  group_by(Prestadora = isp) %>%
  summarise(N = n(),
            Media = mean(Votos/3),
            DP = sd(Votos/3)) %>% 
  filter(N >= 102) 

# Feira de Santana #

base_scm_ind4 %>%
  filter(UF == "BA", municipio == "Feira de Santana") %>%
  group_by(isp, deviceid) %>%
  summarise(N = n(),
            Decil_1 = quantile(`download_rate(bps)`, probs = 0.1),
            Decil_2 = quantile(`download_rate(bps)`, probs = 0.2),
            Decil_3 = quantile(`download_rate(bps)`, probs = 0.3),
            Decil_4 = quantile(`download_rate(bps)`, probs = 0.4),
            Decil_5 = quantile(`download_rate(bps)`, probs = 0.5),
            Decil_6 = quantile(`download_rate(bps)`, probs = 0.6),
            Decil_7 = quantile(`download_rate(bps)`, probs = 0.7),
            Decil_8 = quantile(`download_rate(bps)`, probs = 0.8),
            Decil_9 = quantile(`download_rate(bps)`, probs = 0.9),
            Quartil_1 = quantile(`download_rate(bps)`, probs = 0.25),
            Quartil_2 = quantile(`download_rate(bps)`, probs = 0.5),
            Quartil_3 = quantile(`download_rate(bps)`, probs = 0.75)) %>%
  filter(N >= 10) %>%
  mutate(Votos = ifelse(Decil_1 >= 10000000,1,0) + ifelse(Decil_2 >= 10000000,1,0) + 
           ifelse(Decil_3 >= 10000000,1,0) + ifelse(Decil_4 >= 10000000,1,0) +
           ifelse(Decil_5 >= 10000000,1,0) + ifelse(Decil_6 >= 10000000,1,0) + 
           ifelse(Decil_7 >= 10000000,1,0) + ifelse(Decil_8 >= 10000000,1,0) +
           ifelse(Decil_9 >= 10000000,1,0)) %>%
  group_by(Prestadora = isp) %>%
  summarise(N = n(),
            Media = mean(Votos/9),
            DP = sd(Votos/9)) %>%
  filter(N >= 102)

base_scm_ind4 %>%
  filter(UF == "BA", municipio == "Feira de Santana") %>%
  group_by(isp, deviceid) %>%
  summarise(N = n(),
            Decil_1 = quantile(`download_rate(bps)`, probs = 0.1),
            Decil_2 = quantile(`download_rate(bps)`, probs = 0.2),
            Decil_3 = quantile(`download_rate(bps)`, probs = 0.3),
            Decil_4 = quantile(`download_rate(bps)`, probs = 0.4),
            Decil_5 = quantile(`download_rate(bps)`, probs = 0.5),
            Decil_6 = quantile(`download_rate(bps)`, probs = 0.6),
            Decil_7 = quantile(`download_rate(bps)`, probs = 0.7),
            Decil_8 = quantile(`download_rate(bps)`, probs = 0.8),
            Decil_9 = quantile(`download_rate(bps)`, probs = 0.9),
            Quartil_1 = quantile(`download_rate(bps)`, probs = 0.25),
            Quartil_2 = quantile(`download_rate(bps)`, probs = 0.5),
            Quartil_3 = quantile(`download_rate(bps)`, probs = 0.75)) %>%
  filter(N >= 3) %>%
  mutate(Votos = ifelse(Quartil_1 >= 10000000,1,0) + 
           ifelse(Quartil_2 >= 10000000,1,0) + 
           ifelse(Quartil_3 >= 10000000,1,0)) %>%
  group_by(Prestadora = isp) %>%
  summarise(N = n(),
            Media = mean(Votos/3),
            DP = sd(Votos/3)) %>% 
  filter(N >= 102) 

# Natal #

base_scm_ind4 %>%
  filter(UF == "RN", municipio == "Natal") %>%
  group_by(isp, deviceid) %>%
  summarise(N = n(),
            Decil_1 = quantile(`download_rate(bps)`, probs = 0.1),
            Decil_2 = quantile(`download_rate(bps)`, probs = 0.2),
            Decil_3 = quantile(`download_rate(bps)`, probs = 0.3),
            Decil_4 = quantile(`download_rate(bps)`, probs = 0.4),
            Decil_5 = quantile(`download_rate(bps)`, probs = 0.5),
            Decil_6 = quantile(`download_rate(bps)`, probs = 0.6),
            Decil_7 = quantile(`download_rate(bps)`, probs = 0.7),
            Decil_8 = quantile(`download_rate(bps)`, probs = 0.8),
            Decil_9 = quantile(`download_rate(bps)`, probs = 0.9),
            Quartil_1 = quantile(`download_rate(bps)`, probs = 0.25),
            Quartil_2 = quantile(`download_rate(bps)`, probs = 0.5),
            Quartil_3 = quantile(`download_rate(bps)`, probs = 0.75)) %>%
  filter(N >= 10) %>%
  mutate(Votos = ifelse(Decil_1 >= 10000000,1,0) + ifelse(Decil_2 >= 10000000,1,0) + 
           ifelse(Decil_3 >= 10000000,1,0) + ifelse(Decil_4 >= 10000000,1,0) +
           ifelse(Decil_5 >= 10000000,1,0) + ifelse(Decil_6 >= 10000000,1,0) + 
           ifelse(Decil_7 >= 10000000,1,0) + ifelse(Decil_8 >= 10000000,1,0) +
           ifelse(Decil_9 >= 10000000,1,0)) %>%
  group_by(Prestadora = isp) %>%
  summarise(N = n(),
            Media = mean(Votos/9),
            DP = sd(Votos/9)) %>%
  filter(N >= 102)

base_scm_ind4 %>%
  filter(UF == "RN", municipio == "Natal") %>%
  group_by(isp, deviceid) %>%
  summarise(N = n(),
            Decil_1 = quantile(`download_rate(bps)`, probs = 0.1),
            Decil_2 = quantile(`download_rate(bps)`, probs = 0.2),
            Decil_3 = quantile(`download_rate(bps)`, probs = 0.3),
            Decil_4 = quantile(`download_rate(bps)`, probs = 0.4),
            Decil_5 = quantile(`download_rate(bps)`, probs = 0.5),
            Decil_6 = quantile(`download_rate(bps)`, probs = 0.6),
            Decil_7 = quantile(`download_rate(bps)`, probs = 0.7),
            Decil_8 = quantile(`download_rate(bps)`, probs = 0.8),
            Decil_9 = quantile(`download_rate(bps)`, probs = 0.9),
            Quartil_1 = quantile(`download_rate(bps)`, probs = 0.25),
            Quartil_2 = quantile(`download_rate(bps)`, probs = 0.5),
            Quartil_3 = quantile(`download_rate(bps)`, probs = 0.75)) %>%
  filter(N >= 3) %>%
  mutate(Votos = ifelse(Quartil_1 >= 10000000,1,0) + 
           ifelse(Quartil_2 >= 10000000,1,0) + 
           ifelse(Quartil_3 >= 10000000,1,0)) %>%
  group_by(Prestadora = isp) %>%
  summarise(N = n(),
            Media = mean(Votos/3),
            DP = sd(Votos/3)) %>% 
  filter(N >= 102) 

# Contagem #

base_scm_ind4 %>%
  filter(UF == "MG", municipio == "Contagem") %>%
  group_by(isp, deviceid) %>%
  summarise(N = n(),
            Decil_1 = quantile(`download_rate(bps)`, probs = 0.1),
            Decil_2 = quantile(`download_rate(bps)`, probs = 0.2),
            Decil_3 = quantile(`download_rate(bps)`, probs = 0.3),
            Decil_4 = quantile(`download_rate(bps)`, probs = 0.4),
            Decil_5 = quantile(`download_rate(bps)`, probs = 0.5),
            Decil_6 = quantile(`download_rate(bps)`, probs = 0.6),
            Decil_7 = quantile(`download_rate(bps)`, probs = 0.7),
            Decil_8 = quantile(`download_rate(bps)`, probs = 0.8),
            Decil_9 = quantile(`download_rate(bps)`, probs = 0.9),
            Quartil_1 = quantile(`download_rate(bps)`, probs = 0.25),
            Quartil_2 = quantile(`download_rate(bps)`, probs = 0.5),
            Quartil_3 = quantile(`download_rate(bps)`, probs = 0.75)) %>%
  filter(N >= 10) %>%
  mutate(Votos = ifelse(Decil_1 >= 10000000,1,0) + ifelse(Decil_2 >= 10000000,1,0) + 
           ifelse(Decil_3 >= 10000000,1,0) + ifelse(Decil_4 >= 10000000,1,0) +
           ifelse(Decil_5 >= 10000000,1,0) + ifelse(Decil_6 >= 10000000,1,0) + 
           ifelse(Decil_7 >= 10000000,1,0) + ifelse(Decil_8 >= 10000000,1,0) +
           ifelse(Decil_9 >= 10000000,1,0)) %>%
  group_by(Prestadora = isp) %>%
  summarise(N = n(),
            Media = mean(Votos/9),
            DP = sd(Votos/9)) %>%
  filter(N >= 102)

base_scm_ind4 %>%
  filter(UF == "MG", municipio == "Contagem") %>%
  group_by(isp, deviceid) %>%
  summarise(N = n(),
            Decil_1 = quantile(`download_rate(bps)`, probs = 0.1),
            Decil_2 = quantile(`download_rate(bps)`, probs = 0.2),
            Decil_3 = quantile(`download_rate(bps)`, probs = 0.3),
            Decil_4 = quantile(`download_rate(bps)`, probs = 0.4),
            Decil_5 = quantile(`download_rate(bps)`, probs = 0.5),
            Decil_6 = quantile(`download_rate(bps)`, probs = 0.6),
            Decil_7 = quantile(`download_rate(bps)`, probs = 0.7),
            Decil_8 = quantile(`download_rate(bps)`, probs = 0.8),
            Decil_9 = quantile(`download_rate(bps)`, probs = 0.9),
            Quartil_1 = quantile(`download_rate(bps)`, probs = 0.25),
            Quartil_2 = quantile(`download_rate(bps)`, probs = 0.5),
            Quartil_3 = quantile(`download_rate(bps)`, probs = 0.75)) %>%
  filter(N >= 3) %>%
  mutate(Votos = ifelse(Quartil_1 >= 10000000,1,0) + 
           ifelse(Quartil_2 >= 10000000,1,0) + 
           ifelse(Quartil_3 >= 10000000,1,0)) %>%
  group_by(Prestadora = isp) %>%
  summarise(N = n(),
            Media = mean(Votos/3),
            DP = sd(Votos/3)) %>% 
  filter(N >= 102) 