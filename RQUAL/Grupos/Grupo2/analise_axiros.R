#################
### DIRETORIO ###
#################

# getwd()
setwd("/Users/alexvlima/Library/Mobile Documents/com~apple~CloudDocs/ABR_Telecom_ESAQ/Grupo2/")

#################
### LIBRARIES ###
#################

library(tidyverse)
library(stringr)
library(scales)

###############
### DATASET ###
###############

# df <- read_delim("IN_Axiros_Jul20_com_VIVO_v2.txt", delim = "\t", 
#                  col_types = list(col_double(),col_character(),
#                                   col_character(),col_character(),
#                                   col_double(),col_double(),col_double()))
# glimpse(df)

# df <- read_delim("IN_Axiros_Jul20_sem_VIVO_v1.txt", delim = "\t")

df <- read.delim("IN_Axiros_Jul20_sem_VIVO_v2.csv", sep = "\t", 
                 stringsAsFactors = FALSE, dec = ",")
glimpse(df)

df2 <- read_delim("Base_Down_up", delim = "\t")
glimpse(df2)

df_vivo <- read.delim("Latencia e Jitter - VIVO", sep = "\t", stringsAsFactors = FALSE, dec = ",")
glimpse(df_vivo)

#################
### WRANGLING ###
#################

df_vivo <- df_vivo %>% select(-DATETIME,-CGI)
colnames(df_vivo) <- c("Tecnologia","Cod_IBGE","Jitter","Latencia","Prestadora")
glimpse(df_vivo)

df$udp_latency.ms. <- as.numeric(str_replace_all(df$udp_latency.ms., ",", "."))
df$udp_jitter.ms. <- as.numeric(str_replace_all(df$udp_jitter.ms., ",", "."))
df$Codigo.ibge <- as.numeric(df$Codigo.ibge)
# colnames(df) <- c("id","time","isp","wan","lattency","jitter","loss_percent")

df <- df %>% dplyr::select(Tecnologia, Cod_IBGE = Codigo.ibge, Jitter = udp_jitter.ms.,
                     Latencia = udp_latency.ms., Prestadora)

df <- df %>% bind_rows(df_vivo)

glimpse(df)

###############
### SUMMARY ###
###############

summary(df)
table(df$wan_mode)

summary(df2)
table(df2$TECNOLOGIA)
table(df2$Prestadora)

# estat <- 
#   df %>%
#   group_by(isp, wan_mode) %>%
#   summarise(latencia_50 = median(lattency, na.rm = T),
#             latencia_75 = quantile(lattency, 0.75, na.rm = T),
#             latencia_95 = quantile(lattency, 0.95, na.rm = T),
#             jitter_50 = median(jitter, na.rm = T),
#             jitter_75 = quantile(jitter, 0.75, na.rm = T),
#             jitter_95 = quantile(jitter, 0.95, na.rm = T),
#             perda_50 = median(loss_percent, na.rm = T),
#             perda_75 = quantile(loss_percent, 0.75, na.rm = T),
#             perda_95 = quantile(loss_percent, 0.55, na.rm = T))
# 
# # write_csv2(estat, "estatistica.csv")

#############
### PLOTS ###
#############

# LATENCIA #
df %>%
  filter(Prestadora != "Prestadora", Latencia > 0,
         Latencia < 5000, Tecnologia == "4G", is.na(Cod_IBGE) == FALSE) %>%
  group_by(Cod_IBGE, Prestadora) %>%
  dplyr::summarise(Indicador = sum(Latencia <= 100, na.rm = T) / n()) %>%
  group_by(Prestadora, Indicador) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  ggplot(aes(x = Indicador, color = Prestadora)) +
  stat_ecdf(size = 1) + 
  scale_y_continuous(name = "Percentagem", labels=percent, breaks = c(seq(0,1,by = 0.1))) +
  scale_x_continuous(labels=percent, breaks = c(seq(0,1,by = 0.1))) + 
  ggtitle("CDF - Função de Densidade Acumulada (Latência 4G - Referência 100 ms)") + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

df %>%
  filter(Prestadora != "Prestadora", Latencia > 0,
         Latencia < 5000, Tecnologia == "3G", 
         is.na(Cod_IBGE) == FALSE) %>%
  group_by(Cod_IBGE, Prestadora) %>%
  dplyr::summarise(Indicador = sum(Latencia <= 170, na.rm = T) / n()) %>%
  group_by(Prestadora, Indicador) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  ggplot(aes(x = Indicador, color = Prestadora)) +
  stat_ecdf(size = 1) + 
  scale_y_continuous(name = "Percentagem", labels=percent, breaks = c(seq(0,1,by = 0.1))) +
  scale_x_continuous(labels=percent, breaks = c(seq(0,1,by = 0.1))) + 
  ggtitle("CDF - Função de Densidade Acumulada (Latência 3G - Referência 170 ms)") + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

# JITTER #

df %>%
  filter(Prestadora != "Prestadora", Jitter > 0,
         Jitter < 5000, Tecnologia == "4G", 
         is.na(Cod_IBGE) == FALSE) %>%
  group_by(Cod_IBGE, Prestadora) %>%
  dplyr::summarise(Indicador = sum(Jitter <= 20, na.rm = T) / n()) %>%
  group_by(Prestadora, Indicador) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  ggplot(aes(x = Indicador, color = Prestadora)) +
  stat_ecdf(size = 1) + 
  scale_y_continuous(name = "Percentagem", labels=percent, breaks = c(seq(0,1,by = 0.1))) +
  scale_x_continuous(labels=percent, breaks = c(seq(0,1,by = 0.1))) + 
  ggtitle("CDF - Função de Densidade Acumulada (Jitter 4G - Referência 20 ms)") + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

df %>%
  filter(Prestadora != "Prestadora", Jitter > 0,
         Jitter < 5000, Tecnologia == "3G", 
         is.na(Cod_IBGE) == FALSE) %>%
  group_by(Cod_IBGE, Prestadora) %>%
  dplyr::summarise(Indicador = sum(Jitter <= 50, na.rm = T) / n()) %>%
  group_by(Prestadora, Indicador) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  ggplot(aes(x = Indicador, color = Prestadora)) +
  stat_ecdf(size = 1) + 
  scale_y_continuous(name = "Percentagem", labels=percent, breaks = c(seq(0,1,by = 0.1))) +
  scale_x_continuous(labels=percent, breaks = c(seq(0,1,by = 0.1))) + 
  ggtitle("CDF - Função de Densidade Acumulada (Jitter 3G - Referência 50 ms)") + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

# DOWNLOAD #

df2 %>%
  filter(Prestadora != "Prestadora", DOWN > 0,
         DOWN < 50000, TECNOLOGIA == "4G", 
         FILTRO_UF == TRUE) %>%
  group_by(Município, Prestadora) %>%
  summarise(Indicador = sum(DOWN/1000 >= 7, na.rm = T) / n()) %>%
  group_by(Prestadora, Indicador) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  ggplot(aes(x = Indicador, color = Prestadora)) +
  stat_ecdf(size = 1) + 
  scale_y_continuous(name = "Percentagem", labels=percent, breaks = c(seq(0,1,by = 0.1))) +
  scale_x_continuous(labels=percent, breaks = c(seq(0,1,by = 0.1))) + 
  ggtitle("CDF - Função de Densidade Acumulada (Download 4G - Referência 7 Mbps)") + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

df2 %>%
  filter(Prestadora != "Prestadora", DOWN > 0,
         DOWN < 50000, TECNOLOGIA == "4G", 
         FILTRO_UF == TRUE) %>%
  group_by(Município, Prestadora) %>%
  summarise(Indicador = sum(DOWN/1000 >= 30, na.rm = T) / n()) %>%
  group_by(Prestadora, Indicador) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  ggplot(aes(x = Indicador, color = Prestadora)) +
  stat_ecdf(size = 1) + 
  scale_y_continuous(name = "Percentagem", labels=percent, breaks = c(seq(0,1,by = 0.1))) +
  scale_x_continuous(labels=percent, breaks = c(seq(0,1,by = 0.1))) + 
  ggtitle("CDF - Função de Densidade Acumulada (Download 4G - Referência 30 Mbps)") + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

df2 %>%
  filter(Prestadora != "Prestadora", DOWN > 0,
         DOWN < 50000, TECNOLOGIA == "3G", 
         FILTRO_UF == TRUE) %>%
  group_by(Município, Prestadora) %>%
  summarise(Indicador = sum(DOWN/1000 >= 1.3, na.rm = T) / n()) %>%
  group_by(Prestadora, Indicador) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  ggplot(aes(x = Indicador, color = Prestadora)) +
  stat_ecdf(size = 1) + 
  scale_y_continuous(name = "Percentagem", labels=percent, breaks = c(seq(0,1,by = 0.1))) +
  scale_x_continuous(labels=percent, breaks = c(seq(0,1,by = 0.1))) + 
  ggtitle("CDF - Função de Densidade Acumulada (Download 3G - Referência 1.3 Mbps)") + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

df2 %>%
  filter(Prestadora != "Prestadora", DOWN > 0,
         DOWN < 50000, TECNOLOGIA == "3G", 
         FILTRO_UF == TRUE) %>%
  group_by(Município, Prestadora) %>%
  summarise(Indicador = sum(DOWN/1000 >= 6.5, na.rm = T) / n()) %>%
  group_by(Prestadora, Indicador) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  ggplot(aes(x = Indicador, color = Prestadora)) +
  stat_ecdf(size = 1) + 
  scale_y_continuous(name = "Percentagem", labels=percent, breaks = c(seq(0,1,by = 0.1))) +
  scale_x_continuous(labels=percent, breaks = c(seq(0,1,by = 0.1))) + 
  ggtitle("CDF - Função de Densidade Acumulada (Download 3G - Referência 6.5 Mbps)") + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

# UPLOAD #

df2 %>%
  filter(Prestadora != "Prestadora", UP > 0,
         UP < 50000, TECNOLOGIA == "4G", 
         FILTRO_UF == TRUE) %>%
  group_by(Município, Prestadora) %>%
  summarise(Indicador = sum(UP/1000 >= 4.2, na.rm = T) / n()) %>%
  group_by(Prestadora, Indicador) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  ggplot(aes(x = Indicador, color = Prestadora)) +
  stat_ecdf(size = 1) + 
  scale_y_continuous(name = "Percentagem", labels=percent, breaks = c(seq(0,1,by = 0.1))) +
  scale_x_continuous(labels=percent, breaks = c(seq(0,1,by = 0.1))) + 
  ggtitle("CDF - Função de Densidade Acumulada (Upload 4G - Referência 4.2 Mbps)") + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

df2 %>%
  filter(Prestadora != "Prestadora", UP > 0,
         UP < 50000, TECNOLOGIA == "4G", 
         FILTRO_UF == TRUE) %>%
  group_by(Município, Prestadora) %>%
  summarise(Indicador = sum(UP/1000 >= 17, na.rm = T) / n()) %>%
  group_by(Prestadora, Indicador) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  ggplot(aes(x = Indicador, color = Prestadora)) +
  stat_ecdf(size = 1) + 
  scale_y_continuous(name = "Percentagem", labels=percent, breaks = c(seq(0,1,by = 0.1))) +
  scale_x_continuous(labels=percent, breaks = c(seq(0,1,by = 0.1))) + 
  ggtitle("CDF - Função de Densidade Acumulada (Upload 4G - Referência 17 Mbps)") + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

df2 %>%
  filter(Prestadora != "Prestadora", UP > 0,
         UP < 50000, TECNOLOGIA == "3G", 
         FILTRO_UF == TRUE) %>%
  group_by(Município, Prestadora) %>%
  summarise(Indicador = sum(UP/1000 >= 0.7, na.rm = T) / n()) %>%
  group_by(Prestadora, Indicador) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  ggplot(aes(x = Indicador, color = Prestadora)) +
  stat_ecdf(size = 1) + 
  scale_y_continuous(name = "Percentagem", labels=percent, breaks = c(seq(0,1,by = 0.1))) +
  scale_x_continuous(labels=percent, breaks = c(seq(0,1,by = 0.1))) + 
  ggtitle("CDF - Função de Densidade Acumulada (Upload 3G - Referência 700 Kbps)") + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

df2 %>%
  filter(Prestadora != "Prestadora", UP > 0,
         UP < 50000, TECNOLOGIA == "3G", 
         FILTRO_UF == TRUE) %>%
  group_by(Município, Prestadora) %>%
  summarise(Indicador = sum(UP/1000 >= 2.5, na.rm = T) / n()) %>%
  group_by(Prestadora, Indicador) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  ggplot(aes(x = Indicador, color = Prestadora)) +
  stat_ecdf(size = 1) + 
  scale_y_continuous(name = "Percentagem", labels=percent, breaks = c(seq(0,1,by = 0.1))) +
  scale_x_continuous(labels=percent, breaks = c(seq(0,1,by = 0.1))) + 
  ggtitle("CDF - Função de Densidade Acumulada (Upload 3G - Referência 2.5 Mbps)") + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
