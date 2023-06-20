setwd("/Users/alexvlima/Downloads/")

list.files()

library(tidyverse)

df <- read_csv2("BASE_PROCESSADA_SCM_IND4aIND7_082022_OI.csv")
glimpse(df)
sum(is.na(df$BSSID) == T)
sum(is.na(df$ID_COLETOR) == T)

considerando_bssid <- 
  df %>%
  filter(BSSID != "02:00:00:00:00:00") %>%
  group_by(BSSID, CODIGO_IBGE) %>%
  summarise(N = n(),
            DOWNLOAD_KBPS = mean(DOWNLOAD_KBPS, na.rm = T),
            UPLOAD_KBPS = mean(UPLOAD_KBPS, na.rm = T)) %>%
  filter(N > 2) %>%
  group_by(CODIGO_IBGE) %>%
  summarise(N_commbssid = n(),
            mean_down_commbssid = mean(DOWNLOAD_KBPS, na.rm = T),
            sd_down_commbssid = sd(DOWNLOAD_KBPS, na.rm = T),
            mean_up_commbssid = mean(UPLOAD_KBPS, na.rm = T),
            sd_up_commbssid = sd(UPLOAD_KBPS, na.rm = T))
# considerando_bssid
sum(considerando_bssid$N_commbssid)

desconsiderando_bssid <- 
  df %>%
  # filter(BSSID != "02:00:00:00:00:00") %>%
  group_by(ID_COLETOR, CODIGO_IBGE) %>%
  summarise(DOWNLOAD_KBPS = mean(DOWNLOAD_KBPS, na.rm = T),
            UPLOAD_KBPS = mean(UPLOAD_KBPS, na.rm = T)) %>%
  group_by(CODIGO_IBGE) %>%
  summarise(N_sembssid = n(),
            mean_down_sembssid = mean(DOWNLOAD_KBPS, na.rm = T),
            sd_down_sembssid = sd(DOWNLOAD_KBPS, na.rm = T),
            mean_up_sembssid = mean(UPLOAD_KBPS, na.rm = T),
            sd_up_sembssid = sd(UPLOAD_KBPS, na.rm = T))
# desconsiderando_bssid

df_final <- considerando_bssid %>%
  left_join(desconsiderando_bssid, by = "CODIGO_IBGE")

write_csv2(df_final, "comparacao.csv")


df %>%
  filter(BSSID != "02:00:00:00:00:00") %>%
  group_by(BSSID) %>%
  summarise(N = n(), .groups = "drop") %>%
  summarise(media = mean(N, na.rm = T),
            mediana = median(N, na.rm = T),
            Quartil_3 = quantile(N, probs = 0.75))

df %>%
  filter(BSSID != "02:00:00:00:00:00") %>%
  group_by(ID_COLETOR) %>%
  summarise(N = n(), .groups = "drop") %>%
  summarise(media = mean(N, na.rm = T),
            mediana = median(N, na.rm = T),
            Quartil_3 = quantile(N, probs = 0.75))

df %>%
  filter(BSSID != "02:00:00:00:00:00") %>%
  group_by(BSSID) %>%
  summarise(N = n(), .groups = "drop") %>%
  ggplot(aes(N)) +
  geom_boxplot(fill = "steelblue") + 
  theme_minimal()



# Qtde de BSSID em ID Coletor 
# Agrupar BSSID > 3
# Agrupar IC Coletor > 3
# Visao abrangente
# Boxplot comparando as 2 metodologias

