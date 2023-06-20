#################
### DIRETORIO ###
#################

getwd()
setwd("/Users/alexvlima/Library/Mobile Documents/com~apple~CloudDocs/ABR_Telecom_ESAQ/Grupo2")

###################
### BIBLIOTECAS ###
###################

library(tidyverse)
library(lubridate)
library(boot)
library(Rmisc)

################
### DATASETS ###
################

list.files()

### DATASET ###

df_claro <- read_csv2("./SMP/SMP_Down_Up_2020-06_CLARO.csv.gz")
glimpse(df_claro)

df_vivo <- read_csv2("./SMP/SMP_Down_Up_2020-06_VIVO.csv.gz")
glimpse(df_vivo)

df_tim <- read_csv2("./SMP/SMP_Down_Up_2020-06_TIM.csv.gz")
glimpse(df_tim)

df_oi <- read_csv2("./SMP/SMP_Down_Up_2020-06_OI.csv.gz")
glimpse(df_oi)

###############
### AJUSTES ###
###############

# Alterar para cada prestadora #
df_ookla_claro <- 
  df_claro %>%
  filter(is.na(`Codigo IBGE`) == FALSE, 
         is.na(`Tipo de Descarte`) == TRUE,
         is.na(as.numeric(speedDown)) == FALSE,
         is.na(as.numeric(speedUp)) == FALSE,
         is.na(VelocidadeDOWN) == FALSE,
         is.na(VelocidadeUP) == FALSE) %>%
  group_by(deviceID, COD_IBGE = `Codigo IBGE`) %>%
  dplyr::summarize(media = mean(as.numeric(speedDown)/VelocidadeDOWN >= 1 & as.numeric(speedUp)/VelocidadeUP >= 1),
                   mediana = median(as.numeric(speedDown)/VelocidadeDOWN >= 1 & as.numeric(speedUp)/VelocidadeUP >= 1),
                   qtde_medidas = n())
# glimpse(df_ookla_claro)

df_ookla_vivo <- 
  df_vivo %>%
  filter(is.na(`Codigo IBGE`) == FALSE, 
         is.na(`Tipo de Descarte`) == TRUE,
         is.na(as.numeric(speedDown)) == FALSE,
         is.na(as.numeric(speedUp)) == FALSE,
         is.na(VelocidadeDOWN) == FALSE,
         is.na(VelocidadeUP) == FALSE) %>%
  group_by(deviceID, COD_IBGE = `Codigo IBGE`) %>%
  dplyr::summarize(media = mean(as.numeric(speedDown)/VelocidadeDOWN >= 1000000 & as.numeric(speedUp)/VelocidadeUP >= 1000000),
                   mediana = median(as.numeric(speedDown)/VelocidadeDOWN >= 1000000 & as.numeric(speedUp)/VelocidadeUP >= 1000000),
                   qtde_medidas = n())
# glimpse(df_ookla_vivo)

df_ookla_tim <- 
  df_tim %>%
  filter(is.na(`Codigo IBGE`) == FALSE, 
         is.na(`Tipo de Descarte`) == TRUE,
         is.na(as.numeric(speedDown)) == FALSE,
         is.na(as.numeric(speedUp)) == FALSE,
         is.na(VelocidadeDOWN) == FALSE,
         is.na(VelocidadeUP) == FALSE) %>%
  group_by(deviceID, COD_IBGE = `Codigo IBGE`) %>%
  dplyr::summarize(media = mean(as.numeric(speedDown)/VelocidadeDOWN >= 1 & as.numeric(speedUp)/VelocidadeUP >= 1),
            mediana = median(as.numeric(speedDown)/VelocidadeDOWN >= 1 & as.numeric(speedUp)/VelocidadeUP >= 1),
            qtde_medidas = n())
# glimpse(df_ookla_tim)

df_ookla_oi <- 
  df_oi %>%
  filter(is.na(`Codigo IBGE`) == FALSE, 
         is.na(`Tipo de Descarte`) == TRUE,
         is.na(as.numeric(speedDown)) == FALSE,
         is.na(as.numeric(speedUp)) == FALSE,
         is.na(VelocidadeDOWN) == FALSE,
         is.na(VelocidadeUP) == FALSE) %>%
  group_by(deviceID, COD_IBGE = `Codigo IBGE`) %>%
  dplyr::summarize(media = mean(as.numeric(speedDown)/VelocidadeDOWN >= 1 & as.numeric(speedUp)/VelocidadeUP >= 1),
                   mediana = median(as.numeric(speedDown)/VelocidadeDOWN >= 1 & as.numeric(speedUp)/VelocidadeUP >= 1),
                   qtde_medidas = n())
# glimpse(df_ookla_oi)

################
### ANALISES ###
################

# Quantidade de Municipios com mais de 30 coletores #
df_ookla_claro %>% 
  group_by(COD_IBGE) %>% 
  dplyr::summarise(n = n()) %>% 
  filter(n >= 30) %>% 
  distinct(COD_IBGE) %>% 
  dplyr::count()

df_ookla_vivo %>% 
  group_by(COD_IBGE) %>% 
  dplyr::summarise(n = n()) %>% 
  filter(n >= 30) %>% 
  distinct(COD_IBGE) %>% 
  dplyr::count()

df_ookla_tim %>% 
  group_by(COD_IBGE) %>% 
  dplyr::summarise(n = n()) %>% 
  filter(n >= 30) %>% 
  distinct(COD_IBGE) %>% 
  dplyr::count()

df_ookla_oi %>% 
  group_by(COD_IBGE) %>% 
  dplyr::summarise(n = n()) %>% 
  filter(n >= 30) %>% 
  distinct(COD_IBGE) %>% 
  dplyr::count()

# Funcao para o boostrap #
samp_mean <- function(x, i) {
  mean(x[i])
}

# Exemplo - São Paulo # 
# Alterar para cada prestadora #
Exemplo_SP <- df_ookla_vivo %>% filter(COD_IBGE == 3550308)  
glimpse(Exemplo_SP)

output <- boot(Exemplo_SP$media, statistic = samp_mean, R = 1000)
output2 <- boot(Exemplo_SP$mediana, statistic = samp_mean, R = 1000)

plot(output)
plot(output2)

CI(output$t)
CI(output2$t)

# Exemplo Rio de Janeiro #
Exemplo_RiodeJaneiro <- df_ookla_oi %>% filter(COD_IBGE == 3304557)  

output <- boot(Exemplo_RiodeJaneiro$media, statistic = samp_mean, R = 1000)
output2 <- boot(Exemplo_RiodeJaneiro$mediana, statistic = samp_mean, R = 1000)
CI(output$t)
CI(output2$t)

# Exemplo Castanhal-PA #
Exemplo_Castanhal <- df_ookla_vivo %>% filter(COD_IBGE == 1502400)  

output <- boot(Exemplo_Castanhal$media, statistic = samp_mean, R = 1000)
output2 <- boot(Exemplo_Castanhal$mediana, statistic = samp_mean, R = 1000)

CI(output$t)
CI(output2$t)

# Exemplo Porto Alegre #
Exemplo_PortoAlegre <- df_ookla_oi %>% filter(COD_IBGE == 4314902)  

output <- boot(Exemplo_PortoAlegre$media, statistic = samp_mean, R = 1000)
output2 <- boot(Exemplo_PortoAlegre$mediana, statistic = samp_mean, R = 1000)
CI(output$t)
CI(output2$t)

#################
### ANALISE 2 ###
#################

df_oi_2 <- 
  df_oi %>%
  filter(is.na(`Codigo IBGE`) == FALSE, 
         is.na(`Tipo de Descarte`) == TRUE,
         is.na(as.numeric(speedDown)) == FALSE,
         is.na(as.numeric(speedUp)) == FALSE,
         is.na(VelocidadeDOWN) == FALSE,
         is.na(VelocidadeUP) == FALSE) %>%
  group_by(deviceID, COD_IBGE = `Codigo IBGE`) %>%
  dplyr::summarize(media = mean(as.numeric(speedDown)/VelocidadeDOWN >= 1 & as.numeric(speedUp)/VelocidadeUP >= 1),
                   mediana = median(as.numeric(speedDown)/VelocidadeDOWN >= 1 & as.numeric(speedUp)/VelocidadeUP >= 1),
                   qtde_medidas = n())

df_oi_2 <- 
  df_oi_2 %>% 
  group_by(COD_IBGE) %>% 
  dplyr::summarise(n = n()) %>% 
  filter(n >= 30) %>% 
  distinct(COD_IBGE) 

df_claro_2 <- 
  df_claro %>%
  filter(is.na(`Codigo IBGE`) == FALSE, 
         is.na(`Tipo de Descarte`) == TRUE,
         is.na(as.numeric(speedDown)) == FALSE,
         is.na(as.numeric(speedUp)) == FALSE,
         is.na(VelocidadeDOWN) == FALSE,
         is.na(VelocidadeUP) == FALSE) %>%
  group_by(deviceID, COD_IBGE = `Codigo IBGE`) %>%
  dplyr::summarize(media = mean(as.numeric(speedDown)/VelocidadeDOWN >= 1 & as.numeric(speedUp)/VelocidadeUP >= 1),
                   mediana = median(as.numeric(speedDown)/VelocidadeDOWN >= 1 & as.numeric(speedUp)/VelocidadeUP >= 1),
                   qtde_medidas = n())

df_claro_2 <- 
  df_claro_2 %>% 
  group_by(COD_IBGE) %>% 
  dplyr::summarise(n = n()) %>% 
  filter(n >= 30) %>% 
  distinct(COD_IBGE) 

df_vivo_2 <- 
  df_vivo %>%
  filter(is.na(`Codigo IBGE`) == FALSE, 
         is.na(`Tipo de Descarte`) == TRUE,
         is.na(as.numeric(speedDown)) == FALSE,
         is.na(as.numeric(speedUp)) == FALSE,
         is.na(VelocidadeDOWN) == FALSE,
         is.na(VelocidadeUP) == FALSE) %>%
  group_by(deviceID, COD_IBGE = `Codigo IBGE`) %>%
  dplyr::summarize(media = mean(as.numeric(speedDown)/VelocidadeDOWN >= 1 & as.numeric(speedUp)/VelocidadeUP >= 1),
                   mediana = median(as.numeric(speedDown)/VelocidadeDOWN >= 1 & as.numeric(speedUp)/VelocidadeUP >= 1),
                   qtde_medidas = n())

df_vivo_2 <- 
  df_vivo_2 %>% 
  group_by(COD_IBGE) %>% 
  dplyr::summarise(n = n()) %>% 
  filter(n >= 30) %>% 
  distinct(COD_IBGE) 

df_tim_2 <- 
  df_tim %>%
  filter(is.na(`Codigo IBGE`) == FALSE, 
         is.na(`Tipo de Descarte`) == TRUE,
         is.na(as.numeric(speedDown)) == FALSE,
         is.na(as.numeric(speedUp)) == FALSE,
         is.na(VelocidadeDOWN) == FALSE,
         is.na(VelocidadeUP) == FALSE) %>%
  group_by(deviceID, COD_IBGE = `Codigo IBGE`) %>%
  dplyr::summarize(media = mean(as.numeric(speedDown)/VelocidadeDOWN >= 1 & as.numeric(speedUp)/VelocidadeUP >= 1),
                   mediana = median(as.numeric(speedDown)/VelocidadeDOWN >= 1 & as.numeric(speedUp)/VelocidadeUP >= 1),
                   qtde_medidas = n())

df_tim_2 <- 
  df_tim_2 %>% 
  group_by(COD_IBGE) %>% 
  dplyr::summarise(n = n()) %>% 
  filter(n >= 30) %>% 
  distinct(COD_IBGE) 

df_competicao_total <- 
  df_oi_2 %>% 
  inner_join(df_claro_2, by = "COD_IBGE") %>% 
  inner_join(df_vivo_2, by = "COD_IBGE") %>%
  inner_join(df_tim_2, by = "COD_IBGE")
