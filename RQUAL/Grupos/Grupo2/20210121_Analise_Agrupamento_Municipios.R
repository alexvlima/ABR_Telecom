#################
### DIRETORIO ###
#################

getwd()
setwd("/Users/alexvlima/Library/Mobile Documents/com~apple~CloudDocs/ABR_Telecom_ESAQ/Grupo2/")

###################
### BIBLIOTECAS ###
###################

library(tidyverse)
library(readxl)
library(brazilmaps)

###############
### DATASET ###
###############

rm(list = ls())

df <- read_excel("dados_avaliacao_p.xlsx")

df <- df %>%
  select(Ano, Mês, Prestadora, `Codigo IBGE`, `%Down_SCM5`, `%Up_SCM5`, Medição_Meta_Anatel_Down_UP_SCM4, Medidas_Totais, Coletores)
colnames(df) <- c("Ano","Mes","Prestadora","Cod_IBGE","Down","Up","Meta_SMP10","Medidas","Coletores")
glimpse(df)  

pop2017 <- brazilmaps::pop2017
glimpse(pop2017)

###############
### AJUSTES ###
###############

unique(df$Prestadora)
df <- 
  df %>%
  filter(Prestadora != "Prestadora", is.na(Prestadora) == FALSE)

df <- 
  df %>%
  left_join(pop2017, by = c("Cod_IBGE" = "mun"))

df$Tipo <- cut(df$pop2017, 
               breaks = c(0,100000,300000,200000000),
               labels = c("Pequeno/Medio","Medio-Grande","Grande"))

df$Ind4 <- df$Meta_SMP10 / df$Medidas
df$Medidas_Media <- df$Medidas / df$Coletores

rm(pop2017)

tab_ibge <- read_excel("../regioes_geograficas_composicao_por_municipios_2017_20180911.xls")
glimpse(tab_ibge)

tab_ibge$CD_GEOCODI <- as.numeric(tab_ibge$CD_GEOCODI)
tab_ibge$cod_rgi <- as.numeric(tab_ibge$cod_rgi)
tab_ibge$cod_rgint <- as.numeric(tab_ibge$cod_rgint)

df <- df %>% left_join(tab_ibge, by = c("Cod_IBGE" = "CD_GEOCODI"))
glimpse(df)

rm(tab_ibge)

#################################
### DATAFRAME - MICRO E MACRO ###
#################################

df_micro <- 
  df %>% 
  filter(Tipo == "Pequeno/Medio", Prestadora != "NEXTEL", Coletores < 50000) %>% 
  group_by(Mes, Prestadora, cod_rgi) %>%
  summarise(Medidas = sum(Medidas), 
            Coletores = sum(Coletores), 
            Pop_2017 = sum(pop2017))
glimpse(df_micro)  

df_meso <- 
  df %>% 
  filter(Tipo == "Pequeno/Medio", Prestadora != "NEXTEL", Coletores < 50000) %>% 
  group_by(Mes, Prestadora, cod_rgint) %>%
  summarise(Medidas = sum(Medidas), 
            Coletores = sum(Coletores), 
            Pop_2017 = sum(pop2017))
glimpse(df_meso) 

################
### ANALISES ###
################

### Analisa a quantidade de medidas por tamanho da cidade e Mes ###
medidas_munic <- 
  df %>%
  filter(Coletores < 50000, Prestadora != "NEXTEL") %>%
  group_by(Tipo,Mes) %>%
  summarize(Min = min(Medidas),
            Percentil_5 = quantile(Medidas, 0.05),
            Quartil_1 = quantile(Medidas, 0.25),
            Mediana = median(Medidas),
            Media = mean(Medidas),
            Quartil_3 = quantile(Medidas, 0.75),
            Percentil_95 = quantile(Medidas, 0.95),
            Max = max(Medidas),
            N = n()) 

# write_csv2(medidas_munic, "medidas_munic.csv")

df_micro %>%
  filter(Coletores < 50000, Prestadora != "NEXTEL") %>%
  group_by(Mes) %>%
  summarize(Min = min(Medidas),
            Percentil_5 = quantile(Medidas, 0.05),
            Quartil_1 = quantile(Medidas, 0.25),
            Mediana = median(Medidas),
            Media = mean(Medidas),
            Quartil_3 = quantile(Medidas, 0.75),
            Percentil_95 = quantile(Medidas, 0.95),
            Max = max(Medidas),
            N = n())

df_meso %>%
  filter(Coletores < 50000, Prestadora != "NEXTEL") %>%
  group_by(Mes) %>%
  summarize(Min = min(Medidas),
            Percentil_5 = quantile(Medidas, 0.05),
            Quartil_1 = quantile(Medidas, 0.25),
            Mediana = median(Medidas),
            Media = mean(Medidas),
            Quartil_3 = quantile(Medidas, 0.75),
            Percentil_95 = quantile(Medidas, 0.95),
            Max = max(Medidas),
            N = n())

### Analisa a quantidade de medidas por tamanho da cidade e Mes ###
medidas_coletores <- 
  df %>%
  filter(Coletores < 50000, Prestadora != "NEXTEL") %>%
  group_by(Tipo,Mes) %>%
  summarize(Min = min(Coletores),
            Percentil_5 = quantile(Coletores, 0.05),
            Quartil_1 = quantile(Coletores, 0.25),
            Mediana = median(Coletores),
            Media = mean(Coletores),
            Quartil_3 = quantile(Coletores, 0.75),
            Percentil_95 = quantile(Coletores, 0.95),
            Max = max(Coletores),
            N = n()) 

# write_csv2(medidas_coletores, "medidas_coletores.csv")

df_micro %>%
  filter(Coletores < 50000, Prestadora != "NEXTEL") %>%
  group_by(Mes) %>%
  summarize(Min = min(Coletores),
            Percentil_5 = quantile(Coletores, 0.05),
            Quartil_1 = quantile(Coletores, 0.25),
            Mediana = median(Coletores),
            Media = mean(Coletores),
            Quartil_3 = quantile(Coletores, 0.75),
            Percentil_95 = quantile(Coletores, 0.95),
            Max = max(Coletores),
            N = n())

df_meso %>%
  filter(Coletores < 50000, Prestadora != "NEXTEL") %>%
  group_by(Mes) %>%
  summarize(Min = min(Coletores),
            Percentil_5 = quantile(Coletores, 0.05),
            Quartil_1 = quantile(Coletores, 0.25),
            Mediana = median(Coletores),
            Media = mean(Coletores),
            Quartil_3 = quantile(Coletores, 0.75),
            Percentil_95 = quantile(Coletores, 0.95),
            Max = max(Coletores),
            N = n())

### Avalia a quantidade media de medidas por coletor ###
mean(df$Medidas_Media)
median(df$Medidas_Media)
max(df$Medidas_Media)

### Boxplot com a quantidade de medidas por coletor em cada mes ###
df %>%
  filter(Coletores < 50000, Prestadora != "NEXTEL") %>%
  ggplot(aes(x = factor(Mes, labels = c("Jan","Fev","Mar","Abr","Mai","Jun")), y = Medidas_Media)) +
  geom_boxplot(fill = "red") +
  xlab('Mês') +
  ylab("Qtde de Medidas") +
  ggtitle("Média de Medidas por Coletor") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

### Quantidade de municipios que estariam de acordo com a qtde de medidas (213) ###


df %>% 
  filter(Coletores < 50000, Prestadora != "NEXTEL", is.na(Tipo) == FALSE) %>%
  mutate(DeAcordo = Medidas >= 213) %>%
  group_by(Mes, Tipo) %>%
  summarize(Qtde_Cidades = sum(DeAcordo),
            Prop_Cidades = mean(DeAcordo)) %>%
  ggplot(aes(x = factor(Mes, labels = c("Jan","Fev","Mar","Abr","Mai","Jun")), y = Prop_Cidades)) +
  geom_col(fill = "darkblue") +
  xlab("Mês") + 
  scale_y_continuous(labels = scales::percent) +
  ylab("Proporção de Municípios") + 
  facet_wrap(~Tipo, scales = "free_y") + 
  theme_minimal()

df %>% 
  filter(Coletores < 50000, Prestadora != "NEXTEL", Tipo == "Pequeno/Medio") %>%
  mutate(DeAcordo = Medidas >= 213) %>%
  group_by(Mes) %>%
  summarize(Qtde_Cidades = sum(DeAcordo),
            Prop_Cidades = mean(DeAcordo)) %>%
  ggplot(aes(x = factor(Mes, labels = c("Jan","Fev","Mar","Abr","Mai","Jun")), y = Prop_Cidades)) +
  geom_col(fill = "darkblue") +
  ggtitle("Pequeno") +
  xlab("Mês") + 
  scale_y_continuous(labels = scales::percent) +
  ylab("Proporção de Municípios") + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

df_micro %>% 
  mutate(DeAcordo = Medidas >= 213) %>%
  group_by(Mes) %>%
  summarize(Qtde_Cidades = sum(DeAcordo),
            Prop_Cidades = mean(DeAcordo)) %>%
  ggplot(aes(x = factor(Mes, labels = c("Jan","Fev","Mar","Abr","Mai","Jun")), y = Prop_Cidades)) +
  geom_col(fill = "darkblue") +
  ggtitle("Regiões Imediatas") +
  xlab("Mês") + 
  scale_y_continuous(labels = scales::percent) +
  ylab("Proporção de Municípios") + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

df_meso %>% 
  mutate(DeAcordo = Medidas >= 213) %>%
  group_by(Mes) %>%
  summarize(Qtde_Cidades = sum(DeAcordo),
            Prop_Cidades = mean(DeAcordo)) %>%
  ggplot(aes(x = factor(Mes, labels = c("Jan","Fev","Mar","Abr","Mai","Jun")), y = Prop_Cidades)) +
  geom_col(fill = "darkblue") +
  ggtitle("Regiões Intermediárias") +
  xlab("Mês") + 
  scale_y_continuous(labels = scales::percent) +
  ylab("Proporção de Municípios") + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))


