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
               breaks = c(0,50000,100000,300000,2000000),
               labels = c("Pequeno","Medio","Medio-Grande","Grande"))

df$Ind4 <- df$Meta_SMP10 / df$Medidas
df$Medidas_Media <- df$Medidas / df$Coletores

rm(pop2017)

################
### ANALISES ###
################

### Analisa a quantidade de medidas por municipio ###
df %>%
  filter(Coletores < 50000, Prestadora != "NEXTEL") %>%
  summarize(Min = min(Medidas),
            Percentil_5 = quantile(Medidas, 0.05),
            Quartil_1 = quantile(Medidas, 0.25),
            Mediana = median(Medidas),
            Media = mean(Medidas),
            Quartil_3 = quantile(Medidas, 0.75),
            Percentil_95 = quantile(Medidas, 0.95),
            Max = max(Medidas),
            N = n()) 

df %>%
  filter(Coletores < 50000, Prestadora != "NEXTEL", Mes == 6) %>%
  ggplot(aes(Medidas)) +
  geom_histogram(fill = "red", binwidth = 500) +
  xlab("Qtde de Municípios") + 
  ylab("Qtde de Medidas") + 
  theme_minimal()


### Gera um data-frame com o indicador agrupado pelo tamanho da cidade ###
df %>%
  filter(Prestadora != "NEXTEL") %>%
  group_by(Tipo,Mes) %>%
  summarize(Min = min(Ind4, na.rm = T),
            Quartil_1 = quantile(Ind4, 0.25, na.rm = T),
            Mediana = median(Ind4,  na.rm = T),
            Media = mean(Ind4, na.rm = T),
            Quartil_3 = quantile(Ind4, 0.75, na.rm = T),
            Max = max(Ind4, na.rm = T),
            N = n()) %>%
  filter(is.na(Tipo) == FALSE) %>%
  print(n = 30)

### Analisa o indicador em cidades com pelo menos 19 medicoes (mediana) em Junho ###
df %>%
  filter(Medidas >= 19, Coletores < 50000, Prestadora != "NEXTEL", Mes == 6) %>% 
  ggplot(aes(Ind4)) +
  geom_histogram(fill = "blue") +
  scale_x_continuous(labels = scales::percent) + 
  xlab("Indicador") + 
  ylab("Frequência") + 
  theme_minimal()

### Analisa o indicador por mes ###
df %>%
  filter(Medidas >= 19, Coletores < 50000, Prestadora != "NEXTEL") %>%
  ggplot(aes(factor(Mes, labels = c("Jan","Fev","Mar","Abr","Mai","Jun")), y = Ind4)) +
  geom_boxplot(fill = "red") +
  scale_y_continuous(labels = scales::percent) + 
  xlab("") + 
  ylab("Indicador") + 
  theme_minimal()

### Analisa indicador em cidades com pelo menos 19 medicoes ###
df %>%
  filter(Medidas >= 19,Coletores < 50000, Prestadora != "NEXTEL") %>%
  summarize(Min = min(Ind4),
            Percentil_5 = quantile(Ind4, 0.05),
            Quartil_1 = quantile(Ind4, 0.25),
            Mediana = median(Ind4),
            Media = mean(Ind4),
            Quartil_3 = quantile(Ind4, 0.75),
            Percentil_95 = quantile(Ind4, 0.95),
            Max = max(Ind4),
            N = n())

### Analisa a quantidade de coletores por tamanho da cidade e Mes ###
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

### Quantidade de municipios que estariam de acordo com a qtde de medidas (362) ###

### Mes ###

df %>% 
  filter(Coletores < 50000, Prestadora != "NEXTEL", is.na(Tipo) == FALSE) %>%
  mutate(DeAcordo = Medidas >= 362) %>%
  group_by(Mes,Prestadora, Tipo) %>%
  summarize(Qtde_Cidades = sum(DeAcordo),
            Prop_Cidades = mean(DeAcordo),
            N = n()) %>%
  print(n = 100)

df %>% 
  filter(Coletores < 50000, Prestadora != "NEXTEL", is.na(Tipo) == FALSE) %>%
  mutate(DeAcordo = Medidas >= 362) %>%
  group_by(Mes) %>%
  summarize(Qtde_Cidades = sum(DeAcordo),
            Prop_Cidades = mean(DeAcordo),
            N = n()) %>%
  ggplot(aes(x = factor(Mes, labels = c("Jan","Fev","Mar","Abr","Mai","Jun")), y = Prop_Cidades)) +
  geom_col(fill = "darkblue") +
  xlab("Mês") + 
  scale_y_continuous(labels = scales::percent) +
  ylab("Proporção de Municípios") + 
  theme_minimal()

df %>% 
  filter(Coletores < 50000, Prestadora != "NEXTEL", is.na(Tipo) == FALSE) %>%
  mutate(DeAcordo = Medidas >= 362) %>%
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

### Trimestre ###

df %>% 
  filter(Coletores < 50000, Prestadora != "NEXTEL", is.na(Tipo) == FALSE) %>%
  select(Mes, Cod_IBGE, Medidas, Prestadora, Tipo,Ind4) %>%
  mutate(Trimestre = cut(Mes, breaks = 2, labels = c("Trimestre1","Trimestre2"))) %>%
  group_by(Cod_IBGE, Prestadora, Tipo, Trimestre) %>%
  summarize(Medidas_Trimestre = sum(Medidas)) %>%
  mutate(DeAcordo = Medidas_Trimestre >= 362) %>%
  group_by(Prestadora, Tipo, Trimestre) %>%
  summarize(Qtde_Cidades = sum(DeAcordo),
            Prop_Cidades = mean(DeAcordo)) %>%
  print(n = 100)

df %>% 
  filter(Coletores < 50000, Prestadora != "NEXTEL", is.na(Tipo) == FALSE) %>%
  select(Mes, Cod_IBGE, Medidas, Prestadora, Tipo,Ind4) %>%
  mutate(Trimestre = cut(Mes, breaks = 2, labels = c("Trimestre1","Trimestre2"))) %>%
  group_by(Cod_IBGE, Prestadora, Tipo, Trimestre) %>%
  summarize(Medidas_Trimestre = sum(Medidas)) %>%
  mutate(DeAcordo = Medidas_Trimestre >= 362) %>%
  group_by(Trimestre) %>%
  summarize(Qtde_Cidades = sum(DeAcordo),
            Prop_Cidades = mean(DeAcordo)) %>%
  ggplot(aes(x = Trimestre, y = Prop_Cidades)) +
  geom_col(fill = "darkblue") +
  xlab("Trimestre") + 
  scale_y_continuous(labels = scales::percent) +
  ylab("Proporção de Municípios") + 
  theme_minimal()

### Semestre ###

df %>%
  filter(Coletores < 50000, Prestadora != "NEXTEL", is.na(Tipo) == FALSE) %>%
  group_by(Cod_IBGE, Prestadora, Tipo) %>%
  summarize(Medidas_Semestre = sum(Medidas)) %>%
  mutate(DeAcordo = Medidas_Semestre >= 362) %>%
  group_by(Prestadora, Tipo) %>%
  summarize(Qtde_Cidades = sum(DeAcordo),
            Prop_Cidades = mean(DeAcordo)) %>%
  print(n = 100)

df %>%
  filter(Coletores < 50000, Prestadora != "NEXTEL", is.na(Tipo) == FALSE) %>%
  group_by(Cod_IBGE, Prestadora, Tipo) %>%
  summarize(Medidas_Semestre = sum(Medidas)) %>%
  mutate(DeAcordo = Medidas_Semestre >= 362) %>%
  group_by(1) %>%
  summarize(Qtde_Cidades = sum(DeAcordo),
            Prop_Cidades = mean(DeAcordo))

df %>% 
  filter(Coletores < 50000, Prestadora != "NEXTEL", is.na(Tipo) == FALSE) %>%
  group_by(Cod_IBGE, Prestadora, Tipo) %>%
  summarize(Medidas_Semestre = sum(Medidas)) %>%
  mutate(DeAcordo = Medidas_Semestre >= 362) %>%
  group_by(1) %>%
  summarize(Qtde_Cidades = sum(DeAcordo),
            Prop_Cidades = mean(DeAcordo)) %>%
  ggplot(aes(x = 1, y = Prop_Cidades)) +
  geom_col(fill = "darkblue") +
  xlab("Semestre") + 
  scale_y_continuous(labels = scales::percent) +
  ylab("Proporção de Municípios") + 
  theme_minimal() + 
  theme(axis.text.x = element_blank())

### ESCALONAR ERRO ###

# Pequeno = 10% #

df %>% 
  filter(Coletores < 50000, Prestadora != "NEXTEL", is.na(Tipo) == FALSE, Tipo == "Pequeno") %>%
  mutate(DeAcordo = Medidas >= 90) %>%
  group_by(Mes) %>%
  summarize(Qtde_Cidades = sum(DeAcordo),
            Prop_Cidades = mean(DeAcordo),
            N = n()) %>%
  ggplot(aes(x = factor(Mes, labels = c("Jan","Fev","Mar","Abr","Mai","Jun")), y = Prop_Cidades)) +
  geom_col(fill = "darkblue") +
  xlab("Mês") + 
  scale_y_continuous(labels = scales::percent) +
  ylab("Proporção de Municípios") + 
  theme_minimal()

df %>% 
  filter(Coletores < 50000, Prestadora != "NEXTEL", is.na(Tipo) == FALSE, Tipo == "Pequeno") %>%
  select(Mes, Cod_IBGE, Medidas, Prestadora, Tipo,Ind4) %>%
  mutate(Trimestre = cut(Mes, breaks = 2, labels = c("Trimestre1","Trimestre2"))) %>%
  group_by(Cod_IBGE, Prestadora, Tipo, Trimestre) %>%
  summarize(Medidas_Trimestre = sum(Medidas)) %>%
  mutate(DeAcordo = Medidas_Trimestre >= 90) %>%
  group_by(Trimestre) %>%
  summarize(Qtde_Cidades = sum(DeAcordo),
            Prop_Cidades = mean(DeAcordo)) %>%
  ggplot(aes(x = Trimestre, y = Prop_Cidades)) +
  geom_col(fill = "darkblue") +
  xlab("Trimestre") + 
  scale_y_continuous(labels = scales::percent) +
  ylab("Proporção de Municípios") + 
  theme_minimal()

df %>% 
  filter(Coletores < 50000, Prestadora != "NEXTEL", is.na(Tipo) == FALSE, Tipo == "Pequeno") %>%
  group_by(Cod_IBGE, Prestadora, Tipo) %>%
  summarize(Medidas_Semestre = sum(Medidas)) %>%
  mutate(DeAcordo = Medidas_Semestre >= 90) %>%
  group_by(1) %>%
  summarize(Qtde_Cidades = sum(DeAcordo),
            Prop_Cidades = mean(DeAcordo)) %>%
  ggplot(aes(x = 1, y = Prop_Cidades)) +
  geom_col(fill = "darkblue") +
  xlab("Semestre") + 
  scale_y_continuous(labels = scales::percent) +
  ylab("Proporção de Municípios") + 
  theme_minimal() + 
  theme(axis.text.x = element_blank())

# Médio = 8% #

df %>% 
  filter(Coletores < 50000, Prestadora != "NEXTEL", is.na(Tipo) == FALSE, Tipo == "Medio") %>%
  mutate(DeAcordo = Medidas >= 140) %>%
  group_by(Mes) %>%
  summarize(Qtde_Cidades = sum(DeAcordo),
            Prop_Cidades = mean(DeAcordo),
            N = n()) %>%
  ggplot(aes(x = factor(Mes, labels = c("Jan","Fev","Mar","Abr","Mai","Jun")), y = Prop_Cidades)) +
  geom_col(fill = "darkblue") +
  xlab("Mês") + 
  scale_y_continuous(labels = scales::percent) +
  ylab("Proporção de Municípios") + 
  theme_minimal()

df %>% 
  filter(Coletores < 50000, Prestadora != "NEXTEL", is.na(Tipo) == FALSE, Tipo == "Medio") %>%
  select(Mes, Cod_IBGE, Medidas, Prestadora, Tipo,Ind4) %>%
  mutate(Trimestre = cut(Mes, breaks = 2, labels = c("Trimestre1","Trimestre2"))) %>%
  group_by(Cod_IBGE, Prestadora, Tipo, Trimestre) %>%
  summarize(Medidas_Trimestre = sum(Medidas)) %>%
  mutate(DeAcordo = Medidas_Trimestre >= 141) %>%
  group_by(Trimestre) %>%
  summarize(Qtde_Cidades = sum(DeAcordo),
            Prop_Cidades = mean(DeAcordo)) %>%
  ggplot(aes(x = Trimestre, y = Prop_Cidades)) +
  geom_col(fill = "darkblue") +
  xlab("Trimestre") + 
  scale_y_continuous(labels = scales::percent) +
  ylab("Proporção de Municípios") + 
  theme_minimal()

df %>% 
  filter(Coletores < 50000, Prestadora != "NEXTEL", is.na(Tipo) == FALSE, Tipo == "Medio") %>%
  group_by(Cod_IBGE, Prestadora, Tipo) %>%
  summarize(Medidas_Semestre = sum(Medidas)) %>%
  mutate(DeAcordo = Medidas_Semestre >= 141) %>%
  group_by(1) %>%
  summarize(Qtde_Cidades = sum(DeAcordo),
            Prop_Cidades = mean(DeAcordo)) %>%
  ggplot(aes(x = 1, y = Prop_Cidades)) +
  geom_col(fill = "darkblue") +
  xlab("Semestre") + 
  scale_y_continuous(labels = scales::percent) +
  ylab("Proporção de Municípios") + 
  theme_minimal() + 
  theme(axis.text.x = element_blank())

# Médio-Grande e Grande = 5% #

df %>% 
  filter(Coletores < 50000, Prestadora != "NEXTEL", is.na(Tipo) == FALSE, pop2017 >= 100000) %>%
  mutate(DeAcordo = Medidas >= 362) %>%
  group_by(Mes) %>%
  summarize(Qtde_Cidades = sum(DeAcordo),
            Prop_Cidades = mean(DeAcordo),
            N = n()) %>%
  ggplot(aes(x = factor(Mes, labels = c("Jan","Fev","Mar","Abr","Mai","Jun")), y = Prop_Cidades)) +
  geom_col(fill = "darkblue") +
  xlab("Mês") + 
  scale_y_continuous(labels = scales::percent) +
  ylab("Proporção de Municípios") + 
  theme_minimal()

df %>% 
  filter(Coletores < 50000, Prestadora != "NEXTEL", is.na(Tipo) == FALSE, pop2017 >= 100000) %>%
  select(Mes, Cod_IBGE, Medidas, Prestadora, Tipo,Ind4) %>%
  mutate(Trimestre = cut(Mes, breaks = 2, labels = c("Trimestre1","Trimestre2"))) %>%
  group_by(Cod_IBGE, Prestadora, Tipo, Trimestre) %>%
  summarize(Medidas_Trimestre = sum(Medidas)) %>%
  mutate(DeAcordo = Medidas_Trimestre >= 362) %>%
  group_by(Trimestre) %>%
  summarize(Qtde_Cidades = sum(DeAcordo),
            Prop_Cidades = mean(DeAcordo)) %>%
  ggplot(aes(x = Trimestre, y = Prop_Cidades)) +
  geom_col(fill = "darkblue") +
  xlab("Trimestre") + 
  scale_y_continuous(labels = scales::percent) +
  ylab("Proporção de Municípios") + 
  theme_minimal()

df %>% 
  filter(Coletores < 50000, Prestadora != "NEXTEL", is.na(Tipo) == FALSE, pop2017 >= 100000) %>%
  group_by(Cod_IBGE, Prestadora, Tipo) %>%
  summarize(Medidas_Semestre = sum(Medidas)) %>%
  mutate(DeAcordo = Medidas_Semestre >= 362) %>%
  group_by(1) %>%
  summarize(Qtde_Cidades = sum(DeAcordo),
            Prop_Cidades = mean(DeAcordo)) %>%
  ggplot(aes(x = 1, y = Prop_Cidades)) +
  geom_col(fill = "darkblue") +
  xlab("Semestre") + 
  scale_y_continuous(labels = scales::percent) +
  ylab("Proporção de Municípios") + 
  theme_minimal() + 
  theme(axis.text.x = element_blank())

### REFERENCIA ATUAL ###
### Quantidade de municipios que estariam de acordo com a qtde de medidas (53) ###

### Mes ###

df %>% 
  filter(Coletores < 50000, Prestadora != "NEXTEL", is.na(Tipo) == FALSE) %>%
  mutate(DeAcordo = Medidas >= 53) %>%
  group_by(Mes,Prestadora, Tipo) %>%
  summarize(Qtde_Cidades = sum(DeAcordo),
            Prop_Cidades = mean(DeAcordo),
            N = n()) %>%
  print(n = 100)

df %>% 
  filter(Coletores < 50000, Prestadora != "NEXTEL", is.na(Tipo) == FALSE) %>%
  mutate(DeAcordo = Medidas >= 53) %>%
  group_by(Mes) %>%
  summarize(Qtde_Cidades = sum(DeAcordo),
            Prop_Cidades = mean(DeAcordo),
            N = n()) %>%
  ggplot(aes(x = factor(Mes, labels = c("Jan","Fev","Mar","Abr","Mai","Jun")), y = Prop_Cidades)) +
  geom_col(fill = "darkblue") +
  xlab("Mês") + 
  scale_y_continuous(labels = scales::percent) +
  ylab("Proporção de Municípios") + 
  theme_minimal()

df %>% 
  filter(Coletores < 50000, Prestadora != "NEXTEL", is.na(Tipo) == FALSE) %>%
  mutate(DeAcordo = Medidas >= 53) %>%
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

### Trimestre ###

df %>% 
  filter(Coletores < 50000, Prestadora != "NEXTEL", is.na(Tipo) == FALSE) %>%
  select(Mes, Cod_IBGE, Medidas, Prestadora, Tipo,Ind4) %>%
  mutate(Trimestre = cut(Mes, breaks = 2, labels = c("Trimestre1","Trimestre2"))) %>%
  group_by(Cod_IBGE, Prestadora, Tipo, Trimestre) %>%
  summarize(Medidas_Trimestre = sum(Medidas)) %>%
  mutate(DeAcordo = Medidas_Trimestre >= 53) %>%
  group_by(Prestadora, Tipo, Trimestre) %>%
  summarize(Qtde_Cidades = sum(DeAcordo),
            Prop_Cidades = mean(DeAcordo)) %>%
  print(n = 100)

df %>% 
  filter(Coletores < 50000, Prestadora != "NEXTEL", is.na(Tipo) == FALSE) %>%
  select(Mes, Cod_IBGE, Medidas, Prestadora, Tipo,Ind4) %>%
  mutate(Trimestre = cut(Mes, breaks = 2, labels = c("Trimestre1","Trimestre2"))) %>%
  group_by(Cod_IBGE, Prestadora, Tipo, Trimestre) %>%
  summarize(Medidas_Trimestre = sum(Medidas)) %>%
  mutate(DeAcordo = Medidas_Trimestre >= 53) %>%
  group_by(Trimestre) %>%
  summarize(Qtde_Cidades = sum(DeAcordo),
            Prop_Cidades = mean(DeAcordo)) %>%
  ggplot(aes(x = Trimestre, y = Prop_Cidades)) +
  geom_col(fill = "darkblue") +
  xlab("Trimestre") + 
  scale_y_continuous(labels = scales::percent) +
  ylab("Proporção de Municípios") + 
  theme_minimal()

### Semestre ###

df %>%
  filter(Coletores < 50000, Prestadora != "NEXTEL", is.na(Tipo) == FALSE) %>%
  group_by(Cod_IBGE, Prestadora, Tipo) %>%
  summarize(Medidas_Semestre = sum(Medidas)) %>%
  mutate(DeAcordo = Medidas_Semestre >= 53) %>%
  group_by(Prestadora, Tipo) %>%
  summarize(Qtde_Cidades = sum(DeAcordo),
            Prop_Cidades = mean(DeAcordo)) %>%
  print(n = 100)

df %>%
  filter(Coletores < 50000, Prestadora != "NEXTEL", is.na(Tipo) == FALSE) %>%
  group_by(Cod_IBGE, Prestadora, Tipo) %>%
  summarize(Medidas_Semestre = sum(Medidas)) %>%
  mutate(DeAcordo = Medidas_Semestre >= 53) %>%
  group_by(1) %>%
  summarize(Qtde_Cidades = sum(DeAcordo),
            Prop_Cidades = mean(DeAcordo))

df %>% 
  filter(Coletores < 50000, Prestadora != "NEXTEL", is.na(Tipo) == FALSE) %>%
  group_by(Cod_IBGE, Prestadora, Tipo) %>%
  summarize(Medidas_Semestre = sum(Medidas)) %>%
  mutate(DeAcordo = Medidas_Semestre >= 53) %>%
  group_by(1) %>%
  summarize(Qtde_Cidades = sum(DeAcordo),
            Prop_Cidades = mean(DeAcordo)) %>%
  ggplot(aes(x = 1, y = Prop_Cidades)) +
  geom_col(fill = "darkblue") +
  xlab("Semestre") + 
  scale_y_continuous(labels = scales::percent) +
  ylab("Proporção de Municípios") + 
  theme_minimal() + 
  theme(axis.text.x = element_blank())



