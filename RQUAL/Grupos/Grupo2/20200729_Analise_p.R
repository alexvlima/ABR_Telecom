getwd()
setwd("/Users/alexvlima/Library/Mobile Documents/com~apple~CloudDocs/ABR_Telecom_ESAQ/Grupo2/")

library(tidyverse)
library(readxl)

rm(list = ls())

df <- read_excel("p_SMP2.xlsx")
glimpse(df)

df %>%
  filter(Tipo == "Pequeno", Prestadora != "NEXTEL") %>%
  ggplot(aes(x = factor(Mês, labels = c("Jan","Fev","Mar","Abr","Mai","Jun")), y = IND4_DOWN)) +
  geom_boxplot(fill = "red") +
  xlab('Mês') +
  ylab("IND4 - Download") +
  scale_y_continuous(labels = scales::percent) + 
  ggtitle("Pequeno") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

df %>%
  filter(Tipo == "Médio", Prestadora != "NEXTEL") %>%
  ggplot(aes(x = factor(Mês, labels = c("Jan","Fev","Mar","Abr","Mai","Jun")), y = IND4_DOWN)) +
  geom_boxplot(fill = "red") +
  xlab('Mês') +
  ylab("IND4 - Download") +
  scale_y_continuous(labels = scales::percent) + 
  ggtitle("Médio") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

df %>%
  filter(Tipo == "Médio-Grande", Prestadora != "NEXTEL") %>%
  ggplot(aes(x = factor(Mês, labels = c("Jan","Fev","Mar","Abr","Mai","Jun")), y = IND4_DOWN)) +
  geom_boxplot(fill = "red") +
  xlab('Mês') +
  ylab("IND4 - Download") +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Médio-Grande") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

df %>%
  filter(Tipo == "Grande", Prestadora != "NEXTEL") %>%
  ggplot(aes(x = factor(Mês, labels = c("Jan","Fev","Mar","Abr","Mai","Jun")), y = IND4_DOWN)) +
  geom_boxplot(fill = "red") +
  xlab('Mês') +
  ylab("IND4 - Download") +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Grande") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

dados <- 
  df %>%
  filter(Prestadora != "NEXTEL") %>%
  group_by(Tipo,Mês) %>%
  summarize(Min = min(IND4_DOWN),
            Quartil_1 = quantile(IND4_DOWN, 0.25),
            Mediana = median(IND4_DOWN),
            Media = mean(IND4_DOWN),
            Quartil_3 = quantile(IND4_DOWN, 0.75),
            Max = max(IND4_DOWN),
            N = n())

write_csv2(dados, "dados.csv")

df %>%
  filter(Tipo == "Pequeno", Prestadora != "NEXTEL") %>%
  ggplot(aes(x = factor(Mês, labels = c("Jan","Fev","Mar","Abr","Mai","Jun")), y = IND4_UP)) +
  geom_boxplot(fill = "red") +
  xlab('Mês') +
  ylab("IND4 - Upload") +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Pequeno") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

df %>%
  filter(Tipo == "Médio", Prestadora != "NEXTEL") %>%
  ggplot(aes(x = factor(Mês, labels = c("Jan","Fev","Mar","Abr","Mai","Jun")), y = IND4_UP)) +
  geom_boxplot(fill = "red") +
  xlab('Mês') +
  ylab("IND4 - Upload") +
  scale_y_continuous(labels = scales::percent) + 
  ggtitle("Médio") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

df %>%
  filter(Tipo == "Médio-Grande", Prestadora != "NEXTEL") %>%
  ggplot(aes(x = factor(Mês, labels = c("Jan","Fev","Mar","Abr","Mai","Jun")), y = IND4_UP)) +
  geom_boxplot(fill = "red") +
  xlab('Mês') +
  ylab("IND4 - Upload") +
  ggtitle("Médio-Grande") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

df %>%
  filter(Tipo == "Grande", Prestadora != "NEXTEL") %>%
  ggplot(aes(x = factor(Mês, labels = c("Jan","Fev","Mar","Abr","Mai","Jun")), y = IND4_UP)) +
  geom_boxplot(fill = "red") +
  xlab('Mês') +
  ylab("IND4 - Upload") +
  scale_y_continuous(labels = scales::percent) + 
  ggtitle("Grande") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

dados2 <- 
  df %>%
  group_by(Tipo,Mes) %>%
  summarize(Min = min(IND4_UP),
            Quartil_1 = quantile(IND4_UP, 0.25),
            Mediana = median(IND4_UP),
            Media = mean(IND4_UP),
            Quartil_3 = quantile(IND4_UP, 0.75),
            Max = max(IND4_UP),
            N = n())

write_csv2(dados2, "dados2.csv")

dados3 <- 
  df %>%
  filter(Coletores < 20000, Prestadora != "NEXTEL") %>%
  group_by(Tipo,Mês) %>%
  summarize(Min = min(Coletores),
            Percentil_5 = quantile(Coletores, 0.05),
            Quartil_1 = quantile(Coletores, 0.25),
            Mediana = median(Coletores),
            Media = mean(Coletores),
            Quartil_3 = quantile(Coletores, 0.75),
            Percentil_95 = quantile(Coletores, 0.95),
            Max = max(Coletores),
            N = n())


write_csv2(dados3, "dados3.csv")

df %>%
  filter(Tipo == "Pequeno", Coletores < 50000, Prestadora != "NEXTEL") %>%
  ggplot(aes(x = factor(Mês, labels = c("Jan","Fev","Mar","Abr","Mai","Jun")), y = Coletores)) +
  geom_boxplot(fill = "red") +
  xlab('Mês') +
  ylab("Coletores") +
  ggtitle("Pequeno") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

df %>%
  filter(Tipo == "Pequeno", Coletores < 300, Mês == 6, Prestadora != "NEXTEL") %>%
  ggplot(aes(Coletores)) +
  geom_histogram(fill = "red", binwidth = 10) +
  ggtitle("Pequeno") + 
  ylab("Municípios") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))         

df %>%
  filter(Tipo == "Médio", Coletores < 300, Mês == 6, Prestadora != "NEXTEL") %>%
  ggplot(aes(Coletores)) +
  geom_histogram(fill = "red", binwidth = 10) +
  ggtitle("Médio") + 
  ylab("Municípios") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))   

df %>%
  filter(Tipo == "Médio-Grande", Coletores < 600, Mês == 6, Prestadora != "NEXTEL") %>%
  ggplot(aes(Coletores)) +
  geom_histogram(fill = "red", binwidth = 10) +
  ggtitle("Médio-Grande") + 
  ylab("Municípios") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 

df %>%
  filter(Tipo == "Grande", Coletores < 1000, Mês == 6, Prestadora != "NEXTEL") %>%
  ggplot(aes(Coletores)) +
  geom_histogram(fill = "red", binwidth = 10) +
  ggtitle("Grande") + 
  ylab("Municípios") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 

df$Medidas_Media <- df$Total_Medidas / df$Coletores

mean(df$Medidas_Media)
median(df$Medidas_Media)
max(df$Medidas_Media)

df %>%
  filter(Coletores < 50000, Prestadora != "NEXTEL") %>%
  ggplot(aes(x = factor(Mês, labels = c("Jan","Fev","Mar","Abr","Mai","Jun")), y = Medidas_Media)) +
  geom_boxplot(fill = "red") +
  xlab('Mês') +
  ylab("Qtde de Medidas") +
  ggtitle("M?dia de Medidas por Coletor") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

### NOVAS ANALISES ###

df <- read_csv("SMP10_11.csv")
glimpse(df)  

df$Ind4 <- df$`Medi??o_Meta(SMP10)` / df$Medidas

df %>%
  filter(Coletores < 50000, Prestadora != "NEXTEL") %>%
  summarize(quartil_1 = quantile(Medidas, 0.25),
            mediana = median(Medidas),
            quartil_3 = quantile(Medidas, 0.75))


df %>%
  filter(Medidas >= 67, Coletores < 50000, Prestadora != "NEXTEL", M?s == 6) %>% 
  ggplot(aes(Ind4)) +
  geom_histogram(fill = "blue") +
  scale_x_continuous(labels = scales::percent) + 
  xlab("Indicador") + 
  ylab("Frequ?ncia") + 
  theme_minimal()

df %>%
  filter(Medidas >= 67, Coletores < 50000, Prestadora != "NEXTEL") %>%
  ggplot(aes(factor(M?s, labels = c("Jan","Fev","Mar","Abr","Mai","Jun")), y = Ind4)) +
  geom_boxplot(fill = "red") +
  scale_y_continuous(labels = scales::percent) + 
  xlab("") + 
  ylab("Indicador") + 
  theme_minimal()

df %>%
  filter(Medidas >= 67,Coletores < 50000, Prestadora != "NEXTEL") %>%
  summarise(quartil_1 = quantile(Ind4, 0.25),
            mediana = median(Ind4),
            quartil_3 = quantile(Ind4, 0.75))

  

