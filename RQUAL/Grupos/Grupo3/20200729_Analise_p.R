getwd()
setwd("/Users/alexvlima/Desktop/Desktop - Alexandre???s MacBook Pro/ABR_Telecom_ESAQ/Grupo3/")

library(tidyverse)
library(readxl)

rm(list = ls())

df <- read_excel("p - Jan a jun 2020.csv.xlsx")
glimpse(df)

df %>%
  filter(Tipo == "Pequeno") %>%
  ggplot(aes(x = factor(Mes, labels = c("Jan","Fev","Mar","Abr","Mai","Jun")), y = IND4_DOWN)) +
  geom_boxplot(fill = "red") +
  xlab('Mês') +
  ylab("IND4 - Download") +
  scale_y_continuous(labels = scales::percent) + 
  ggtitle("Pequeno") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

df %>%
  filter(Tipo == "Médio") %>%
  ggplot(aes(x = factor(Mes, labels = c("Jan","Fev","Mar","Abr","Mai","Jun")), y = IND4_DOWN)) +
  geom_boxplot(fill = "red") +
  xlab('Mês') +
  ylab("IND4 - Download") +
  scale_y_continuous(labels = scales::percent) + 
  ggtitle("Médio") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

df %>%
  filter(Tipo == "Médio-Grande") %>%
  ggplot(aes(x = factor(Mes, labels = c("Jan","Fev","Mar","Abr","Mai","Jun")), y = IND4_DOWN)) +
  geom_boxplot(fill = "red") +
  xlab('Mês') +
  ylab("IND4 - Download") +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Médio-Grande") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

df %>%
  filter(Tipo == "Grande") %>%
  ggplot(aes(x = factor(Mes, labels = c("Jan","Fev","Mar","Abr","Mai","Jun")), y = IND4_DOWN)) +
  geom_boxplot(fill = "red") +
  xlab('Mês') +
  ylab("IND4 - Download") +
  scale_y_continuous(labels = scales::percent) + 
  ggtitle("Grande") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

dados <- 
  df %>%
  group_by(Tipo,Mes) %>%
  summarize(Min = min(IND4_DOWN),
            Quartil_1 = quantile(IND4_DOWN, 0.25),
            Mediana = median(IND4_DOWN),
            Media = mean(IND4_DOWN),
            Quartil_3 = quantile(IND4_DOWN, 0.75),
            Max = max(IND4_DOWN),
            N = n())

write_csv2(dados, "dados.csv")

df %>%
  filter(Tipo == "Pequeno") %>%
  ggplot(aes(x = factor(Mes, labels = c("Jan","Fev","Mar","Abr","Mai","Jun")), y = IND4_UP)) +
  geom_boxplot(fill = "red") +
  xlab('Mês') +
  ylab("IND4 - Upload") +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Pequeno") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

df %>%
  filter(Tipo == "Médio") %>%
  ggplot(aes(x = factor(Mes, labels = c("Jan","Fev","Mar","Abr","Mai","Jun")), y = IND4_UP)) +
  geom_boxplot(fill = "red") +
  xlab('Mês') +
  ylab("IND4 - Upload") +
  scale_y_continuous(labels = scales::percent) + 
  ggtitle("Médio") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

df %>%
  filter(Tipo == "Médio-Grande") %>%
  ggplot(aes(x = factor(Mes, labels = c("Jan","Fev","Mar","Abr","Mai","Jun")), y = IND4_UP)) +
  geom_boxplot(fill = "red") +
  xlab('Mês') +
  ylab("IND4 - Upload") +
  ggtitle("Médio-Grande") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

df %>%
  filter(Tipo == "Grande") %>%
  ggplot(aes(x = factor(Mes, labels = c("Jan","Fev","Mar","Abr","Mai","Jun")), y = IND4_UP)) +
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


df <- read_excel("p - Jan a jun 2020v2.xlsx")
glimpse(df)

dados3 <- 
  df %>%
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

write_csv2(dados3, "dados3.csv")


