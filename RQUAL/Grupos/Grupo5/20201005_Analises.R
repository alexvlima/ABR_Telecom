getwd()
setwd("/Users/alexvlima/Library/Mobile Documents/com~apple~CloudDocs/ABR_Telecom_ESAQ/Grupo5/")

library(readxl)
library(tidyverse)

df <- read_excel("Analise_Final_Grupo5.xlsx", sheet = "dados_2")
glimpse(df)

unique(df$Prestadora)

df %>%
  group_by(faixas = cut(df$`Quantidade total de Agendamentos no mês`, breaks = c(0,1,5,10,20,49,69,99,149,199,399,499,799,999,1999,4999,1000000000))) %>%
  filter(is.na(Município) == FALSE) %>%
  distinct(Município) %>%
  summarise(n = n())

df %>%
  group_by(faixas = cut(df$`Quantidade total de Agendamentos no mês`, breaks = c(0,1,5,10,20,49,69,99,149,199,399,499,799,999,1999,4999,1000000000))) %>%
  filter(is.na(Município) == FALSE) %>%
  summarise(volatidade = sd(Indicador),
            media = mean(Indicador))

df2 <- read_excel("Analise_Final_Grupo5.xlsx", sheet = "Folha4")
glimpse(df2)

df2 %>%
  group_by(faixas = cut(df2$`Quantidade total de Agendamentos no mês`, breaks = c(0,49,99,199,499,999,1999,1000000000))) %>%
  filter(is.na(Município) == FALSE, lubridate::month(Mes) == 3) %>%
  group_by(Prestadora, faixas) %>%
  distinct(Município) %>%
  summarise(n = n())  %>%
  print(n = 30)

