#################
### DIRETORIO ###
#################

getwd()
setwd("/Users/alexvlima/Library/Mobile Documents/com~apple~CloudDocs/ABR_Telecom_ESAQ/Grupo2/")

###################
### BIBLIOTECAS ###
###################

library(tidyverse)
library(lubridate)

################
### ANALISES ###
################

list.files()

df_claro <- read_csv2("./SMP/SMP_Down_Up_2020-06_CLARO.csv.gz")
df_vivo <- read_csv2("./SMP/SMP_Down_Up_2020-06_VIVO.csv.gz")
df_tim <- read_csv2("./SMP/SMP_Down_Up_2020-06_TIM.csv.gz")
df_oi <- read_csv2("./SMP/SMP_Down_Up_2020-06_OI.csv.gz")

glimpse(df_claro)
glimpse(df_vivo)
glimpse(df_tim)
glimpse(df_oi)

df_claro$Prestadora <- "A"
df_vivo$Prestadora <- "B"
df_tim$Prestadora <- "C"
df_oi$Prestadora <- "D"

df_claro2 <- 
  df_claro %>%
  filter(is.na(location) == FALSE, is.na(`Tipo de Descarte`) == TRUE) %>%
  group_by(COD_IBGE = `Codigo IBGE`, DIA = lubridate::day(dateTime), Prestadora) %>%
  summarise(n = n())

df_vivo2 <- 
  df_vivo %>%
  filter(is.na(location) == FALSE, is.na(`Tipo de Descarte`) == TRUE) %>%
  group_by(COD_IBGE = `Codigo IBGE`, DIA = lubridate::day(dateTime), Prestadora) %>%
  summarise(n = n())

df_tim2 <- 
  df_tim %>%
  filter(is.na(location) == FALSE, is.na(`Tipo de Descarte`) == TRUE) %>%
  group_by(COD_IBGE = `Codigo IBGE`, DIA = lubridate::day(dateTime), Prestadora) %>%
  summarise(n = n())

df_oi2 <- 
  df_oi %>%
  filter(is.na(location) == FALSE, is.na(`Tipo de Descarte`) == TRUE) %>%
  group_by(COD_IBGE = `Codigo IBGE`, DIA = lubridate::day(dateTime), Prestadora) %>%
  summarise(n = n())

df <- bind_rows(df_claro2, df_vivo2, df_tim2, df_oi2)
glimpse(df)

table(df$Prestadora)

dp <- df %>%
  group_by(Prestadora) %>%
  summarise(dp = sd(n))
glimpse(dp)  

df %>%
  group_by(Prestadora = factor(Prestadora, 
                               levels = c("A","B","C","D"),
                               labels = c("Prestadora A", "Prestadora B", "Prestadora C", "Prestadora D")), Dia = DIA) %>%
  summarise(media = mean(n)) %>%
  ggplot(aes(x = Dia, y = media)) + 
  geom_line() +
  geom_ribbon(aes(ymin = -10 + media, ymax = 10 + media),
              fill = "grey", alpha = 0.4) + 
  ylab("Quantidade de Medições") + 
  facet_wrap(~Prestadora) + 
  theme_minimal()

  

### CLARO ###

df_claro <- read_csv2("./SMP/SMP_Down_Up_2020-06_CLARO.csv.gz")
glimpse(df_claro)

# sum(is.na(df_claro$location))
# table(df_claro$Tipo.de.Descarte)

qtde_municipios <- 
  df_claro %>%
  filter(is.na(location) == FALSE, is.na(`Tipo de Descarte`) == TRUE) %>%
  group_by(COD_IBGE = `Codigo IBGE`, DIA = lubridate::day(dateTime)) %>%
  summarize(QTDE = n())

write_csv2(qtde_municipios, "distribuicao_medidas_claro.csv")

rm(df_claro, qtde_municipios)

### VIVO ###

df_vivo <- read_csv2("./SMP/SMP_Down_Up_2020-06_VIVO.csv.gz")
glimpse(df_vivo)

qtde_municipios <- 
  df_vivo %>%
  filter(is.na(location) == FALSE, is.na(`Tipo de Descarte`) == TRUE) %>%
  group_by(COD_IBGE = `Codigo IBGE`, DIA = lubridate::day(dateTime)) %>%
  summarize(QTDE = n())

write_csv2(qtde_municipios, "distribuicao_medidas_vivo.csv")

rm(df_vivo, qtde_municipios)

### TIM ###

df_tim <- read_csv2("./SMP/SMP_Down_Up_2020-06_TIM.csv.gz")
glimpse(df_tim)

qtde_municipios <- 
  df_tim %>%
  filter(is.na(location) == FALSE, is.na(`Tipo de Descarte`) == TRUE) %>%
  group_by(COD_IBGE = `Codigo IBGE`, DIA = lubridate::day(dateTime)) %>%
  summarize(QTDE = n())

write_csv2(qtde_municipios, "distribuicao_medidas_tim.csv")

rm(df_tim, qtde_municipios)

### OI ###

df_oi <- read_csv2("./SMP/SMP_Down_Up_2020-06_OI.csv.gz")
glimpse(df_oi)

qtde_municipios <- 
  df_oi %>%
  filter(is.na(location) == FALSE, is.na(`Tipo de Descarte`) == TRUE) %>%
  group_by(COD_IBGE = `Codigo IBGE`, DIA = lubridate::day(dateTime)) %>%
  summarize(QTDE = n())

write_csv2(qtde_municipios, "distribuicao_medidas_oi.csv")

rm(df_oi, qtde_municipios)
