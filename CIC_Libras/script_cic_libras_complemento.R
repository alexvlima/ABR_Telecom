#################
### DIRETORIO ###
#################

# getwd()
setwd("/Users/alexvlima/Library/Mobile Documents/com~apple~CloudDocs/ABR_Telecom_ESAQ/CIC_Libras/")

###################
### BIBLIOTECAS ###
###################

library(tidyverse)
library(lubridate)
library(stringr)
library(data.table)
library(scales)

###############
### DATASET ###
###############

# rm(list = ls())

df <- read.csv2(file = "historico-chamada-2021-03-01-18-05-30_20210301.csv",
                fileEncoding = "latin1", stringsAsFactors = FALSE)
glimpse(df)

###############
### AJUSTES ###
###############

df$INICIO <- dmy_hm(df$Início.da.conferência)
df$FIM <- dmy_hm(df$Fim.da.conferência)
df$OCUPACAO <- hms(df$Tempo.de.ocupação)  
df$ATENDIMENTO <- hms(df$Tempo.de.atendimento)

df_tempo_atendente <- 
  df %>%
  filter(is.na(OCUPACAO) == FALSE) %>%
  select(INICIO, FIM, ATENDENTE = Atendente, OCUPACAO, ATENDIMENTO)
glimpse(df_tempo_atendente)

df_tempo_atendente$ANOMESDIA <- 
  paste0(year(df_tempo_atendente$INICIO), 
         str_pad(month(df_tempo_atendente$INICIO), width = 2, pad = "0"), 
         str_pad(day(df_tempo_atendente$INICIO), width = 2, pad = "0"))

################
### ANALISES ###
################

df_amostra <- 
  df_tempo_atendente %>%
  filter(ANOMESDIA == 20210131)
glimpse(df_amostra)

minutos <- 
  format(seq(as.POSIXct("2021-07-12 00:00:00", tz="America/Sao_Paulo"),
             length.out=1440, by='1 min'), '%H:%M')


atendimentos <- data.frame(QTD_ATENDIMENTOS = numeric(0))

for(i in 1:length(minutos)){

    temp <- 
      ifelse(format(df_amostra$INICIO, format = "%H:%M") <= minutos[i] & 
               format(df_amostra$FIM, format = "%H:%M") >= minutos[i],
             1, 0)
      

    atendimentos <- rbind(atendimentos, temp)

}

rm(temp, i)

final <- data.frame(TIME = minutos, 
                    QTD_ATENDIMENTOS = rowSums(atendimentos))
final$TIME <- as.POSIXct(strptime(final$TIME, format="%H:%M"), tz = "America/Sao_Paulo")
glimpse(final)

rm(minutos, atendimentos)

hora <- format(seq(as.POSIXct("2021-07-12 00:00:00", tz="America/Sao_Paulo"),
           length.out=24, by='1 hour'), '%Y-%m-%d %H:%M')

final %>%
  ggplot(aes(x = TIME, y = QTD_ATENDIMENTOS)) +
  geom_line(color = "steelblue") + 
  geom_hline(yintercept = c(2,6,13), col = "red", linetype = "dashed") +
  geom_vline(xintercept = as.numeric(as.POSIXct(hora)), col = "blue", linetype = "dotted") +
  scale_x_datetime("",
                   expand = c(0, 0),
                   limits = c(
                     as.POSIXct("2021-07-12 00:00:00 "),
                     as.POSIXct("2021-07-13 00:00:00")),
                   breaks = date_breaks("1 hour"),
                   labels = date_format("%H:%M", tz = "America/Sao_Paulo")) +
  ylab("ATENDIMENTOS") +
  ggtitle("Quantidade de Atendimentos", subtitle = "31/01/2021") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

rm(hora)

