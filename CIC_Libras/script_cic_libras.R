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
library(queueing)

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

df$Inicio_Acesso <- dmy_hm(df$Início.do.acesso)
df$Inicio_Fila <- dmy_hm(df$Início.Fila)
df$Ocupacao <- hms(df$Tempo.de.ocupação)  
df$Atendimento <- hms(df$Tempo.de.atendimento)
df$Fila <- hms(df$Tempo.de.espera)

df_tempos <- 
  df %>% 
  select(Inicio_Acesso, Inicio_Fila, Ocupacao, Atendimento, Fila)

df_tempos$Horario_Acesso <- 
  cut(x = hour(df_tempos$Inicio_Acesso), 
      breaks = c(0,4,8,12,16,20,24))

df_tempos$Horario_Acesso2 <- 
  cut(x = hour(df_tempos$Inicio_Acesso), 
      breaks = c(0,6,12,18,24))

df_tempos <- df_tempos %>% filter(Fila >= 0)

# rm(df)

################
### ANALISES ###
################

# DESCRITIVA #

summary(df_tempos)

# df_tempos %>%
#   filter(is.na(Horario_Acesso) == FALSE) %>%
#   group_by(Horario_Acesso) %>%
#   summarise(media = mean(as.numeric(Atendimento), na.rm = T),
#             maximo = quantile(as.numeric(Atendimento), probs = 0.95, na.rm = T)) %>%
#   mutate(estresse = maximo / media)
# 
# df_tempos %>%
#   filter(is.na(Horario_Acesso2) == FALSE) %>%
#   group_by(Horario_Acesso2) %>%
#   summarise(media = mean(as.numeric(Atendimento), na.rm = T),
#             maximo = quantile(as.numeric(Atendimento), probs = 0.95, na.rm = T)) %>%
#   mutate(estresse = maximo / media)

df_tempos %>%
  filter(is.na(Horario_Acesso) == FALSE) %>%
  group_by(Horario_Acesso) %>%
  summarise(media = mean(as.numeric(Fila), na.rm = T),
            perc_95 = quantile(as.numeric(Fila), probs = 0.95, na.rm = T),
            perc_99 = quantile(as.numeric(Fila), probs = 0.99, na.rm = T),
            maximo = max(as.numeric(Fila), na.rm = T)) %>%
  mutate(ind_estresse1 = perc_95 / media, 
         ind_estresse2 = perc_99 / media, 
         ind_estresse3 = maximo / media) 

df_tempos %>%
  filter(is.na(Horario_Acesso2) == FALSE) %>%
  group_by(Horario_Acesso2) %>%
  summarise(media = mean(as.numeric(Fila), na.rm = T),
            perc_95 = quantile(as.numeric(Fila), probs = 0.95, na.rm = T),
            perc_99 = quantile(as.numeric(Fila), probs = 0.99, na.rm = T),
            maximo = max(as.numeric(Fila), na.rm = T)) %>%
  mutate(ind_estresse1 = perc_95 / media, 
         ind_estresse2 = perc_99 / media, 
         ind_estresse3 = maximo / media)

# df_tempos %>%
#   filter(is.na(Horario_Acesso) == FALSE) %>%
#   group_by(day(Inicio_Acesso),month(Inicio_Acesso),Horario_Acesso) %>%
#   summarise(media = mean(as.numeric(Fila), na.rm = T),
#             maximo = quantile(as.numeric(Fila), probs = 0.95, na.rm = T)) %>%
#   mutate(estresse = maximo / media) %>%
#   arrange(desc(estresse))
# 
# df_tempos %>%
#   filter(is.na(Horario_Acesso2) == FALSE) %>%
#   group_by(day(Inicio_Acesso),month(Inicio_Acesso),Horario_Acesso2) %>%
#   summarise(media = mean(as.numeric(Fila), na.rm = T),
#             maximo = quantile(as.numeric(Fila), probs = 0.95, na.rm = T)) %>%
#   mutate(estresse = maximo / media) %>%
#   arrange(desc(estresse))

# Graficos #

df_tempos %>% 
  filter(is.na(Horario_Acesso) == FALSE, Ocupacao >= 0) %>%
  ggplot(aes(Ocupacao)) +
  geom_density(color = "steelblue", size = 1.5) +
  xlab("Ocupação (min)") + 
  ylab("Densidade") + 
  theme_light() + 
  facet_wrap(~Horario_Acesso)
  
df_tempos %>% 
  filter(is.na(Horario_Acesso) == FALSE, Fila >= 0) %>%
  ggplot(aes(Fila)) +
  geom_density(color = "steelblue", size = 1.5) +
  xlab("Tempo na Fila (min)") + 
  ylab("Densidade") + 
  theme_light() + 
  facet_wrap(~Horario_Acesso)

quantile(df_tempos$Fila, probs = 0.67, na.rm = T)

# GRAFICOS CRUZADOS #

# 4/4 Horas #
base_modelo <-
  df_tempos %>%
  filter(is.na(Horario_Acesso) == FALSE) %>%
  group_by(Horario_Acesso, Dia_Semana = weekdays(Inicio_Acesso)) %>%
  summarise(Pessoas = n(),
            Tempo_Total = as.numeric(difftime(max(Inicio_Acesso), 
                                   min(Inicio_Acesso),
                                   units = "hours")),
            Tempo_Atendimento = mean(as.numeric(Atendimento), na.rm = T)/3600,
            Tempo_Atendimento_Mediano = median(as.numeric(Atendimento), na.rm = T)/3600,
            Tempo_Fila = mean(as.numeric(Fila), na.rm = T)/3600,
            Tempo_Fila_Max = max(as.numeric(Fila), na.rm = T)/3600,
            DP_Fila = sd(as.numeric(Fila), na.rm = T)/3600,
            Tempo_Ocupacao = mean(as.numeric(Ocupacao), na.rm = T)/3600) %>%
  arrange(Dia_Semana, Horario_Acesso) %>%
  mutate(Taxa_Chegada = (Pessoas/Tempo_Total) , 
         Taxa_Servico = 1 / Tempo_Atendimento_Mediano,
         Ind_estresse = Tempo_Fila_Max / Tempo_Fila)

# 6/6 Horas #
base_modelo2 <-
  df_tempos %>%
  filter(is.na(Horario_Acesso) == FALSE) %>%
  group_by(Horario_Acesso = Horario_Acesso2, Dia_Semana = weekdays(Inicio_Acesso)) %>%
  summarise(Pessoas = n(),
            Tempo_Total = as.numeric(difftime(max(Inicio_Acesso), 
                                              min(Inicio_Acesso),
                                              units = "hours")),
            Tempo_Atendimento = mean(as.numeric(Atendimento), na.rm = T)/3600,
            Tempo_Atendimento_Mediano = median(as.numeric(Atendimento), na.rm = T)/3600,
            Tempo_Fila = mean(as.numeric(Fila), na.rm = T)/3600,
            Tempo_Fila_Max = max(as.numeric(Fila), na.rm = T)/3600,
            DP_Fila = sd(as.numeric(Fila), na.rm = T)/3600,
            Tempo_Ocupacao = mean(as.numeric(Ocupacao), na.rm = T)/3600) %>%
  arrange(Dia_Semana, Horario_Acesso) %>%
  mutate(Taxa_Chegada = (Pessoas/Tempo_Total) , 
         Taxa_Servico = 1 / Tempo_Atendimento_Mediano,
         Ind_estresse = Tempo_Fila_Max / Tempo_Fila)

base_modelo$Dia_Semana <- 
  factor(x = base_modelo$Dia_Semana, 
         levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
         labels = c("Segunda","Terça", "Quarta", "Quinta", "Sexta", "Sábado", "Domingo"))

base_modelo2$Dia_Semana <- 
  factor(x = base_modelo2$Dia_Semana, 
         levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
         labels = c("Segunda","Terça", "Quarta", "Quinta", "Sexta", "Sábado", "Domingo"))

base_modelo %>%
  ggplot(aes(x = Dia_Semana, y = Tempo_Fila*60)) + 
  geom_col(fill = "steelblue") + 
  geom_text(aes(label = round(Tempo_Fila*60, digits = 0)), 
            vjust=-0.25, size = 3) + 
  xlab("") + 
  ylab("Minutos") + 
  theme_light() + 
  facet_wrap(~Horario_Acesso, scales = "free_x") +
  theme(strip.text = element_text(face="bold", size=14))

base_modelo2 %>%
  ggplot(aes(x = Dia_Semana, y = Tempo_Fila*60)) + 
  geom_col(fill = "steelblue") + 
  geom_text(aes(label = round(Tempo_Fila*60, digits = 0)), 
            vjust=-0.25, size = 3) + 
  xlab("") + 
  ylab("Minutos") + 
  theme_light() + 
  facet_wrap(~Horario_Acesso, scales = "free_x") +
  theme(strip.text = element_text(face="bold", size=14))

base_modelo %>%
  ggplot(aes(x = Dia_Semana, y = Tempo_Atendimento*60)) + 
  geom_col(fill = "steelblue") + 
  geom_text(aes(label = round(Tempo_Atendimento*60, digits = 0)), 
            vjust=-0.25, size = 3) + 
  xlab("") + 
  ylab("Minutos") + 
  theme_light() + 
  facet_wrap(~Horario_Acesso, scales = "free_x") +
  theme(strip.text = element_text(face="bold", size=14))

base_modelo2 %>%
  ggplot(aes(x = Dia_Semana, y = Tempo_Atendimento*60)) + 
  geom_col(fill = "steelblue") + 
  geom_text(aes(label = round(Tempo_Atendimento*60, digits = 0)), 
            vjust=-0.25, size = 3) + 
  xlab("") + 
  ylab("Minutos") + 
  theme_light() + 
  facet_wrap(~Horario_Acesso, scales = "free_x") +
  theme(strip.text = element_text(face="bold", size=14))

base_modelo %>%
  mutate(Taxa_Chegada = Taxa_Chegada) %>%
  ggplot(aes(x = Dia_Semana, y = Taxa_Chegada)) + 
  geom_col(fill = "steelblue") + 
  geom_text(aes(label = round(Taxa_Chegada, digits = 1)), 
            vjust=-0.25, size = 3) + 
  xlab("") + 
  ylab("Acessos / Hora") + 
  theme_light() + 
  facet_wrap(~Horario_Acesso, scales = "free_x") +
  theme(strip.text = element_text(face="bold", size=14))

  base_modelo2 %>%
  mutate(Taxa_Chegada = Taxa_Chegada) %>%
  ggplot(aes(x = Dia_Semana, y = Taxa_Chegada)) + 
  geom_col(fill = "steelblue") + 
  geom_text(aes(label = round(Taxa_Chegada, digits = 1)), 
            vjust=-0.25, size = 3) + 
  xlab("") + 
  ylab("Acessos / Hora") + 
  theme_light() + 
  facet_wrap(~Horario_Acesso, scales = "free_x") +
  theme(strip.text = element_text(face="bold", size=14))

base_modelo %>%
  ggplot(aes(x = Horario_Acesso, y = Pessoas)) + 
  geom_col(fill = "steelblue") +
  geom_text(aes(label = round(Pessoas, digits = 0)), 
            vjust= 0, size = 2.5) + 
  theme_light() +
  facet_wrap(~Dia_Semana, scales = "free_x") +
  theme(strip.text = element_text(face="bold", size=12))

base_modelo2 %>%
  ggplot(aes(x = Horario_Acesso, y = Pessoas)) + 
  geom_col(fill = "steelblue") +
  geom_text(aes(label = round(Pessoas, digits = 0)), 
            vjust= 0, size = 2.5) + 
  theme_light() +
  facet_wrap(~Dia_Semana, scales = "free_x") +
  theme(strip.text = element_text(face="bold", size=12))

# TEORIA DE FILAS #

calculatewq <- 
  function(c) { 
    P0inv <- Rho^c / factorial(c)*(1-(Rho/c))
    for (i in 1:c-1) {
      P0inv = P0inv + (Rho^i)/factorial(i)
    }
    P0 = 1/P0inv
    Lq = (Rho^(c+1))*P0/(factorial(c-1)*(c-Rho)^2)
    Wq = 60*Lq/Lambda
    Ls <- Lq + Rho
    Ws <- 60*Ls/Lambda
    a <- cbind(Lq,Wq,Ls,Ws)
    return(a)
  }

# RESULTADO 4/4 HORAS #

Resultados <- data.frame(Dia = character(0),
                         Horario = character(0),
                         Atendentens = numeric(0),
                         Tempo_Medio_Fila = numeric(0), 
                         Tempo_99_Fila = numeric(0))

for(j in 1:13){
  
  Atendentes <- j
  
  for(i in 1:nrow(base_modelo)){
    Lambda <-  6*base_modelo$Taxa_Chegada[i]
    Mue <- base_modelo$Taxa_Servico[i]
    
    Rho <- Lambda / Mue
    
    
    temp <- data.frame(as.character(base_modelo$Dia_Semana[i]), 
                         as.character(base_modelo$Horario_Acesso[i]), 
                         Atendentes, 
                         Tempo_Medio_Fila = calculatewq(Atendentes)[,2] * 60, 
                         Tempo_99_Fila = calculatewq(Atendentes)[,2] * 60 * base_modelo$Ind_estresse[i])
      
    Resultados <- rbind(Resultados, temp)
    rm(temp, Taxa_Chegada, Taxa_Servico)
  }
}

rm(Lambda, Mue, Rho, i, j, Atendentes)

colnames(Resultados) <- c("Dia_Semana","Horario","Atendentes","Tempo_Medio_Fila","Tempo_Fila_99P")
glimpse(Resultados)

for(i in 1:7) { 

  grafico1 <- 
    Resultados %>%
    filter(Dia_Semana == as.character(unique(Resultados$Dia_Semana))[i]) %>%
    ggplot(aes(x = Atendentes, y = Tempo_Medio_Fila)) +
    geom_line(size = 1, color = "darkblue") +
    geom_hline(yintercept = 5, color = "red", linetype = "dashed") + 
    scale_x_continuous("PAs", breaks = seq(1:13)) +
    scale_y_continuous("Tempo Fila", limits = c(0,100)) +
    ggtitle(paste0("Gráfico da Fila de ", 
                   as.character(unique(Resultados$Dia_Semana))[i], 
                   " por Quantidade de PAs")) +
    theme_light() +
    theme(plot.title = element_text(hjust = 0.5)) +
    facet_wrap(~Horario, scales = "free_x")
  
  grafico2 <- 
    Resultados %>%
    filter(Dia_Semana == as.character(unique(Resultados$Dia_Semana))[i]) %>%
    ggplot(aes(x = Atendentes, y = Tempo_Fila_99P)) +
    geom_line(size = 1, color = "darkblue") +
    geom_hline(yintercept = 5, color = "red", linetype = "dashed") + 
    scale_x_continuous("PAs", breaks = seq(1:13)) +
    scale_y_continuous("Tempo Fila", limits = c(0,100)) +
    ggtitle(paste0("Gráfico da Fila de ", 
                   as.character(unique(Resultados$Dia_Semana))[i], 
                   " por Quantidade de PAs")) +
    theme_light() +
    theme(plot.title = element_text(hjust = 0.5)) +
    facet_wrap(~Horario, scales = "free_x")
  
  print(grafico1)
  print(grafico2)
  
}

rm(i, grafico1, grafico2, Resultados)

# RESULTADO 6/6 HORAS #

Resultados2 <- data.frame(Dia = character(0),
                         Horario = character(0),
                         Atendentens = numeric(0),
                         Tempo_Medio_Fila = numeric(0), 
                         Tempo_99_Fila = numeric(0))

for(j in 1:13){
  
  Atendentes <- j
  
  for(i in 1:nrow(base_modelo)){
    Lambda <-  4 * base_modelo2$Taxa_Chegada[i]
    Mue <- base_modelo2$Taxa_Servico[i]
    
    Rho <- Lambda / Mue
    
    
    temp <- data.frame(as.character(base_modelo2$Dia_Semana[i]), 
                       as.character(base_modelo2$Horario_Acesso[i]), 
                       Atendentes, 
                       Tempo_Medio_Fila = calculatewq(Atendentes)[,2] * 60, 
                       Tempo_99_Fila = calculatewq(Atendentes)[,2] * 60 * base_modelo2$Ind_estresse[i])
    
    Resultados2 <- rbind(Resultados2, temp)
    rm(temp, Taxa_Chegada, Taxa_Servico)
  }
}

rm(Lambda, Mue, Rho, i, j, Atendentes, calculatewq)

colnames(Resultados2) <- c("Dia_Semana","Horario","Atendentes","Tempo_Medio_Fila","Tempo_Fila_99P")
glimpse(Resultados2)

for(i in 1:7) { 
  
  grafico1 <- 
    Resultados2 %>%
    filter(Dia_Semana == as.character(unique(Resultados2$Dia_Semana))[i]) %>%
    ggplot(aes(x = Atendentes, y = Tempo_Medio_Fila)) +
    geom_line(size = 1, color = "darkblue") +
    geom_hline(yintercept = 2, color = "red", linetype = "dashed") + 
    scale_x_continuous("PAs", breaks = seq(1:13)) +
    scale_y_continuous("Tempo Fila", limits = c(0,100)) +
    ggtitle(paste0("Gráfico da Fila de ", 
                   as.character(unique(Resultados2$Dia_Semana))[i], 
                   " por Quantidade de PAs")) +
    theme_light() +
    theme(plot.title = element_text(hjust = 0.5)) +
    facet_wrap(~Horario, scales = "free_x")
  
  grafico2 <- 
    Resultados2 %>%
    filter(Dia_Semana == as.character(unique(Resultados2$Dia_Semana))[i]) %>%
    ggplot(aes(x = Atendentes, y = Tempo_Fila_99P)) +
    geom_line(size = 1, color = "darkblue") +
    geom_hline(yintercept = 2, color = "red", linetype = "dashed") + 
    scale_x_continuous("PAs", breaks = seq(1:13)) +
    scale_y_continuous("Tempo Fila", limits = c(0,100)) +
    ggtitle(paste0("Gráfico da Fila de ", 
                   as.character(unique(Resultados2$Dia_Semana))[i], 
                   " por Quantidade de PAs")) +
    theme_light() +
    theme(plot.title = element_text(hjust = 0.5)) +
    facet_wrap(~Horario, scales = "free_x")
  
  print(grafico1)
  print(grafico2)
  
}

rm(i, grafico1, grafico2, Resultados2)


# Usar o histórico para parametro da simulacao de estresse 
# simular com o corte de 6 horas


