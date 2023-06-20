#################
### DIRETORIO ###
#################

setwd("/Users/alexvlima/Library/Mobile Documents/com~apple~CloudDocs/ABR_Telecom_ESAQ/Grupo3/")

###################
### BIBLIOTECAS ###
###################

library(tidyverse)

################
### DATASETS ###
################

# rm(list = ls())
# list.files()

df_claro <- read.delim("CLARO.TXT", sep = "\t", 
                       stringsAsFactors = FALSE)

df_vivo <- read.delim("VIVO.TXT", sep = "\t", 
                       stringsAsFactors = FALSE)

df_tim <- read.delim("TIM.TXT", sep = "\t", 
                      stringsAsFactors = FALSE)

df_oi <- read.delim("OI.TXT", sep = "\t", 
                      stringsAsFactors = FALSE)

df_oimovel <- read.delim("OI_MOVEL.TXT", sep = "\t", 
                      stringsAsFactors = FALSE)

df_sky <- read.delim("SKY.TXT", sep = "\t", 
                      stringsAsFactors = FALSE)

###############
### AJUSTES ###
###############

df <- 
  df_claro %>% 
  rbind(df_oi) %>% 
  rbind(df_oimovel) %>% 
  rbind(df_sky) %>% 
  rbind(df_tim) %>%
  rbind(df_vivo)

glimpse(df)

###############
### ANALISE ###
###############



df$Down <- (as.numeric(gsub(x = df$X.Down, pattern = ",", replacement = ".")))

df2 <- dplyr::select(.data = df, 
                     Cod_IBGE = Codigo.IBGE, Prestadora, Down, Tecnologia)
glimpse(df2)

df2$Cod_IBGE <- as.numeric(df2$Cod_IBGE)
df2$Prestadora <- as.factor(df2$Prestadora)

df2 %>%
  filter(is.na(Cod_IBGE) == FALSE, Tecnologia != "Tecnologia") %>%
  group_by(Cod_IBGE, Prestadora) %>%
  summarise(Media_Down = mean(Down, na.rm = T)) %>%
  group_by(Prestadora) %>%
  summarise(Quartil_1 = quantile(Media_Down, probs = 0.25),
            Quartil_3 = quantile(Media_Down, probs = 0.75)) %>%
  ggplot(aes(x = Prestadora, y = Quartil_3, label = Prestadora)) +
  geom_segment(aes(y = Quartil_1, x = Prestadora, 
                   yend = Quartil_3, xend = Prestadora),
               color="#a3c4dc", size = 1.5) + 
  scale_y_continuous(label=scales::percent) +
  labs(x=NULL, 
       y=NULL, 
       title="Atingimento Download por Prestadora (DVR - SCM)", 
       subtitle="Intervalo Interquartil do Indicador", 
       caption="Fonte: ESAQ") +
  coord_flip() + 
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        plot.background=element_rect(fill="#f7f7f7"),
        panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(),
        axis.ticks=element_blank(),
        panel.border=element_blank()) 

df2 %>%
  filter(is.na(Cod_IBGE) == FALSE, Tecnologia != "Tecnologia") %>%
  group_by(Cod_IBGE, Prestadora) %>%
  summarise(Media_Down = mean(Down, na.rm = T)) %>%
  group_by(Prestadora) %>%
  summarise(Percentil_5 = quantile(Media_Down, probs = 0.05),
            Percentil_95 = quantile(Media_Down, probs = 0.95)) %>%
  ggplot(aes(x = Prestadora, y = Quartil_3, label = Prestadora)) +
  geom_segment(aes(y = Percentil_5, x = Prestadora, 
                   yend = Percentil_95, xend = Prestadora),
               color="#a3c4dc", size = 1.5) + 
  scale_y_continuous(label=scales::percent) +
  labs(x=NULL, 
       y=NULL, 
       title="Atingimento Download por Prestadora (DVR - SCM)", 
       subtitle="Intervalo Percentil 5% - 95% do Indicador", 
       caption="Fonte: ESAQ") +
  coord_flip() + 
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        plot.background=element_rect(fill="#f7f7f7"),
        panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(),
        axis.ticks=element_blank(),
        panel.border=element_blank())

# POR TECNOLOGIA #

# unique(df$Tecnologia)

df2 %>%
  filter(is.na(Cod_IBGE) == FALSE, Tecnologia != "Tecnologia") %>%
  group_by(Cod_IBGE, Prestadora, Tecnologia) %>%
  summarise(Media_Down = mean(Down, na.rm = T)) %>%
  group_by(Prestadora, Tecnologia) %>%
  summarise(Quartil_1 = quantile(Media_Down, probs = 0.25),
            Quartil_3 = quantile(Media_Down, probs = 0.75)) %>%
  ggplot(aes(x = Prestadora, y = Quartil_3, label = Prestadora)) +
  geom_segment(aes(y = Quartil_1, x = Prestadora, 
                   yend = Quartil_3, xend = Prestadora),
               color="#a3c4dc", size = 1.5) + 
  scale_y_continuous(label=scales::percent) +
  labs(x=NULL, 
       y=NULL, 
       title="Atingimento Download por Prestadora (DVR - SCM)", 
       subtitle="Intervalo Interquartil do Indicador", 
       caption="Fonte: ESAQ") +
  coord_flip() + 
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        plot.background=element_rect(fill="#f7f7f7"),
        panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(),
        axis.ticks=element_blank(),
        panel.border=element_blank()) +
  facet_wrap(~factor(Tecnologia))

df2 %>%
  filter(is.na(Cod_IBGE) == FALSE, Tecnologia != "Tecnologia") %>%
  group_by(Cod_IBGE, Prestadora, Tecnologia) %>%
  summarise(Media_Down = mean(Down, na.rm = T)) %>%
  group_by(Prestadora, Tecnologia) %>%
  summarise(Percentil_5 = quantile(Media_Down, probs = 0.05),
            Percentil_95 = quantile(Media_Down, probs = 0.95)) %>%
  ggplot(aes(x = Prestadora, y = Quartil_3, label = Prestadora)) +
  geom_segment(aes(y = Percentil_5, x = Prestadora, 
                   yend = Percentil_95, xend = Prestadora),
               color="#a3c4dc", size = 1.5) + 
  scale_y_continuous(label=scales::percent) +
  labs(x=NULL, 
       y=NULL, 
       title="Atingimento Download por Prestadora (DVR - SCM)", 
       subtitle="Intervalo Percentil 5% - 95% do Indicador", 
       caption="Fonte: ESAQ") +
  coord_flip() + 
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        plot.background=element_rect(fill="#f7f7f7"),
        panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(),
        axis.ticks=element_blank(),
        panel.border=element_blank()) +
  facet_wrap(~factor(Tecnologia))
