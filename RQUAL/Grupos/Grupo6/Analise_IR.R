###################
### BIBLIOTECAS ###
###################

library(dplyr)
library(ggplot2)
library(readxl)
library(brazilmaps)
library(sf)
library(geobr)
library(RColorBrewer)
library(ggrepel)

###############
### DATASET ###
###############

df_IR <- read_xlsx("IR_Anatel.xlsx", sheet = "Consolidado")
head(df_IR)

df_IR_Resumo <- df_IR %>%
  filter(`Ano Mês` == "2020-06") %>%
  group_by(`Ano Mês`, Grupo, ServNorm) %>%
  summarise(Reclamacoes = sum(Total_Reclamações),
          Acessos = sum(`Acessos sem M2M`),
          IR_MES = (Reclamacoes/Acessos)*1000)  

IR_coluna <- df_IR_Resumo %>%
  ggplot(aes(x= Grupo, y = IR_MES))+
  geom_col(color = "red", fill = "red", alpha = 0.5)+
  facet_wrap(~ ServNorm)
plot(IR_coluna)

pop2017 <- brazilmaps::pop2017

df_IR_all <- 
  df_IR %>%
  left_join(pop2017, by = c("Cod_IBGE" = "mun"))

df_IR_all$Tipo <- cut(df_IR_all$pop2017, 
               breaks = c(0,30000,100000,300000,200000000),
               labels = c("Pequeno","Medio","Medio-Grande","Grande"))

df_IR_all$Estatistica <- cut(df_IR_all$`Acessos sem M2M`, 
                  breaks = c(0,2000,200000000),
                  labels = c("Sem Validade","Com Validade"))


df_IR_Tipo <- df_IR_all %>%
  filter(`Ano Mês` == "2020-06") %>%
  group_by(Grupo, ServNorm, Tipo) %>%
  summarise(QtdMun = n())

df_IR_estat <- df_IR_all %>%
  filter(`Ano Mês` == "2020-06") %>%
  group_by(Grupo, ServNorm, Tipo, Estatistica) %>%
  summarise(QtdMun = n())

municipios <- read_municipality(code_muni = "all") %>% 
  select(code_muni,abbrev_state, geometry=geom)

estados <- read_state(code_state = "all")

df_IR_all <- df_IR_all %>%
  left_join(municipios, by = c("Cod_IBGE" = "code_muni")) %>%
  mutate(IR = (Total_Reclamações/`Acessos sem M2M`)*1000)

# Erro = sqrt( ((`Acessos sem M2M`-385)*((1.96^2)*0.5*(1-0.5))) / (385*(`Acessos sem M2M`-1)))

####################################################
####### Graficos de quantidade de medidas ##########
####################################################

graf_tipo <- df_IR_Tipo %>%
ggplot(aes(x= Tipo, y = QtdMun))+
    geom_col()+
  geom_label(aes(label = QtdMun))+
  facet_grid(Grupo ~ ServNorm)+
  theme_minimal()+
  labs(y = "Quantidade de Municípios", x = "Prestadora",
       title = "Quantidade de municipios por tipo, serviço e prestadora")+
  theme(plot.title = element_text(hjust =0.5), axis.text.x = element_text(angle = 90))
plot(graf_tipo)
 
graf_tipo_est <- df_IR_estat %>%
  ggplot(aes(x= Tipo, y = QtdMun, fill=Estatistica))+
  geom_col()+
  geom_label_repel(aes(label = QtdMun))+
  facet_grid(Grupo ~ ServNorm)+
  theme_bw()+
  labs(y = "Quantidade de Municípios", x = "Prestadora",
       title = "Quantidade de municipios por tipo, serviço e prestadora")+
  theme(plot.title = element_text(hjust =0.5), axis.text.x = element_text(angle = 90))
plot(graf_tipo_est)

######################################
####### Definicao dos Mapas ##########
######################################


# mapa_munic_VAL <- ggplot(df_IR)+
#   geom_sf(aes(fill=Estatistica))+
#   geom_sf(data = estados, fill='transparent')+
#   labs(title = "Validade (>2000 Acessos) por munic?pio")+
#   theme_minimal()+
#   theme(plot.title = element_text(hjust =0.5))
# ggsave("mapa_munic_VAL.png")
# 
# mapa_munic_TIPO <- ggplot(df_IR)+
#   geom_sf(aes(fill=Tipo))+
#   geom_sf(data = estados, fill='transparent')+
#   labs(title = "Distribui??o por tipo de munic?pio")+
#   theme_minimal()+
#   theme(plot.title = element_text(hjust =0.5))
# ggsave("mapa_munic_TIPO.png")

########################################
####### Distribui??o das Medidas #######
########################################

unique(df_IR_all$ServNorm)

point_IR_acesso <- df_IR_all %>% 
  mutate(Grupo = factor(Grupo, labels = c("A","B","C","D","E","F","G")),
         Trimestre = ifelse(`Ano Mês` %in% c("2020-01","2020-02","2020-03"),
                            "1ºTri","2ºTri")) %>%
  filter(ServNorm == "TV por Assinatura") %>%
  ggplot(aes(x = `Acessos sem M2M`, y = IR)) +
  geom_point(aes(color = Estatistica))+
  geom_vline(aes(xintercept = 2000))+
  scale_x_continuous(breaks = seq(0,10000,2000),
                     limits = c(0,10000),
                     na.value = NA_real_,
                     trans = "identity", position = "bottom") +
  scale_y_continuous(breaks = seq(0,200,20),
                     limits = c(0,200),
                     na.value = NA_real_) +
  labs(x = "Quantidade de Acessos", 
       y = "IR",
       title = "Distribuição do IR - TV por Assinatura - por Acesso segundo Prestadora")+
  theme_minimal()+
  theme(plot.title = element_text(hjust =0.5)) +
  facet_wrap(~Grupo+Trimestre, scales = "free_x")
plot(point_IR_acesso)
#ggsave("point_IR_acesso.png")

point_IR_acesso_zoom <- df_IR_all %>%
  ggplot(aes(x = `Acessos sem M2M`, y = IR)) +
  geom_point(aes(color = Estatistica))+
  geom_vline(aes(xintercept = 2000))+
  scale_x_continuous(breaks = seq(0,10000,1000),
                     limits = c(0,10000),
                     na.value = NA_real_,
                     trans = "identity", position = "bottom") +
  scale_y_continuous(breaks = seq(0,200,20),
                     limits = c(0,50),
                     na.value = NA_real_) +
  labs(x = "Quantidade de Acessos", 
       y = "IR",
       title = "Distribuição do IR por Acesso")+
  theme_minimal()+
  theme(plot.title = element_text(hjust =0.5))+
  facet_wrap(~ ServNorm)
plot(point_IR_acesso_zoom)
#ggsave("point_IR_acesso.png")

###########################################
############### Histogramas ###############
###########################################


hist_serv_bl <- df_IR_all %>%
  filter(ServNorm == "Banda Larga Fixa") %>% 
  ggplot(aes(x = IR)) +
  geom_histogram(aes(y = ..count../sum(..count..)), binwidth = 0.1, boundary = 0, fill = 'red', color = 'red', alpha = 0.5) +
  scale_x_continuous(breaks = seq(0,20,0.5),
                     #labels = scales::number_format(accuracy = 3),
                     limits = c(0,20),
                     na.value = NA_real_,
                     trans = "identity", position = "bottom") +
  scale_y_continuous(labels = scales::percent_format(),
                     na.value = NA_real_,
                     trans = "identity", position = "top") +
  labs(x = "IR", 
       y = "Percentual de amostras", 
       title = "Distribui??o - IR - Banda Larga Fixa")+
  theme_minimal()+
  theme(plot.title = element_text(hjust =0.5))+
  geom_rect(
    xmin = 0.43,
    xmax = 4.94,
    ymin = 0,
    ymax = Inf,
    fill = "grey",
    alpha = 0.01
  )
plot(hist_serv_bl)
#ggsave("hist_serv_bl.png")

hist_serv_stfc <- df_IR_all %>%
  filter(ServNorm == "Telefone Fixo") %>% 
  ggplot(aes(x = IR)) +
  geom_histogram(aes(y = ..count../sum(..count..)), binwidth = 0.1, boundary = 0, fill = 'red', color = 'red', alpha = 0.5) +
  scale_x_continuous(breaks = seq(0,20,0.5),
                     #labels = scales::number_format(accuracy = 3),
                     limits = c(0,20),
                     na.value = NA_real_,
                     trans = "identity", position = "bottom") +
  scale_y_continuous(labels = scales::percent_format(),
                     na.value = NA_real_,
                     trans = "identity", position = "top") +
  labs(x = "IR", 
       y = "Percentual de amostras", 
       title = "Distribui??o - IR - Telefone Fixo")+
  theme_minimal()+
  theme(plot.title = element_text(hjust =0.5))+
  geom_rect(
    xmin = 0.26,
    xmax = 3.09,
    ymin = 0,
    ymax = Inf,
    fill = "grey",
    alpha = 0.01
  )
plot(hist_serv_stfc)
#ggsave("hist_serv_stfc.png")

hist_serv_smp <- df_IR_all %>%
  filter(ServNorm == "Telefonia Celular") %>% 
  ggplot(aes(x = IR)) +
  geom_histogram(aes(y = ..count../sum(..count..)), binwidth = 0.1, boundary = 0, fill = 'red', color = 'red', alpha = 0.5) +
  scale_x_continuous(breaks = seq(0,20,0.5),
                     #labels = scales::number_format(accuracy = 3),
                     limits = c(0,20),
                     na.value = NA_real_,
                     trans = "identity", position = "bottom") +
  scale_y_continuous(labels = scales::percent_format(),
                     na.value = NA_real_,
                     trans = "identity", position = "top") +
  labs(x = "IR", 
       y = "Percentual de amostras", 
       title = "Distribui??o - IR - Telefonia Celular")+
  theme_minimal()+
  theme(plot.title = element_text(hjust =0.5))+
  geom_rect(
    xmin = 0.11,
    xmax = 1.79,
    ymin = 0,
    ymax = Inf,
    fill = "grey",
    alpha = 0.01
  )
plot(hist_serv_smp)
#ggsave("hist_serv_smp.png")

hist_serv_tva <- df_IR_all %>%
  filter(ServNorm == "TV por Assinatura") %>% 
  ggplot(aes(x = IR)) +
  geom_histogram(aes(y = ..count../sum(..count..)), binwidth = 0.1, boundary = 0, fill = 'red', color = 'red', alpha = 0.5) +
  scale_x_continuous(breaks = seq(0,20,0.5),
                     #labels = scales::number_format(accuracy = 3),
                     limits = c(0,20),
                     na.value = NA_real_,
                     trans = "identity", position = "bottom") +
  scale_y_continuous(labels = scales::percent_format(),
                     na.value = NA_real_,
                     trans = "identity", position = "top") +
  labs(x = "IR", 
       y = "Percentual de amostras", 
       title = "Distribui??o - IR - TV por Assinatura")+
  theme_minimal()+
  theme(plot.title = element_text(hjust =0.5))+
  geom_rect(
    xmin = 0.39,
    xmax = 3.87,
    ymin = 0,
    ymax = Inf,
    fill = "grey",
    alpha = 0.01
  )
plot(hist_serv_tva)
#ggsave("hist_serv_tva.png")

  #Telefonia Celular
  #TV por Assinatura

  
###########################################
############### Boxplot ###################
###########################################

unique(df_IR_all$ServNorm)
  
bp_all_prest <- df_IR_all %>%
  filter(Estatistica == "Com Validade") %>% 
  mutate(Grupo = factor(Grupo, labels = c("A","B","C","D","E","F","G")),
         Trimestre = ifelse(`Ano Mês` %in% c("2020-01","2020-02","2020-03"),
                            "1ºTri","2ºTri")) %>%
  filter(ServNorm == "Telefone Fixo") %>%
  ggplot(aes(y = IR)) +
  geom_boxplot(fill = 'red', color = 'red', alpha = 0.5) +
  # scale_y_continuous(breaks = seq(0,20,1),
  #                    limits = c(0,20),
  #                    na.value = NA_real_,
  #                    trans = "identity", position = "top") +
  labs(y = "IR",
       title = "Boxplot para medidas válidas (maior que 2000 acessos) para Telefone Fixo")+
  coord_flip()+
  theme_minimal()+
  theme(plot.title = element_text(hjust =0.5))+
  facet_wrap(~Grupo+Trimestre)
plot(bp_all_prest)


######################################
############### Resumo ###############
######################################

resumo <- df_IR_all %>%
  filter(Estatistica == "Com Validade") %>% 
  group_by(ServNorm) %>% 
  summarize(Min = min(IR),
            Percentil_5 = quantile(IR, 0.05),
            Quartil_1 = quantile(IR, 0.25),
            Mediana = median(IR),
            Media = mean(IR),
            Quartil_3 = quantile(IR, 0.75),
            Percentil_95 = quantile(IR, 0.95),
            Max = max(IR),
            N = n()) 


dash <- df_IR_Resumo %>% 
  group_by(ServNorm) %>% 
  summarise(Rec = sum(Reclamacoes),
            Access = sum(Acessos),
            IR = (Rec / Access)*1000)

df_IR_all %>%
  filter(Estatistica == "Com Validade") %>% 
  group_by(ServNorm) %>% 
  summarize(Min = min(IR),
            Percentil_5 = quantile(IR, 0.05),
            Percentil_10 = quantile(IR, 0.10),
            Percentil_15 = quantile(IR, 0.15),
            Percentil_20 = quantile(IR, 0.20),
            Quartil_1 = quantile(IR, 0.25),
            Percentil_30 = quantile(IR, 0.30),
            Percentil_35 = quantile(IR, 0.35),
            Percentil_40 = quantile(IR, 0.40),
            Percentil_45 = quantile(IR, 0.45),
            Mediana = median(IR),
            Percentil_55 = quantile(IR, 0.55),
            Percentil_60 = quantile(IR, 0.60),
            Percentil_65 = quantile(IR, 0.65),
            Percentil_70 = quantile(IR, 0.70),
            Media = mean(IR),
            Quartil_3 = quantile(IR, 0.75),
            Percentil_95 = quantile(IR, 0.95),
            Max = max(IR),
            N = n()) 


