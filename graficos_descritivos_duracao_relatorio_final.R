#############ATUALIZAÇÃO PARA RELATÓRIO FINAL - GRÁFICOS DESCRITIVOS DE DURAÇÃO DO PROCESSO#########
fim <- c(246, 22, 848, 861, 870) #SEM ARQUIVAMENTO PROVISÓRIO DO FIM (codigo 245)

setwd("C:/Users/gabri/OneDrive/Documents/[CNJ - Insper]/Núcleo Quantitativo/Dados Datajud")

#abrir banco acoes

banco_acoes <- read_csv("acoes_consolidado.csv")

ids_acoes <- banco_acoes %>% pull(id)


#abrir banco movimentos

banco_movimentos <- read_csv("movimentos_consolidado.csv")

#movimentos

#fim

df_fim <- banco_movimentos %>% 
  filter(codigo_movimentacao %in% fim) %>%
  filter(id %in% ids_acoes) %>%
  arrange(dpj_DataHora) %>% 
  group_by(id) %>% 
  summarize(data_fim = min(dpj_DataHora)) %>%
  mutate(fim = 1)

banco_acoes <- banco_acoes %>%
  left_join(df_fim) %>%
  mutate(fim = case_when(is.na(fim) ~ 0,
                         TRUE ~ as.numeric(fim))) %>%
  mutate(data_fim = case_when(is.na(data_fim) ~ ymd("2022-05-01"),
                              TRUE ~ as.Date(data_fim)))

#primeiro
df_primeiro_movimento <- banco_movimentos %>%
  filter(id %in% ids_acoes) %>%
  arrange(dpj_DataHora) %>% 
  group_by(id) %>% 
  summarize(primeiro_movimento = min(dpj_DataHora)) 


banco_acoes <- banco_acoes %>%
  left_join(df_primeiro_movimento) %>% mutate(primeiro_movimento = as.Date(primeiro_movimento))


banco_acoes <- banco_acoes %>%
  mutate(duracao_fim = difftime(data_fim, primeiro_movimento, units = "days")) %>%
  mutate(duracao_fim = as.numeric(duracao_fim))

banco_acoes <- banco_acoes %>% mutate(duracaogeral = data_fim - primeiro_movimento, units = "days")

banco_acoes_fim <- banco_acoes %>% filter(fim==1)
#dfs por periodo

#2020-2022

banco_acoes_2020 <- banco_acoes %>%
  filter(year(primeiro_movimento) > 2019 & year(primeiro_movimento) < 2023) %>%
  filter(ano_cnj > 2019 & ano_cnj < 2023)


#2010-2019

banco_acoes_2010 <- banco_acoes %>%
  filter(year(primeiro_movimento) > 2009 & year(primeiro_movimento) < 2020) %>%
  filter(ano_cnj > 2009 & ano_cnj < 2020)


#graficos de duracao

#2010 a 2019
banco_acoes_2010 %>% filter(fim==1) %>% filter(duracao_fim < 4000) %>% ggplot() + 
  geom_histogram(aes(x=duracao_fim)) + theme_minimal(base_size=11, base_rect_size=11) +
  labs(x="Dias de duração", y="Número de decisões", title="2010-2019")

summary(banco_acoes_2010$duracao_fim)
#2020 em diante
banco_acoes_2020 %>% filter(fim==1) %>% ggplot() + geom_bar(aes(x=duracao_fim)) + theme_minimal(base_size=11, base_rect_size=11) +
  labs(x="Dias de duração", y="Número de decisões", title="2020-2022")
summary(banco_acoes_2020$duracao_fim)
##JF

#2010 a 2019
banco_acoes_2010 %>% filter(fim==1) %>% filter(duracao_fim < 4000) %>% filter(digito_14 == "JF") %>% ggplot() + 
  geom_histogram(aes(x=duracao_fim)) + theme_minimal(base_size=11, base_rect_size=11) +
  labs(x="Dias de duração", y="Número de decisões", title="2010-2019")
banco_acoes_2010_JF <- banco_acoes_2010 %>% filter(digito_14 == "JF")
banco_acoes_2010_JF_fim <- banco_acoes_2010_JF %>% filter(fim==1) #72209
summary(banco_acoes_2010_JF$duracao_fim)
#2020 em diante
banco_acoes_2020 %>% filter(fim==1) %>% filter(digito_14 == "JF") %>% ggplot() + geom_bar(aes(x=duracao_fim)) + theme_minimal(base_size=11, base_rect_size=11) +
  labs(x="Dias de duração", y="Número de decisões", title="2020-2022")
banco_acoes_2020_JF <- banco_acoes_2020 %>% filter(digito_14 == "JF")
banco_acoes_2020_JF_fim <- banco_acoes_2020_JF %>% filter(fim==1) #72209
summary(banco_acoes_2020_JF$duracao_fim)
#JEST
#2010 a 2019
banco_acoes_2010 %>% filter(fim==1) %>% filter(duracao_fim < 4000) %>% filter(digito_14 == "JEST") %>% ggplot() + 
  geom_histogram(aes(x=duracao_fim)) + theme_minimal(base_size=11, base_rect_size=11) +
  labs(x="Dias de duração", y="Número de decisões", title="2010-2019")
banco_acoes_2010_JEST <- banco_acoes_2010 %>% filter(digito_14 == "JEST")
banco_acoes_2010_JEST_fim <- banco_acoes_2010_JEST %>% filter(fim==1) #168636
summary(banco_acoes_2010_JEST$duracao_fim)
#2020 em diante
banco_acoes_2020 %>% filter(fim==1) %>% filter(digito_14 == "JEST") %>% ggplot() + geom_bar(aes(x=duracao_fim)) + theme_minimal(base_size=11, base_rect_size=11) +
  labs(x="Dias de duração", y="Número de decisões", title="2020-2022")
banco_acoes_2020_JEST <- banco_acoes_2020 %>% filter(digito_14 == "JEST")
banco_acoes_2020_JEST_fim <- banco_acoes_2020_JEST %>% filter(fim==1) #26463
summary(banco_acoes_2020_JEST$duracao_fim)


###comparar por assunto

banco_assuntos <- read_csv("assuntos_consolidado.csv") %>%
  filter(id %in% ids_acoes) %>%
  mutate(assunto_consolidado = case_when(dpj_codigoAssuntoNacional %in% c(10837,
                                                                          3568,
                                                                          9921) ~ 
                                           "Corrupçao ativa",
                                         dpj_codigoAssuntoNacional %in% c(10982,
                                                                          10983) ~
                                           "Lavagem ou ocultaçao de bens, direitos ou valores oriundos de corrupçao",
                                         dpj_codigoAssuntoNacional %in% c(3628,
                                                                          9888) ~ 
                                           "Lavagem ou ocultaçao de bens, direitos ou valores",
                                         dpj_codigoAssuntoNacional %in% c(3555,
                                                                          98350,
                                                                          9835) ~
                                           "Corrupçao passiva",
                                         dpj_codigoAssuntoNacional %in% c(10835,
                                                                          9829,
                                                                          3548) ~
                                           "Peculato",
                                         dpj_codigoAssuntoNacional %in% c(10836,
                                                                          3553,
                                                                          9833) ~
                                           "Concussão",
                                         dpj_codigoAssuntoNacional %in% c(3567,
                                                                          9920) ~
                                           "Trafico de influencia",
                                         dpj_codigoAssuntoNacional %in% c(10208,
                                                                          10202,
                                                                          10215) ~
                                           "Afastamento do cargo",
                                         dpj_codigoAssuntoNacional %in% c(10011,
                                                                          11559) ~
                                           "Improbidade administrativa",
                                         dpj_codigoAssuntoNacional %in% c(10206,
                                                                          10212,
                                                                          10218) ~
                                           "Indisponibilidade de bens",
                                         dpj_codigoAssuntoNacional %in% c(10838,
                                                                          3557,
                                                                          9837) ~
                                           "Prevaricaçao",
                                         dpj_codigoAssuntoNacional %in% c(10985,
                                                                          3614,
                                                                          9874) ~
                                           "Crimes contra a ordem tributaria",
                                         dpj_codigoAssuntoNacional %in% c(11726,
                                                                          11728,
                                                                          14938) ~
                                           "Perda ou suspensao de direitos politicos",
                                         dpj_codigoAssuntoNacional %in% c(3556,
                                                                          3574,
                                                                          9836,
                                                                          9927) ~
                                           "Contrabando ou descaminho",
                                         dpj_codigoAssuntoNacional %in% c(3642,
                                                                          9899) ~
                                           "Crimes da lei de licitaçoes",
                                         dpj_codigoAssuntoNacional %in% c(3552,
                                                                          9832) ~
                                           "Emprego irregular de verbas ou rendas publicas",
                                         dpj_codigoAssuntoNacional %in% c(3554,
                                                                          9834) ~
                                           "Excesso de exaçao",
                                         dpj_codigoAssuntoNacional %in% c(3558,
                                                                          9838) ~
                                           "Condescendência criminosa",
                                         dpj_codigoAssuntoNacional %in% c(3559,
                                                                          9839) ~
                                           "Advocacia administrativa",
                                         dpj_codigoAssuntoNacional %in% c(3564,
                                                                          9844) ~
                                           "Violaçao do sigilo de proposta de concorrencia",
                                         dpj_codigoAssuntoNacional %in% c(3569,
                                                                          9922) ~
                                           "Impedimento, perturbação ou fraude de concorrencia",
                                         dpj_codigoAssuntoNacional %in% c(3596,
                                                                          9845) ~
                                           "Inserçao de dados falsos em sistema de informaçao",
                                         dpj_codigoAssuntoNacional %in% c(3598,
                                                                          9928) ~
                                           "Sonegaçao de contribuiçao previdenciaria",
                                         dpj_codigoAssuntoNacional %in% c(5873,
                                                                          9929) ~
                                           "Crimes praticados por particular contra a administraçao publica estrangeira",
                                         dpj_codigoAssuntoNacional %in% c(5875,
                                                                          9955) ~
                                           "Crimes contra as finanças publicas",
                                         dpj_codigoAssuntoNacional %in% c(9830,
                                                                          3550) ~
                                           "Peculato mediante erro de outrem",
                                         TRUE ~ as.character(dpj_nomeAssuntoNacional))) %>%
  mutate(id_assunto = 1:nrow(.))

ids_assunto_multiplo <-
  banco_assuntos %>%
  count(id) %>%
  filter(n > 1) %>%
  pull(id)

ids_assunto_principal <-
  banco_assuntos %>%
  filter(id %in% ids_assunto_multiplo & principal == TRUE) %>%
  pull(id_assunto)

banco_assuntos <- banco_assuntos %>%
  filter(id %!in% ids_assunto_multiplo |
           id_assunto %in% ids_assunto_principal)


banco_acoes <- banco_acoes %>%
  mutate(digito_17_18 = str_sub(digito_17_20, 1, 2)) %>%
  mutate(digito_17_18 = as.numeric(digito_17_18)) %>%
  mutate(digito_17_18 = case_when(tribunal_cnj == "TRF4" & digito_17_18 == 70 ~ "PR",
                                  tribunal_cnj == "TRF4" & digito_17_18 == 71 ~ "RS",
                                  tribunal_cnj == "TRF4" & digito_17_18 == 72 ~ "SC",
                                  tribunal_cnj == "TRF3" & digito_17_18 == 60 ~ "MS", 
                                  tribunal_cnj == "TRF3" & digito_17_18 == 61 ~ "SP",
                                  tribunal_cnj == "TRF1" & digito_17_18 == 30 ~ "AC",
                                  tribunal_cnj == "TRF1" & digito_17_18 == 31 ~ "AP",
                                  tribunal_cnj == "TRF1" & digito_17_18 == 32 ~ "AM",
                                  tribunal_cnj == "TRF1" & digito_17_18 == 33~"BA",
                                  tribunal_cnj == "TRF1" & digito_17_18 == 34~"DF",
                                  tribunal_cnj == "TRF1" & digito_17_18 == 35~"GO",
                                  tribunal_cnj == "TRF1" & digito_17_18 == 37~"MA", 
                                  tribunal_cnj == "TRF1" & digito_17_18 == 38~"MG",
                                  tribunal_cnj == "TRF1" & digito_17_18 == 39~"PA",
                                  tribunal_cnj == "TRF1" & digito_17_18 == 40~"PI",
                                  tribunal_cnj == "TRF1" & digito_17_18 == 41~"RO",
                                  tribunal_cnj == "TRF1" & digito_17_18 == 42~"RR",
                                  tribunal_cnj == "TRF1" & digito_17_18 == 43~"TO",
                                  tribunal_cnj == "TRF2" & digito_17_18 == 50~"ES",
                                  tribunal_cnj == "TRF2" & digito_17_18 == 51~"RJ",
                                  tribunal_cnj == "TRF5" & digito_17_18 == 80~"AL",
                                  tribunal_cnj == "TRF5" & digito_17_18 == 81~"CE",
                                  tribunal_cnj == "TRF5" & digito_17_18 == 82~"PB",
                                  tribunal_cnj == "TRF5" & digito_17_18 == 83~"PE",
                                  tribunal_cnj == "TRF5" & digito_17_18 == 84~"RN",
                                  tribunal_cnj == "TRF5" & digito_17_18 == 85~"SE",
                                  tribunal_cnj == "TRF5" & digito_17_18 == 36~"MT")) %>%
  mutate(estado_cnj = case_when(digito_17_18 == "PR" ~ "PR",
                                tribunal_cnj == "TJPR" ~ "PR",
                                digito_17_18 == "RS" ~ "RS",
                                tribunal_cnj == "TJRS" ~"RS", 
                                digito_17_18 == "SC" ~"SC",
                                tribunal_cnj == "TJSC" ~ "SC",
                                digito_17_18 == "MS" ~ "MS",
                                tribunal_cnj == "TJMS" ~ "MS",
                                digito_17_18 == "SP" ~ "SP",
                                tribunal_cnj == "TJSP" ~ "SP",
                                digito_17_18 == "AC" ~ "AC",
                                tribunal_cnj == "TJAC" ~ "AC",
                                digito_17_18 == "AP" ~ "AP",
                                tribunal_cnj == "TJAP" ~ "AP",
                                digito_17_18 == "AM" ~ "AM",
                                tribunal_cnj == "TJAM" ~ "AM",
                                digito_17_18 == "BA" ~ "BA",
                                tribunal_cnj == "TJBA" ~ "BA",
                                digito_17_18 == "DF" ~ "DF",
                                tribunal_cnj == "TJDF" ~ "DF",
                                digito_17_18 == "GO" ~ "GO",
                                tribunal_cnj == "TJGO" ~ "GO",
                                digito_17_18 == "MA" ~ "MA",
                                tribunal_cnj == "TJMA" ~ "MA",
                                digito_17_18 == "MG" ~ "MG",
                                tribunal_cnj == "TJMG" ~ "MG",
                                digito_17_18 == "PA" ~ "PA",
                                tribunal_cnj == "TJPA" ~ "PA",
                                digito_17_18 == "PI" ~ "PI",
                                tribunal_cnj == "TJPI" ~ "PI", 
                                digito_17_18 == "RO" ~ "RO",
                                tribunal_cnj == "TJRO" ~ "RO", 
                                digito_17_18 == "RR" ~ "RR",
                                tribunal_cnj == "TJRR" ~ "RR", 
                                digito_17_18 == "TO" ~ "TO",
                                tribunal_cnj == "TJTO" ~ "TO", 
                                digito_17_18 == "ES" ~ "ES",
                                tribunal_cnj == "TJES" ~ "ES", 
                                digito_17_18 == "RJ" ~ "RJ",
                                tribunal_cnj == "TJRJ" ~ "RJ", 
                                digito_17_18 == "AL" ~ "AL",
                                tribunal_cnj == "TJAL" ~ "AL",
                                digito_17_18 == "CE" ~ "CE",
                                tribunal_cnj == "TJCE" ~ "CE", 
                                digito_17_18 == "PB" ~ "PB",
                                tribunal_cnj == "TJPB" ~ "PB", 
                                digito_17_18 == "PE" ~ "PE",
                                tribunal_cnj == "TJPE" ~ "PE", 
                                digito_17_18 == "RN" ~ "RN",
                                tribunal_cnj == "TJRN" ~ "RN", 
                                digito_17_18 == "SE" ~ "SE",
                                tribunal_cnj == "TJSE" ~ "SE",
                                digito_17_18 == "MT" ~ "MT",
                                tribunal_cnj == "TJMT" ~ "MT",
                                tribunal_cnj == "Superior" ~ "Superior",
                                tribunal_cnj == "TRF1" & digito_17_18 == "00" ~ "TRF1",
                                tribunal_cnj == "TRF2" & digito_17_18 == "00" ~ "TRF2",
                                tribunal_cnj == "TRF3" & digito_17_18 == "00" ~ "TRF3",
                                tribunal_cnj == "TRF4" & digito_17_18 == "00" ~ "TRF4",
                                tribunal_cnj == "TRF5" & digito_17_18 == "00" ~ "TRF5"))

banco_acoes %>%
  count(estado_cnj, digito_17_18, tribunal_cnj, digito_15_16) %>% print(n = 20)


banco_acoes <- banco_acoes %>%
  left_join(banco_assuntos) %>%
  mutate(assunto_regressao = case_when(assunto_consolidado == "Corrupçao passiva" ~
                                         "Corrupçao Passiva",
                                       assunto_consolidado == "Corrupçao ativa" ~
                                         "Corrupçao Ativa",
                                       assunto_consolidado %in% c("Peculato",
                                                                  "Peculato mediante erro de outrem") ~
                                         "Peculato",
                                       assunto_consolidado %in% c("Lavagem ou ocultaçao de bens, direitos ou valores",
                                                                  "Lavagem ou ocultaçao de bens, direitos ou valores oriundos de corrupçao") ~
                                         "Lavagem",
                                       # assunto_consolidado == "Crimes contra a ordem tributária" ~ "Crimes contra a ordem tributária",
                                       # assunto_consolidado == "Improbidade administrativa" ~ "Improbidade administrativa",
                                       # assunto_consolidado == "Crimes da lei de licitações" ~ "Crimes da lei de licitações",
                                       TRUE ~ "Outros")) %>%
  mutate(assunto_regressao = as_factor(assunto_regressao)) %>%
  mutate(assunto_regressao = fct_relevel(assunto_regressao, "Outros")) %>%
  rename(esfera = digito_14)

banco_acoes %>%
  count(assunto_regressao, sort = T)

#dfs por periodo de novo

#2020-2022
banco_acoes_2020 <- banco_acoes %>%
  filter(year(primeiro_movimento) > 2019 & year(primeiro_movimento) < 2023) %>%
  filter(ano_cnj > 2019 & ano_cnj < 2023)


#2010-2019

banco_acoes_2010 <- banco_acoes %>%
  filter(year(primeiro_movimento) > 2009 & year(primeiro_movimento) < 2020) %>%
  filter(ano_cnj > 2009 & ano_cnj < 2020)

##graficos

#2010 a 2019
df_2010_fim <- banco_acoes_2010 %>% filter(fim==1) 
banco_acoes_2010 %>% filter(fim==1) %>% filter(duracao_fim < 4000) %>% ggplot() + 
  geom_histogram(aes(x=duracao_fim, fill=assunto_regressao)) + theme_minimal(base_size=11, base_rect_size=11) +
  labs(x="Dias de duração", y="Número de decisões", fill="Assunto da ação", title="2010-2019")

lavagem <- banco_acoes_2010 %>% filter(assunto_regressao=="Lavagem")
lavagem$duracao_fim %>% summary()

ativa <- banco_acoes_2010 %>% filter(assunto_regressao=="Corrupçao Ativa")
ativa$duracao_fim %>% summary()

passiva <- banco_acoes_2010 %>% filter(assunto_regressao=="Corrupçao Passiva")
passiva$duracao_fim %>% summary()

peculato <- banco_acoes_2010 %>% filter(assunto_regressao=="Peculato")
peculato$duracao_fim %>% summary()

outros <- banco_acoes_2010 %>% filter(assunto_regressao=="Outros")
outros$duracao_fim %>% summary()

#2020 em diante
df_2020_fim <- banco_acoes_2020 %>% filter(fim==1)
banco_acoes_2020 %>% filter(fim==1) %>% ggplot() + 
  geom_histogram(aes(x=duracao_fim, fill=assunto_regressao)) + theme_minimal(base_size=11, base_rect_size=11) +
  labs(x="Dias de duração", y="Número de decisões", fill="Assunto da ação", title="2020-2022")

lavagem <- banco_acoes_2020 %>% filter(fim==1) %>% filter(assunto_regressao=="Lavagem")
lavagem$duracao_fim %>% summary()

ativa <- banco_acoes_2020 %>% filter(fim==1) %>% filter(assunto_regressao=="Corrupçao Ativa")
ativa$duracao_fim %>% summary()

passiva <- banco_acoes_2020 %>% filter(fim==1)%>% filter(assunto_regressao=="Corrupçao Passiva")
passiva$duracao_fim %>% summary()

peculato <- banco_acoes_2020 %>% filter(fim==1)%>% filter(assunto_regressao=="Peculato")
peculato$duracao_fim %>% summary()

outros <- banco_acoes_2020 %>% filter(fim==1)%>% filter(assunto_regressao=="Outros")
outros$duracao_fim %>% summary()