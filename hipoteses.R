library(tidyverse)
library(readr)
library(readxl)
library(broom)

`%!in%` <- Negate(`%in%`)

options(scipen = 999)

#setwd("C:/Users/gabri/OneDrive/Documents/[CNJ - Insper]/Núcleo Quantitativo/Dados Datajud")
#setwd("G:\\Meu Drive\\cnj")

###############################################################################
# Arrumando variáveis do banco
###############################################################################


df_complexidade <- read_csv("movimentos_consolidado.csv") %>%
  count(id) %>%
  rename(movimentos = n)


banco_acoes <- read_csv("acoes_consolidado.csv")

ids_acoes <- banco_acoes %>% pull(id)

banco_assuntos <- read_csv("banco_assuntos_consolidados.csv") %>%
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

totais_penal <- readxl::read_excel("Justiça em Números - Totais .xlsx") %>%
  pivot_longer(2:9, "ano", values_to = "n") %>%
  mutate(ano = as.numeric(ano)) %>%
  rename(total_penal = n) %>%
  filter(Tribunal %in% c("TJAC", "TJAL", "TJAM", "TJAP", "TJBA", "TJCE", "TJDFT",
                         "TJES", "TJGO", "TJMA", "TJMG", "TJMS", "TJMT","TJPA", 
                         "TJPB", "TJPE", "TJPI", "TJPR", "TJRJ", "TJRN","TJRO", "TJRR", "TJRS", "TJSC", 
                         "TJSE", "TJSP", "TJTO")) %>%
  mutate(Tribunal = str_remove_all(Tribunal, "TJ"))

banco_acoes <- banco_acoes %>%
  left_join(totais_penal, by = c("estado_cnj" = "Tribunal", "ano_cnj" = "ano")) %>%
  left_join(df_complexidade) %>%
  filter(estado_cnj != "Superior")

#################################################################################

transferencias <- read_excel("transferencias_para_estados_ano_uf.xlsx") %>%
  mutate(`Valor Consolidado` = as.numeric(`Valor Consolidado`)) %>%
  group_by(UF, Ano) %>%
  summarise(valor_transferencia = sum(`Valor Consolidado`))

####################

pib <- read_excel("ipeadata.xls") %>%
  pivot_longer(4:ncol(.), names_to = "ano", values_to = "pib_per_capta") %>%
  mutate(ano = as.numeric(ano))

#####################

mortes <- read_csv2("mortes_UF_FBSP_2011_2021.csv") %>%
  pivot_longer(3:ncol(.), names_to = "ano", values_to = "mortes") %>%
  mutate(ano = as.numeric(ano))

#####################

banco_acoes <- banco_acoes %>%
  left_join(pib, by = c("ano" = "ano", "estado_cnj" = "Sigla")) %>%
  left_join(transferencias, by = c("ano" = "Ano", "estado_cnj" = "UF")) %>%
  left_join(mortes, by = c("ano" = "ano", "estado_cnj" = "UF"))

##############################
# Regressão 1 - Duração
##############################

library(lubridate)

banco_movimentos <- read_csv("movimentos_consolidado.csv")

###fim do processo
fim <- c(246, 22, 848, 861, 870)
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

acoes_2021 <- banco_acoes %>% filter(ano_cnj == 2020 | ano_cnj == 2021 | ano_cnj == 2022)
#n acoes 2020 e 2021 e 2022 = 98013


df_primeiro_movimento <- banco_movimentos %>%
  filter(id %in% ids_acoes) %>%
  arrange(dpj_DataHora) %>% 
  group_by(id) %>% 
  summarize(primeiro_movimento = min(dpj_DataHora)) 


banco_acoes <- banco_acoes %>%
  left_join(df_primeiro_movimento)


banco_acoes <- banco_acoes %>%
  mutate(duracao_fim = difftime(data_fim, primeiro_movimento, units = "days")) %>%
  mutate(duracao_fim = as.numeric(duracao_fim))



ids_acordo <- banco_movimentos %>%
  filter(codigo_movimentacao %in% c(12028, 884, 12733, 12735)) %>%
  pull(id) %>%
  unique()

banco_acoes <- banco_acoes %>%
  mutate(acordo = case_when(id %in% ids_acordo ~ "acordo",
                            id %!in% ids_acordo ~ "sem acordo")) 





library(survival)

surv_object <-  Surv(time = banco_acoes$duracao_fim, 
                     event = banco_acoes$fim)

fit.coxph <- coxph(surv_object ~  assunto_regressao + estado_cnj + acordo + 
                     log(total_penal) + esfera + movimentos, 
                   data = banco_acoes)


library(survminer)
ggforest(fit.coxph, data = banco_acoes, 
         main = "Risco relativo - Todo o período") 


####

banco_acoes_2020 <- banco_acoes %>%
  filter(year(primeiro_movimento) > 2019 & year(primeiro_movimento) < 2023) %>%
  filter(ano_cnj > 2019 & ano_cnj < 2023)


surv_object <-  Surv(time = banco_acoes_2020$duracao_fim, 
                     event = banco_acoes_2020$fim)

fit.coxph <- coxph(surv_object ~  assunto_regressao + estado_cnj + acordo + 
                     log(total_penal) + esfera + movimentos,  
                   data = banco_acoes_2020)


ggforest(fit.coxph, data = banco_acoes_2020, main = "")

ggsave("fim_2020.png", device = "png", width = 17, height = 22,
       units = "cm", dpi = 300, bg = "white")

banco_acoes_2020$assunto_regressao %>% table() %>% sort()
banco_acoes_2020$estado_cnj %>% table() %>% sort()
banco_acoes_2020$esfera %>% table() %>% sort()
banco_acoes_2020$movimentos %>% mean(na.rm = T)
banco_acoes_2020$total_penal %>% mean(na.rm = T) %>% log()

SP <-
survfit(fit.coxph,newdata=data.frame(assunto_regressao = "Lavagem",
                                     estado_cnj = "SP",
                                     acordo = "sem acordo",
                                     esfera = "JEST",
                                     total_penal = 13.18013,
                                     movimentos = 27.6762)) %>% surv_median()

PR <-
survfit(fit.coxph,newdata=data.frame(assunto_regressao = "Lavagem",
                                     estado_cnj = "PR",
                                     acordo = "sem acordo",
                                     esfera = "JEST",
                                     total_penal = 13.18013,
                                     movimentos = 27.6762)) %>% surv_median()
MS <-
survfit(fit.coxph,newdata=data.frame(assunto_regressao = "Lavagem",
                                     estado_cnj = "MS",
                                     acordo = "sem acordo",
                                     esfera = "JEST",
                                     total_penal = 13.18013,
                                     movimentos = 27.6762)) %>% surv_median()

PE <-
survfit(fit.coxph,newdata=data.frame(assunto_regressao = "Lavagem",
                                     estado_cnj = "PE",
                                     acordo = "sem acordo",
                                     esfera = "JEST",
                                     total_penal = 13.18013,
                                     movimentos = 27.6762)) %>% surv_median()

AM <-
survfit(fit.coxph,newdata=data.frame(assunto_regressao = "Lavagem",
                                     estado_cnj = "AM",
                                     acordo = "sem acordo",
                                     esfera = "JEST",
                                     total_penal = 13.18013,
                                     movimentos = 27.6762)) %>% surv_median()

casos <- rbind(SP, PR, MS, AM, PE) %>%
  mutate(UF = c("SP", "PR", "MS", "AM", "PE"))

casos %>%
ggplot(aes(median, UF)) +
  geom_pointrangeh(aes(xmin = lower, xmax = upper)) +
  theme_minimal() +
  labs(x = "Dias")




##

library(ggeffects)

ggpredict(fit.coxph, c("acordo", "assunto_regressao"), type = "survival") %>% plot() +
  labs(title = "Probabilidade do processo não se encerrar", y = "Probabilidade",
       x = "Dias")


###


banco_acoes_2010 <- banco_acoes %>%
  filter(year(primeiro_movimento) > 2009 & year(primeiro_movimento) < 2020) %>%
  filter(ano_cnj > 2009 & ano_cnj < 2020)


surv_object <-  Surv(time = banco_acoes_2010$duracao_fim, 
                     event = banco_acoes_2010$fim)

fit.coxph <- coxph(surv_object ~  assunto_regressao + estado_cnj + acordo + 
                     log(total_penal) + esfera + movimentos, 
                   data = banco_acoes_2010)


ggforest(fit.coxph, data = banco_acoes_2010, 
         main = "") 

ggsave("fim_2010.png", device = "png", width = 17, height = 22,
       units = "cm", dpi = 300, bg = "white")

ggpredict(fit.coxph, c("acordo"), type = "survival") %>% plot() +
  labs(title = "Probabilidade do processo não se encerrar", y = "Probabilidade",
       x = "Dias")

predict(fit.coxph, type = "expected")






#############################################################################

vesp <- read_excel("varas especializadas sid.xlsx", sheet = 2) %>%
  select(Tribunal, Orgao, UF, Cidade, vesp_tipo) %>%
  filter(vesp_tipo != "nao_vesp") %>%
  mutate(Orgao = str_to_lower(Orgao))

vesp %>%
  filter(str_to_lower(vesp$Orgao) %!in% str_to_lower(banco_acoes$nome_orgao))

banco_acoes <- banco_acoes %>%
  mutate(nome_orgao = str_to_lower(nome_orgao)) %>%
  left_join(vesp, by = c("tribunal_cnj" = "Tribunal", 
                         "estado_cnj" = "UF",
                         "nome_orgao" = "Orgao"))

banco_acoes <- banco_acoes %>%
  mutate(vesp_tipo = case_when(is.na(vesp_tipo) ~ "Não especializada",
                               TRUE ~ as.character(vesp_tipo))) 



table(banco_acoes$vesp_tipo)


# banco_acoes %>%
#   mutate(nome_orgao = str_to_lower(nome_orgao)) %>%
#   filter(str_detect(nome_orgao, "13a Vara vederal")) %>%
#   filter(estado_cnj == "CE") %>%
#   filter(tribunal_cnj == "TRF4") %>%
#   pull(nome_orgao) %>%
#   unique()


# 
# 
# banco_acoes %>%
#   mutate(nome_orgao = str_to_lower(nome_orgao)) %>%
#   filter(nome_orgao == "1ª vara criminal da comarca de criciúma")
# 
# str_to_lower(vesp$Orgao) %in% str_to_lower(banco_acoes$nome_orgao) %>% table()
# 
# 
# vesp_nome <- c("12ª - Brasília", "2ª - Salvador", "11ª - Goiânia", "10ª - Brasília", "4ª - Belém", "1ª - Teresina", "4ª - Manaus", "11ª - Belo Horizonte", "1ª - São Luís", "4ª - Belo Horizonte",
#                "5ª - Cuiabá", "3ª VARA FEDERAL DE CAMPO GRANDE", "09ª VARA FEDERAL DE CAMPINAS COM JUIZADO ESPECIAL FEDERAL CRIMINAL ADJUNTO", "01ª VARA FEDERAL DE CAMPINAS COM JUIZADO ESPECIAL FEDERAL CRIMINAL ADJUNTO", 
#                "10ª VARA FEDERAL CRIMINAL DE SÃO PAULO", "10ª Vara Federal Criminal De São Paulo", "2ª VARA FEDERAL CRIMINAL DE SÃO PAULO", "4ª VARA FEDERAL DE RIBEIRÃO PRETO",
#                "06ª Vara Federal Criminal De São Paulo", "5ª Vara Federal De Campo Grande", "13ª Vara Federal de Curitiba", "23ª Vara Federal de Curitiba", "14ª Vara Federal de Curitiba",
#                "22ª Vara Federal de Porto Alegre", "1ª Vara Federal de Joinville", "7ª Vara Federal de Florianópolis", "7ª Vara Federal de Porto Alegre", "1ª Vara Federal de Chapecó", "1ª Vara Federal de Criciúma",
#                "1ª Vara Federal de Florianópolis", "1ª Vara Federal de Itajaí", "9ª Vara Federal de Curitiba", "11ª Vara Federal de Porto Alegre", "7ª Vara Federal Criminal do Rio de Janeiro",
#                "2ª Vara Federal Criminal de Vitória", "1ª Vara Federal Criminal de Vitória", "2ª Vara Federal Criminal do Rio de Janeiro", "3ª Vara Federal Criminal do Rio de Janeiro", "5ª Vara Federal Criminal do Rio de Janeiro", "11ª Vara Federal",
#                "2ª Vara Federal", "32ª Vara Federal", "13ª VARA FEDERAL", "4ª VARA FEDERAL")
# 
# proc_vesp_fed <- banco_acoes %>% filter(nome_orgao %in% vesp_nome)
# 
# #Exclusoes porque as varas sem nome do TRF5 só entram se forem de um estado específico:
# proc_vesp_exc <- proc_vesp_fed %>% 
#   filter(digito_17_18 != "PE" & nome_orgao == "13ª VARA FEDERAL" | digito_17_18 != "PE" & nome_orgao == "4ª VARA FEDERAL" | digito_17_18 != "CE" & nome_orgao == "11ª Vara Federal" |
#                                             digito_17_18 != "CE" & nome_orgao == "32ª Vara Federal" | digito_17_18 != "RN" & nome_orgao == "2ª Vara Federal")
# 
# id_exc <- proc_vesp_exc$id
# 
# ids_vesp <- proc_vesp_fed %>% 
#   filter(id %!in% id_exc) %>% 
#   filter(esfera == "JF") %>%
#   pull(id)
# 
# banco_acoes_fed <- banco_acoes %>%
#   mutate(vesp = case_when(id %in% ids_vesp ~ "Especializada",
#                           id %!in% ids_vesp ~ "Não especializada")) %>%
#   filter(esfera == "JF")

banco_acoes_fed <- banco_acoes %>%
    filter(esfera == "JF")

surv_object <-  Surv(time = banco_acoes_fed$duracao_fim, 
                     event = banco_acoes_fed$fim)

fit.coxph <- coxph(surv_object ~  assunto_regressao + estado_cnj + acordo + 
                     log(total_penal) + vesp_tipo + movimentos, 
                   data = banco_acoes_fed)


ggforest(fit.coxph, data = banco_acoes_fed) 



#####

banco_acoes_fed_2010 <- banco_acoes_fed %>%
  filter(year(primeiro_movimento) > 2009 & year(primeiro_movimento) < 2020) %>%
  filter(ano_cnj > 2009 & ano_cnj < 2020)

surv_object <-  Surv(time = banco_acoes_fed_2010$duracao_fim, 
                     event = banco_acoes_fed_2010$fim)

fit.coxph <- coxph(surv_object ~  assunto_regressao + estado_cnj + acordo + 
                     log(total_penal) + vesp_tipo + movimentos, 
                   data = banco_acoes_fed_2010)


ggforest(fit.coxph, data = banco_acoes_fed_2010, 
         main = "") 

ggsave("fim_fed_2010.png", device = "png", width = 17, height = 22,
       units = "cm", dpi = 300, bg = "white")

ggpredict(fit.coxph, c("vesp_tipo"), type = "survival") %>% plot() +
  labs(title = "Probabilidade do processo não se encerrar", y = "Probabilidade",
       x = "Dias")


###


banco_acoes_fed_2020 <- banco_acoes_fed %>%
  filter(year(primeiro_movimento) > 2019 & year(primeiro_movimento) < 2023) %>%
  filter(ano_cnj > 2019 & ano_cnj < 2023)

surv_object <-  Surv(time = banco_acoes_fed_2020$duracao_fim, 
                     event = banco_acoes_fed_2020$fim)

fit.coxph <- coxph(surv_object ~  assunto_regressao + estado_cnj + acordo + 
                     log(total_penal) + vesp_tipo + movimentos, 
                   data = banco_acoes_fed_2020)


ggforest(fit.coxph, data = banco_acoes_fed_2020, 
         main = "") 


ggsave("fim_fed_2020.png", device = "png", width = 17, height = 22,
       units = "cm", dpi = 300, bg = "white")


ggpredict(fit.coxph, c("acordo"), type = "survival") %>% plot() +
  labs(title = "Probabilidade do processo não se encerrar", y = "Probabilidade",
       x = "Dias")


ggpredict(fit.coxph, c("vesp_tipo"), type = "survival") %>% plot() +
  labs(title = "Probabilidade do processo não se encerrar", y = "Probabilidade",
       x = "Dias")

banco_acoes_fed$estado_cnj %>% table()
banco_acoes_fed_2020$total_penal %>% mean(na.rm = T)
banco_acoes_fed_2020$movimentos %>% mean(na.rm = T)

banco_acoes_2020 %>%
  filter(assunto_regressao == "Lavagem" &
           estado_cnj == "SP" &
           vesp_tipo == "vesp_exclusiva" &
           acordo == "sem acordo") %>%
  summarise(mean(total_penal, na.rm = T),
            mean(movimentos))




#######################################################################
###########################################################################

decisao_1grau <- c(1063, 12430, 817, 264, 12455, 402, 12733, 968, 11014, 207,
                   12164, 3)

df_decisao_1grau <- banco_movimentos %>% 
  filter(codigo_movimentacao %in% decisao_1grau) %>%
  filter(id %in% ids_acoes) %>%
  arrange(dpj_DataHora) %>% 
  group_by(id) %>% 
  summarize(data_decisao = min(dpj_DataHora)) %>%
  mutate(decisao = 1)


banco_acoes <- banco_acoes %>%
  left_join(df_decisao_1grau) %>%
  mutate(decisao = case_when(is.na(decisao) ~ 0,
                         TRUE ~ as.numeric(decisao))) %>%
  mutate(data_decisao = case_when(is.na(data_decisao) ~ ymd("2022-05-01"),
                              TRUE ~ as.Date(data_decisao))) %>%
  mutate(duracao_decisao = difftime(data_decisao, primeiro_movimento, units = "days"))

# banco_acoes <- banco_acoes %>%
  # mutate(vesp = case_when(id %in% ids_vesp ~ "Especializada",
  #                         id %!in% ids_vesp ~ "Não especializada"))

surv_object <-  Surv(time = banco_acoes$duracao_decisao, 
                     event = banco_acoes$decisao)

fit.coxph <- coxph(surv_object ~  assunto_regressao + estado_cnj + acordo + 
                     log(total_penal) + esfera + movimentos, 
                   data = banco_acoes)


library(survminer)

ggforest(fit.coxph, data = banco_acoes, 
         main = "Risco relativo - Todo o período") 

####

banco_acoes_2020 <- banco_acoes %>%
  filter(year(primeiro_movimento) > 2019 & year(primeiro_movimento) < 2023) %>%
  filter(ano_cnj > 2019 & ano_cnj < 2023)


surv_object <-  Surv(time = banco_acoes_2020$duracao_decisao, 
                     event = banco_acoes_2020$decisao)

fit.coxph <- coxph(surv_object ~  assunto_regressao + estado_cnj + acordo + 
                     log(total_penal) + esfera + movimentos, 
                   data = banco_acoes_2020)


ggforest(fit.coxph, data = banco_acoes_2020, 
         main = "") 


ggsave("decisao_2020.png", device = "png", width = 17, height = 22,
       units = "cm", dpi = 300, bg = "white")


ggpredict(fit.coxph, c("acordo"), type = "survival") %>% plot() +
  labs(title = "Probabilidade do processo ter decisão", y = "Probabilidade",
       x = "Dias")







###


banco_acoes_2010 <- banco_acoes %>%
  filter(year(primeiro_movimento) > 2009 & year(primeiro_movimento) < 2020) %>%
  filter(ano_cnj > 2009 & ano_cnj < 2020)


surv_object <-  Surv(time = banco_acoes_2010$duracao_decisao, 
                     event = banco_acoes_2010$decisao)

fit.coxph <- coxph(surv_object ~  assunto_regressao + estado_cnj + acordo + 
                     log(total_penal) + esfera + movimentos, 
                   data = banco_acoes_2010)


ggforest(fit.coxph, data = banco_acoes_2010, 
         main = "") 


ggsave("decisao_2010.png", device = "png", width = 17, height = 22,
       units = "cm", dpi = 300, bg = "white")


####

banco_acoes_fed <- banco_acoes %>%
  filter(esfera == "JF")

surv_object <-  Surv(time = banco_acoes_fed$duracao_decisao, 
                     event = banco_acoes_fed$decisao)

fit.coxph <- coxph(surv_object ~  assunto_regressao + estado_cnj + acordo + 
                     log(total_penal) + vesp_tipo + movimentos, 
                   data = banco_acoes_fed)


ggforest(fit.coxph, data = banco_acoes_fed, 
         main = "Risco relativo - Todo o período") 



#####

banco_acoes_fed_2010 <- banco_acoes_fed %>%
  filter(year(primeiro_movimento) > 2009 & year(primeiro_movimento) < 2020) %>%
  filter(ano_cnj > 2009 & ano_cnj < 2020)

surv_object <-  Surv(time = banco_acoes_fed_2010$duracao_decisao, 
                     event = banco_acoes_fed_2010$decisao)

fit.coxph <- coxph(surv_object ~  assunto_regressao + estado_cnj + acordo + 
                     log(total_penal) + vesp_tipo + movimentos, 
                   data = banco_acoes_fed_2010)


ggforest(fit.coxph, data = banco_acoes_fed_2010, 
         main = "") 


ggsave("decisao_fed_2010.png", device = "png", width = 17, height = 22,
       units = "cm", dpi = 300, bg = "white")

ggpredict(fit.coxph, c("acordo"), type = "survival") %>% plot() +
  labs(title = "Probabilidade do processo ter decisão", y = "Probabilidade",
       x = "Dias")

ggpredict(fit.coxph, c("vesp_tipo"), type = "survival") %>% plot() +
  labs(title = "Probabilidade do processo ter decisão", y = "Probabilidade",
       x = "Dias")

###


banco_acoes_fed_2020 <- banco_acoes_fed %>%
  filter(year(primeiro_movimento) > 2019 & year(primeiro_movimento) < 2023) %>%
  filter(ano_cnj > 2019 & ano_cnj < 2023)

surv_object <-  Surv(time = banco_acoes_fed_2020$duracao_decisao, 
                     event = banco_acoes_fed_2020$decisao)

fit.coxph <- coxph(surv_object ~  assunto_regressao + estado_cnj + acordo + 
                     log(total_penal) + vesp_tipo + movimentos, 
                   data = banco_acoes_fed_2020)


ggforest(fit.coxph, data = banco_acoes_fed_2020, 
         main = "") 


ggsave("decisao_fed_2020.png", device = "png", width = 17, height = 22,
       units = "cm", dpi = 300, bg = "white")

ggpredict(fit.coxph, c("acordo"), type = "survival") %>% plot() +
  labs(title = "Probabilidade do processo ter decisão", y = "Probabilidade",
       x = "Dias")

ggpredict(fit.coxph, c("vesp_tipo"), type = "survival") %>% plot() +
  labs(title = "Probabilidade do processo ter decisão", y = "Probabilidade",
       x = "Dias")






############################################################################
#
# Segunda regressão
#
########################################################################

library(nnet)

banco_acoes <- banco_acoes %>%
  mutate(fronteira = case_when(estado_cnj %in% c("AP", "PA", "RR", "AM", "AC",
                                                 "RO", "MT", "MS", "PR", "SC",
                                                 "RS") ~ "fronteira",
                               TRUE ~ "sem fronteira"))



regressao_multinomial <-
multinom(assunto_regressao ~ estado_cnj + esfera + fronteira + log(total_penal) +
           mortes + log(valor_transferencia), 
         data = banco_acoes, 
         model = TRUE)

library(ggstance)

coef_multinom <-
broom::tidy(regressao_multinomial, conf.int=TRUE, exp = T) 

coef_multinom %>%
  ggplot(aes(x=estimate,y=term, color = y.level))+
  geom_pointrangeh(aes(xmin=conf.low,
                       xmax=conf.high),
                   position=position_dodgev(height=0.75)) +
  facet_wrap(vars(y.level), scales = "free_x") +
  geom_vline(xintercept = 1)

####



banco_acoes_2020 <- banco_acoes %>%
  filter(year(primeiro_movimento) > 2019 & year(primeiro_movimento) < 2023) %>%
  filter(ano_cnj > 2019 & ano_cnj < 2023)


regressao_multinomial <-
  multinom(assunto_regressao ~ estado_cnj + esfera + fronteira + log(total_penal) +
             mortes + log(valor_transferencia), 
           data = banco_acoes_2020, 
           model = TRUE)

plot <-
sjPlot::plot_model(regressao_multinomial, show.p =T, show.values = T,
                   value.offset = 0.4) +
  theme_minimal() + labs(title = "") 

plot$facet$params$nrow=2
plot$facet$params$ncol=2



sjPlot::plot_grid(plot, nrow = 2)

summary(regressao_multinomial)


coef_multinom <-
  broom::tidy(regressao_multinomial, conf.int=TRUE, exp = T) 

plot <-
coef_multinom %>%
  filter(term!="(Intercept)") %>%
  ggplot(aes(x=estimate,y=term, color = y.level, label = round(estimate, 1)))+
  geom_pointrangeh(aes(xmin=conf.low,
                       xmax=conf.high),
                   position=position_dodgev(height=0.75)) +
  facet_wrap(vars(y.level), scales = "free_x") +
  geom_text(nudge_y = 0.9, nudge_x = 0.05,   check_overlap = T) +
  geom_vline(xintercept = 1, linetype = "dotdash") +
  # scale_linetype_manual(values = "dotdash") %>%
  theme_minimal() +
  theme(legend.position = "none") 

ggpreview(plot, device = "png",  width = 17, height = 20,
       units = "cm", dpi = 300, bg = "white")

ggsave("assuntos_2020.png", device = "png",  width = 17, height = 20,
          units = "cm", dpi = 300, bg = "white")


plot <- 
  coef_multinom %>%
  filter(str_detect(term,"estado")) %>%
  mutate(term = str_remove(term, "estado_cnj")) %>%
  ggplot(aes(x=estimate,y=term, color = y.level))+
  geom_pointrangeh(aes(xmin=conf.low,
                       xmax=conf.high),
                   position=position_dodgev(height=0.75)) +
  facet_wrap(vars(term), scales = "free") +
  geom_vline(xintercept = 1) +
  theme_minimal() +
  theme(legend.position = "bottom", axis.text.y=element_blank()) +
  labs(color = "Crime")

ggpreview(plot, device = "png",  width = 17, height = 20,
          units = "cm", dpi = 300, bg = "white")

ggsave("assuntos_estados_2020.png", device = "png",  width = 17, height = 20,
       units = "cm", dpi = 300, bg = "white")
###



banco_acoes_2010 <- banco_acoes %>%
  filter(year(primeiro_movimento) > 2009 & year(primeiro_movimento) < 2020) %>%
  filter(ano_cnj > 2009 & ano_cnj < 2020)



regressao_multinomial <-
  multinom(assunto_regressao ~ estado_cnj + esfera + fronteira + log(total_penal) +
             mortes + log(valor_transferencia), 
           data = banco_acoes_2010, 
           model = TRUE)



coef_multinom <-
  broom::tidy(regressao_multinomial, conf.int=TRUE, exp = T) 

plot <- 
coef_multinom %>%
  filter(term!="(Intercept)") %>%
  ggplot(aes(x=estimate,y=term, color = y.level))+
  geom_pointrangeh(aes(xmin=conf.low,
                       xmax=conf.high),
                   position=position_dodgev(height=0.75)) +
  facet_wrap(vars(y.level), scales = "free_x") +
  geom_vline(xintercept = 1) +
  theme_minimal() +
  theme(legend.position = "none") 

ggpreview(plot, device = "png",  width = 17, height = 20,
          units = "cm", dpi = 300, bg = "white")

ggsave("assuntos_2010.png", device = "png",  width = 17, height = 20,
       units = "cm", dpi = 300, bg = "white")


#

plot <- 
  coef_multinom %>%
  filter(str_detect(term,"estado")) %>%
  mutate(term = str_remove(term, "estado_cnj")) %>%
  ggplot(aes(x=estimate,y=term, color = y.level))+
  geom_pointrangeh(aes(xmin=conf.low,
                       xmax=conf.high),
                   position=position_dodgev(height=0.75)) +
  facet_wrap(vars(term), scales = "free") +
  geom_vline(xintercept = 1) +
  theme_minimal() +
  theme(legend.position = "bottom", axis.text.y=element_blank()) +
  labs(color = "Crime")

ggpreview(plot, device = "png",  width = 17, height = 20,
          units = "cm", dpi = 300, bg = "white")

ggsave("assuntos_estados_2010.png", device = "png",  width = 17, height = 20,
       units = "cm", dpi = 300, bg = "white")

####




regressao_multinomial <-
  multinom(assunto_regressao ~ estado_cnj + esfera + fronteira + log(total_penal) +
             mortes + log(valor_transferencia) + log(pib_per_capta), 
           data = banco_acoes_2010, 
           model = TRUE)



coef_multinom <-
  broom::tidy(regressao_multinomial, conf.int=TRUE, exp = T) 

coef_multinom %>%
  filter(term!="(Intercept)") %>%
  ggplot(aes(x=estimate,y=term, color = y.level))+
  geom_pointrangeh(aes(xmin=conf.low,
                       xmax=conf.high),
                   position=position_dodgev(height=0.75)) +
  facet_wrap(vars(y.level), scales = "free_x") +
  geom_vline(xintercept = 1)


##########################################################
#
# Terceira regressão
#
############################################################


reversao <- c(12254, 220, 11878, 221, 237, 198, 443, 11876, 871, 460, 1045,
              471, 451, 973, 11879, 972, 1043, 12475, 11373, 11795)
reversao_parte <- c(889, 241, 238, 12253, 240)
confirmacao <- c(239, 12252, 242, 219, 200, 235, 447, 230, 461, 236, 466, 901, 
                 12458, 456, 12319, 12459)

ids_reversao <- banco_movimentos %>%
  filter(codigo_movimentacao %in% reversao) %>%
  pull(id)

ids_reversao_parte <- banco_movimentos %>%
  filter(codigo_movimentacao %in% reversao_parte) %>%
  pull(id)

ids_confirmacao <- banco_movimentos %>%
  filter(codigo_movimentacao %in% confirmacao) %>%
  pull(id)

banco_acoes <- banco_acoes %>%
  mutate(confirmacao = case_when(id %in% ids_confirmacao ~ 1,
                              id %in% ids_reversao ~ 0,
                              id %in% ids_reversao_parte ~ 0))



regressao <-
  glm(confirmacao ~  esfera +   movimentos +
             assunto_regressao + acordo + log(total_penal) + estado_cnj, 
         data = banco_acoes, family = "binomial")

library(sjPlot)

plot_model(regressao) 

###



banco_acoes_2020 <- banco_acoes %>%
  filter(year(primeiro_movimento) > 2019 & year(primeiro_movimento) < 2023) %>%
  filter(ano_cnj > 2019 & ano_cnj < 2023)

regressao <-
  glm(confirmacao ~  esfera +   movimentos +
        assunto_regressao + acordo + log(total_penal), 
      data = banco_acoes_2020, family = "binomial")


plot_model(regressao)


###



banco_acoes_2010 <- banco_acoes %>%
  filter(year(primeiro_movimento) > 2009 & year(primeiro_movimento) < 2020) %>%
  filter(ano_cnj > 2009 & ano_cnj < 2020)

regressao <-
  glm(confirmacao ~  esfera +   movimentos +
        assunto_regressao + acordo + log(total_penal), 
      data = banco_acoes_2010, family = "binomial")


plot_model(regressao)



###

banco_acoes_fed <- banco_acoes %>%
  filter(esfera == "JF") %>%
  mutate(vesp = case_when(id %in% ids_vesp ~ "Especializada",
                          id %!in% ids_vesp ~ "Não especializada"))


regressao <-
  glm(confirmacao ~  vesp + movimentos + 
             assunto_regressao + acordo + log(total_penal) + estado_cnj, 
           data = banco_acoes_fed, 
           family = "binomial")

plot_model(regressao)

##

banco_acoes_fed_2020 <- banco_acoes_fed %>%
  filter(year(primeiro_movimento) > 2019 & year(primeiro_movimento) < 2023) %>%
  filter(ano_cnj > 2019 & ano_cnj < 2023)

regressao <-
  glm(confirmacao ~  vesp + movimentos + 
        assunto_regressao + acordo + log(total_penal), 
      data = banco_acoes_fed_2020, 
      family = "binomial")

plot_model(regressao)

table(banco_acoes_fed_2020$confirmacao, banco_acoes_fed_2020$acordo)

table(banco_acoes_fed_2020$confirmacao, banco_acoes_fed_2020$vesp)

plot_model(regressao, rm.terms = "acordo [sem acordo]")


###

banco_acoes_fed_2010 <- banco_acoes_fed %>%
  filter(year(primeiro_movimento) > 2009 & year(primeiro_movimento) < 2020) %>%
  filter(ano_cnj > 2009 & ano_cnj < 2020)

regressao <-
  glm(confirmacao ~  vesp + movimentos + 
        assunto_regressao + acordo + log(total_penal), 
      data = banco_acoes_fed_2010, 
      family = "binomial")

plot_model(regressao)
#log(total_penal) +
#acord





banco_acoes$acordo %>% table()




















################################################################

vesp_nome <- c("12ª - Brasília", "2ª - Salvador", "11ª - Goiânia", "10ª - Brasília", "4ª - Belém", "1ª - Teresina", "4ª - Manaus", "11ª - Belo Horizonte", "1ª - São Luís", "4ª - Belo Horizonte",
               "5ª - Cuiabá", "3ª VARA FEDERAL DE CAMPO GRANDE", "09ª VARA FEDERAL DE CAMPINAS COM JUIZADO ESPECIAL FEDERAL CRIMINAL ADJUNTO", "01ª VARA FEDERAL DE CAMPINAS COM JUIZADO ESPECIAL FEDERAL CRIMINAL ADJUNTO",
               "10ª VARA FEDERAL CRIMINAL DE SÃO PAULO", "10ª Vara Federal Criminal De São Paulo", "2ª VARA FEDERAL CRIMINAL DE SÃO PAULO", "4ª VARA FEDERAL DE RIBEIRÃO PRETO",
               "06ª Vara Federal Criminal De São Paulo", "5ª Vara Federal De Campo Grande", "13ª Vara Federal de Curitiba", "23ª Vara Federal de Curitiba", "14ª Vara Federal de Curitiba",
               "22ª Vara Federal de Porto Alegre", "1ª Vara Federal de Joinville", "7ª Vara Federal de Florianópolis", "7ª Vara Federal de Porto Alegre", "1ª Vara Federal de Chapecó", "1ª Vara Federal de Criciúma",
               "1ª Vara Federal de Florianópolis", "1ª Vara Federal de Itajaí", "9ª Vara Federal de Curitiba", "11ª Vara Federal de Porto Alegre", "7ª Vara Federal Criminal do Rio de Janeiro",
               "2ª Vara Federal Criminal de Vitória", "1ª Vara Federal Criminal de Vitória", "2ª Vara Federal Criminal do Rio de Janeiro", "3ª Vara Federal Criminal do Rio de Janeiro", "5ª Vara Federal Criminal do Rio de Janeiro", "11ª Vara Federal",
               "2ª Vara Federal", "32ª Vara Federal", "13ª VARA FEDERAL", "4ª VARA FEDERAL")

proc_vesp_fed <- banco_acoes %>% filter(nome_orgao %in% vesp_nome)

#Exclusoes porque as varas sem nome do TRF5 só entram se forem de um estado específico:
proc_vesp_exc <- proc_vesp_fed %>%
  filter(digito_17_18 != "PE" & nome_orgao == "13ª VARA FEDERAL" | digito_17_18 != "PE" & nome_orgao == "4ª VARA FEDERAL" | digito_17_18 != "CE" & nome_orgao == "11ª Vara Federal" |
           digito_17_18 != "CE" & nome_orgao == "32ª Vara Federal" | digito_17_18 != "RN" & nome_orgao == "2ª Vara Federal")

id_exc <- proc_vesp_exc$id

ids_vesp <-
  proc_vesp_fed <- proc_vesp_fed %>%
  filter(id %!in% id_exc) %>%
  filter(esfera == "JF") %>%
  pull(id)

banco_acoes_fed <- banco_acoes %>%
  mutate(vesp = case_when(id %in% ids_vesp ~ "Especializada",
                          id %!in% ids_vesp ~ "Não especializada")) %>%
  filter(esfera == "JF")




surv_object <-  Surv(time = banco_acoes_fed$duracao_decisao,
                     event = banco_acoes_fed$decisao)

fit.coxph <- coxph(surv_object ~  assunto_regressao + estado_cnj + acordo +
                     log(total_penal) + vesp + movimentos,
                   data = banco_acoes_fed)


ggforest(fit.coxph, data = banco_acoes_fed,
         main = "Risco relativo - Todo o período")



#####

banco_acoes_fed_2010 <- banco_acoes_fed %>%
  filter(year(primeiro_movimento) > 2009 & year(primeiro_movimento) < 2020) %>%
  filter(ano_cnj > 2009 & ano_cnj < 2020)

surv_object <-  Surv(time = banco_acoes_fed_2010$duracao_decisao,
                     event = banco_acoes_fed_2010$decisao)

fit.coxph <- coxph(surv_object ~  assunto_regressao + estado_cnj + acordo +
                     log(total_penal) + vesp + movimentos,
                   data = banco_acoes_fed_2010)


ggforest(fit.coxph, data = banco_acoes_fed_2010,
         main = "Risco relativo - 2014 a 2019 ")


###


banco_acoes_fed_2020 <- banco_acoes_fed %>%
  filter(year(primeiro_movimento) > 2019 & year(primeiro_movimento) < 2023) %>%
  filter(ano_cnj > 2019 & ano_cnj < 2023)

surv_object <-  Surv(time = banco_acoes_fed_2020$duracao_decisao,
                     event = banco_acoes_fed_2020$decisao)

fit.coxph <- coxph(surv_object ~  assunto_regressao + estado_cnj + acordo +
                     log(total_penal) + vesp + movimentos,
                   data = banco_acoes_fed_2020)


ggforest(fit.coxph, data = banco_acoes_fed_2020,
         main = "Risco relativo - 2020 e 2021 ")


#####################################################################

banco_acoes_2010 %>%
  ggplot(aes(fct_infreq(instancia))) +
  geom_bar() +
  theme_minimal() +
  coord_flip() +
  labs(x = "Contagem", y = "Instância")

banco_acoes_2020 %>%
  ggplot(aes(fct_infreq(instancia))) +
  geom_bar() +
  theme_minimal() +
  coord_flip()



banco_assuntos <- read_csv("banco_assuntos_consolidados.csv") %>%
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

banco_assuntos %>%
  filter(id %in% banco_acoes_2010$id) %>%
  count(id) %>%
  ggplot(aes(n)) +
  geom_bar() +
  theme_minimal() +
  labs(x = "Número de assuntos por ação", y = "Contagem") +
  xlim(0.5, 5)

banco_assuntos %>%
  filter(id %in% banco_acoes_2020$id) %>%
  count(id) %>%
  ggplot(aes(n)) +
  geom_bar() +
  theme_minimal() +
  labs(x = "Número de assuntos por ação", y = "Contagem") +
  xlim(0.5, 5)
  


banco_assuntos %>%
  filter(id %in% banco_acoes_2010$id) %>%
  count(assunto_consolidado, sort = T) %>%
  top_n(30) %>%
  mutate(assunto_consolidado = fct_reorder(assunto_consolidado, n)) %>%
  ggplot(aes(n, assunto_consolidado)) +
  geom_col() +
  theme_minimal() 


banco_assuntos %>%
  filter(id %in% banco_acoes_2020$id) %>%
  count(assunto_consolidado, sort = T) %>%
  top_n(30) %>%
  mutate(assunto_consolidado = fct_reorder(assunto_consolidado, n)) %>%
  ggplot(aes(n, assunto_consolidado)) +
  geom_col() +
  theme_minimal() 

banco_acoes
