library(tidyverse)
library(readr)
library(ggthemes)

`%!in%` <- Negate(`%in%`)

options(scipen = 999)

#setwd("C:/Users/gabri/OneDrive/Documents/[CNJ - Insper]/N?cleo Quantitativo/Dados Datajud")
#setwd("C:\\Google Drive\\cnj")

banco_acoes <- read_csv("acoes_consolidado (1).csv")

banco_acoes <- banco_acoes %>%
  mutate(periodo = case_when(ano_cnj > 2019 & ano_cnj < 2023 ~ "2020-2022",
                             ano_cnj > 2009 & ano_cnj < 2020 ~ "2010-2019"))

banco_acoes %>%
  count(instancia, periodo) %>%
  mutate(instancia = fct_reorder(instancia, n)) %>%
  ggplot(aes(n, instancia)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  facet_wrap(vars(periodo), scale = "free_x")

ggsave("Graficos\\grafico_instancia.png", device = "png", bg = "white",
       dpi = 300, width = 14, height = 7, units = "cm")

###############################################################################

id_acoes <- banco_acoes %>% pull(id)

id_2020 <- banco_acoes %>%
  filter(periodo == "2020-2022") %>%
  pull(id)

id_2010 <- banco_acoes %>%
  filter(periodo == "2010-2019") %>%
  pull(id)

banco_assuntos <- read_csv("assuntos.csv") %>%
  filter(id %in% id_acoes) %>%
  mutate(periodo = case_when(id %in% id_2020 ~ "2020-2022",
                             id %in% id_2010 ~ "2010-2019"))

banco_assuntos %>%
  count(id, periodo) %>%
  ggplot(aes(n)) +
  geom_bar() +
  theme_minimal() +
  labs(x = "Número de assuntos por ação", y = "Contagem") +
  scale_x_continuous(limits = c(0.5,5), breaks = 1:5) +
  facet_wrap(vars(periodo), ncol = 1, scales = "free_y")

ggsave("Graficos\\grafico_n_assuntos.png", device = "png", bg = "white",
       dpi = 300, width = 14, height = 10, units = "cm")

###############################################################################

banco_assuntos <- banco_assuntos %>%
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
  mutate(id_assunto = 1:nrow(banco_assuntos))

banco_assuntos %>%
  count(dpj_nomeAssuntoNacional)

banco_assuntos %>%
  filter(assunto_consolidado %in% c("Corrupçao ativa",
                                    "Lavagem ou ocultaçao de bens, direitos ou valores oriundos de corrupçao",
                                    "Lavagem ou ocultaçao de bens, direitos ou valores",
                                    "Corrupçao passiva",
                                    "Peculato",
                                    "Concussão",
                                    "Trafico de influencia",
                                    "Afastamento do cargo",
                                    "Improbidade administrativa",
                                    "Indisponibilidade de bens",
                                    "Prevaricaçao",
                                    "Crimes contra a ordem tributaria",
                                    "Perda ou suspensao de direitos politicos",
                                    "Contrabando ou descaminho",
                                    "Crimes da lei de licitaçoes",
                                    "Emprego irregular de verbas ou rendas publicas",
                                    "Excesso de exaçao",
                                    "Condescendência criminosa",
                                    "Advocacia administrativa",
                                    "Violaçao do sigilo de proposta de concorrencia",
                                    "Impedimento, perturbação ou fraude de concorrencia",
                                    "Inserçao de dados falsos em sistema de informaçao",
                                    "Sonegaçao de contribuiçao previdenciaria",
                                    "Crimes praticados por particular contra a administraçao publica estrangeira",
                                    "Crimes contra as finanças publicas",
                                    "Peculato mediante erro de outrem")) %>%
  count(assunto_consolidado, periodo) %>%
  mutate(assunto_consolidado = fct_reorder(assunto_consolidado, n)) %>%
  ggplot(aes(n, assunto_consolidado)) +
  geom_col() +
  theme_minimal() +
  labs(x = "Contagem", y = "Assuntos") +
  facet_wrap(vars(periodo), ncol = 1, scales = "free") +
  theme(axis.text=element_text(size=6))

ggsave("Graficos\\grafico_assuntos_principais.png", device = "png", bg = "white",
       dpi = 300, width = 14, height = 20, units = "cm")

###############################################################################

banco_assuntos %>%
filter(periodo == "2020-2022") %>%
count(assunto_consolidado, periodo, sort = T) %>%
  filter(!is.na(assunto_consolidado)) %>%
  top_n(90) %>%
  mutate(assunto_consolidado = fct_reorder(assunto_consolidado, n)) %>%
  ggplot(aes(n, assunto_consolidado)) +
  geom_col() +
  theme_minimal() +
  labs(x = "Contagem", y = "Assuntos") +
  facet_wrap(vars(periodo), ncol = 1, scales = "free") +
  theme(axis.text=element_text(size=6))

ggsave("Graficos\\grafico_assuntos_todos_2020.png", device = "png", bg = "white",
       dpi = 300, width = 14, height = 21, units = "cm")

################################################################################


banco_assuntos %>%
  filter(periodo == "2010-2019") %>%
  count(assunto_consolidado, periodo, sort = T) %>%
  filter(!is.na(assunto_consolidado)) %>%
  top_n(90) %>%
  mutate(assunto_consolidado = fct_reorder(assunto_consolidado, n)) %>%
  ggplot(aes(n, assunto_consolidado)) +
  geom_col() +
  theme_minimal() +
  labs(x = "Contagem", y = "Assuntos") +
  facet_wrap(vars(periodo), ncol = 1, scales = "free") +
  theme(axis.text=element_text(size=6))

ggsave("Graficos\\grafico_assuntos_todos_2010.png", device = "png", bg = "white",
       dpi = 300, width = 14, height = 22, units = "cm")


###############################################################################

banco_acoes %>%
  filter(periodo == "2010-2019") %>%
  count(tribunal_cnj, periodo, sort = T) %>%
  # top_n(80) %>%
  mutate(tribunal_cnj = fct_reorder(tribunal_cnj, n)) %>%
  ggplot(aes(n, tribunal_cnj)) +
  geom_col() +
  theme_minimal() +
  labs(x = "Contagem", y = "Tribunal") +
  facet_wrap(vars(periodo), ncol = 1, scales = "free") 

ggsave("Graficos\\grafico_tribunais_2010.png", device = "png", bg = "white",
       dpi = 300, width = 14, height = 23, units = "cm")

###############################################################################

banco_acoes %>%
  filter(periodo == "2020-2022") %>%
  count(tribunal_cnj, periodo, sort = T) %>%
  # top_n(80) %>%
  mutate(tribunal_cnj = fct_reorder(tribunal_cnj, n)) %>%
  ggplot(aes(n, tribunal_cnj)) +
  geom_col() +
  theme_minimal() +
  labs(x = "Contagem", y = "Tribunal") +
  facet_wrap(vars(periodo), ncol = 1, scales = "free") 

ggsave("Graficos\\grafico_tribunais_2020.png", device = "png", bg = "white",
       dpi = 300, width = 14, height = 23, units = "cm")

###############################################################################


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
                                tribunal_cnj == "TRF1" & is.na(digito_17_18) ~ "TRF1",
                                tribunal_cnj == "TRF2" & is.na(digito_17_18) ~ "TRF2",
                                tribunal_cnj == "TRF3" & is.na(digito_17_18) ~ "TRF3",
                                tribunal_cnj == "TRF4" & is.na(digito_17_18) ~ "TRF4",
                                tribunal_cnj == "TRF5" & is.na(digito_17_18) ~ "TRF5")) %>%
  mutate(regiao = case_when(estado_cnj %in% c("RS", "SC", "PR") ~ "Sul",
                            estado_cnj %in% c("SP", "RJ", "MG", "ES") ~ "Sudeste",
                            estado_cnj %in% c("AC", "AM", "AP", "PA", "RO", 
                                                "RR", "TO") ~ "Norte",
                            estado_cnj %in% c("AL", "BA", "CE", "MA", "PI", 
                                                "PE", "PB", "RN", "SE") ~ "Nordeste",
                            estado_cnj %in% c("MT", "MS", "GO", "DF") ~ "Centro-Oeste"))


banco_acoes %>%
  count(regiao, periodo, sort = T) %>%
  filter(!is.na(regiao)) %>%
  mutate(regiao = fct_reorder(regiao, n)) %>%
  ggplot(aes(n, regiao)) + 
  geom_col() +
  theme_minimal() +
  labs(x = "Contagem", y = "Região") +
  facet_wrap(vars(periodo), ncol = 1, scales = "free") 


ggsave("Graficos\\grafico_regiao.png", device = "png", bg = "white",
       dpi = 300, width = 14, height = 10, units = "cm")

###############################################################################

banco_acoes <-
banco_acoes %>%
  mutate(justica = case_when(digito_14 == "JEST" ~ "Estadual",
                             digito_14 == "JF" ~ "Federal",
                             digito_14 == "JELEL" ~ "Eleitoral",
                             digito_14 %in% c("JME", "JMU") ~ "Militar",
                             digito_14 == "JT" ~ "Trabalhista"))

banco_acoes %>%
  count(justica, periodo, sort = T) %>%
  filter(!is.na(justica)) %>%
  mutate(justica = fct_reorder(justica, n)) %>%
  ggplot(aes(n, justica)) +
  geom_col() +
  theme_minimal() +
  labs(x = "Contagem", y = "Justiça") +
  facet_wrap(vars(periodo), ncol = 1, scales = "free") 

ggsave("Graficos\\grafico_justica.png", device = "png", bg = "white",
       dpi = 300, width = 14, height = 10, units = "cm")

###############################################################################

banco_acoes %>%
  # filter(!is.na(justica),
  #        !is.na(regiao)) %>%
  count(justica, regiao, periodo) %>%
  ggplot(aes(regiao, n, fill = justica)) +
  geom_bar(position = "fill", stat = "identity") +
  theme_minimal() +
  labs(x = "Região", y = "Proporção") +
  facet_wrap(vars(periodo), ncol = 1, scales = "free") +
  scale_fill_ptol()

ggsave("Graficos\\grafico_justica_regiao.png", device = "png", bg = "white",
       dpi = 300, width = 14, height = 14, units = "cm")

###############################################################################

codificação_movimentos <- read_csv("movimentos_datajud.csv")

banco_movimentos <- read_csv("movimentos_consolidado.csv") %>%
  filter(id %in% id_acoes) %>%
  left_join(codificação_movimentos) %>%
  mutate(periodo = case_when(id %in% id_2020 ~ "2020-2022",
                             id %in% id_2010 ~ "2010-2019"))

banco_movimentos %>%
  filter(periodo == "2020-2022") %>%
  count(descricao_movimentacao, periodo, sort = T) %>%
  filter(!is.na(descricao_movimentacao)) %>%
  top_n(80) %>%
  mutate(descricao_movimentacao = fct_reorder(descricao_movimentacao, n)) %>%
  ggplot(aes(n, descricao_movimentacao)) +
  geom_col() +
  theme_minimal() +
  labs(x = "Contagem", y = "Movimentação") +
  facet_wrap(vars(periodo), ncol = 1, scales = "free") +
  theme(axis.text=element_text(size=6))

ggsave("Graficos\\grafico_movimentos_2020.png", device = "png", bg = "white",
       dpi = 300, width = 14, height = 23, units = "cm")


###############################################################################

banco_movimentos %>%
  filter(periodo == "2010-2019") %>%
  count(descricao_movimentacao, periodo, sort = T) %>%
  filter(!is.na(descricao_movimentacao)) %>%
  top_n(80) %>%
  mutate(descricao_movimentacao = fct_reorder(descricao_movimentacao, n)) %>%
  ggplot(aes(n, descricao_movimentacao)) +
  geom_col() +
  theme_minimal() +
  labs(x = "Contagem", y = "Movimentação") +
  facet_wrap(vars(periodo), ncol = 1, scales = "free") +
  theme(axis.text=element_text(size=6))

ggsave("Graficos\\grafico_movimentos_2019.png", device = "png", bg = "white",
       dpi = 300, width = 14, height = 23, units = "cm")

###############################################################################

rm(banco_movimentos)
gc()

id_ativa <- banco_assuntos %>% 
  filter(assunto_consolidado == "Corrupçao ativa") %>%
  pull(id)

banco_acoes %>%
  filter(periodo == "2010-2019",
         id %in% id_ativa) %>%
  count(tribunal_cnj, periodo, sort = T) %>%
  # top_n(80) %>%
  mutate(tribunal_cnj = fct_reorder(tribunal_cnj, n)) %>%
  ggplot(aes(n, tribunal_cnj)) +
  geom_col() +
  theme_minimal() +
  labs(x = "Contagem", y = "Tribunal") +
  facet_wrap(vars(periodo), ncol = 1, scales = "free") 

ggsave("Graficos\\grafico_tribunais_2010_ativa.png", device = "png", bg = "white",
       dpi = 300, width = 14, height = 23, units = "cm")

###############################################################################

banco_acoes %>%
  filter(periodo == "2020-2022",
         id %in% id_ativa) %>%
  count(tribunal_cnj, periodo, sort = T) %>%
  # top_n(80) %>%
  mutate(tribunal_cnj = fct_reorder(tribunal_cnj, n)) %>%
  ggplot(aes(n, tribunal_cnj)) +
  geom_col() +
  theme_minimal() +
  labs(x = "Contagem", y = "Tribunal") +
  facet_wrap(vars(periodo), ncol = 1, scales = "free") 

ggsave("Graficos\\grafico_tribunais_2020_ativa.png", device = "png", bg = "white",
       dpi = 300, width = 14, height = 23, units = "cm")

###############################################################################

id_passiva <- banco_assuntos %>% 
  filter(assunto_consolidado == "Corrupçao passiva") %>%
  pull(id)

banco_acoes %>%
  filter(periodo == "2010-2019",
         id %in% id_passiva) %>%
  count(tribunal_cnj, periodo, sort = T) %>%
  # top_n(80) %>%
  mutate(tribunal_cnj = fct_reorder(tribunal_cnj, n)) %>%
  ggplot(aes(n, tribunal_cnj)) +
  geom_col() +
  theme_minimal() +
  labs(x = "Contagem", y = "Tribunal") +
  facet_wrap(vars(periodo), ncol = 1, scales = "free") 

ggsave("Graficos\\grafico_tribunais_2010_passiva.png", device = "png", bg = "white",
       dpi = 300, width = 14, height = 23, units = "cm")

###############################################################################

banco_acoes %>%
  filter(periodo == "2020-2022",
         id %in% id_passiva) %>%
  count(tribunal_cnj, periodo, sort = T) %>%
  # top_n(80) %>%
  mutate(tribunal_cnj = fct_reorder(tribunal_cnj, n)) %>%
  ggplot(aes(n, tribunal_cnj)) +
  geom_col() +
  theme_minimal() +
  labs(x = "Contagem", y = "Tribunal") +
  facet_wrap(vars(periodo), ncol = 1, scales = "free") 

ggsave("Graficos\\grafico_tribunais_2020_passiva.png", device = "png", bg = "white",
       dpi = 300, width = 14, height = 23, units = "cm")

###############################################################################

id_lavagem <- banco_assuntos %>% 
  filter(str_detect(assunto_consolidado, "Lavagem")) %>%
  pull(id)

banco_acoes %>%
  filter(periodo == "2010-2019",
         id %in% id_lavagem) %>%
  count(tribunal_cnj, periodo, sort = T) %>%
  # top_n(80) %>%
  mutate(tribunal_cnj = fct_reorder(tribunal_cnj, n)) %>%
  ggplot(aes(n, tribunal_cnj)) +
  geom_col() +
  theme_minimal() +
  labs(x = "Contagem", y = "Tribunal") +
  facet_wrap(vars(periodo), ncol = 1, scales = "free") 

ggsave("Graficos\\grafico_tribunais_2010_lavagem.png", device = "png", bg = "white",
       dpi = 300, width = 14, height = 23, units = "cm")

###############################################################################

banco_acoes %>%
  filter(periodo == "2020-2022",
         id %in% id_lavagem) %>%
  count(tribunal_cnj, periodo, sort = T) %>%
  # top_n(80) %>%
  mutate(tribunal_cnj = fct_reorder(tribunal_cnj, n)) %>%
  ggplot(aes(n, tribunal_cnj)) +
  geom_col() +
  theme_minimal() +
  labs(x = "Contagem", y = "Tribunal") +
  facet_wrap(vars(periodo), ncol = 1, scales = "free") 

ggsave("Graficos\\grafico_tribunais_2020_lavagem.png", device = "png", bg = "white",
       dpi = 300, width = 14, height = 23, units = "cm")

################################################################################

id_passiva_ativa <- c(id_passiva, id_ativa)

banco_assuntos %>%
  filter(id %in% id_passiva_ativa,
         periodo == "2020-2022") %>%
  count(assunto_consolidado, periodo, sort = T) %>%
  filter(!is.na(assunto_consolidado)) %>%
  top_n(60) %>%
  mutate(assunto_consolidado = fct_reorder(assunto_consolidado, n)) %>%
  ggplot(aes(n, assunto_consolidado)) +
  geom_col() +
  theme_minimal() +
  labs(x = "Contagem", y = "Assuntos") +
  facet_wrap(vars(periodo), ncol = 1, scales = "free") +
  theme(axis.text=element_text(size=6))

ggsave("Graficos\\grafico_assuntos_passiva_ativa_2020.png", device = "png", bg = "white",
       dpi = 300, width = 14, height = 20, units = "cm")


################################################################################
  


banco_assuntos %>%
  filter(id %in% id_passiva_ativa,
         periodo == "2010-2019") %>%
  count(assunto_consolidado, periodo, sort = T) %>%
  filter(!is.na(assunto_consolidado)) %>%
  top_n(60) %>%
  mutate(assunto_consolidado = fct_reorder(assunto_consolidado, n)) %>%
  ggplot(aes(n, assunto_consolidado)) +
  geom_col() +
  theme_minimal() +
  labs(x = "Contagem", y = "Assuntos") +
  facet_wrap(vars(periodo), ncol = 1, scales = "free") +
  theme(axis.text=element_text(size=6))

ggsave("Graficos\\grafico_assuntos_passiva_ativa_2010.png", device = "png", bg = "white",
       dpi = 300, width = 14, height = 20, units = "cm")

###############################################################################

banco_assuntos %>%
  filter(id %in% id_lavagem,
         periodo == "2020-2022") %>%
  count(assunto_consolidado, periodo, sort = T) %>%
  filter(!is.na(assunto_consolidado)) %>%
  top_n(60) %>%
  mutate(assunto_consolidado = fct_reorder(assunto_consolidado, n)) %>%
  ggplot(aes(n, assunto_consolidado)) +
  geom_col() +
  theme_minimal() +
  labs(x = "Contagem", y = "Assuntos") +
  facet_wrap(vars(periodo), ncol = 1, scales = "free") +
  theme(axis.text=element_text(size=6))

ggsave("Graficos\\grafico_assuntos_lavagem_2020.png", device = "png", bg = "white",
       dpi = 300, width = 14, height = 20, units = "cm")


################################################################################



banco_assuntos %>%
  filter(id %in% id_lavagem,
         periodo == "2010-2019") %>%
  count(assunto_consolidado, periodo, sort = T) %>%
  filter(!is.na(assunto_consolidado)) %>%
  top_n(60) %>%
  mutate(assunto_consolidado = fct_reorder(assunto_consolidado, n)) %>%
  ggplot(aes(n, assunto_consolidado)) +
  geom_col() +
  theme_minimal() +
  labs(x = "Contagem", y = "Assuntos") +
  facet_wrap(vars(periodo), ncol = 1, scales = "free") +
  theme(axis.text=element_text(size=6))

ggsave("Graficos\\grafico_assuntos_lavagem_2010.png", device = "png", bg = "white",
       dpi = 300, width = 14, height = 20, units = "cm")