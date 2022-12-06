###REVERSAO DA SENTENÇA###


###MOVIMENTOS CORRIGIDOS

reversao <- c(219, 11878, 237, 198, 443, 12254, 11876, 460, 1045, 471, 973, 11879, 972, 1043, 12475, 11373, 11795)
reversao_parte <- c(871, 451, 889, 12253, 241, 240)
confirmacao <- c(239, 220, 235, 200, 447, 230, 461, 236, 12252, 466, 901, 242, 12458, 456, 12319, 12459)

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



###2020 - INSTANCIA REV OU ESP

banco_acoes_2020_REV <- banco_acoes %>%
  filter(year(primeiro_movimento) > 2019 & year(primeiro_movimento) < 2023) %>%
  filter(ano_cnj > 2019 & ano_cnj < 2023) %>% filter(instancia == "REV" | instancia == "SUP")

regressao <- 
  glm(confirmacao ~  esfera + 
        assunto_regressao + acordo + movimentos + log(total_penal) + valor_transferencia,
      data = banco_acoes_2020_REV, family = "binomial")


sjPlot::plot_model(regressao, show.values=TRUE, value.offset = 0.4) + ylim(0,2) +
  theme_minimal() 


ggsave("reversao_2020.png", device = "png", width = 17, height = 10,
       units = "cm", dpi = 300, bg = "white")

###2010 - INSTANCIA REV OU ESP

banco_acoes_2010_REV <- banco_acoes %>%
  filter(year(primeiro_movimento) > 2009 & year(primeiro_movimento) < 2020) %>%
  filter(ano_cnj > 2009 & ano_cnj < 2020) %>% filter(instancia == "REV" | instancia == "SUP")

regressao <-
  glm(confirmacao ~  esfera + 
        assunto_regressao + acordo + movimentos +log(total_penal) + valor_transferencia, 
      data = banco_acoes_2010_REV, family = "binomial")

summary(regressao)

sjPlot::plot_model(regressao, show.values=TRUE, value.offset = 0.4) + ylim(0,2) +
  theme_minimal() 


ggsave("reversao_2010.png", device = "png", width = 17, height = 10,
       units = "cm", dpi = 300, bg = "white")


###FED 

banco_acoes_fed <- banco_acoes %>%
  # mutate(vesp = case_when(id %in% ids_vesp ~ "Especializada",
  #                         id %!in% ids_vesp ~ "N?o especializada")) %>%
  filter(esfera == "JF")


##FED - 2020 - 2022

banco_acoes_fed_2020 <- banco_acoes_fed %>%
  filter(year(primeiro_movimento) > 2019 & year(primeiro_movimento) < 2023) %>%
  filter(ano_cnj > 2019 & ano_cnj < 2023)
regressao <-
  glm(confirmacao ~  vesp_tipo + movimentos + 
        assunto_regressao + acordo + log(total_penal) + valor_transferencia, 
      data = banco_acoes_fed_2020, 
      family = "binomial")

sjPlot::plot_model(regressao, show.values=TRUE, value.offset = 0.4) + ylim(0,2) +
  theme_minimal() 


ggsave("reversao_fed_2020.png", device = "png", width = 17, height = 10,
       units = "cm", dpi = 300, bg = "white")

ggeffect(regressao, "vesp_tipo") %>% plot() + labs(title = "",
                                                   y = "Probabilidade de confirmação",
                                                   x = "Tipo de vara")

###FED-2010-2019

banco_acoes_fed_2010 <- banco_acoes_fed %>%
  filter(year(primeiro_movimento) > 2009 & year(primeiro_movimento) < 2020) %>%
  filter(ano_cnj > 2009 & ano_cnj < 2020)

regressao <-
  glm(confirmacao ~  vesp_tipo + movimentos + 
        assunto_regressao + acordo + log(total_penal) + valor_transferencia, 
      data = banco_acoes_fed_2010, 
      family = "binomial")

sjPlot::plot_model(regressao, show.values=TRUE, value.offset = 0.4) + ylim(0,2) +
  theme_minimal() 


ggsave("reversao_fed_2010.png", device = "png", width = 17, height = 10,
       units = "cm", dpi = 300, bg = "white")