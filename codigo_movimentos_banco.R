library(tidyverse)
library(rjson)
library(lubridate)
library(data.table)
library(RcppSimdJson)

options(scipen = 999)

`%!in%` <- Negate(`%in%`)

na_null <- function(x) if (is.null(x)) NA else x


setwd("C:\\Users\\rodri\\Downloads\\datajud")


# listviewer::jsonedit(fload(paste0("json/", substr(ids, 1, 1), "/", ids, ".json")))

id <- i
#525726
#526008
#526011
#526024
#526043
#966228
#971582
#971583 


i <- 1

while(id <= 1089552){
  
  init <- Sys.time()
  
  banco_movimentos <- data.table()
  
  
  files <- paste0("json/", substr(id, 1, 1), "/", id, ".json")
  
  dataJud <- map(files, ~ fload(.x, 
                                max_simplify_lvl = "list",
                                keep_temp_files = F
  ))
  
  movimentos <- list()
  

  # for(i in 1:length(dataJud[[1]]$movimento)) {
    
    
    movimentos[[length(movimentos)+1]] <- 
      data.table(id = id,

                              identificadorMovimento = map_chr(
                                dataJud[[i]][["movimento"]],
                                ~ na_null(.x$identificadorMovimento)),
                 
                               descricaoComplemento = map_chr(
                                 dataJud[[i]][["movimento"]],
                                 ~ na_null(.x$complementoNacional$descricaoComplemento)),

                              codComplementoTabelado = map_chr(
                                 dataJud[[i]][["movimento"]],
                                 ~ na_null(.x$complementoNacional$codComplementoTabelado)),

                             
                             codComplemento = map_chr(
                               dataJud[[i]][["movimento"]],
                               ~ na_null(.x$complementoNacional$codComplemento)),

                              dataHora = map_chr(
                                dataJud[[i]][["movimento"]],
                                ~ na_null(.x$dataHora)),

                              instancia = map_chr(
                                dataJud[[i]][["movimento"]],
                                ~ na_null(.x$orgaoJulgador$instancia)),
                             
                              codigoMunicipioIBGE = map_chr(
                                dataJud[[i]][["movimento"]],
                                ~ na_null(.x$orgaoJulgador$codigoMunicipioIBGE)),
                             
                              nivelSigilo = map_chr(
                                dataJud[[i]][["movimento"]],
                                ~ na_null(.x$nivelSigilo)),
                             
                              dpj_DataHora = map_chr(
                                dataJud[[i]][["movimento"]],
                                ~ na_null(.x$dpj_DataHora)),
                             
                             codigoNacional = map_chr(
                               dataJud[[i]][["movimento"]],
                               ~ na_null(.x$movimentoNacional$codigoNacional)),
                 
                 codigoPaiNacional = map_chr(
                   dataJud[[i]][["movimento"]],
                   ~ na_null(.x$movimentoLocal$codigoPaiNacional)),
                 
                 descricao = map_chr(
                   dataJud[[i]][["movimento"]],
                   ~ na_null(.x$movimentoLocal$descricao)),
                 
                             dpj_codigoMovimentoNacional = map_chr(
                               dataJud[[i]][["movimento"]],
                               ~ na_null(.x$dpj_codigoMovimentoNacional)),
                
                  # variavel esquisita
                 codigoMovimento = map_chr(
                   dataJud[[i]][["movimento"]],
                   ~ na_null(.x$movimentoLocal$codigoMovimento)),
                 
                             complemento = map_chr(
                               dataJud[[i]][["movimento"]],
                               ~ na_null(.x$movimentoNacional$complemento[[1]]))
                 
                 
    )
    
    banco_movimentos <- bind_rows(banco_movimentos, movimentos) %>%
      distinct()
    
    teste <-
    banco_movimentos %>%
      filter(!is.na(codigoNacional) | !is.na(dpj_codigoMovimentoNacional) |
                                               !is.na(codigoMovimento))
    
    if(nrow(teste)<1 & !is.null(dataJud[[i]][["movimento"]])) {
      stop("MOVIMENTO NAO IDENTIFICADO")
    }
    
    # banco_movimentos <- rbindlist(list(banco_movimentos,
    #                                    movimentos),
    #                               use.names = T,
    #                               fill = T)
    
    print(id)
    
  # }
  # movimentos <- list()
  # 
  # for(i in 1:length(dataJud)) {
    
    # assign(paste0("assuntos_", i),
    #        rbindlist(dataJud[[i]]$dadosBasicos$assunto, 
    #                  use.names = T,
    #                  fill = T) %>%
    #          distinct() %>% 
    #          mutate(id = ids[i]))
    # for(l in 1:length(dataJud[[i]]$movimento)){
    # 
    # 
    # movimentos <- movimentos %>% 
    #   append(bind_ro(dataJud[[i]]$movimento[l],
    #                            use.names = T,
    #                            fill = T) %>%
    #   distinct() %>%
    #   mutate(id = ids[i]))
      # unnest_auto(complementoNacional) %>%
      # mutate(movimentoNacional = map(movimentoNacional, as.character)) %>%
      # unnest_auto(movimentoNacional)
    
    
    
    # assuntos <- rbindlist(dataJud[[i]]$dadosBasicos$assunto, 
    #                       use.names = T,
    #                       fill = T) %>%
    #   distinct() %>% 
    #   mutate(id = ids[i])
    
    # banco_assuntos <- rbindlist(list(banco_assuntos, assuntos), use.names = T, 
    #                             fill = T)
#   }
#   
  
banco_movimentos %>%
  mutate(movimentacao = 1:nrow(banco_movimentos)) %>%
    fwrite(file = "movimentos_definitivo.csv", append = T)
  
  # print(lenght(movimentos))
  
  rm(dataJud)
  rm(banco_movimentos)
  rm(movimentos)
  gc()
#   
#   
#   movimentos <- rbindlist(movimentos,
#                         use.names = T,
#                         fill = T)
#   
#   banco_movimentos <- rbindlist(list(banco_movimentos, movimentos), 
#                               use.names = T,
#                               fill = T)
#   
#   
#   rm(movimentos)
#   gc()
#   
  id <- id + 1

  # if(1089552 %in% ids){
  #   ids <- c(min(ids):1089552)
  # }

  final <- Sys.time()

  print(final - init)




}



id_acoes <- read_csv("C:\\Google Drive\\cnj\\acoes_consolidado (1).csv") %>%
  pull(id) 

fread("movimentos_definitivo.csv") %>%
  filter(id %in% id_acoes) %>%
  fwrite(file = "movimentos_consolidado.csv")

banco_consolidado <- 
read_csv_chunked("movimentos_definitivo.csv",
                 callback = DataFrameCallback$new(function(x, pos) subset(x, id %in% id_acoes)))

banco_consolidado %>% fwrite(file = "movimentos_consolidado.csv")


library(naniar)

gg_miss_var(banco_movimentos)



banco_movimentos %>%
  filter(!is.na(codigoNacional) & !is.na(dpj_codigoMovimentoNacional)) %>%
  select(codigoNacional, dpj_codigoMovimentoNacional)

banco_movimentos %>%
  filter(!is.na(dpj_codigoMovimentoNacional) & !is.na(codigoPaiNacional)) %>%
  select(dpj_codigoMovimentoNacional, codigoPaiNacional)

banco_movimentos <- banco_movimentos %>%
  mutate(codigo_movimentacao = case_when(!is.na(codigoNacional) ~ codigoNacional,
                                         is.na(codigoNacional) &
                                                 !is.na(dpj_codigoMovimentoNacional) ~ dpj_codigoMovimentoNacional,
                                         is.na(codigoNacional) &
                                           is.na(dpj_codigoMovimentoNacional) & 
                                           !is.na(codigoPaiNacional) ~ codigoPaiNacional))
banco_movimentos <- 
banco_movimentos %>%
  write_csv("movimentos_consolidado.csv")

movimentos_unicos <- banco_movimentos %>%
  count(id) %>%
  filter(n == 1) %>%
  pull(id)

ultimas <- banco_movimentos %>%
  group_by(id) %>%
  slice_max(dpj_DataHora, n = 10) %>%
  ungroup() %>%
  count(codigo_movimentacao, sort = T)

ultimas %>%
  top_n(40) %>%
  mutate(descricao_movimentacao = case_when(codigo_movimentacao == 60 ~ "Expedição de documento",
                                            codigo_movimentacao == 581 ~ "Juntada - Documento",
                                            codigo_movimentacao == 85 ~ "Juntada - Petição",
                                            codigo_movimentacao == 132 ~ "Perícia - Recebimento",
                                            codigo_movimentacao == 51 ~ "Conclusão",
                                            codigo_movimentacao == 123 ~ "Remessa",
                                            codigo_movimentacao == 11383 ~ "Ato ordinatório",
                                            codigo_movimentacao == 22 ~ "Baixa definitiva",
                                            codigo_movimentacao == 11010 ~ "Despacho - Mero expediente",
                                            codigo_movimentacao == 246 ~ "Arquivamento - Definitivo",
                                            codigo_movimentacao == 493 ~ "Entrega em carga/vista",
                                            codigo_movimentacao == 92 ~ "Publicação",
                                            codigo_movimentacao == 848 ~ "Trânsito em julgado",
                                            codigo_movimentacao == 26 ~ "Distribuição",
                                            codigo_movimentacao == 1051 ~ "Decurso de prazo",
                                            codigo_movimentacao == 12266 ~ "Intimação eletrônica confirmada",
                                            codigo_movimentacao == 12265 ~ "Intimação eletrônica expedida/certificada",
                                            codigo_movimentacao == 1061 ~ "Disponibilização no Diário da Justiçã Eletrônico",
                                            codigo_movimentacao == 118 ~ "Protocolo de Petição",
                                            codigo_movimentacao == 36 ~ "Redistribuição",
                                            codigo_movimentacao == 48 ~ "Escrivão/Diretor de Secretaria/Secretário Jurídico",
                                            codigo_movimentacao == 982 ~ "Remessa",
                                            codigo_movimentacao == 106 ~ "Oficial de justiça - Devolução - Mandado",
                                            codigo_movimentacao == 1063 ~ "Determinação de arquivamento de procedimentos investigatórios",
                                            codigo_movimentacao == 12164 ~ "Outras decisões",
                                            codigo_movimentacao == 12263 ~ "Intimação",
                                            codigo_movimentacao == 985 ~ "Oficial de justiça - Recebimento - Mandado",
                                            codigo_movimentacao == 10966 ~ "Mudança de Classe Processual",
                                            codigo_movimentacao == 391 ~ "Decisão - Recebimento - Denúncia",
                                            codigo_movimentacao == 12282 ~ "Comunicação eletrônica expedida/certificada",
                                            codigo_movimentacao == 970 ~ "Audiência",
                                            codigo_movimentacao == 12293 ~ "Ato cumprido pela parte ou interessado (sem atributo)",
                                            codigo_movimentacao == 12142 ~ "Mudança de Parte",
                                            codigo_movimentacao == 239 ~ "Julgamento com resolução de mérito - Não provimento",
                                            codigo_movimentacao == 978 ~ "Arquivista - Remessa",
                                            codigo_movimentacao == 417 ~ "Inclusão em pauta",
                                            codigo_movimentacao == 849 ~ "Reativação",
                                            codigo_movimentacao == 12283 ~ "Comunicação eletrônica confirmada",
                                            codigo_movimentacao == 981 ~ "Distribuidor - Recebimento",
                                            codigo_movimentacao == 977 ~ "Arquivista - Recebimento",
                                            codigo_movimentacao == 22 ~ "Baixa definitiva",
                                            codigo_movimentacao == 123 ~ "Arquivista - Recebimento",
                                            codigo_movimentacao == 898 ~ "Suspensão ou sobrestamento por decisão judicial",
                                            codigo_movimentacao == 861 ~ "Arquivamento",
                                            codigo_movimentacao == 14 ~ "Serventuário",
                                            codigo_movimentacao == 245 ~ "Arquivamento provisório",
                                            codigo_movimentacao == 12214 ~ "Processo Encaminhado",
                                            codigo_movimentacao == 12472 ~ "Decisão - Devolução dos autos à origem",
                                            codigo_movimentacao == 870 ~ "Arquivista - Autos Eliminados",
                                            codigo_movimentacao == 12065 ~ "Cumprimento de Suspensão ou Sobrestamento",
                                            codigo_movimentacao == 263 ~ "Decisão - Suspensão ou sobrestamento - Réu revel citado por edital")) %>%
  mutate(descricao_movimentacao = fct_reorder(descricao_movimentacao, n)) %>%
  ggplot(aes(n, descricao_movimentacao)) +
  geom_col() + 
  theme_minimal()

ultima <- banco_movimentos %>%
  group_by(id) %>%
  slice_max(dpj_DataHora, n = 1) %>%
  ungroup() %>%
  count(codigo_movimentacao, sort = T)


ultima %>%
  top_n(40) %>%
  mutate(descricao_movimentacao = case_when(codigo_movimentacao == 60 ~ "Expedição de documento",
                                            codigo_movimentacao == 581 ~ "Juntada - Documento",
                                            codigo_movimentacao == 85 ~ "Juntada - Petição",
                                            codigo_movimentacao == 132 ~ "Perícia - Recebimento",
                                            codigo_movimentacao == 51 ~ "Conclusão",
                                            codigo_movimentacao == 123 ~ "Remessa",
                                            codigo_movimentacao == 11383 ~ "Ato ordinatório",
                                            codigo_movimentacao == 22 ~ "Baixa definitiva",
                                            codigo_movimentacao == 11010 ~ "Despacho - Mero expediente",
                                            codigo_movimentacao == 246 ~ "Arquivamento - Definitivo",
                                            codigo_movimentacao == 493 ~ "Entrega em carga/vista",
                                            codigo_movimentacao == 92 ~ "Publicação",
                                            codigo_movimentacao == 848 ~ "Trânsito em julgado",
                                            codigo_movimentacao == 26 ~ "Distribuição",
                                            codigo_movimentacao == 1051 ~ "Decurso de prazo",
                                            codigo_movimentacao == 12266 ~ "Intimação eletrônica confirmada",
                                            codigo_movimentacao == 12265 ~ "Intimação eletrônica expedida/certificada",
                                            codigo_movimentacao == 1061 ~ "Disponibilização no Diário da Justiçã Eletrônico",
                                            codigo_movimentacao == 118 ~ "Protocolo de Petição",
                                            codigo_movimentacao == 36 ~ "Redistribuição",
                                            codigo_movimentacao == 48 ~ "Escrivão/Diretor de Secretaria/Secretário Jurídico",
                                            codigo_movimentacao == 982 ~ "Remessa",
                                            codigo_movimentacao == 106 ~ "Oficial de justiça - Devolução - Mandado",
                                            codigo_movimentacao == 1063 ~ "Determinação de arquivamento de procedimentos investigatórios",
                                            codigo_movimentacao == 12164 ~ "Outras decisões",
                                            codigo_movimentacao == 12263 ~ "Intimação",
                                            codigo_movimentacao == 985 ~ "Oficial de justiça - Recebimento - Mandado",
                                            codigo_movimentacao == 10966 ~ "Mudança de Classe Processual",
                                            codigo_movimentacao == 391 ~ "Decisão - Recebimento - Denúncia",
                                            codigo_movimentacao == 12282 ~ "Comunicação eletrônica expedida/certificada",
                                            codigo_movimentacao == 970 ~ "Audiência",
                                            codigo_movimentacao == 12293 ~ "Ato cumprido pela parte ou interessado (sem atributo)",
                                            codigo_movimentacao == 12142 ~ "Mudança de Parte",
                                            codigo_movimentacao == 239 ~ "Julgamento com resolução de mérito - Não provimento",
                                            codigo_movimentacao == 978 ~ "Arquivista - Remessa",
                                            codigo_movimentacao == 417 ~ "Inclusão em pauta",
                                            codigo_movimentacao == 849 ~ "Reativação",
                                            codigo_movimentacao == 12283 ~ "Comunicação eletrônica confirmada",
                                            codigo_movimentacao == 981 ~ "Distribuidor - Recebimento",
                                            codigo_movimentacao == 977 ~ "Arquivista - Recebimento",
                                            codigo_movimentacao == 22 ~ "Baixa definitiva",
                                            codigo_movimentacao == 123 ~ "Arquivista - Recebimento",
                                            codigo_movimentacao == 898 ~ "Suspensão ou sobrestamento por decisão judicial",
                                            codigo_movimentacao == 861 ~ "Arquivamento",
                                            codigo_movimentacao == 14 ~ "Serventuário",
                                            codigo_movimentacao == 245 ~ "Arquivamento provisório",
                                            codigo_movimentacao == 12214 ~ "Processo Encaminhado",
                                            codigo_movimentacao == 12472 ~ "Decisão - Devolução dos autos à origem",
                                            codigo_movimentacao == 870 ~ "Arquivista - Autos Eliminados",
                                            codigo_movimentacao == 12065 ~ "Cumprimento de Suspensão ou Sobrestamento",
                                            codigo_movimentacao == 263 ~ "Decisão - Suspensão ou sobrestamento - Réu revel citado por edital",
                                            codigo_movimentacao == 25~ "Decisão - Suspensão ou sobrestamento")) %>%
  mutate(descricao_movimentacao = fct_reorder(descricao_movimentacao, n)) %>%
  ggplot(aes(n, descricao_movimentacao)) +
  geom_col() + 
  theme_minimal()

