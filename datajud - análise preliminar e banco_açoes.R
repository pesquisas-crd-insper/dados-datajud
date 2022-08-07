
library(tidyverse)
library(rjson)
library(listviewer)
library(stringr)
library(ggplot2)

setwd("C:/Users/gabri/OneDrive/Documents/[CNJ - Insper]/Núcleo Quantitativo/Dados Datajud/20 mil 11.04")


# Primeiro vamos verificar se todos os processos possuem a mesma estrutura

nomes <- vector()

for(i in 1:20000){
  assign("processo", db)

  
  nomes <- c(nomes, 
             names(processo))
  
}

table(nomes)


xtabs(~autor_tipo + df$resultado_favoravel_autor, data = df)
tribunal <- table(db$sigla_tribunal)

#Gráfico de frequência das variáveis:

#GRAU

grau <- df$grau %>% sort (decreasing=T)

grau <- as_tibble(grau)

grau %>% ggplot() +
  geom_bar(aes(y=value)) + labs(x="Número de processos", y = "Grau") + theme_minimal()


#INSTANCIA
db %>% ggplot() +
  geom_bar(aes(y=instancia)) + labs(x="Número de processos", y = "Instância") + theme_minimal()


#TRIBUNAL
db %>% ggplot() +
  geom_bar(aes(y=sigla_tribunal)) + labs(x="Número de processos", y = "Tribunal") + theme_minimal()


#data_ajuizamento
ajuiz <- df$data_ajuizamento %>% sort (decreasing=T)

ajuiz <- as_tibble(ajuiz)


#ORGAO
orgao <- df$dpj_codigo_orgao_nacional
orgao <- as_tibble(orgao)

orgao %>% ggplot() +
  geom_bar(aes(y=value)) + labs(x="Número de processos", y = "Órgão") + theme_minimal()
print(orgao)



tab_movimento %>% ggplot() +
  geom_col(aes(x=n, y=movimento)) + labs(x="Número de observações", y = "Variáveis relacionadas a movimentos") + theme_minimal()


df <- as_tibble(db)



# Aqui vemos que todos os processos possuem o campo dadosBasicos, mas os outros
# campos não estão presentes em todos os processos

################################################################################


# Agora vamos fazer o mesmo processo, mas para o campo dadosBasicos
dados_basicos <- vector()

for(i in 1:1000){
  
  assign("processo",
         fromJSON(file = paste0("first/", i, ".json")))
  
  dados_basicos <- c(dados_basicos, 
                     names(processo[["dadosBasicos"]]))
  
}

table(dados_basicos)

tab_basicos <- table(dados_basicos)
tab_basicos <- as_tibble(tab_basicos)


#Gráfico de frequência das variáveis:
tab_basicos %>% ggplot() +
  geom_col(aes(x=dados_basicos, y=n)) + labs(x="Variáveis", y = "Número de observações") + theme_minimal()

# Todos tem assunto, classeProcessual, codigoLocalidade, dataAjuizamento,
# numero, orgaoJulgador, mas os outros campos estão incompletos


################################################################################

# Agora vamos fazer o mesmo processo, mas para o campo assunto
campo_assunto <- vector()

for(i in 1:1000){
  
  assign("processo",
         fromJSON(file = paste0("first/", i, ".json")))
  
  campo_assunto <- c(campo_assunto, 
                     unlist(processo[["dadosBasicos"]][["assunto"]]) %>% names() %>% unique())
  
}

table(campo_assunto)

tab_assunto <- table(campo_assunto)
tab_assunto <- as_tibble(tab_assunto)


#Gráfico de frequência das variáveis:
tab_assunto %>% ggplot() +
  geom_col(aes(x=n, y=campo_assunto)) + labs(x="Número de observações", y = "Assunto") + theme_minimal()


################################################################################

# Agora vamos fazer o mesmo processo, mas para o campo movimento
movimento <- vector()

for(i in 1:1000){
  
  assign("processo",
         fromJSON(file = paste0("first/", i, ".json")))
  
  movimento <- c(movimento, 
                 unlist(processo[["movimento"]]) %>% names() %>% unique())
  
}

table(movimento) %>% sort(decreasing = T) %>% head(20)

tab_movimento <- table(movimento)%>% sort(decreasing = T) %>% head(20)
tab_movimento <- as_tibble(tab_movimento) %>% sort (decreasing = T)


#Gráfico de frequência das variáveis:
tab_movimento %>% ggplot() +
  geom_col(aes(x=n, y=movimento)) + labs(x="Número de observações", y = "Variáveis relacionadas a movimentos") + theme_minimal()

############################################################################

# Verificar quais são os assuntos possíveis no banco de dados
assunto <- vector()

for(i in 1:1000){
  
  assign("processo",
         fromJSON(file = paste0("first/", i, ".json")))
  
  tamanho <- length(processo[["dadosBasicos"]][["assunto"]])
  
  assunto_temp <- vector()
  
  for(t in 1:tamanho){
    
    assunto_temp <- c(assunto_temp, 
                      processo[["dadosBasicos"]][["assunto"]][[t]][["dpj_nomeAssuntoNacional"]])
    
  }
  
  assunto_temp <- assunto_temp %>% unique()
  
  assunto <- c(assunto, assunto_temp)
  
}

table(assunto) %>% sort(decreasing = T) %>% head(10)

#Ressalva que não são todos os casos que o assunto está no campo
# dpj_nomeAssuntoNacional, então os numeros não estao corretos


tab_assuntos_freq <- table(assunto)%>% sort(decreasing = T) %>% head(10)
tab_assuntos_freq <- as_tibble(tab_assuntos_freq)


#Gráfico de frequência dos assuntos:
tab_assuntos_freq %>% ggplot() +
  geom_col(aes(x=n, y=assunto)) + labs(x="Número de observações", y = "Assuntos mais frequentes") + theme_minimal()

################################################################################

# criar um banco de dados com as informações das açoes e incluir alguns assuntos de especial interesse para exploraçao preliminar

banco_dados_basicos <- data.frame(id = rep(NA, 1000),
                                  dataProtocolo = rep(NA, 1000),
                                  protocoloRN = rep(NA, 1000),
                                  siglaTribunal = rep(NA, 1000),
                                  procEl = rep(NA, 1000),
                                  classeProcessual = rep(NA, 1000),
                                  dataAjuizamento = rep(NA, 1000),
                                  grau = rep(NA, 1000),
                                  codigoLocalidade = rep(NA, 1000),
                                  numero = rep(NA, 1000),
                                  dscSistema = rep(NA, 1000),
                                  nomeOrgao = rep(NA, 1000),
                                  codigoMunicipioIBGE = rep(NA, 1000),
                                  codigoOrgao = rep(NA, 1000),
                                  instancia = rep(NA, 1000),
                                  CorrupAtiva = rep(NA, 1000),
                                  CorrupPassiva = rep(NA, 1000),
                                  Peculato = rep(NA, 1000),
                                  LeiLicitacoes = rep(NA, 1000),
                                  Prevaricacao = rep(NA, 1000),
                                  Quadrilha = rep(NA, 1000),
                                  CrimesLavagem = rep(NA, 1000),
                                  Concussao = rep(NA, 1000),
                                  AdvoAdministrativa = rep(NA, 1000),
                                  Lavagem = rep(NA, 1000))


for(i in 1:1000){
  
  processo <- fromJSON(file = paste0("first/", i, ".json"))
  
  banco_dados_basicos$id[i] <- i
  if(is.null(processo[["dataProtocolo"]])){
    banco_dados_basicos$dataProtocolo[i] <- NA
  } else {
    banco_dados_basicos$dataProtocolo[i] <- processo[["dataProtocolo"]]
  }
  
  if(is.null(processo[["protocoloRN"]])){
    banco_dados_basicos$protocoloRN[i] <- NA
  } else {
    banco_dados_basicos$protocoloRN[i] <- processo[["protocoloRN"]]
  }
  
  if(is.null(processo[["dadosBasicos"]][["siglaTribunal"]])){
    banco_dados_basicos$siglaTribunal[i] <- NA
  } else {
    banco_dados_basicos$siglaTribunal[i] <- 
      processo[["dadosBasicos"]][["siglaTribunal"]]
  }
  
  if(is.null(processo[["dadosBasicos"]][["procEl"]])){
    banco_dados_basicos$procEl[i] <- NA
  } else {
    banco_dados_basicos$procEl[i] <- 
      processo[["dadosBasicos"]][["procEl"]]
  }
  
  if(is.null(processo[["dadosBasicos"]][["classeProcessual"]])){
    banco_dados_basicos$classeProcessual[i] <- NA
  } else {
    banco_dados_basicos$classeProcessual[i] <- 
      processo[["dadosBasicos"]][["classeProcessual"]]
  }
  
  if(is.null(processo[["dadosBasicos"]][["dataAjuizamento"]])){
    banco_dados_basicos$dataAjuizamento[i] <- NA
  } else {
    banco_dados_basicos$dataAjuizamento[i] <- 
      processo[["dadosBasicos"]][["dataAjuizamento"]]
  }
  
  if(is.null(processo[["dadosBasicos"]][["grau"]])){
    banco_dados_basicos$grau[i] <- NA
  } else {
    banco_dados_basicos$grau[i] <- 
      processo[["dadosBasicos"]][["grau"]]
  }
  
  if(is.null(processo[["dadosBasicos"]][["codigoLocalidade"]])){
    banco_dados_basicos$codigoLocalidade[i] <- NA
  } else {
    banco_dados_basicos$codigoLocalidade[i] <- 
      processo[["dadosBasicos"]][["codigoLocalidade"]]
  }
  
  if(is.null(processo[["dadosBasicos"]][["numero"]])){
    banco_dados_basicos$numero[i] <- NA
  } else {
    banco_dados_basicos$numero[i] <- 
      processo[["dadosBasicos"]][["numero"]]
  }
  
  if(is.null(processo[["dadosBasicos"]][["dscSistema"]])){
    banco_dados_basicos$dscSistema[i] <- NA
  } else {
    banco_dados_basicos$dscSistema[i] <- 
      processo[["dadosBasicos"]][["dscSistema"]]
  }
  
  if(is.null(processo[["dadosBasicos"]][["orgaoJulgador"]][["nomeOrgao"]])){
    banco_dados_basicos$nomeOrgao[i] <- NA
  } else {
    banco_dados_basicos$nomeOrgao[i] <- 
      processo[["dadosBasicos"]][["orgaoJulgador"]][["nomeOrgao"]]
  }
  
  if(is.null(processo[["dadosBasicos"]][["orgaoJulgador"]][["codigoMunicipioIBGE"]])){
    banco_dados_basicos$codigoMunicipioIBGE[i] <- NA
  } else {
    banco_dados_basicos$codigoMunicipioIBGE[i] <- 
      processo[["dadosBasicos"]][["orgaoJulgador"]][["codigoMunicipioIBGE"]]
  }
  
  if(is.null(processo[["dadosBasicos"]][["orgaoJulgador"]][["codigoOrgao"]])){
    banco_dados_basicos$codigoOrgao[i] <- NA
  } else {
    banco_dados_basicos$codigoOrgao[i] <- 
      processo[["dadosBasicos"]][["orgaoJulgador"]][["codigoOrgao"]]
  }
  
  if(is.null(processo[["dadosBasicos"]][["orgaoJulgador"]][["instancia"]])){
    banco_dados_basicos$instancia[i] <- NA
  } else {
    banco_dados_basicos$instancia[i] <- 
      processo[["dadosBasicos"]][["orgaoJulgador"]][["instancia"]]
  }
  
  
  if(TRUE %in%
     str_detect(processo[["dadosBasicos"]][["assunto"]],"Corrupção ativa")){
    banco_dados_basicos$CorrupAtiva[i] <- 1 
  } 
  
  if(TRUE %in%
     str_detect(processo[["dadosBasicos"]][["assunto"]],"Corrupção passiva")){
    banco_dados_basicos$CorrupPassiva[i] <- 1 
  } 
  
  if(TRUE %in%
     str_detect(processo[["dadosBasicos"]][["assunto"]],"Peculato")){
    banco_dados_basicos$Peculato[i] <- 1 
  } 
  
  if(TRUE %in%
     str_detect(processo[["dadosBasicos"]][["assunto"]],"Crimes da Lei de licitações")){
    banco_dados_basicos$LeiLicitacoes[i] <- 1 
  } 
  
  if(TRUE %in%
     str_detect(processo[["dadosBasicos"]][["assunto"]],"Prevaricação")){
    banco_dados_basicos$Prevaricacao[i] <- 1 
  } 
  
  if(TRUE %in%
     str_detect(processo[["dadosBasicos"]][["assunto"]],"Quadrilha ou Bando")){
    banco_dados_basicos$Quadrilha[i] <- 1 
  } 
  
  if(TRUE %in%
     str_detect(processo[["dadosBasicos"]][["assunto"]],"Crimes de &quot;Lavagem&quot; ou Ocultação de Bens, Direitos ou Valores")){
    banco_dados_basicos$CrimesLavagem[i] <- 1 
  } 
  
  if(TRUE %in%
     str_detect(processo[["dadosBasicos"]][["assunto"]],"Concussão")){
    banco_dados_basicos$Concussao[i] <- 1 
  } 
  
  if(TRUE %in%
     str_detect(processo[["dadosBasicos"]][["assunto"]],"Advocacia administrativa")){
    banco_dados_basicos$AdvoAdministrativa[i] <- 1 
  } 
  
  if(TRUE %in%
     str_detect(processo[["dadosBasicos"]][["assunto"]],'ou Ocultação de Bens, Direitos ou Valores Oriundos de Corrupção')){
    banco_dados_basicos$Lavagem[i] <- 1 
  } 
  
  
}

table(banco_dados_basicos$siglaTribunal)
table(banco_dados_basicos$dscSistema)
table(banco_dados_basicos$procEl)
table(banco_dados_basicos$classeProcessual)
table(banco_dados_basicos$codigoLocalidade)
table(banco_dados_basicos$grau)
table(banco_dados_basicos$nomeOrgao)
table(banco_dados_basicos$codigoMunicipioIBGE)
table(banco_dados_basicos$codigoOrgao)
table(banco_dados_basicos$instancia)
table(banco_dados_basicos$CorrupAtiva)
table(banco_dados_basicos$CorrupPassiva)
table(banco_dados_basicos$Peculato)
table(banco_dados_basicos$LeiLicitacoes)
table(banco_dados_basicos$Prevaricacao)
table(banco_dados_basicos$Quadrilha)
table(banco_dados_basicos$CrimesLavagem)
table(banco_dados_basicos$Concussao)
table(banco_dados_basicos$AdvoAdministrativa)
table(banco_dados_basicos$Lavagem)


#Gráfico que cruza tribunais e grau decisório:

siglas_sem_NA <- banco_dados_basicos %>% filter(banco_dados_basicos$siglaTribunal != "NA")
ggplot(siglas_sem_NA, aes(y = as.factor(siglaTribunal), fill = grau)) + geom_bar() + theme_light() + 
  labs(x = "Nº ações", y = "Tribunal")

df %>% filter(sigla_tribunal != "NA") %>%
  ggplot(df, aes (y=as.factor(sigla_tribunal), fill=instancia)) + geom_bar() + theme_light() + 
  labs(x = "Nº ações", y = "Tribunal")


  ggplot(df, aes (y=as.factor(instancia), fill=esfera)) + geom_bar() + theme_light() + 
  labs(x = "Nº ações", y = "Instância")

#Gráfico que cruza tribunais e corrupçao:

siglas_sem_NA <- banco_dados_basicos %>% filter(banco_dados_basicos$siglaTribunal != "NA")
ggplot(siglas_sem_NA, aes(y = as.factor(siglaTribunal), fill = CrimesLavagem)) + geom_bar() + theme_light() + 
  labs(x = "Nº ações", y = "Tribunal")


###################################################################################################################3
#Agora vamos olhar a frequência de preenchimento abrindo o campo orgaoJulgador

orgao_julgador <- vector()

for(i in 1:1000){
  
  assign("processo",
         fromJSON(file = paste0("first/", i, ".json")))
  
  orgao_julgador <- c(orgao_julgador, 
                     unlist(processo[["dadosBasicos"]][["orgaoJulgador"]]) %>% names() %>% unique())
  
}

table(orgao_julgador)

tab_orgao <- table(orgao_julgador)
tab_orgao <- as_tibble(tab_orgao)


#Gráfico de frequência das variáveis:
tab_orgao %>% ggplot() +
  geom_col(aes(x=orgao_julgador, y=n)) + labs(x="Variáveis", y = "Número de observações") + theme_minimal()

#Gráfico de frequência das instâncias:

table(banco_dados_basicos$instancia)
tab_instancia <- table(instancia)
tab_instancia <- as_tibble(tab_instancia)
tab_orgao %>% ggplot() +
  geom_col(aes(x=instancia, y=n)) + labs(x="Variáveis", y = "Número de observações") + theme_minimal()
