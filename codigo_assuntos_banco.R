library(tidyverse)
library(rjson)
library(lubridate)
library(data.table)

setwd("C:\\Users\\rodri\\Downloads\\datajud")


banco_assuntos <- data.table()

ids <- 1:10000

while(1089552 != max(banco_assuntos$id)){
  
init <- Sys.time()

files <- paste0("json/", substr(ids, 1, 1), "/", ids, ".json")

dataJud <- map(files, ~ fload(.x, 
                              max_simplify_lvl = "list",
                              keep_temp_files = F
                              ))

assuntos <- list()

for(i in 1:length(dataJud)) {
  
  # assign(paste0("assuntos_", i),
  #        rbindlist(dataJud[[i]]$dadosBasicos$assunto, 
  #                  use.names = T,
  #                  fill = T) %>%
  #          distinct() %>% 
  #          mutate(id = ids[i]))
  
  assuntos[[i]] <- rbindlist(dataJud[[i]]$dadosBasicos$assunto,
                        use.names = T,
                        fill = T) %>%
    distinct() %>%
    mutate(id = ids[i])
  
# assuntos <- rbindlist(dataJud[[i]]$dadosBasicos$assunto, 
#                       use.names = T,
#                       fill = T) %>%
#   distinct() %>% 
#   mutate(id = ids[i])

# banco_assuntos <- rbindlist(list(banco_assuntos, assuntos), use.names = T, 
#                             fill = T)
}

assuntos <- rbindlist(assuntos,
                      use.names = T,
                      fill = T)

banco_assuntos <- rbindlist(list(banco_assuntos, assuntos), 
                            use.names = T,
                            fill = T)


rm(dataJud)
rm(assuntos)
gc()

ids <- seq(max(ids) + 1, max(ids)+10000)

  if(1089552 %in% ids){
    ids <- c(min(ids):1089552)
  }

final <- Sys.time()

print(final - init)
print(min(ids))


}



banco_assuntos <- banco_assuntos %>% distinct()

summary(banco_assuntos$id)

`%!in%` <- Negate(`%in%`)
ids <- c(1:1089552)[which(1:1089552 %!in% banco_assuntos$id)]

banco_assuntos %>% count(dpj_nomeAssuntoNacional, sort = T) %>% head(20)


