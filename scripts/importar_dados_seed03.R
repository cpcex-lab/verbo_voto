##### Importar dados e carregar pacotes #####
library(tidyverse)
library(tidytext)
library(purrr)
library(jsonlite)
library(networkD3)
library(lubridate)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(quanteda.textmodels)
library(stm)
library(plotly)

# Observação: É possível colocar qualquer número em 'n' para o 'for', pois o 
# máximo que vai acontecer é a conexão não ser estabelecida
n <- 30
aux <- tibble()

for(i in 1:n){
  link <- paste0("https://escriba.aosfatos.org/banco-de-discursos/api/?format=json&page=", i)
  try({output <- fromJSON(link)
  output <- output[["results"]] %>% 
    unnest(transcription) %>%
    mutate(url_api = link)
  aux <- rbind(aux, output)})
  print(i)
}

disc_pres <- aux
save(disc_pres, file = "data/disc_pres_des.RData")

#