##### Processar Dados #####

# Como os autores estão em formato de lista, é preciso usar unnest e depois 
# reuni-los em um só label
# Por enquanto, remover source == "Debate" devido à baixa estruturação
# Remover authors == "Vera Lúcia" pelo baixo percentual de intenção de votos

# packages
library(stringr)
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

# rm(list = ls())
load("data/disc_pres_des.RData")

disc_pres <- disc_pres %>%
  unnest(authors) %>% # print(n = 100)
  mutate(doc_id = paste(date, authors, source, url, sep = "_")) %>%
  group_by(doc_id) %>% 
  mutate(doc_id_seq = dense_rank(start),
         doc_id_seq = str_pad(doc_id_seq, 5, pad = "0"),
         doc_id_seq = paste(doc_id, doc_id_seq, sep = "_")) # %>% # print(n = 100)

disc_pres <- disc_pres %>%
#  unnest(authors) %>%  print(n = 100)
  group_by(date, source, url, start, end, text, doc_id, doc_id_seq) %>%
  summarise(authors = paste(authors, collapse = ", ")) %>%
  ungroup() %>% 
  filter(authors != "Vera Lúcia (PSTU)",
         source != "Debate")

# Criar nomes para os documentos e uma versão reduzida do nome dos autores

disc_pres <- disc_pres %>% 
  group_by(authors) %>% 
  mutate(# docname = row_number(),
         authors_min = case_when(authors == "Ciro Gomes (PDT)" ~ "ciro",
                                 authors == "Jair Bolsonaro (PL)" ~ "bolsonaro",
                                 authors == "Luiz Inácio Lula da Silva (PT)" ~ "lula",
                                 authors == "Simone Tebet (MDB)" ~ "simone"),
         # docname = paste(authors_min, docname, sep = ""),
         date = ymd(date)) %>% 
  ungroup() %>%
  filter(date > ymd("2022-08-15")) %>% # a campanha começa em 16/8/2022
  arrange(date, doc_id_seq) # %>% print(n = 100, width = Inf)

# # Variáveis de número de horas e discursos por autor

disc_pres_count <- disc_pres %>%
  group_by(authors_min, url) %>%
  summarise(author_dur_disc = max(end) / 60) %>%
  unique() %>%
  mutate(author_n_disc = n()) %>%
  ungroup() %>%
  group_by(authors_min, author_n_disc) %>%
  summarise(author_dur_disc = sum(author_dur_disc)) %>%
  ungroup()

disc_pres <- disc_pres %>%
  left_join(disc_pres_count, by = "authors_min")

# Variáveis de informações sobre a base

disc_pres <- disc_pres %>% 
  mutate(df_last_date = max(date),
         df_n_spch = length(unique(disc_pres$doc_id)),
         df_n_hours = disc_pres %>%
           group_by(authors_min, doc_id) %>% 
           summarise(disc_dur = max(end) / 60) %>%
           ungroup() %>% 
           pull(disc_dur) %>% 
           sum() %>%
           `/`(60) %>%
           round(),
         semana = case_when(date >= "2022-08-16" & date <= "2022-08-22" ~ "Semana 1",
                            date >= "2022-08-23" & date <= "2022-08-29" ~ "Semana 2",
                            date >= "2022-08-30" & date <= "2022-09-05" ~ "Semana 3",
                            date >= "2022-09-06" & date <= "2022-09-12" ~ "Semana 4",
                            date >= "2022-09-13" ~ "Semana 5"))
  
# Criar objetos base para análise de texto (corpus, tokens e dfm)

corpus_pres = corpus(disc_pres, 
                     docid_field = 'doc_id_seq', 
                     text_field = 'text')

# Remover ao máximo palavras que causam ruído

toks_pres <- corpus_pres %>% 
  tokens(remove_punct = TRUE, 
         remove_numbers = TRUE, 
         remove_symbol = TRUE,
         remove_url = TRUE) %>% 
  tokens_tolower() %>% 
  tokens_remove(pattern = c(stopwords("pt"), "senhor*", 'pra', 'pro', 'pras', 
                            'pros')) %>%  
  tokens_replace("pt", "pt_") %>% 
  tokens_replace("fé", "fé_") %>% 
  tokens_select(min_nchar = 3) %>% 
  tokens_replace("pt_", "pt") %>% 
  tokens_replace("fé_", "fé")

# Criar uma base com os discursos agrupados, para facilitar análises específicas

disc_pres_group <- disc_pres %>%
  group_by(date, authors, source, url, authors_min, semana) %>%
  summarise(text = paste(text, collapse = " ")) %>%
  mutate(text = str_trim(text),
         text = str_squish(text)) %>%
  ungroup() %>%
  group_by(authors) %>%
  mutate(docname = row_number(),
         docname = paste(authors_min, docname, sep = "")) %>%
  ungroup()

corpus_pres_group = corpus(disc_pres_group, 
                           docid_field = 'docname', 
                           text_field = 'text')

toks_pres_group <- corpus_pres_group %>% 
  tokens(remove_punct = TRUE, 
         remove_numbers = TRUE, 
         remove_symbol = TRUE,
         remove_url = TRUE) %>% 
  tokens_tolower() %>% 
  tokens_remove(pattern = c(stopwords("pt"), "senhor*", 'pra', 'pro', 'pras', 
                            'pros')) %>%  
  tokens_replace("pt", "pt_") %>% 
  tokens_replace("fé", "fé_") %>% 
  tokens_select(min_nchar = 3) %>% 
  tokens_replace("pt_", "pt") %>% 
  tokens_replace("fé_", "fé")

save(disc_pres, disc_pres_group, toks_pres, toks_pres_group, 
     file = "data/disc_pres_prod.RData")

#