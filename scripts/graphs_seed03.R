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
library(syuzhet)
library(lexiconPT)

# rm(list = ls())

#### Visualizações #####

# data
load("data/disc_pres_prod.RData")
#
#### Wordcloud - Quanteda ####
set.seed(123)

# Geral

toks_pres %>% 
  dfm() %>% 
  textplot_wordcloud(comparison = F, 
                     max_words = 100)

# Comparação

toks_pres %>% 
  dfm() %>%  
  dfm_group(groups = authors) %>% 
  textplot_wordcloud(comparison = TRUE, 
                     max_words = 100,
                     color = c("orange", "blue", "red", "darkgreen"))

# Temático (Exemplo: Corrupção)

toks_pres %>% 
  tokens_keep(pattern = "corrup*", window = 3) %>% 
  tokens_remove(pattern = "corrup*") %>% 
  dfm() %>% 
  dfm_group(groups = authors) %>% 
  textplot_wordcloud(comparison = TRUE, 
                     max_words = 100,
                     color = c("orange", "blue", "red", "darkgreen"))

# Sobre Lula
# Um por vez

toks_pres %>% 
  tokens_subset(authors == "Jair Bolsonaro (PL)") %>% 
  tokens_keep(pattern = "lula*", window = 3) %>% 
  tokens_remove(pattern = "lula*") %>% 
  dfm() %>% 
  #dfm_group(groups = authors) %>% 
  textplot_wordcloud(comparison = F, 
                     max_words = 100,
                     color = "blue")

toks_pres %>% 
  tokens_subset(authors == "Simone Tebet (MDB)") %>% 
  tokens_keep(pattern = "lula*", window = 3) %>% 
  tokens_remove(pattern = "lula*") %>% 
  dfm() %>% 
  #dfm_group(groups = authors) %>% 
  textplot_wordcloud(comparison = F, 
                     max_words = 100,
                     color = "darkgreen")

toks_pres %>% 
  tokens_subset(authors == "Ciro Gomes (PDT)") %>% 
  tokens_keep(pattern = "lula*", window = 3) %>% 
  tokens_remove(pattern = "lula*") %>% 
  dfm() %>% 
  #dfm_group(groups = authors) %>% 
  textplot_wordcloud(comparison = F, 
                     max_words = 100,
                     color = "orange")

# Comparação

set.seed(123)
toks_pres %>% 
  tokens_subset(authors %in% c("Jair Bolsonaro (PL)", "Simone Tebet (MDB)", "Ciro Gomes (PDT)")) %>% 
  tokens_keep(pattern = "lula*", window = 3) %>% 
  tokens_remove(pattern = "lula*") %>% 
  dfm() %>% 
  dfm_group(groups = authors) %>% 
  textplot_wordcloud(comparison = T, 
                     max_words = 100,
                     color = c("orange", "blue", "darkgreen"))

#### Tidytext - TF-IDF ####

pres_tidy <- disc_pres %>%
  unnest_tokens(word, text) %>%
  count(authors_min, word, sort = TRUE)

total_words <- pres_tidy %>% 
  group_by(authors_min) %>% 
  summarize(total = sum(n))

pres_tidy <- left_join(pres_tidy, total_words)

pres_tidy <- pres_tidy %>%
  bind_tf_idf(word, authors_min, n)

pres_tidy %>%
  group_by(authors_min) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = authors_min)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~authors_min, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)

#### Quanteda - TF-IDF ####

dfmat_pres_tfidf <- toks_pres %>% 
  dfm() %>% 
  dfm_group(groups = authors) %>% 
  dfm_tfidf()

ciro_tidy <- dfmat_pres_tfidf %>% 
  tidy() %>% 
  tibble() %>% 
  group_by(document) %>% 
  arrange(desc(count), .by_group = T) %>% 
  filter(document == "Ciro Gomes (PDT)",
         count != 0) %>% 
  ungroup() %>% 
  select(-document)

wordcloud2::wordcloud2(ciro_tidy)

dfmat_pres_tfidf <- toks_pres %>% 
  tokens_keep(pattern = "lula*", window = 3) %>% 
  tokens_remove(pattern = "lula*") %>% 
  dfm() %>% 
  dfm_group(groups = authors) %>% 
  dfm_tfidf()

ciro_tidy <- dfmat_pres_tfidf %>% 
  tidy() %>% 
  group_by(document) %>% 
  arrange(desc(count), .by_group = T) %>% 
  filter(document == "Ciro Gomes (PDT)",
         count != 0) %>% 
  ungroup() %>% 
  select(-document)

wordcloud2::wordcloud2(ciro_tidy)

#### Ataques ####

topwords <- toks_pres %>% 
  #tokens_subset(semana == "Semana 1") %>% 
  tokens_keep(pattern = c('lula', 'ciro', 'bolsonaro', 'simone')) %>% 
  dfm() %>% 
  textstat_frequency(groups = authors_min) %>% 
  tibble() 

topwords <- topwords %>% 
  select(group, feature, frequency) %>% 
  left_join(disc_pres, by = c('group' = 'authors_min')) %>% 
  select(group, feature, frequency, author_dur_disc) %>% 
  unique() %>% 
  mutate(freq_dur = frequency / author_dur_disc) %>% 
  rename(source = group,
         target = feature,
         value = freq_dur) %>% 
  filter(source != target) 

topwords <- topwords %>% 
  mutate(id_source = case_when(source == "bolsonaro" ~ 0,
                               source == "ciro" ~ 1,
                               source == "lula" ~ 2,
                               source == "simone" ~ 3),
         id_target = case_when(target == "bolsonaro" ~ 4,
                               target == "ciro" ~ 5,
                               target == "lula" ~ 6,
                               target == "simone" ~ 7),
         link_color = case_when(source == "bolsonaro" ~ scales::alpha("#2c7bb6", .4),
                                source == "ciro" ~ scales::alpha("#fdae61", .4),
                                source == "lula" ~ scales::alpha("#d7191c", .4),
                                source == "simone" ~ scales::alpha("#018571", .4))) 

fig <- plot_ly(type = "sankey",
               orientation = "h",
               arrangement = "fixed",
               node = list(label = c("Bolsonaro", "Ciro", "Lula", "Simone", 
                                     "Bolsonaro", "Ciro", "Lula", "Simone"),
                           color = c("#2c7bb6", "#fdae61", "#d7191c", "#018571",
                                     "#2c7bb6", "#fdae61", "#d7191c", "#018571"),
                           # x = c(0.017, 0.017, 0.017, 0.017),
                           # y = c(0.06, 0.47, 0.85, 0.96),
                           pad = 15,
                           thickness = 20,
                           line = list(color = "black",
                                       width = 0.5)),
               link = list(source = topwords$id_source,
                           target = topwords$id_target,
                           value =  topwords$frequency,
                           color = topwords$link_color#,
#                           line = list(color = "black",
#                                       width = 0.5)
)
               )
fig %>% 
  layout(title = "Quem cita quem?",
         font = list(size = 10))

#### Ataques por Data ####

topwords_date <- toks_pres %>% 
  tokens_keep(pattern = c('lula', 'ciro', 'bolsonaro', 'simone')) %>% 
  dfm() %>% 
  textstat_frequency(groups = interaction(authors_min, date)) %>% 
  tibble() %>% 
  separate(group, c("authors_min", "date"), "\\.") %>% 
  filter(feature != authors_min) %>% 
  mutate(date = ymd(date))

#### Dicionário Temas ####

dict_temas_camp <- dictionary(list(religião = c('deus', 'relig*'),
                                   meio_ambiente = c('ambient*', 'sustent*', 
                                                     'amaz*', 'desmatamento'),
                                   economia = c('econ*', 'banco*', 'produtividade', 
                                                'emprego*', 'inflação', 'negócios', 
                                                'juros', 'mercado*'),
                                   pandemia = c('pandemi*', 'covid*', 
                                                'vacina*'),
                                   corrupção = c('corrup*', 'esquema', 
                                                 'fisiologia', 'mensalão'),
                                   segurança = c('segurança pública', 
                                                 'polícia*', 'viol*', 'arma*'),
                                   educação = c('educ*', 'ensin*', 'professor*', 
                                                'ciência', 'escola*', 'professor*', 
                                                'universidade*'),
                                   gênero = c('gênero', 'mulher*', 'femini*', 
                                              'machis*', 'lgbt*', 'gay*')))

dfm_pres_group <- toks_pres_group %>% 
  tokens_compound(dict_temas_camp) %>% 
  tokens_lookup(dictionary = dict_temas_camp,
                nested_scope = "dictionary") %>% 
  dfm()

tokens_number <- toks_pres_group %>% 
  ntoken() %>%  
  enframe()

pres_group_cat <- dfm_pres_group %>% 
  convert(to = "data.frame") %>% 
  left_join(disc_pres_group, by = c("doc_id" = "docname"))

pres_group_cat <- pres_group_cat %>% 
  left_join(tokens_number, by = c("doc_id" = "name")) %>% 
  pivot_longer(cols = names(dict_temas_camp), names_to = "category", values_to = "cat_count") %>%
  group_by(doc_id) %>% 
  mutate(cat_percent = (cat_count / value) * 100) %>% 
  ungroup()

pres_group_cat <- pres_group_cat %>% 
  group_by(authors_min, category) %>% 
  summarise(cat_percent = mean(cat_percent)) %>% 
  ungroup()

pres_group_cat <- pres_group_cat %>% 
  group_by(authors_min) %>%
  arrange(cat_percent, .by_group = TRUE) %>% 
  ungroup() %>% 
  mutate(order = row_number()) 

pres_group_cat %>%
  ggplot(aes(order, cat_percent)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ authors_min, scales = "free") +
  coord_flip() +
  labs(x = NULL, y = NULL) +
  scale_x_continuous(  # This handles replacement of .r for x
    breaks = pres_group_cat$order,     # notice need to reuse data frame
    labels = pres_group_cat$category) +
  theme_classic()

#### Wordfish ####

dfmat_pres <- toks_pres %>%
  dfm() %>% 
  dfm_group(groups = authors)

tmod_wf <- textmodel_wordfish(dfmat_pres, dir = c(2, 3))

textplot_scale1d(tmod_wf)

#### Keyness ####

tstat_key <- toks_pres %>%
  dfm() %>% 
  dfm_group(groups = authors) %>% 
  dfm_subset(authors %in% c("Jair Bolsonaro (PL)", "Luiz Inácio Lula da Silva (PT)")) %>% 
  textstat_keyness(target = "Jair Bolsonaro (PL)")

textplot_keyness(tstat_key,
                 color = c("blue", "red"))

#### Lexical Diversity ####

tstat_lexdiv <- toks_pres %>%
  dfm() %>%  
  dfm_group(groups = authors) %>%
  textstat_lexdiv() %>% 
  arrange(desc(TTR))

tstat_lexdiv

#### Related Keywords ####

religiao <- c("religi*", "deus*")

toks_pres_group <- toks_pres %>% 
  tokens_group(groups = authors_min)

toks_inside <- tokens_keep(toks_pres_group, pattern = religiao, window = 10)

toks_inside <- tokens_remove(toks_inside, pattern = religiao) # remove the keywords

toks_outside <- tokens_remove(toks_pres_group, pattern = religiao, window = 10)

dfmat_inside <- dfm(toks_inside)

dfmat_outside <- dfm(toks_outside)

tstat_key_inside <- textstat_keyness(rbind(dfmat_inside, dfmat_outside), 
                                     target = seq_len(ndoc(dfmat_inside)))

head(tstat_key_inside, 50)

#

#### Dicionário Sentimento ####

nrc_port <- get_sentiment_dictionary(dictionary = "nrc", language = "portuguese")

sent_port_dict <- dictionary(list(raiva = nrc_port$word[nrc_port$sentiment == "anger"],
                                  antecipação = nrc_port$word[nrc_port$sentiment == "anticipation"],
                                  desgosto = nrc_port$word[nrc_port$sentiment == "disgust"],
                                  medo = nrc_port$word[nrc_port$sentiment == "fear"],
                                  alegria = nrc_port$word[nrc_port$sentiment == "joy"],
                                  tristeza = nrc_port$word[nrc_port$sentiment == "sadness"],
                                  surpresa = nrc_port$word[nrc_port$sentiment == "surprise"],
                                  confiança = nrc_port$word[nrc_port$sentiment == "trust"]))

dfm_pres_group <- toks_pres_group %>% 
  tokens_compound(sent_port_dict) %>% 
  tokens_lookup(dictionary = sent_port_dict,
                nested_scope = "dictionary") %>% 
  dfm()

teste_vocab <- toks_pres_group %>%
  tokens_keep(sent_port_dict) %>% 
  dfm() %>% 
  textstat_frequency() %>% 
  tibble() %>% 
  mutate(sentimento = case_when(feature %in% sent_port_dict$raiva ~ "raiva",
                                feature %in% sent_port_dict$antecipação ~ "antecipação",
                                feature %in% sent_port_dict$desgosto ~ "desgosto",
                                feature %in% sent_port_dict$medo ~ "medo",
                                feature %in% sent_port_dict$alegria ~ "alegria",
                                feature %in% sent_port_dict$tristeza ~ "tristeza",
                                feature %in% sent_port_dict$surpresa ~ "surpresa",
                                feature %in% sent_port_dict$confiança ~ "confiança"))

tokens_number <- toks_pres_group %>% 
  ntoken() %>%  
  enframe()

pres_group_cat <- dfm_pres_group %>% 
  convert(to = "data.frame") %>% 
  left_join(disc_pres_group, by = c("doc_id" = "docname")) %>% 
  tibble()

pres_group_cat <- pres_group_cat %>% 
  left_join(tokens_number, by = c("doc_id" = "name")) %>% 
  pivot_longer(cols = names(sent_port_dict), names_to = "category", values_to = "cat_count") %>%
  filter(!category %in% c("negativo", "positivo")) %>%
  select(-text) %>% 
  group_by(doc_id) %>% 
  mutate(cat_percent = (cat_count / value) * 100) %>% 
  ungroup()

pres_group_cat <- pres_group_cat %>% 
  group_by(authors_min, category) %>% 
  summarise(cat_percent = mean(cat_percent)) %>% 
  ungroup()

pres_group_cat <- pres_group_cat %>% 
  group_by(authors_min) %>%
  arrange(cat_percent, .by_group = TRUE) %>% 
  ungroup() %>% 
  mutate(order = row_number()) 

pres_group_cat %>%
  ggplot(aes(order, cat_percent)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ authors_min, scales = "free") +
  coord_flip() +
  labs(x = NULL, y = NULL) +
  scale_x_continuous(breaks = pres_group_cat$order,     
                     labels = pres_group_cat$category) +
  scale_y_continuous(breaks = c(0,3,6,9),
                     limits = c(0,9)) +
  theme_classic()

#### Visualização Sentimento por Candidato ####

pres_group_cat %>% 
  filter(authors_min == "lula") %>% 
  ggplot(aes(as.factor(category), cat_percent, fill=as.factor(category) )) + 
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set3") +
  labs(x = "Emoções", y = NULL, title = "Lula (PT)") +
  scale_y_continuous(limits = c(0,max(pres_group_cat$cat_percent))) +
  theme_bw() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))

#### LexiconPT ####

data("oplexicon_v3.0")
data("sentiLex_lem_PT02")
op30 <- oplexicon_v3.0
sent <- sentiLex_lem_PT02

sent_lexiconpt <- toks_pres %>% 
  dfm() %>% 
  dfm_group(groups = authors) %>% 
  tidy()

sent_lexiconpt <- sent_lexiconpt %>% 
  left_join(op30, by = "term") %>% 
  left_join(sent %>% select(term, lex_polarity = polarity), by = "term") %>% 
  select(document, term, polarity, lex_polarity, count)

sent_lexiconpt$polarity[sent_lexiconpt$polarity == "1"] <- "positivo"
sent_lexiconpt$polarity[sent_lexiconpt$polarity == "-1"] <- "negativo"
sent_lexiconpt$polarity[sent_lexiconpt$polarity == "0"] <- NA
sent_lexiconpt$lex_polarity[sent_lexiconpt$lex_polarity == "1"] <- "positivo"
sent_lexiconpt$lex_polarity[sent_lexiconpt$lex_polarity == "-1"] <- "negativo"
sent_lexiconpt$lex_polarity[sent_lexiconpt$lex_polarity == "0"] <- NA

sent_lexiconpt <- sent_lexiconpt %>% 
  filter(!is.na(polarity) | !is.na(lex_polarity)) %>% 
  group_by(document, lex_polarity) %>% 
  summarise(count = sum(count)) %>% 
  ungroup() %>% 
  drop_na()

sent_lexiconpt <- sent_lexiconpt %>% 
  group_by(document) %>%
  arrange(count, .by_group = TRUE) %>% 
  ungroup() %>% 
  mutate(order = row_number()) 

sent_lexiconpt %>%
  ggplot(aes(order, count)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ document, scales = "free") +
  coord_flip() +
  labs(x = NULL, y = NULL) +
  scale_x_continuous(  # This handles replacement of .r for x
    breaks = sent_lexiconpt$order,     # notice need to reuse data frame
    labels = sent_lexiconpt$lex_polarity) +
  scale_y_continuous(breaks = c(0,1000,2000,3000), limits = c(0,max(sent_lexiconpt$count))) +
  theme_classic()

