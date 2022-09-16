#######WORDCLOUD  -----------------------------------------------------------
##TRANSFORMAR A BASE  -----------------------------------------------------------
# Criar objetos base para análise de texto (corpus, tokens e dfm)

corpus_pres = corpus(disc_pres, 
                     docid_field = 'docname', 
                     text_field = 'text')

# Remover ao máximo palavras que causam ruído

toks_pres <- corpus_pres %>% 
  tokens(remove_punct = TRUE, 
         remove_numbers = TRUE, 
         remove_symbol = TRUE,
         remove_url = TRUE) %>% 
  tokens_tolower() %>% 
  tokens_remove(pattern = c(stopwords("pt"), "senhor*")) %>%  
  #tokens_wordstem() %>% 
  tokens_select(min_nchar = 4)

dfmat_pres <- toks_pres %>% 
  dfm()

pres_tidy <- disc_pres %>%
  unnest_tokens(word, text) %>%
  count(authors_min, word, sort = TRUE)

total_words <- pres_tidy %>% 
  group_by(authors_min) %>% 
  summarize(total = sum(n))

pres_tidy <- left_join(pres_tidy, total_words)

pres_tidy <- pres_tidy %>%
  bind_tf_idf(word, authors_min, n)
library(dplyr)
pres_tidy_lula <- pres_tidy %>%  filter(authors_min == "lula") %>% 
  select(word, tf_idf)

pres_tidy_bolso <- pres_tidy %>%  filter(authors_min == "bolsonaro") %>% 
  select(word, tf_idf)

pres_tidy_ciro <- pres_tidy %>%  filter(authors_min == "ciro") %>% 
  select(word, tf_idf)

pres_tidy_simone <- pres_tidy %>%  filter(authors_min == "simone") %>% 
  select(word, tf_idf)

#VISUALIZAZAO -----------------------------------------------------------


wordcloud2::wordcloud2(pres_tidy_lula)

wordcloud2::wordcloud2(pres_tidy_ciro)

wordcloud2::wordcloud2(pres_tidy_bolso)

wordcloud2::wordcloud2(pres_tidy_simone)



#SANKEY ------------------------------------------
library(quanteda.textstats)
toks_pres_att <- corpus_pres %>% 
  tokens() %>% 
  tokens_tolower() %>% 
  tokens_keep(pattern = c('lula', 'ciro', 'bolsonaro', 'simone'))

topwords <- toks_pres_att %>% 
  dfm() %>% 
  textstat_frequency(groups = authors_min) %>% 
  tibble() 

disc_pres_count <- disc_pres %>%
  group_by(authors_min, url) %>% 
  summarise(disc_dur = max(end) / 60) %>% 
  unique() %>% 
  mutate(n_disc = n()) %>% 
  ungroup() %>% 
  group_by(authors_min, n_disc) %>% 
  summarise(disc_dur = sum(disc_dur))

topwords <- topwords %>% 
  select(group, feature, frequency) %>% 
  left_join(disc_pres_count, by = c('group' = 'authors_min')) %>% 
  mutate(freq_dur = frequency / disc_dur) %>% 
  rename(source = group,
         target = feature,
         value = freq_dur)

library(networkD3)

topwords$target <- paste(topwords$target, "_citado", sep = "") 

nodes <- data.frame(name=c(as.character(topwords$source), 
                           as.character(topwords$target)) %>%
                      unique())

topwords$IDsource <- match(topwords$source, nodes$name)-1 
topwords$IDtarget <- match(topwords$target, nodes$name)-1

sankeyNetwork(Links = topwords, Nodes = nodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "value", NodeID = "name", 
              sinksRight=FALSE,
              fontFamily = "calibri",
              fontSize = 14)


######LINHA DO TEMPO  --------------------------------------------------------
#TRANSFORMACAO BASE -----------------------------------------------------------

library(stringr)
library(tidyr)

test <- disc_pres_group %>% 
  filter(authors != "Vera Lúcia (PSTU)") %>% 
  mutate(text = str_to_lower(text),
         Bolsonaro = str_count(text, "bolsonaro"),
         Lula = str_count(text, "lula"),
         Ciro = str_count(text, "ciro"),
         Simone = str_count(text, "simone|tebet"),
  ) %>% 
  pivot_longer(cols = Bolsonaro:Simone, names_to = "Citado", values_to = "frequência") %>% 
  select(-text)

test <- test %>% 
  mutate(
    datee = as.Date(date)
  ) %>% 
  filter(frequência > 0)


#VISUALIZAZAO -----------------------------------------------------------



shared_test <- SharedData$new(test)

#edicao crosstalk
bscols(widths = c(3, NA),
       list(
         filter_select("authors", "Autores", shared_test, ~authors),
         filter_select("source", "Fonte", shared_test, ~source)
       ),
       #edicao ggplot
       plotly::ggplotly(shared_test %>% 
                          ggplot(aes(x = datee,y=frequência,
                                     color = Citado,
                                     size=frequência,
                                     alpha =frequência,
                                     group=paste({authors},"<br>",{source})
                          )
                          ) + 
                          geom_point() +
                          theme_minimal(base_family = "Calibri", base_size = 14) +
                          labs(color = "Quem foi citado", size= "", alpha= "", y = "Frequência")+
                          theme(panel.grid.major.y = element_blank(),
                                panel.grid.minor.y = element_blank(),
                                plot.subtitle = element_text(face = "italic", size = 13))+
                          scale_x_date(date_breaks = "1 month", date_labels = "%B"),
                        tooltip = c("x", "group", "color", "y")
       ) %>% 
         #plotly
         layout(xaxis = list(rangeslider = list(type = "date"))) %>%
         config(displaylogo = FALSE) %>%
         config(modeBarButtonsToRemove = c('lasso2d', 'select2d', 'zoom2d'))
       
)
