# import important libraries 
library(wordcloud) 
library(igraph)
library(twinetverse)
#________________________________________________________________________
# data cleaning remove(stop-words,special charters , numbers,url)
words <- df %>%
  mutate(text = str_remove_all(text, "&amp;|&lt;|&gt;"),
         text = str_remove_all(text, "\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)"),
         text = str_remove_all(text, "[^\x01-\x7F]")) %>% 
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'"),
         str_detect(word, "[a-z]"),
         !str_detect(word, "^#"),  
         !str_detect(word, "^$"),
         !str_detect(word, "@\\S+")) %>%
  count(word, sort = TRUE)
# word cloud 
words %>% 
  with(wordcloud(word, n, random.order = FALSE, max.words = 100, colors = "#800000"))
#________________________________________________________________________
