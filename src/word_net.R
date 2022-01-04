# set the path of your project folder 

setwd("...")

# import libraries 
library(tm) # corpus
library(tidyverse)  # count 
library(tidytext)
library(textstem) # lemmatization
library(wordcloud)
library(ggraph)
library(igraph)
library(Rgraphviz)# word network association 
library(dplyr)
library(quanteda)

# load the cleaned tweet data
load("processed_data/cleaned_tweets.RData")

# drop unwanted columns from the data-frame 
tweets <-tweets  %>% 
      select(text,created_at,count,week_number)

# display how many user have how many tokens in their tweets  
table(tweets$count)

# Generate dataset / vector of text
myCorpus <- Corpus(VectorSource(tweets$text))
inspect(myCorpus[1])

#lemmatize strings 
myCorpus <- tm_map(myCorpus, lemmatize_strings)
inspect(myCorpus[1])

# word cloud
wordcloud(myCorpus, min.freq = 700,
          max.words=300, random.order=FALSE, 
          colors=brewer.pal(10, "Dark2"))


# term document Matrix/ frequency of terms that occur in all texts

tdm <- TermDocumentMatrix(myCorpus, control = list(wordLengths = c(3, Inf)))
tdm$dimnames$Terms

# how many times nomore mentioned in the doc
idx <- which(dimnames(tdm)$Terms == "nomore")
# co-occurrence of no more with other terms
as.matrix(tdm[idx, 1:100])
inspect(tdm[idx + (0:20), 1:210])

# Plot word frequencies

# term doc matrix of only the first 30000 corpus due to memory issue

tdm <- TermDocumentMatrix(myCorpus[1:30000], control = list(wordLengths = c(2, Inf)))
term.freq <- rowSums(as.matrix(tdm))

# convert it as data frame 

df <- data.frame(term = names(term.freq), freq = term.freq)

# select top 10 frequent term

df <- df %>%
    top_n(10)
# term frequency of the first 10

theme_set(theme_gray())
g <- ggplot(df, aes(x=term, y=freq))
g + geom_bar(stat="identity", width = 0.5, fill="#9ACD32") + 
  labs(title="Term frequency", 
       x = "Terms", y = "count",)+ coord_flip()
  theme(axis.text.x = element_text(angle=65, vjust=0.6))


# word network association 

tdm <- TermDocumentMatrix(myCorpus, control = list(wordLengths = c(3, Inf)))
  
#inspect frequent words , low frequency of 20,000

freq.terms <- findFreqTerms(tdm, lowfreq=20000)
freq.terms

windows()
plot(tdm, term = freq.terms, corThreshold = 0.07, weighting = T,
     attrs = list(graph = list(rankdir = "BT"),
                  node = list(shape = "plaintext", fontsize=60)))

#______________________________________________________________________

# generate couple of consecutive words Bi-gram

words <- tweets %>%
  unnest_tokens(word, text, token = "ngrams", n = 2)  %>%
  count(word, sort = TRUE)

# split couples of words in variables
words <- words %>%
  separate(word, c("word1", "word2"), sep = " ")

#  plot word network
windows()
words %>%
  filter(n >= 3000) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point( color = "#B8860B",size = 2) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 4) +
  labs(title = "Bi-gram Word Network",
       subtitle = " ",
       x = "", y = "")

