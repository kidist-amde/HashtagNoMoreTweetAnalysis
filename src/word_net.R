# set the path of your project folder 

setwd("/home/kidist/UOT/01-2nd-1st-sem/DSD/dsd-project")

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

# load the cleaned tweet data
load("processed_data/cleaned_tweets.RData")
# drop unwanted columns from the dataframe 
tweets <-tweets  %>% 
      select(text,created_at,count,week_number)

# display how many user have how many tokens in their tweets  
table(tweets$count)

# select only users having word more than 10
tweets <- tweets %>%
  filter (count > 10)

# Generate tm dataset
myCorpus <- Corpus(VectorSource(tweets$text))
inspect(myCorpus[1])

#lemmatize strings 
myCorpus <- tm_map(myCorpus, lemmatize_strings)
inspect(myCorpus[1])

# word cloud
wordcloud(myCorpus, min.freq = 700,
          max.words=300, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# Frequency words and Association

tdm <- TermDocumentMatrix(myCorpus, control = list(wordLengths = c(2, Inf)))

 #inspect frequent words

freq.terms = findFreqTerms(tdm, lowfreq = 500)
freq.terms

idx <- which(dimnames(tdm)$Terms == "nomore")
as.matrix(tdm[idx, 1:100])
inspect(tdm[idx + (0:20), 1:210])


# Plot word frequencies

# term doc matrix of only the first 50 corpus due to memoty issue
tdm <- TermDocumentMatrix(myCorpus[1:50], control = list(wordLengths = c(2, Inf)))
term.freq <- rowSums(as.matrix(tdm))
df <- data.frame(term = names(term.freq), freq = term.freq)
# select only users having word more than 10

df1 <- df %>%
  filter (freq >=15)%>%
  top_n(10)
# term frequency of the first 10

theme_set(theme_classic())
g <- ggplot(df1, aes(x=term, y=freq))
g + geom_bar(stat="identity", width = 0.5, fill="#9ACD32") + 
  labs(title="Term frequency", 
       x = "Terms", y = "count",)+ coord_flip()
  theme(axis.text.x = element_text(angle=65, vjust=0.6))


# word network association 

tdm <- TermDocumentMatrix(myCorpus, control = list(wordLengths = c(3, Inf)))

# select only 20,000x  frequent 
freq.terms <- findFreqTerms(tdm, lowfreq=20000)
freq.terms

windows()
plot(tdm, term = freq.terms, corThreshold = 0.07, weighting = T,
     attrs = list(graph = list(rankdir = "BT"),
                  node = list(shape = "plaintext", fontsize=80)))

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
  filter(n >= 800) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "#B8860B", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 4) +
  labs(title = "Word Network",
       subtitle = " ",
       x = "", y = "")
# Cancel single couples of network

tweet_counts1 <- words %>%
  filter(n >= 800)
wx1 <- tweet_counts1  %>%
  count(word1, sort=TRUE) 
wx2 <- tweet_counts1 %>%
  count(word2, sort=TRUE) 

#merge the frequencies
tweet_counts1 <- rename(tweet_counts1, c(ntot="n"))
tweet_counts1<-merge(tweet_counts1,wx2,by.x="word2",by.y="word2", all.x=TRUE)

#Rename Variable
tweet_counts1 <- rename(tweet_counts1, c(n2="n"))
tweet_counts1<-merge(tweet_counts1,wx1,by.x="word1",by.y="word1", all.x=TRUE)
tweet_counts1 <- rename(tweet_counts1, c(n1="n"))

# Select the sub sample
tweet_counts1 <- subset(tweet_counts1, subset = n1>1 | n2>1)

tweet_counts1$n1 <- NULL
tweet_counts1$n2 <- NULL

tweet_counts1$lnn<- log(tweet_counts1$ntot)

# plot word network
windows()
tweet_counts1 %>%
  filter(ntot >= 800) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = lnn, edge_width = lnn)) +
  geom_node_point(color = "#B8860B", size = 2.5) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 4) +
  labs(title = "Word Network: Tweets text: language=en",
       subtitle = "Text mining twitter data ",
       x = "", y = "")

