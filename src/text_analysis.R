# import libraries 
library(tm) # corpus
library(quanteda) # find ntoken
library(tidyverse)  # count 
library(textstem) # lemmatization
library(wordcloud)
library(graph)
library(Rgraphviz)# word network association 


# load the cleaned tweet data
load("all_csv_file/cleaned_tweets.RData")

#Find number of tokens/ count and put the frequency of each word on new column  

tweets$count <- ntoken(tweets$text)

# display how many user have how many word 
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



