# set the path of your project folder 

setwd("...")

# load the libraries
library(tidyverse)
library(tidytext)
library(quanteda)
require(lubridate)
require(topicmodels)
require(tm)
library(stringr)
library(stm)

# load data 
load("processed_data/cleaned_tweets.RData")

# select only important columns for the task
tweets<-tweets %>% 
  select(created_at, text,count,week_number)

#Generate the qunteda corpus

tweets_corpus <- corpus(tweets) 

# Assigns a unique identifier to each text
docvars(tweets_corpus, "Text_ID") <- sprintf("%02d", 1:ndoc(tweets_corpus))

# Document variables list
tweets_corpus %>% docvars() %>% names()

# inspect the document-level variables
head(docvars(tweets_corpus))
tail(docvars(tweets_corpus))

# Generate quanteda corpus

q_df<-dfm(tweets_corpus, 
           include_docvars = TRUE) 

# keep only words occurring 90% times and in at most 75% of the documents

q_df1<-dfm_trim(q_df, 
                min_termfreq = 0.9, 
                termfreq_type = "quantile", 
                max_docfreq = 0.75, 
                docfreq_type = "prop")

length(q_df) - length(q_df1)

# Calculate the Structural topic models (STM) 

dfm2stm <- convert(q_df1, to = "stm")

# define the number of topics
n.topic <- 5

# estimate the STM model

model.stm <- stm(
  dfm2stm$documents,
  dfm2stm$vocab,
  K = n.topic,
  data = dfm2stm$meta,
  init.type = "Spectral"
)

# print the terms that appear in each topic.
topic_term <- as.data.frame(t(labelTopics(model.stm, n = 10)$prob))


# plot different topics at the overall corpus.

plot(
  model.stm,
  type = "summary",
  text.cex = 0.9,
  main = "STM topic shares",
  xlab = "Share estimation"
)



# Using the package stm, we can now visualize a topic with a wordcloud.

cloud(model.stm,
      topic = 5,
      scale = c(5, .3))


plot(model.stm, 
     type = "hist", 
     topics = sample(1:n.topic, 
                     size = 5, 
                     replace = TRUE), 
     main = "histogram of the topic shares within the documents")

# set up graph parameter visualisation
par(mar=c(1.5, 1.5, 1.5, 1.5))
par(mfcol=c(1,1))
plot(model.stm, type = "labels", topics = c(1, 2, 3, 4,5), main = "Topic terms")

plot(model.stm,
     type = "perspectives",
     topics = c(1, 2),
     main = "Putting two different topics in perspective")

plot(model.stm,
     type = "perspectives",
     topics = c(2,3),
     main = "Putting two different topics in perspective")

plot(model.stm,
     type = "perspectives",
     topics = c(3, 4),
     main = "Putting two different topics in perspective")

plot(model.stm,
     type = "perspectives",
     topics = c(4, 5),
     main = "Putting two different topics in perspective")

plot(model.stm,
     type = "perspectives",
     topics = c(1, 5),
     main = "Putting two different topics in perspective")


# STM topic (count) tuning ==>  searchK
#number of topics can also be determined for an STM model.,

mein.stm.idealK <- searchK(dfm2stm$documents, 
                           dfm2stm$vocab, 
                           K = seq(4, 20, by = 2), 
                           max.em.its = 75)
plot(mein.stm.idealK)


# correlations between topics. 
mod.out.corr <- topicCorr(model.stm, 
                          method = c("simple", "huge"), 
                          cutoff = 0.01, 
                          verbose = TRUE)
par(mfrow=c(1,1))

#Graphical display of topic correlations
plot(mod.out.corr)
plot.topicCorr(mod.out.corr)



