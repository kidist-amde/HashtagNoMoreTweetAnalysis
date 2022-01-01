# set the path of your project folder 

setwd("/home/kidist/UOT/01-2nd-1st-sem/DSD/dsd-project")

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

# drop unwanted columns
tweets<-tweets %>% 
  select(created_at, text,count)

# Select only tweets with more than 5 words and from 01:00-04:00 UTC time
day1_tweets <- tweets %>%
  filter(count > 2 &
           created_at >= as.POSIXlt("2021-11-13 02:44:11+00:00", "UTC") & 
           created_at < as.POSIXlt("2021-11-14 00:00:00+00:00", "UTC"))


# sort by time: created_at
day1_tweets <- day1_tweets[order(day1_tweets$created_at),]

# generate vars hours and minutes
day1_tweets$hr <- hour(day1_tweets$created_at)
day1_tweets$mint <- minute(day1_tweets$created_at)

# recode time minutes in quarter
day1_tweets <- day1_tweets %>%
  mutate(m15 = ifelse(mint %in% 0:14, 1,
                      ifelse(mint %in% 15:29, 2,
                             ifelse(mint %in% 30:44, 3, 4))))
day1_tweets <- day1_tweets %>%
  mutate(h = ifelse(hr %in% 1, 0,
                    ifelse(hr %in% 2, 4, 8)))

table(day1_tweets$h,day1_tweets$m15)

# Generate new time interval variable
day1_tweets$timeclas <- day1_tweets$h+day1_tweets$m15

# Check the correctness of recode
table(day1_tweets$hr,day1_tweets$timeclas)
table(day1_tweets$mint,day1_tweets$timeclas)
# drop the column we don't need
day1_tweets$hr <- NULL
day1_tweets$mint <- NULL
day1_tweets$h <- NULL
day1_tweets$m15 <- NULL

# add id to tweets
day1_tweets<- day1_tweets %>% 
  mutate(
    id = 1:n()
  )

#Generate the qunteda corpus

day1_tweets_corpus <- corpus(day1_tweets) 
day1_tweets_corpus

# Assigns a unique identifier to each text
docvars(day1_tweets_corpus, "Textno") <- sprintf("%02d", 1:ndoc(day1_tweets_corpus))

# Document variables list
day1_tweets_corpus %>% docvars() %>% names()

# inspect the document-level variables
head(docvars(day1_tweets_corpus))
tail(docvars(day1_tweets_corpus))

# Generate quanteda corpus

pippo<-dfm(day1_tweets_corpus, 
           include_docvars = TRUE) 

pippo<-dfm(pippo, remove = "$")

# drop tweet less than 2 words
pippo<-dfm_subset(pippo, ntoken(pippo) > 2)
ndoc(pippo)

#Further, after removal of function words and punctuation in dfm(), 
#we keep only the top 5% of the most frequent features (min_termfreq = 0.90) 
#that appear in less than 10% of all documents (max_docfreq = 0.1) using dfm_trim() 
#to focus on common but distinguishing features.

pippo<-dfm_trim(pippo, 
                min_termfreq = 0.90, 
                termfreq_type = "quantile", 
                max_docfreq = 0.1, 
                docfreq_type = "prop")

# drop tweet less than 3 words
pippo<-dfm_subset(pippo, ntoken(pippo) > 3)
ndoc(pippo)

#Structural topic models (STM) 
# Calculate the STM 
dfm2stm <- convert(pippo, to = "stm")

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

#* To get a first insight, we print the terms that appear in each topic.
xx1 <- as.data.frame(t(labelTopics(model.stm, n = 10)$prob))

#* The following plot allows us to intuitively get information on the share of 
#* the different topics at the overall corpus.

plot(
  model.stm,
  type = "summary",
  text.cex = 0.9,
  main = "STM topic shares",
  xlab = "Share estimation"
)


#* 
#* Using the package stm, we can now visualize the different words of a topic with 
#* a wordcloud. Since topic 4 has the highest share, we use it for the next 
#* visualization. The location of the words is randomized and changes each time 
#* we plot the wordcloud while the size of the words is relative to their 
#* frequency and remains the same.
#* 

cloud(model.stm,
      topic = 2,
      scale = c(3.25, .5))


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
     topics = c(2, 4),
     main = "Putting two different topics in perspective")



plot(model.stm,
     type = "perspectives",
     topics = c(5, 4),
     main = "Putting two different topics in perspective")

# Next, we calculate the prevalence of the topics over time. 

modell.stm.labels <- labelTopics(model.stm, 1:n.topic)

dfm2stm$meta$datum <- as.numeric(pippo$timeclas)

modell.stm.effekt <- estimateEffect(1:n.topic ~ s(timeclas), 
                                    model.stm, 
                                    meta = dfm2stm$meta,
                                    prior = 1e-5)
par(mfrow=c(4,2))

modell.stm.labels <- labelTopics(model.stm, 1:n.topic)

dfm2stm$meta$datum <- as.numeric(pippo$timeclas)

modell.stm.effekt <- estimateEffect(1:n.topic ~ s(timeclas), 
                                    model.stm, 
                                    meta = dfm2stm$meta,
                                    prior = 1e-5)
par(mfrow=c(4,2))

for (i in 1:5)
{
  plot(modell.stm.effekt, 
       "timeclas", 
       method = "continuous", 
       topics = i, 
       main = paste0(modell.stm.labels$prob[i,1:3], 
                     collapse = ", "), 
       ylab = "", printlegend = F)
}

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



