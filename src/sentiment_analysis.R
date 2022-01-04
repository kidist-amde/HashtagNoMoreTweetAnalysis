# set the path of your project folder 

setwd("...")

# libraries 
library(tidyverse)
library(tidytext)
library(quanteda) # ntoken
library(syuzhet) # sentiment 
library("gridExtra")
library("cowplot")
require(lubridate)
library(dplyr)


# load cleaned tweet_data
load("processed_data/cleaned_tweets.RData")

# convert sentences tokens from a text into a vector
sentences_vec <- get_sentences(tweets$text)

class(sentences_vec) 
str(sentences_vec)  
head(sentences_vec)

# asses the sentiment of each sentence using syuzhet
sentences_vec_sentiment <- get_sentiment(sentences_vec, method="syuzhet")

head(sentences_vec_sentiment, n=20)

# summary  
summary(sentences_vec_sentiment)

# append the sentiment vector  value to data-frame as new column
tweets["sent_vec"] = sentences_vec_sentiment

# create new column with date info 
tweets["date"] = as.Date(tweets$created_at)

# mean  sentiment values for each date 
sentiment_by_date = tweets %>%
  group_by(date) %>%
  summarise_at(vars(sent_vec), list(sentiment_mean = mean))

#plot the values in a graph
plot(
  sentiment_by_date, 
  type="l", 
  main="Emotional valence in the tweets of each day", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence"
)

# add the time zone information 

tweets["created_at"]<- as.POSIXct(tweets$created_at, tz="UTC")


# create new column with only date info 
tweets["hours"] = format(tweets$created_at,"%H")

# mean  sentiment values for each hours 
sentiment_by_hour = tweets %>%
  group_by(hours) %>%
  summarise_at(vars(sent_vec), list(sentiment_mean = mean))

plot(
  sentiment_by_hour, 
  type="l", 
  main="Emotional valence in the tweets of each Hours", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence"
)

# Generate the emotion then attach as a new column name 

tweets$emotion <- as.data.frame(get_nrc_sentiment(tweets$text))

# classify  each emotion according to the class they belong to

tweets$anger <- tweets$emotion$anger
tweets$anticipation <- tweets$emotion$anticipation
tweets$disgust <- tweets$emotion$disgust
tweets$fear <- tweets$emotion$fear
tweets$joy <- tweets$emotion$joy
tweets$sadness <- tweets$emotion$sadness
tweets$surprise <- tweets$emotion$surprise
tweets$trust <- tweets$emotion$trust
tweets$negative <- tweets$emotion$negative
tweets$positive <- tweets$emotion$positive

# drop the emotion column
tweets$emotion <- NULL

#* Plot sentiments score over time

ggplot(tweets, 
       aes(x=created_at , 
           y=sent_vec, colour=hours)) + geom_line() +
  ggtitle("Nomore tweet Sentiment") + 
  xlab("Hour") + 
  ylab("Sentiment") + 
  scale_x_datetime(date_labels ='%H:%M', date_breaks ="hours")

#Here we insert the parameter required
ggplot(tweets, 
       aes(x=created_at, 
           y=sent_vec)) + 
  geom_smooth(method = 'gam', formula= y ~ s(x, bs = "cs")) +
  ggtitle("Nomore tweets Sentiment") + 
  xlab("Hour") + 
  ylab("Sentiment") + 
  scale_x_datetime(date_labels = '%H:%M', date_breaks = "hours")

# or 
ggplot(tweets, 
       aes(x=created_at, 
           y=sent_vec)) + 
  geom_smooth(method = 'gam', formula= y ~ s(x, bs = "cs"), se = FALSE) +
  ggtitle("Nomore tweets Sentiment") + 
  xlab("Hour") + 
  ylab("Sentiment") + 
  scale_x_datetime(date_labels = '%H', date_breaks = "hours")

#* Plot the emotion distribution as an histogram

# Select all  the  emotions from the data frame
names(tweets)
sent_emo <- data.frame(colSums(tweets[,18:27]))

# rename var
names(sent_emo) <- "Count"
sent_emo <- cbind("sentiment"=rownames(sent_emo),sent_emo)

#* barchart  of detailed emotions
windows()
ggplot(data=sent_emo,
       aes(x=fct_reorder(sentiment, Count, .desc = TRUE), y=Count))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Emotions")+ylab("Total tweets")+
  ggtitle("Nomore tweets emotions")+
  theme_minimal()

#* Plot the positive and negative emotions as an histogram
pos_neg_sent <- data.frame(colSums(tweets[,26:27]))
# rename var
names(pos_neg_sent) <- "Count"
pos_neg_sent <- cbind("sentiment"=rownames(pos_neg_sent),pos_neg_sent)

#* Histogram of pos and neg  emotions

windows()
ggplot(data=pos_neg_sent,
       aes(x=fct_reorder(sentiment, Count, .desc = TRUE), y=Count))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Emotions")+ylab("Total tweets")+
  ggtitle("Nomore tweets postive  & negative emotions ")+
  theme_minimal()


# Split sentiments by hour 

table(tweets$hours,tweets$anger)

# aggregate emotions by hour
emo_hours <- aggregate(x = tweets[,18:27], by = list(tweets$hours), FUN = sum)

# generate new variable group
emo_hours$hours <- paste("Hour", emo_hours$Group.1, sep = "")

# delete the oldest
emo_hours$Group.1 <- NULL

# Labels the row names of data frame with var hour
emo_hours <- column_to_rownames(emo_hours, var = "hours") 
emo_hours$hours = c(0:23)
  
ggplot(emo_hours,
          aes(x=hours,"emotions"))+
geom_line(aes(y=anger,group = 1 ,color="anger") ) +
geom_line(aes(y=anticipation,group = 1 ,color="anticipation") ) +
geom_line(aes(y=disgust,group = 1 ,color="disgust") ) +
geom_line(aes(y=fear,group = 1 ,color="fear") ) +
geom_line(aes(y=joy,group = 1 ,color="joy") ) +
geom_line(aes(y=sadness,group = 1 ,color="sadness") ) +
geom_line(aes(y=surprise,group = 1 ,color="surprise") ) +
geom_line(aes(y=trust,group = 1 ,color="trust") ) +
geom_line(aes(y=negative,group = 1 ,color="negative") ) +
geom_line(aes(y=positive,group = 1 ,color="positive") ) +
labs(x = "Hours",y = "sentiment count")+
  ggtitle("Split sentiments by hour ")
  
  





