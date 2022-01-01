# libraries 
library(tidyverse)
library(tidytext)
library(quanteda) # ntoken
library(syuzhet) # sentiment 
library("gridExtra")
library("cowplot")
require(lubridate)

# load cleaned tweet_data
load("all_csv_file/cleaned_tweets.RData")

# drop unwanted columns
tweets<-tweets %>% 
  select(created_at, text,)


# get the token-count 

tweets$count <- ntoken(tweets$text) 
table(tweets$count) 

# filter a day tweets with count > 2

min(tweets$created_at)

day1_tweets <- tweets %>%
  filter(count > 2 &
          created_at >= as.POSIXlt("2021-11-13 02:44:11+00:00", "UTC") & 
            created_at < as.POSIXlt("2021-11-14 00:00:00+00:00", "UTC"))

# see the changes in the day1 tweet in times  with d/t condition

min(day1_tweets$created_at)
max(day1_tweets$created_at)

day1_tweets<- day1_tweets %>%
  mutate(timex1 = ifelse(created_at < as.POSIXlt("2021-11-13 03:44:25", "UTC"), "0",
                         ifelse((created_at >= as.POSIXlt("2021-11-13 03:44:25", "UTC") & 
                                   created_at < as.POSIXlt("2021-11-13 12:27:17", "UTC")), "1",
                                ifelse((created_at >= as.POSIXlt("2021-11-13 12:27:17", "UTC") & 
                                          created_at < as.POSIXlt("2021-11-13 23:00:00", "UTC")), "2",
                                       ifelse(created_at >= as.POSIXlt("2021-11-13 23:00:00", "UTC"), "3", "F")))))
table(day1_tweets$timex1)

# sort the tweet they the time they created_at
day1_tweets <- day1_tweets[order(day1_tweets$created_at),]

# drop some column we don't use anymore

day1_tweets$count <- NULL


# convert sentences tokens from a text into a vector
sentences_vec <- get_sentences(day1_tweets$text)

class(sentences_vec) 
str(sentences_vec)  
head(sentences_vec)

# asses the sentiment of each sentence using syuzhet
sentences_vec_sentiment <- get_sentiment(sentences_vec, method="syuzhet")
head(sentences_vec_sentiment, n=20)

# the overall emotional valence in the text
sum(sentences_vec_sentiment)

# the mean emotional valence
mean(sentences_vec_sentiment)

# summary  
summary(sentences_vec_sentiment)

#plot the values in a graph
plot(
  sentences_vec_sentiment, 
  type="l", 
  main="Emotional valence in the Nomore tweets ", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence"
)

# represent interns of percentage/how the general emotion change in time

percent_vals <- get_percentage_values(sentences_vec_sentiment, bins = 50)

plot(
  percent_vals, 
  type="l", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence", 
  col="#006400"
)
mytitle = "Emotional valence in the Nomore tweets"
mysubtitle = "using Percentage-Based Means"
mtext(side=3, line=3, at=-0.07, adj=0, cex=1, mytitle)
mtext(side=3, line=2, at=-0.07, adj=0, cex=0.7, mysubtitle)

# create data_frame of the sentiment
sentences_vec_sentiment_df <- as.data.frame(sentences_vec_sentiment)
# attach the time information as a new column
sentences_vec_sentiment_df$time <- day1_tweets$created_at
#  add time zone info on the dataframe time info
typeof(sentences_vec_sentiment_df$time)
sentences_vec_sentiment_df["time"]<- as.POSIXct(sentences_vec_sentiment_df$time, tz="UTC")
# plot the tweets over time not only the distribution

ggplot(sentences_vec_sentiment_df, 
       aes(x=time, 
           y=sentences_vec_sentiment)) + geom_line() +
  ggtitle("Nomore tweets Sentiment") + 
  xlab("Hour") + 
  ylab("Sentiment") + 
  scale_x_datetime(date_labels = '%H:%M', date_breaks = "hours")

#tweets are not evenly distributed /mainly concentrated 12:00 - 18:00. 
# generate a data frame with sentiment and emotions for tweet b/n 01-04

day1_tweets <- day1_tweets %>%
  filter(created_at >= as.POSIXlt("2021-11-13 12:00:00", "UTC") & 
           created_at < as.POSIXlt("2021-11-13 22:00:00", "UTC"))

table(day1_tweets$timex1)

# add the time zone information
day1_tweets["created_at"]<- as.POSIXct(day1_tweets$created_at, tz="UTC")


# generate sentence  vector 
day1_tweets$s_v <- (get_sentiment(day1_tweets$text, method="syuzhet"))

# Generate the emotion classification of tweets using get_nrc_sentiment
# then attach as a new column name 
day1_tweets$emo <- as.data.frame(get_nrc_sentiment(day1_tweets$text))

# classify  each emotion according to the class they belong to
day1_tweets$anger <- day1_tweets$emo$anger
day1_tweets$anticipation <- day1_tweets$emo$anticipation
day1_tweets$disgust <- day1_tweets$emo$disgust
day1_tweets$fear <- day1_tweets$emo$fear
day1_tweets$joy <- day1_tweets$emo$joy
day1_tweets$sadness <- day1_tweets$emo$sadness
day1_tweets$surprise <- day1_tweets$emo$surprise
day1_tweets$trust <- day1_tweets$emo$trust
day1_tweets$negative <- day1_tweets$emo$negative
day1_tweets$positive <- day1_tweets$emo$positive

# delete the emo data frame
day1_tweets$emo <- NULL

#* Plot sentiments score over time

ggplot(day1_tweets, 
       aes(x=created_at , 
           y=s_v, colour=timex1)) + geom_line() +
  ggtitle("Nomore tweet Sentiment") + 
  xlab("Hour") + 
  ylab("Sentiment") + 
  scale_x_datetime(date_labels ='%H:%M', date_breaks ="hours")

#Here we insert the parameter required
ggplot(day1_tweets, 
       aes(x=created_at, 
           y=s_v)) + 
  geom_smooth(method = 'gam', formula= y ~ s(x, bs = "cs")) +
  ggtitle("Nomore tweets Sentiment") + 
  xlab("Hour") + 
  ylab("Sentiment") + 
  scale_x_datetime(date_labels = '%H:%M', date_breaks = "30 min")

# or 
ggplot(day1_tweets, 
       aes(x=created_at, 
           y=s_v)) + 
  geom_smooth(method = 'gam', formula= y ~ s(x, bs = "cs"), se = FALSE) +
  ggtitle("Nomore tweets Sentiment") + 
  xlab("Hour") + 
  ylab("Sentiment") + 
  scale_x_datetime(date_labels = '%H:%M', date_breaks = "30 min")

#* Plot the emotion distribution as an histogram
# Select all  the  emotions from the data frame
names(day1_tweets)
sent_emo <- data.frame(colSums(day1_tweets[,5:14]))

# rename var
names(sent_emo) <- "Score"
sent_emo <- cbind("sentiment"=rownames(sent_emo),sent_emo)

#* Histogram of detailed emotions
windows()
ggplot(data=sent_emo,
       aes(x=fct_reorder(sentiment, Score, .desc = TRUE), y=Score))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Emotions")+ylab("Total tweets")+
  ggtitle("Nomore tweetsemotions")+
  theme_minimal()

#* Plot the positive and negative emotions as an histogram
sent_pone <- data.frame(colSums(day1_tweets[,13:14]))
# rename var
names(sent_pone) <- "Score"
sent_pone <- cbind("sentiment"=rownames(sent_pone),sent_pone)
#* Histogram of pos and neg  emotions

windows()
ggplot(data=sent_pone,
       aes(x=fct_reorder(sentiment, Score, .desc = TRUE), y=Score))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Emotions")+ylab("Total tweets")+
  ggtitle("Nomore tweets + & - emot")+
  theme_minimal()


# Split sentiments by time 

table(day1_tweets$timex1,day1_tweets$anger)

# aggregate emotions
emo_time <- aggregate(x = day1_tweets[,5:14], by = list(day1_tweets$timex1), FUN = sum)

# generate new variable group
emo_time$time <- paste("Time", emo_time$Group.1, sep = "")

# delete the oldest
emo_time$Group.1 <- NULL

# Labels the row names of data frame with var time 
emo_time <- column_to_rownames(emo_time, var = "time") 

# flip data frame/ change row to colum and colum to row 
emo_time_f <- emo_time %>%
  rownames_to_column %>% 
  gather(var, value, -rowname) %>% 
  spread(rowname, value) 

# Calculate the columns percentage/  for each column 
# 
emo_time_f <- emo_time_f %>%
  mutate(
         time1_pc = (Time1 / sum(Time1))*100,
         time2_pc = (Time2 / sum(Time2))*100
  )


#Chart of graphs all time slots on a single page

g1 <- ggplot(data=emo_time_f,
             aes(x=fct_reorder(var, time1_pc, .desc = FALSE), y=time1_pc))+
  geom_bar(aes(fill=var),stat = "identity")+
  xlab("Emotions")+ 
  ylab("")+
  theme_minimal()+
  theme(legend.position="none") + 
  coord_flip()

g2 <- ggplot(data=emo_time_f,
             aes(x=fct_reorder(var, time2_pc, .desc = FALSE), y=time2_pc))+
  geom_bar(aes(fill=var),stat = "identity")+
  xlab("")+ 
  ylab("")+
  theme_minimal() +
  theme(legend.position="none") + 
  coord_flip()

plot_grid(g1, g2, labels=c("13:00-14:00", "after 14:00"), ncol = 2, nrow = 2)












