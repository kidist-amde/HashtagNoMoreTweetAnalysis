library(lubridate) # to get date information 
library(quanteda) # ntoken
library(ggplot2)

# load the data 
load("all_csv_file/cleaned_tweets.RData")

# drop some columns
tweets<-tweets %>% 
  select(created_at, text,)

# get the token-count 
tweets$count <- ntoken(tweets$text) 
table(tweets$count) 

# sort by time: created_at
tweets <- tweets[order(tweets$created_at),]

# get the week number of the year
week_num= strftime(tweets$created_at, format = "%V")
typeof(week_num)
week_num = strtoi(week_num)
typeof(week_num)

# convert the week number in range of 1-6
week_num = week_num - min(week_num) +1 
unique(week_num)

# Split the texts into  weekly  interval and append as new column 

tweets$week_number  = week_num
table(tweets$week_number)

#  plot the weekly distribution of the tweet

ggplot(tweets, aes(x=tweets$week_number)) + 
  xlab("week_number") + 
  ylab("tweet_count") + 
  labs(title = "weekly distribution of NoMore tweets")+
  geom_histogram(color="purple", fill="tomato",binwidth=0.99)
 
  
# save cleaned weekly  tweet data 
save(tweets,file="all_csv_file/cleaned_weekly_tweets.RData")