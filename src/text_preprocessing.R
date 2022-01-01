# set the path of your project folder 

setwd("/home/kidist/UOT/01-2nd-1st-sem/DSD/dsd-project")


library(tidyverse) 
library(tidytext)
library(tm) #stripWhitespace
library(lubridate) # to get date information 
library(quanteda) # ntoken
library(ggplot2)

# Read  the data 

df = read.csv("processed_data/combined_csv.csv")

# drop un wanted column of the dataframe
tweets<-df %>% 
  select(user_id, status_id, created_at, text,hashtags,mentions_screen_name,
         place_full_name,screen_name,source,favorite_count,favourites_count,location)

# remove apostrophes 
tweets$text <-  str_replace_all(tweets$text, "'"," ")
# remove \n
tweets$text <-  str_replace_all(tweets$text, "\n"," ")
# convert word to lowercase 
tweets$text <- tolower(tweets$text)

# remove all punctuation and digits 

tweets <- tweets %>%
  mutate(text = str_remove_all(text, "&amp;|&lt;|&gt;"),
         text = str_remove_all(text, "\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)"),
         text = str_remove_all(text, "[^\x01-\x7F]"),
         text = str_remove_all(text, "@"),
         text = str_remove_all(text, "#"),
         text = str_remove_all(text, "~"),
         text = str_remove_all(text, "-"),
         text = str_remove_all(text, '\\"'),
         text = str_remove_all(text, '\\.'),
         text = str_remove_all(text, '\\,'),
         text = str_remove_all(text, '[[:digit:]]'),
         text = str_remove_all(text, '[[:punct:]]')
  )
tweets$text[110]
# remove stop words 
tweets$text <- removeWords(tweets$text, stopwords("english"))
tweets$text[110]

# collapsed Multiple white-space to a single blank
tweets$text <- stripWhitespace(tweets$text)
tweets$text[110]

# replace different representation of same word to single format 
tweets$text <- str_replace_all(tweets$text, "no more","nomore")
tweets$text <- str_replace_all(tweets$text, "ethio","ethiopia")

# get the token and append to data-frame as new column
tweets$count <- ntoken(tweets$text) 
table(tweets$count) 

# sort the tweets by time: created_at
tweets <- tweets[order(tweets$created_at),]

# get the week number of the year
week_num= strftime(tweets$created_at, format = "%V")
typeof(week_num)
# convert week_num to integer 
week_num = strtoi(week_num)
typeof(week_num)

# set the week number in range of 1-6
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
  geom_histogram(color="#006400", fill="#800000",binwidth=0.99)

#_____________________________________________________________
# save cleaned tweet data
save(tweets,file="processed_data/cleaned_tweets.RData")
#______________________________________________________________




