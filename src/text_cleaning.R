library(tidyverse) 
library(tidytext)
library(tm) #stripWhitespace
# Read  the data 

df = read.csv("all_csv_file/combined_csv.csv")

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

#_____________________________________________________________
# save cleaned tweet data
save(tweets,file="all_csv_file/cleaned_tweets.RData")
#______________________________________________________________


library(lubridate) # to get date information 
# extract data information
tweets$tweet_date <- date(tweets$created_at)
table(tweets$tweet_date)
# Split the texts into  weekly  interval 

tweets <- tweets %>%
  mutate(tweeted_in_week = ifelse(tweet_date <= as.POSIXlt("2021-11-20", "UTC"), "1",
                                  ifelse((tweet_date > as.POSIXlt("2021-11-20", "UTC") & 
                                            tweet_date <= as.POSIXlt("2021-11-27", "UTC")), "2",
                                         ifelse((tweet_date > as.POSIXlt("2021-11-27", "UTC") & 
                                                   tweet_date <= as.POSIXlt("2021-12-04", "UTC")), "3",
                                                ifelse((tweet_date > as.POSIXlt("2021-12-04", "UTC") & 
                                                          tweet_date <= as.POSIXlt("2021-12-11", "UTC")), "4",
                                                       ifelse(tweet_date >= as.POSIXlt("2021-12-12", "UTC"), "5", "F"))))))
table(tweets$tweeted_in_week) 