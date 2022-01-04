# set the path of  your project folder 
setwd("...")

# import libraries 
library(rtweet)
library(tidyverse) 
library(tidytext)
library(ggplot2)
library(tweetrmd)
library(emo)
library(widyr)
library(wordcloud)
library(tm)
library(igraph)
library(ggraph)
library(leaflet)
# load the data frame 
tweets_df <- read.csv(file = "processed_data/combined_csv.csv")

# explore the data
ncol(tweets_df)
nrow(tweets_df)
dim(tweets_df)

# number of users participated in the tweets
length(unique(tweets_df$user_id))

# plot points on top of a leaflet basemap

site_locations <- leaflet(tweets_df) %>%
  addTiles() %>%
  addCircleMarkers(lng = ~longitude, lat = ~latitude, popup = ~text,
                   radius = 3, stroke = FALSE)

site_locations
# How many unique places are captured by tweets containing the word “#NoMore”?

length(unique(tweets_df$place_full_name))
length(unique(tweets_df$location))


#create a table of the top 20 places tweeting about #nomore

place = tweets_df %>% 
  filter(is.na(place_full_name) == FALSE & place_full_name != "") %>% 
  count(place_full_name, sort = TRUE) %>% 
  slice(1:15)


theme_set(theme_classic())

g <- ggplot(place, aes(n,place_full_name))
g + geom_bar(stat="identity", width = 0.5, fill="tomato2") + 
  labs(title="Top places", 
       x = "Frequency", y = "Place name",
       subtitle="Top 15 place name of the tweet", 
       caption="\nSource: Data collected from Twitter's REST API via rtweet") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))


#create a table of the top 20 location tweeting about #nomore
unique(tweets_df$location)
tweets_df %>% 
  filter(is.na(location) == FALSE & location != "") %>% 
  count(location, sort = TRUE) %>% 
  slice(1:20)

tweets_df %>% group_by(location)%>% 
  summarise(Total=n()) %>% arrange(desc(Total)) %>% head(20) %>%
  ggplot(aes(reorder(location, Total), Total, fill = location)) + 
  geom_bar(stat="identity") + coord_flip()  + 
  labs(x = "Place",
       y = "Tweet Count",
       title = "Top 20 #Nomore Tweet users - unique locations ",
        #subtitle="", 
       caption = "\nSource: Data collected from Twitter's REST API via rtweet") +
  theme_light() +
  theme(legend.position='none')

# start and end day of the tweet collection 
start_date = min(tweets_df$created_at)
end_date = max(tweets_df$created_at)
typeof(tweets_df$created_at)

# convert the created_at col as date format and append at the new col 
tweets_df["date"]= as.Date(tweets_df$created_at)

#Plot the frequency of tweet within a week 

ts_plot(tweets_df, "days") +  
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(x = NULL, y = NULL,
       title = "#NoMore and related-hashtag tweet counts aggregated by day ",
       subtitle = paste0(format(min(tweets_df$date), "%d %B %Y"), " to ", 
                         format(max(tweets_df$date),"%d %B %Y")),
       caption = "Data collected from Twitter's REST API via rtweet package") +
  theme_minimal()

#Plot sender device
tweets_df %>% group_by(source)%>% 
  summarise(Total=n()) %>% arrange(desc(Total)) %>% head(10) %>%
  ggplot(aes(reorder(source, Total), Total, fill = source)) + 
  geom_bar(stat="identity") + coord_flip()  + 
  labs(title="Top Tweet Sources for #NoMore tweet", x="", 
       #subtitle="", 
       caption = "\nSource: Data collected from Twitter's REST API via rtweet") +
  theme_light() +
  theme(legend.position='none')


# Most frequently shared link
tweets_df %>% 
  filter(!is.na(urls_expanded_url)) %>% 
  count(urls_expanded_url, sort = TRUE) %>% 
  top_n(10)

# top 3 most re-tweet  tweets 
tweets_df %>% 
  arrange(-retweet_count) %>%
  slice(1:3) %>% 
  select(text, retweet_count)

# take the screenshot of top re-tweet tweet
tweets_df %>% 
  arrange(-retweet_count) %>%
  slice(1) %>% 
  select(status_id, screen_name)

tweet_screenshot(
  tweet_url("SuleimanAbdell7", "1461745787369574406"),
  maxwidth = 300,
  hide_media = TRUE,
  theme = "dark"
)

#* Most liked tweet
tweets_df %>% 
  arrange(-favorite_count) %>%
  top_n(10, favorite_count) %>% 
  select(created_at, screen_name, text, favorite_count)

tweets_df %>% 
  arrange(-favorite_count) %>%
  slice(1) %>% 
  select(status_id, screen_name)

tweet_screenshot(
  tweet_url("BilleneSeyoum", "1467001466359590916"),
  maxwidth = 300,
  hide_media = TRUE,
  theme = "dark"
)


# top 10 mostly used Emoji
 
emo = tweets_df %>%
  mutate(emoji = ji_extract_all(text)) %>% # extracts all the emojis from the text
  unnest(cols = c(emoji)) %>% # split out the  emojis, count, sort
  count(emoji, sort = TRUE) %>%
  top_n(10)

theme_set(theme_classic())
g <- ggplot(emo, aes(emoji, n))
g + geom_bar(stat="identity", width = 0.5, fill="#9370DB") + 
  labs(title="Top Emoji", 
       y = "Frequency", x = "Emojis",
       subtitle="Top 10 mostly used Emoji in the tweet", 
       caption="\nSource: Data collected from Twitter's REST API via rtweet") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

# top 10 hash-tags
hash = tweets_df %>% 
  unnest_tokens(hashtag, text, "tweets", to_lower = FALSE) %>%
  filter(str_detect(hashtag, "^#"))%>%
  count(hashtag, sort = TRUE) %>%
  top_n(10)

theme_set(theme_classic())

g <- ggplot(hash, aes(n,hashtag))
g + geom_bar(stat="identity", width = 0.5, fill="tomato2") + 
  labs(title="Top hashtags", 
       x = "Frequency", y = "hashtags",
       subtitle="Top 10 mostly used Haashtags in the tweet", 
       caption="\nSource: Data collected from Twitter's REST API via rtweet") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

# top  10 mentions

mentions = tweets_df %>% 
  unnest_tokens(mentions, text, "tweets", to_lower = FALSE) %>%
  filter(str_detect(mentions, "^@")) %>%  
  count(mentions, sort = TRUE) %>%
  top_n(10)

g <- ggplot(mentions, aes(n,mentions))
g + geom_bar(stat="identity", width = 0.5, fill="#006400") + 
  labs(title="Top Mentions", 
       x = "Frequency of @", y = "Mentions",
       subtitle="Top 10 mostly mentioned names in the tweet", 
       caption="\nSource: Data collected from Twitter's REST API via rtweet") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

#* To identify the most active tweeters we can use the "screen_name" variable 

active = tweets_df %>% 
  count(screen_name, sort = TRUE) %>%
  top_n(10)

g <- ggplot(active, aes(n,screen_name))
g + geom_bar(stat="identity", width = 0.5, fill="#8B008B") + 
  labs(title="Top user", 
       x = "Frequency", y = "Screen_name",
       subtitle="Top 10 mostly active user in the tweet", 
       caption="\nSource: Data collected from Twitter's REST API via rtweet") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))






