# set the path of  your project folder 
setwd("...")

# import libraries 
library(rtweet)
library(tidyverse) 
library(tidytext)
library(ggplot2)
library(tweetrmd)
library(emo)
library(wordcloud)
library(widyr)
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
tweets_df$
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
g <- ggplot(k, aes(emoji, n))
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
g + geom_bar(stat="identity", width = 0.5, fill="#8B008B") + 
  labs(title="Top @", 
       x = "Frequency of @", y = "Mentions",
       subtitle="Top 10 mostly mentions  in the tweet", 
       caption="\nSource: Data collected from Twitter's REST API via rtweet") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

#* To identify the most active tweeters we can use the "screen_name" variable 

active = tweets_df %>% 
  count(screen_name, sort = TRUE) %>%
  top_n(10)

g <- ggplot(active, aes(n,screen_name))
g + geom_bar(stat="identity", width = 0.5, fill="#006400") + 
  labs(title="Top user", 
       x = "Frequency", y = "Screen_name",
       subtitle="Top 10 mostly active user in the tweet", 
       caption="\nSource: Data collected from Twitter's REST API via rtweet") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))


#* Top words

words <- tweets_df %>%
  mutate(text = str_remove_all(text, "&amp;|&lt;|&gt;"),
         text = str_remove_all(text, "\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)"),
         text = str_remove_all(text, "[^\x01-\x7F]")) %>% 
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'"),
         str_detect(word, "[a-z]"),
         !str_detect(word, "^#"),         
         !str_detect(word, "@\\S+")) %>%
  count(word, sort = TRUE)

#* Then we use the wordcloud package to create a visualization of the word frequencies.
# Create a list of stop words: a list of words that are not worth including
my_stop_words <- stop_words %>% select(-lexicon) %>% 
  bind_rows(data.frame(word = c("cnn", "t.co")))

tweet_words <- words%>%
  filter(!word %in% my_stop_words$word) 
# word cloud 
tweet_words %>% 
  with(wordcloud(word, n, random.order = FALSE, max.words = 100, colors = "#4B0082"))

# plotting, words frequencies

g <- ggplot(tweet_words[1:15,], aes(n,word))
g + geom_bar(stat="identity", width = 0.5, fill="#FF8C00") + 
  labs(title="Top Words", 
       x = "Frequency of word", y = "Words",
       subtitle="Top 15 mostly used words  in the tweet", 
       caption="\nSource: Data collected from Twitter's REST API via rtweet") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))


#________________________________________________________________
# plot word network

tweets<-tweets_df %>% 
  select(created_at, screen_name, text, source)

# remove punctuation, convert to lowercase, add id for each tweet!
words <- tweets %>%
  mutate(text = str_remove_all(text, "&amp;|&lt;|&gt;"),
         text = str_remove_all(text, "\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)"),
         text = str_remove_all(text, "[^\x01-\x7F]")) 

# collapsing words to a common root to aid comparison of vocabulary
words$text <- stemDocument(words$text)

# generate couple of consecutive words Bi-gram

words <- words %>%
  unnest_tokens(word, text, token = "ngrams", n = 2)  %>%
  count(word, sort = TRUE)

# split couples of words in variables
words <- words %>%
  separate(word, c("word1", "word2"), sep = " ")

# delete special characters and others
tweet_filtered <- words %>%
  filter(!word1 %in% stop_words$word,
         !word1 %in% str_remove_all(stop_words$word, "'"),
         str_detect(word1, "[a-z]"),
         !str_detect(word1, "^#"),         
         !str_detect(word1, "@\\S+")) %>%
  filter(!word2 %in% stop_words$word,
         !word2 %in% str_remove_all(stop_words$word, "'"),
         str_detect(word2, "[a-z]"),
         !str_detect(word2, "^#"),         
         !str_detect(word2, "@\\S+")) 

# Create a list of stop words: a list of words that are not worth including
my_stop_words <- stop_words %>% select(-lexicon) %>% 
  bind_rows(data.frame(word = c("cnn", "t.co")))

tweet_filtered <- tweet_filtered %>%
  filter(!word1 %in% my_stop_words$word) %>%
  filter(!word2 %in% my_stop_words$word)

# plot word network
windows()
tweet_filtered %>%
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

tweet_counts1 <- tweet_filtered %>%
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
