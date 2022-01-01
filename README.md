# HashtagNoMoreTweetAnalysis
Sentiment analysis, Semantic Network and Topic modeling: Case study on analyzing tweets towards African NoMore campaign.

![img](map.png)
Figure 1 : Mapping of twitter user location who partcipated on the campaign

## Project Goal

This study examines 260,475 tweets that mention #NoMore in order to analyze the public’s reactions towards the NoMore campaign.  Syuzhet R package is  used to classify the public’s reaction represented by tweets into sentiment class and structural topic models (STM) is used for discovering the abstract "topics" that occur in the tweets.
The project was done for the completion of Digital social data course at the University of Trento.The report paper can be found [here]()

## Dataset
The hashtag tweet dataset used for the project can be found [here](https://drive.google.com/file/d/13KE6-ffIeufCMViDManCfMo63bbnMjsD/view?usp=sharing)

## Dependencies
The project implmented using R and to excute all the src codes you have to install the following dependncies. 
```
- rtweet 
- tidyverse 
- tidytext
- ggplot2
- tweetrmd
- emo
- wordcloud
- widyr
- tm
- igraph
- ggraph
- leaflet
```

## Project Structure

All R scripts found in the src folder and:
- `tweet_scraping.R`: R script to used to scrap the tweet data from the Twitter REST API.
- `merging_raw_datasets.R`: R script to combine all the tweets collected for the last few weeks  into one dataframe by eleminating the duplicate tweets
- `EDA.R`: R script to perform explanratory data analysis
- `tweet_coordinate.ipynb`: Python script to get the coordinate information of the tweets from the place_full_name information of the data and to append the information to the dataframe
- `text_preprocessing.R`: R script to implement all the text preprocessing task ,to get tokens and splitting the text into the weekly interval
- `word_net.R`: R script to explor more on the tweet text data. ngram,word networks.
- `semantic_net.R`: R script to implment the semantic network of tweets
- `sentiment_analysis.R`:R script to implment  tweet sentiment
- `topic_modeling.R`: R script to   model weekly topics 




