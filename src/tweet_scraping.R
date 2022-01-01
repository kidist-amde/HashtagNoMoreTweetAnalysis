# set the path of the directory where your  dataset located 

setwd("...")
#_________________________________________________________________
#  pass you tokens to scrap the data from twitter 

token <- create_token(
  app = "..",
  consumer_key = " ",
  consumer_secret = " ",
  access_token = " ",
  access_secret = " ")

#__________________________________________________________________

# import all the necessary library 

library(rtweet)

# query's related with the popular trend #NoMore 
query <- c("#Nomore","#NoMore Neocolonialism",
           "#nomore #ethiopia","#EthiopiaPrevails",
           "#NOMORE western intervention","#TPLFTerroristGroup",
           "#NoMoreTPLF","#TPLF","#LeaveEthiopia",
           "#NOMORE propaganda","#NOMORE western intervention",
           "#NOMORE biased news #Ethiopia","#NOMORE divisive politics",
           "#PanAfricanismPrevails","#AfricaUnite")

# scrap the data from Twitter REST-API for each query 
results = NULL
for(tweet in query){
    df_tweets <- search_tweets(tweet,n =100000, 
                         include_rts = FALSE, 
                         lang="en",
                         retryonratelimit = TRUE,  
                         since='2021'
    )
    if (is.null(results)){
      results = df_tweets
    }
    else {
      results = rbind(results,df_tweets)
    }
    
}

#Select last (oldest) status ID from previous search

last_status_id <- max_id(df_tweets)

# export the data-frame to csv file for latter use
write_as_csv(results, "raw_data/new_result.csv")



