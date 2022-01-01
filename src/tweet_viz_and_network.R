# import all important libraries
library(rtweet)
library(tidyverse) 
library(tidytext)
library(igraph)
library(twinetverse)
library(widyr)
library(tm)
library(ggraph)
# load the data
tweets = read.csv("all_csv_file//combined_csv.csv")
tweets["date"]= as.Date(tweets$created_at)

# Connecting users to the hash-tags they use (only for the first day tweet including neighbors of the node .

tags <-tweets %>% 
  filter(!is.na(hashtags)) %>%
  select(user_id, status_id, created_at, screen_name, text, source,
         reply_to_status_id, reply_to_user_id, reply_to_screen_name, favorite_count,
         retweet_count,retweet_user_id,retweet_screen_name, quote_count, 
         reply_count, mentions_user_id, mentions_screen_name, hashtags)

glimpse(tags)
colnames(tags)

sample_df <- tags %>% 
  dplyr::filter(created_at > as.POSIXlt("2021-11-13 00:00:00", "UTC") & 
                  created_at < as.POSIXlt("2021-11-13 23:00:00", "UTC"))

net <- sample_df %>% 
  gt_edges(screen_name, hashtags) %>% 
  gt_nodes() %>% 
  gt_collect()

head(net$edges)
head(net$nodes)
# split the edge and  the node 
c(edges, nodes) %<-% net

nodes <- nodes2sg(nodes)
edges <- edges2sg(edges)

nodes$color <- ifelse(nodes$type == "user", "#000080", "#B8860B")

# Visualize
sigmajs(width = "100%", height = "600px") %>% 
  sg_force_start() %>%  
  sg_nodes(nodes, id, size, color, label) %>% 
  sg_edges(edges, id, source, target) %>% 
  sg_layout(layout = igraph::layout_components) %>% 
  sg_settings(
    edgeColor = "default",
    defaultEdgeColor = "#d3d3d3"
  ) %>% 
  sg_neighbours() %>% 
  sg_force_stop(10000) 
#________________________________________________________

# link the user tweeting to the users he or she tags in the tweet
sample_tweet <- tweets %>% 
  dplyr::filter(created_at > as.POSIXlt("2021-11-13 00:00:00", "UTC") & 
                  created_at < as.POSIXlt("2021-11-13 23:00:00", "UTC"))

sample_tweet %>% 
  gt_edges(screen_name, mentions_screen_name) %>% 
  gt_nodes() %>% 
  gt_collect() -> gt

nodes <- gt$nodes %>% 
  mutate(
    id = nodes,
    label = nodes,
    size = n
  ) 
nodes$color <- ifelse(nodes$type == "user", "#000080", "#B8860B")

# select only few number of nodes????????????????

edges <- gt$edges %>% 
  mutate(
    id = 1:n()
  )
sigmajs(width = "100%", height = "600px") %>% 
  sg_force_start() %>%  
  sg_nodes(nodes, id, size, color, label) %>% 
  sg_edges(edges, id, source, target) %>% 
  sg_layout(layout = igraph::layout_components) %>% 
  sg_settings(
    edgeColor = "default",
    defaultEdgeColor = "#d3d3d3"
  ) %>% 
  sg_neighbours() %>% 
  sg_force_stop(10000) 


# _______________________________________________

# Error to fix 


# connect the  @users mentioned in the tweets to each other
xx<-tweets %>% 
  select(user_id, status_id, created_at, screen_name, text, source,
         reply_to_status_id, reply_to_user_id, reply_to_screen_name, favorite_count,
         retweet_count,retweet_user_id,retweet_screen_name, quote_count, 
         reply_count, mentions_user_id,mentions_screen_name, hashtags)


net <- xx %>% 
  gt_co_edges(mentions_screen_name) %>% 
  gt_nodes() %>% 
  gt_collect()

c(edges, nodes) %<-% net

edges <- edges2sg(edges)
nodes <- nodes2sg(nodes)
sg_graph <- function(nodes, edges){
  
  sigmajs(width = "100%", height = "600px") %>% 
    sg_nodes(nodes, id, label, size) %>% 
    sg_edges(edges, id, source, target) %>% 
    sg_cluster(
      colors = c(
        "#0084b4",
        "#00aced",
        "#1dcaff",
        "#c0deed"
      )
    ) %>% 
    sg_force_start() %>% 
    sg_force_stop(2000) %>% 
    sg_neighbours() %>% 
    sg_settings(
      defaultEdgeColor = "#a3a3a3",
      edgeColor = "default"
    )
}

sg_graph(nodes, edges)

#___________________________________________________________
# connect the #hash-tags to each other

net <- xx %>% 
  gt_co_edges(hashtags) %>% 
  gt_nodes() %>% 
  gt_collect()

c(edges, nodes) %<-% net

edges <- edges2sg(edges)
nodes <- nodes2sg(nodes)
edges <- edges %>% 
  edges2sg() %>% 
  dplyr::filter(
    source != "#vicepresidentialdebate2020", 
    target != "#vicepresidentialdebate2020"
  )

nodes <- nodes %>% 
  nodes2sg() %>% 
  dplyr::filter(
    id != "#vicepresidentialdebate2020"
  ) 

sg_graph(nodes, edges)




