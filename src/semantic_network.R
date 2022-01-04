# set the path of your project folder 

setwd("...")

# import all important libraries
library(rtweet)
library(tidyverse) 
library(tidytext)
library(igraph)
library(twinetverse)
library(widyr)
library(tm)
library(igraph)
library(ggraph)
# load the data
load("processed_data/cleaned_tweets.RData")

# Connecting users to the hash-tags they use(only for the first day tweet including neighbors of the node .
tags_df <-tweets %>% 
  filter(!is.na(hashtags)) %>%
  select(user_id, status_id, created_at, screen_name, text, source,
         favorite_count, hashtags,week_number)

glimpse(tags_df)
colnames(tags_df)

sample_df <- tags_df %>% 
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

#________________________________________________________

# link the user tweeting to the users he or she tags/mention in the tweet
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





