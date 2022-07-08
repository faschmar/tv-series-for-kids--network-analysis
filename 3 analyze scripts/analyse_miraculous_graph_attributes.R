serie_graph <- graph_from_adjacency_matrix(table(dialogTable$From, dialogTable$To), weighted=TRUE)
plot(serie_graph)
# Graph based
serie_edge_density_value<-edge_density(serie_graph) 
serie_reciprocity_value<-reciprocity(serie_graph)
serie_assortativity_value<-assortativity_degree(serie_graph, directed = TRUE)
serie_diameter_value<-diameter(serie_graph, directed=T)
## Mean
mean_edge_density_value<-mean(episodeTable$Edge_Density)
mean_reciprocity_value<-mean(episodeTable$Reciprocity)
mean_assortativity_value <- mean(episodeTable$Assortativity)
mean_diameter_value<-mean(episodeTable$Diameter)

#Node based
serie_degree_in  <-degree(serie_graph, mode="in")
serie_degree_out <-degree(serie_graph, mode="out")
serie_eigen_centrality  <-eigen_centrality(serie_graph, directed=T)$vector
serie_betweenness<-betweenness(serie_graph, directed=T)
serie_hub_score<-hub_score(serie_graph)$vector
serie_authority_score<-authority_score(serie_graph)$vector



role_sentiment_from <- dialogTable_gender_role %>% #crosstable
  group_by(Role_From) %>% 
  get_summary_stats(Sentiment, type = "mean_sd")
role_sentiment_to <- dialogTable_gender_role %>% #crosstable
  group_by(Role_To) %>% 
  get_summary_stats(Sentiment, type = "mean_sd")
role_sentiment_from$mean
role_sentiment_table <- data.frame(Role = role_sentiment_from$Role_From, In = role_sentiment_to$mean, Out = role_sentiment_from$mean)


# Debug Diameter
debug_dialogTable <- dialogTable[dialogTable$Episode_Title == "Befana", ]
debug_graph <- graph_from_adjacency_matrix(table(debug_dialogTable$From, debug_dialogTable$To), weighted=TRUE)
plot(debug_graph, layout=layout.auto, edge.curved=FALSE, vertex.size=10)
diameter(debug_graph, directed = T)
get_diameter(debug_graph, directed = T)
