source("./main.R")
source("./metainfo_series_miraculous_Martin.R")

process_transcript<-function(filepath){
  #muss bei wholescript am Beginn erstellt werden
  filepath<-"./data/miraculous/processed/Oni-Chan.txt"
  ep_df_values_wholeseason <- data.frame(matrix(ncol = 8, nrow = 0))
  colnames(ep_df_values_wholeseason) <- data.frame("title", "season", "ep_per_season", "ep_overall", "air_date", "ep_edge_density_value", "ep_reciprocity_value", "ep_diameter_value")
  filetext <- readtext(filepath)
  ep_title <- gsub(".txt", "", filepath)
  ep_title <- gsub("./data/miraculous/processed/", "", ep_title)
  transcript_lines <- str_split(filetext, "\\n")[[1]]
  characterName <- strsplit(transcript_lines, ":::")
  #str(characterName)
  #file_intermediate<-strsplit(file, "/")[[1]]
  #title_intermediate<-file_intermediate[[5]]
  #series<-file_intermediate[[3]]
  
  #### DIAGRAMS ####
  #str(characterName)
  ep_script <- matrix(unlist(characterName), ncol = 2, nrow=length(characterName), byrow = T) # file format suitable for sentiment analysis and graph
  #sentimentMiraculous <- sentiment_score(ep_script[, 2])
  #sentimentProCharacter <- aggregate(sentimentMiraculous, by = list(trimws(ep_script[, 1])), FUN = mean)
  #sentimentProCharacter <- aggregate(sentimentMiraculous, by = list(ep_script[, 1]), FUN = length)
  
  ep_characters <- ep_script[, 1]
  ep_characters <- str_trim(ep_characters) #new
  ep_no_of_Nodes <- table(ep_characters)
  ep_pairs <- matrix(NA, ncol = 2, nrow = length(ep_characters))
  ep_pairs[, 1] <- ep_characters[1:length(ep_characters)]
  ep_pairs[, 2] <- ep_characters[c(2:(length(ep_characters)),1)]
  ep_edges <- as.character(str_split(unlist(ep_pairs), ","))
  
  #table(ep_pairs[, 1], ep_pairs[, 2])
  
  # DialogTable for all eps: From | To | Sentiment | Text
  #dialogTable <- data.frame(ep_pairs[, 1], ep_pairs[, 2], sentimentMiraculous[1:(length(sentimentMiraculous)-1)], ep_script[, 2][1:(length(ep_script[, 2])-1)])
  #write.table(dialogTable,"./data/miraculous/tables/dialogs.csv", row.names = F, append = T, col.names = F, sep = "|")
  # For each ep
  # write.table(dialogTable, paste("./data/miraculous/tables/",title_intermediate,".csv"), row.names = F, col.names = F, sep = "|")
  
  #SociogramPlot: format: series_titel.txt_sociogram
  #sociogram_intermediate <- graph(c(ep_edges))
  #ep_sociogram_plot<-assign(paste(series, title_intermediate,"sociogram", sep="_"),sociogram_intermediate)
  
  #IGRAPH BEGINN - einfügen (alles andere ist schon vorhanden )
  #Tabelle der Häufigkeit der Kommunikation zw. Charakteren (gerichtet als string)
  ep_no_comm_character<-paste(ep_pairs[, 1], ep_pairs[, 2])
  #table(ep_no_comm_character)
  #sum(table(ep_no_comm_character))
  
  #graph_from_adjacency_matrix - Create graphs from adjacency matrices
  ep_sociogram_igraph<-graph_from_adjacency_matrix(table(ep_pairs[, 1], ep_pairs[, 2]), weighted=TRUE) 
  #V(ep_sociogram_igraph) #Vertices of a graph
  #E(ep_sociogram_igraph) #Edges of a graph
  #E(ep_sociogram_igraph)$weight #weights anzeigen
  E(ep_sociogram_igraph)$width<-E(ep_sociogram_igraph)$weight/5 #aufgrund des weight die stärke (Width) der Verbindungen (edges) definieren
  E(ep_sociogram_igraph)$arrow.size <- 0.2
  plot(ep_sociogram_igraph, main=paste(ep_title,"- Plot - Sociogram"))
  
  #development - unnötig
  #ep_sociogram_igraph <- simplify(ep_sociogram_igraph, edge.attr.comb=list(weight="sum","ignore")) /nötig?
  #ep_sociogram_igraph$weight
  #as.numeric(as_adjacency_matrix(ep_sociogram_igraph))
  #ep_sociogram_igraph
  #str(ep_sociogram_igraph)
  #as_adjacency_matrix(ep_sociogram_igraph)
  
  
  #interpretation 
  ep_edge_density_value<-edge_density(ep_sociogram_igraph) #Anzahl an Verbindungen im Verhältnis zu Anzahl aller möglichen Verbindungen; The density of a graph is the ratio of the number of edges and the number of possible edges.
  ep_reciprocity_value<-reciprocity(ep_sociogram_igraph) #Aussage wird getätigt, Antwort an diese Person auf Episodenebene
  
  #Transitivity measures the probability that the adjacent vertices of a vertex are connected. This is sometimes also called the clustering coefficient.
  #transitivity(ep_sociogram_igraph) #global - nicht verwenden, da gerichtet
  #transitivity(ep_sociogram_igraph, type="local") #pro vertex ein wert -nicht verwenden, da gerichtet
  #ep_triad_census<-triad_census(ep_sociogram_igraph) #wenn gerichtet
  #?triad_census
  ep_diameter_value<-diameter(ep_sociogram_igraph, directed=T) #anzahl an möglichen stationen, The diameter of a graph is the length of the longest geodesic
  
  #The degree of a vertex is its most basic structural property, the number of its adjacent edges.
  #welche Personen sind mit einander verbunden (kommunizieren)
  ep_degree_in  <-degree(ep_sociogram_igraph, mode="in") # ?degree
  ep_degree_out <-degree(ep_sociogram_igraph, mode="out") # ?degree
  ep_degree_all <-degree(ep_sociogram_igraph, mode="all") # ?degree
  #deg<-degree(ep_sociogram_igraph, mode="out") # ?degree
  #deg<-degree(ep_sociogram_igraph, mode="all") # ?degree
  plot(ep_sociogram_igraph, vertex.size=ep_degree_in*3,main=paste(ep_title, "- plot of node degree IN"))
  plot(ep_sociogram_igraph, vertex.size=ep_degree_out*3,main=paste(ep_title, "- plot of node degree OUT"))
  plot(ep_sociogram_igraph, vertex.size=ep_degree_all*3,main=paste(ep_title, "- plot of node degree ALL"))
  
  hist(ep_degree_in, breaks=1:vcount(ep_sociogram_igraph)-1, main=paste(ep_title, "- histogram of node degree IN"))
  hist(ep_degree_out, breaks=1:vcount(ep_sociogram_igraph)-1, main=paste(ep_title, "- histogram of node degree OUT"))
  #Fehler, Martin fragen
  #hist(ep_degree_all, breaks=1:vcount(ep_sociogram_igraph)-1, main=paste(ep_title, "- histogram of node degree ALL"))
  
  ep_degree_distribution<-degree_distribution(ep_sociogram_igraph, cumulative = T, mode="all") #all, in, out
  plot(x=0:max(ep_degree_all), y=1-ep_degree_distribution, pch=19, cex=1.2, col="orange", xlab="degree", ylab="cumulative frequency", main=paste(ep_title, "- histogram - degree distribution"))
  
  
  #Closeness: centrality measures how many steps is required to access every other vertex from a given vertex.
  ep_closeness         <-closeness(ep_sociogram_igraph, mode="all") 
  
  #measure of the influence of a node in a network
  ep_eigen_centrality  <-eigen_centrality(ep_sociogram_igraph, directed=T)$vector
  
  ep_betweenness       <-betweenness(ep_sociogram_igraph, directed=T) #zentralste figur
  barplot(ep_betweenness)
  
  #HubScore: The hub scores of the vertices are defined as the principal eigenvector of A*t(A), where A is the adjacency matrix of the graph. 
  ep_hub_score         <-hub_score(ep_sociogram_igraph)$vector
  
  #hubs/authorities !!
  ep_authority_score<-authority_score(ep_sociogram_igraph)$vector
  
  #dataframe: lists
  #not in it: ep_degree_distribution, ep_triad_census
  ep_dataframe_intermediate <- rbind(ep_degree_all, ep_degree_in, ep_degree_out, ep_closeness, ep_eigen_centrality, ep_betweenness, ep_hub_score, ep_authority_score)
  ep_df_characters <- as.data.frame(t(ep_dataframe_intermediate))
  rm(ep_dataframe_intermediate)
  #dataframe: values and metatable
  #find metadata
  subset <- season_ep_list[season_ep_list$title %like% ep_title, ]  
  ep_overall<-paste(subset[1, 1])
  ep_per_season<-paste(subset[1, 2])
  air_date<-paste(subset[1, 4])
  season<-paste(subset[1, 5])
  #rm(subset)
  
  ep_dataframe_value_intermediate <- rbind(season, ep_per_season, ep_overall, air_date, ep_edge_density_value, ep_reciprocity_value, ep_diameter_value)
  colnames(ep_dataframe_value_intermediate)[1] <- ep_title
  ep_df_values <- as.data.frame(t(ep_dataframe_value_intermediate))
  rm(ep_dataframe_value_intermediate)
  
  ep_df_values_wholeseason[nrow(ep_df_values_wholeseason) + 1,] <- c(ep_title, season, ep_per_season, ep_overall, air_date, ep_edge_density_value, ep_reciprocity_value, ep_diameter_value)
  
  #subgroups
  ep_sociogram_igraph.sym <- as.undirected(ep_sociogram_igraph, mode= "collapse", edge.attr.comb=list(weight="sum", "ignore"))
  cliques(ep_sociogram_igraph.sym)
  sapply(cliques(ep_sociogram_igraph.sym), length) # clique sizes largest_cliques(ep_sociogram_igraph.sym) # cliques with max number of nodes
  vcol <- rep("grey80", vcount(ep_sociogram_igraph.sym))
  vcol[unlist(largest_cliques(ep_sociogram_igraph.sym))] <- "gold"
  plot(as.undirected(ep_sociogram_igraph.sym), vertex.label=V(ep_sociogram_igraph.sym)$name, vertex.color=vcol)
  
  #Community detection based on edge betweenness (Newman-Girvan)
  ceb <- cluster_edge_betweenness(ep_sociogram_igraph) 
  # Fehler bei Oni-Chan.txt, Martin fragen
  # dendPlot(ceb, mode="hclust")
  plot(ceb, ep_sociogram_igraph)
  membership(ceb)
  #Community detection based on based on propagating labels 
  clp <- cluster_label_prop(ep_sociogram_igraph) 
  plot(clp, ep_sociogram_igraph)
  #Community detection based on greedy optimization of modularity
  cfg <- cluster_fast_greedy(as.undirected(ep_sociogram_igraph)) 
  plot(cfg, as.undirected(ep_sociogram_igraph))
  V(ep_sociogram_igraph)$community <- cfg$membership
  colrs <- adjustcolor( c("gray50", "tomato", "gold", "yellowgreen"), alpha=.6) 
  plot(ep_sociogram_igraph, vertex.color=colrs[V(ep_sociogram_igraph)$community])
  #IGRAPH END
  
  return(ep_characters)
}     

files <- list.files(path='./data/miraculous/processed', full.names=TRUE, recursive=TRUE, include.dirs=FALSE)
allEpisodes <- list.files(c("./data/miraculous/processed"))
allCharacters <- vector(mode = "list", length = length(files))
i <- 0
for (file in files){
  characters <- process_transcript(file)
  i <- i+1
  allCharacters[[i]] <- unique(characters)
}
subset(table(unlist(allCharacters)), table(unlist(allCharacters))>20)

#?try
