##2022-05-12
source("./main.R")

  filepath<-"./data/miraculous/processed/Simpleman.txt" # ändern
  filetext <- readtext(filepath)
  transcript_lines <- str_split(filetext, "\\n")[[1]]
  characterName <- strsplit(transcript_lines, ":::")
  #file_intermediate<-strsplit(file, "/")[[1]]
  #title_intermediate<-file_intermediate[[5]]
  #series<-file_intermediate[[3]]
  
  #### DIAGRAMS ####
  str(characterName)
  script <- matrix(unlist(characterName), ncol = 2, nrow=length(characterName), byrow = T) # file format suitable for sentiment analysis and graph
  #sentimentMiraculous <- sentiment_score(script[, 2])
  #sentimentProCharacter <- aggregate(sentimentMiraculous, by = list(trimws(script[, 1])), FUN = mean)
  #sentimentProCharacter <- aggregate(sentimentMiraculous, by = list(script[, 1]), FUN = length)
  
  characters <- script[, 1]
  
  nNodes <- table(characters)
  pairs <- matrix(NA, ncol = 2, nrow = length(characters)-1)
  pairs[, 1] <- characters[1:(length(characters)-1)]
  pairs[, 2] <- characters[2:length(characters)]
  myEdges <- as.character(str_split(unlist(pairs), ","))
  
  #table(pairs[, 1], pairs[, 2])
  
  # DialogTable for all eps: From | To | Sentiment | Text
  #dialogTable <- data.frame(pairs[, 1], pairs[, 2], sentimentMiraculous[1:(length(sentimentMiraculous)-1)], script[, 2][1:(length(script[, 2])-1)])
  #write.table(dialogTable,"./data/miraculous/tables/dialogs.csv", row.names = F, append = T, col.names = F, sep = "|")
  # For each ep
  # write.table(dialogTable, paste("./data/miraculous/tables/",title_intermediate,".csv"), row.names = F, col.names = F, sep = "|")
  
  #SociogramPlot: format: series_titel.txt_sociogram
  #sociogram_intermediate <- graph(c(myEdges))
  #ep_sociogram_plot<-assign(paste(series, title_intermediate,"sociogram", sep="_"),sociogram_intermediate)
  
#IGRAPH BEGINN - einfügen (alles andere ist schon vorhanden )
  #Tabelle der Häufigkeit der Kommunikation zw. Charakteren (ungerichtet als string)
    var1<-paste(pairs[, 1], pairs[, 2])
    #table(var1)
    #sum(table(var1))
      
  #graph_from_adjacency_matrix - Create graphs from adjacency matrices
    igraph_sociogram_ep<-graph_from_adjacency_matrix(table(pairs[, 1], pairs[, 2]), weighted=TRUE) 
    #V(igraph_sociogram_ep) #Vertices of a graph
    #E(igraph_sociogram_ep) #Edges of a graph
    #E(igraph_sociogram_ep)$weight #weights anzeigen
    E(igraph_sociogram_ep)$width<-E(igraph_sociogram_ep)$weight/5 #aufgrund des weight die stärke (Width) der Verbindungen (edges) definieren
    E(igraph_sociogram_ep)$arrow.size <- 0.2
    plot(igraph_sociogram_ep)
      
    #development - unnötig
      #igraph_sociogram_ep <- simplify(igraph_sociogram_ep, edge.attr.comb=list(weight="sum","ignore")) /nötig?
      #igraph_sociogram_ep$weight
      #as.numeric(as_adjacency_matrix(igraph_sociogram_ep))
      #igraph_sociogram_ep
      #str(igraph_sociogram_ep)
      #as_adjacency_matrix(igraph_sociogram_ep)

  
  #interpretation 
    edge_density(igraph_sociogram_ep) #Anzahl an Verbindungen im Verhältnis zu Anzahl aller möglichen Verbindungen; The density of a graph is the ratio of the number of edges and the number of possible edges.
  
    reciprocity(igraph_sociogram_ep) #Aussage wird getätigt, Antwort an diese Person auf Episodenebene
  
    #Transitivity measures the probability that the adjacent vertices of a vertex are connected. This is sometimes also called the clustering coefficient.
    transitivity(igraph_sociogram_ep) #global - nicht verwenden, da gerichtet
    transitivity(igraph_sociogram_ep, type="local") #pro vertex ein wert -nicht verwenden, da gerichtet
    triad_census(igraph_sociogram_ep) #wenn gerichtet
  
    diameter(igraph_sociogram_ep, directed=T) #anzahl an möglichen stationen, The diameter of a graph is the length of the longest geodesic
    
    #The degree of a vertex is its most basic structural property, the number of its adjacent edges.
    #welche Personen sind mit einander verbunden (kommunizieren)
    deg<-degree(igraph_sociogram_ep, mode="in") # ?degree
    hist(deg)
    deg.dist<-degree_distribution(igraph_sociogram_ep, cumulative = T, mode="in") #all, in, out
    plot(x=0:max(deg), y=1-deg.dist)
    plot(igraph_sociogram_ep)
  
    #Closeness centrality measures how many steps is required to access every other vertex from a given vertex.
    closeness(igraph_sociogram_ep, mode="all") 
    
    #measure of the influence of a node in a network
    eigen_centrality(igraph_sociogram_ep, directed=T)$vector
  
    t<-betweenness(igraph_sociogram_ep, directed=T) #zentralste figur
    barplot(t)
  
    #HubScore: The hub scores of the vertices are defined as the principal eigenvector of A*t(A), where A is the adjacency matrix of the graph. 
    hub_score(igraph_sociogram_ep)$vector
    
    #hubs/authorities !!
    authority_score(igraph_sociogram_ep)$vector
#IGRAPH END
