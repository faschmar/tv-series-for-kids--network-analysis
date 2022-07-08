# ANALYZE

#prerequisites
# - download script executed
# - cleaning script executed
  # - processed transcript-files (.txt) in folder: "./data/miraculous/processed"
# - empty folders "./data/miraculous/plots", "./data/miraculous/tables" must be present
#sentimentAI Initiated: source("./3 analyze scripts/sentimentAI - initiate.R")

source("./main.R")
source("./3 metadata scripts/metainfo_series_miraculous.R")

process_transcript<-function(filepath){
  #filepath<-"./data/miraculous/processed/Ladybug.txt"
  
  filetext <- readtext(filepath)
  transcript_lines <- str_split(filetext, "\\n")[[1]]
  characterName <- strsplit(transcript_lines, ":::")
  
  #### DIAGRAMS ####
  script <- matrix(unlist(characterName), ncol = 2, nrow=length(characterName), byrow = T) # file format suitable for sentiment analysis and graph
  sentimentMiraculous <- sentiment_score(script[, 2])
  vaderMiraculous <- vader_df(script[, 2])$compound
  vaderMiraculous[is.na(vaderMiraculous)] <- 0
  sentimentProCharacter <- aggregate(sentimentMiraculous, by = list(script[, 1]), FUN = mean)
  vaderProCharacter <- aggregate(vaderMiraculous, by = list(script[, 1]), FUN = mean)
  #sentimentProCharacter <- aggregate(sentimentMiraculous, by = list(script[, 1]), FUN = length)
  
  characters <- script[, 1]
  characters <- str_trim(characters) #ist gut, bezieht sich aber nur auf die liste characters, sollte sich aber auch auf "list(script[, 1])" beziehen
  
  nNodes <- table(characters)
  pairs <- matrix(NA, ncol = 2, nrow = length(characters))
  pairs[, 1] <- characters[1:length(characters)]
  pairs[, 2] <- characters[c(2:(length(characters)),1)]
  ep_edges <- as.character(str_split(unlist(pairs), ","))
  
  ep_sociogram_igraph<-graph_from_adjacency_matrix(table(pairs[, 1], pairs[, 2]), weighted=TRUE) 
  
  ### Metadata
  ep_title <- gsub(".txt", "", filepath)
  ep_title <- gsub("./data/miraculous/processed/", "", ep_title)
  #Änderung-IN
  #ep_title="Mayura (Heroes' Day - Part 2)"
  #ep_title="Catalyst (Heroes' Day - Part 1)"
  ep_title <-strsplit(ep_title, "\\(")
  ep_title <-matrix(unlist(ep_title), ncol = 1)
  ep_title<- paste(ep_title[1, 1])
  ep_title <-str_trim(ep_title)
  if (ep_title == "Felix") {
    ep_title <- "Félix"
  }
  subset <- season_ep_list[grep(ep_title, season_ep_list$title, ignore.case = T), ] #new:ignorecase
  #subset <- season_ep_list[season_ep_list$title %like% ep_title, ] #old
  #Änderung-OUT
  ep_no <- paste(subset[1, 1])
  ep_per_season<-paste(subset[1, 2])
  air_date<-paste(subset[1, 4])
  season<-paste(subset[1, 5])
  
  if (ep_title == "Ladybug" && ep_no==25) {
    ep_no="75"
    ep_per_season="23"
    season="3"
    air_date<-"24 November 2019"
  } else if (ep_title == "Ladybug & Cat Noir"){
    ep_no="25"
    ep_per_season="25"
    season="1"
    air_date<-"30 October 2016"
  }
  
  
  #problem-gelöst: wenn season NA ist, führt dies zu Problemen bei Berechnung, s. Sentiment von Marinette über dies Seasons
  #problem-gelöst: was tun, wenn ep_no,ep_per_season, air_date, season "NA" ist => ersetzen mit "99999" sinnvoll?
  #ep_no<-gsub("NA", "99999",ep_no, perl = TRUE)
  #ep_per_season<-gsub("NA", "99999",ep_per_season, perl = TRUE)
  #air_date<-gsub("NA", "99999",air_date, perl = TRUE)
  #season<-gsub("NA", "99999",season, perl = TRUE)
  #season[is.na(season)] <- 99999
  #end:missing data from metadata
  
  ### SentimentTable+NODE PROPERTIES: Name | Sentiment | season | ep_no | ep_title | degree (in, out, all), closeness, eigen_centrality, betweenness, hub_score, authority score, rank score
  ep_degree_in  <-degree(ep_sociogram_igraph, mode="in") # ?degree
  ep_degree_out <-degree(ep_sociogram_igraph, mode="out") # ?degree
  ep_degree_all <-degree(ep_sociogram_igraph, mode="all") # ?degree
  ep_closeness         <-closeness(ep_sociogram_igraph, mode="all") 
  ep_eigen_centrality  <-eigen_centrality(ep_sociogram_igraph, directed=F)$vector
  ep_betweenness       <-betweenness(ep_sociogram_igraph, directed=T) 
  ep_hub_score         <-hub_score(ep_sociogram_igraph)$vector
  ep_authority_score <-authority_score(ep_sociogram_igraph)$vectorsumm
  ep_rank_score <-sort(page_rank(ep_sociogram_igraph)$vector)
  sentimentProCharacter<- cbind(sentimentProCharacter, vaderProCharacter[,2])
  sentimentProCharacter<- c(sentimentProCharacter, season, ep_no, ep_title)
  #sentimentProCharacter1<- c(sentimentProCharacter, season, ep_no, ep_title, ep_degree_in, ep_degree_out, ep_degree_all, ep_closeness, ep_eigen_centrality, ep_betweenness, ep_hub_score,ep_authority_score,ep_rank_score)
  write.table(sentimentProCharacter,"./data/miraculous/tables/sentiment.csv", row.names = F, append = T, col.names = F, sep = "|")
  
  ###EpisodeTable: ep_title | ep_no | season | ep_per_season | air_date | ep_edge_density_value | ep_reciprocity_value | ep_diameter_value | betweenness_1 | betweenness_2 | betweenness_3 | betweenness_4 | betweenness_5 | eigenvector_1 | eigenvector_2 | eigenvector_3 | eigenvector_4 | eigenvector_5
  ep_df_values <- data.frame(matrix(ncol = 19, nrow = 0))
  ep_edge_density_value<-edge_density(ep_sociogram_igraph) #Anzahl an Verbindungen im Verhältnis zu Anzahl aller möglichen Verbindungen; The density of a graph is the ratio of the number of edges and the number of possible edges.
  ep_reciprocity_value<-reciprocity(ep_sociogram_igraph) #Aussage wird getätigt, Antwort an diese Person auf Episodenebene
  ep_diameter_value<-diameter(ep_sociogram_igraph, directed=T)
  ep_assortativity_value<-assortativity_degree(ep_sociogram_igraph, directed = TRUE)
  ep_betweenness_values <- names(sort(ep_betweenness, decreasing = T)[1:5])
  ep_eigenvector_values <- names(sort(ep_eigen_centrality, decreasing = T)[1:5])
  ep_list_values <- c(ep_title, ep_no, season, ep_per_season, air_date, ep_edge_density_value, ep_reciprocity_value, ep_diameter_value, ep_assortativity_value) #erweitern
  ep_list_values <- do.call(c, list(ep_list_values, ep_betweenness_values,  ep_eigenvector_values, length(nNodes)))
  ep_df_values <- rbind(ep_list_values)
  #colnames(ep_df_values) <- data.frame("title", "no", "season", "ep_per_season", "air_date", "ep_edge_density_value", "ep_reciprocity_value", "ep_diameter_value", "ep_assortativity_value", "betweenness_1", "betweenness_2", "betweenness_3", "betweenness_4", "betweenness_5", "eigenvector_1", "eigenvector_2", "eigenvector_3", "eigenvector_4", "eigenvector_5", "no_nodes")
  write.table(ep_df_values,"./data/miraculous/tables/episodes.csv", row.names = F, append = T, col.names = F, sep = "|") #erweitern #todo:
  
  #todo
  #-betweenness 
  
  ### DialogTable for all eps: From | To | Sentiment | Text | season | ep_no | ep_title 
  dialogTable <- data.frame(pairs[, 1], pairs[, 2], sentimentMiraculous[1:(length(sentimentMiraculous))], vaderMiraculous[1:(length(vaderMiraculous))], script[, 2][1:(length(script[, 2]))], season, ep_no, ep_title)
  write.table(dialogTable,"./data/miraculous/tables/dialogs.csv", row.names = F, append = T, col.names = F, sep = "|")
  # For each ep
  # write.table(dialogTable, paste("./data/miraculous/tables/",title_intermediate,".csv"), row.names = F, col.names = F, sep = "|")
  
  return(characters)
  
}

plotOverall <- function(dialogTable){
  pairs <- dialogTable[,c("From","To")]
  myEdges <- as.character(str_split(unlist(pairs), ","))
  sociogram_intermediate <- graph(c(myEdges))
  ep_sociogram_plot<-assign("Miraculous_sociagram",sociogram_intermediate)
  pdf(file="./data/miraculous/plots/Miracolous.pdf")
  plot(ep_sociogram_plot, main = "plot: sociogram", sub = "Miracolous_sociogram")
}

#Einlesen
files <- list.files(path='./data/miraculous/processed', full.names=TRUE, recursive=TRUE, include.dirs=FALSE)
character_allperep <- vector(mode = "list", length = length(files))
i <- 0
for (file in files){
  characters <- process_transcript(file)
  i <- i+1
  character_allperep[[i]] <- unique(characters)
}
#subset(table(unlist(character_allperep)), table(unlist(character_allperep))>20)
dialogTable <- read.csv("./data/miraculous/tables/dialogs.csv", sep="|", header = F)
episodeTable <- read.csv("./data/miraculous/tables/episodes.csv", sep="|", header = F)
sentimentTable <- read.csv("./data/miraculous/tables/sentiment.csv", sep="|", header = F)
lineTable <- dialogTable
lineTable[2] <- NULL
write.table(lineTable,"./data/miraculous/tables/lines.csv", row.names = F, append = T, col.names = F, sep = "|")
colnames(dialogTable) <- c("From", "To", "Sentiment", "Vader", "Text","Season", "Episode_No_Overall", "Episode_Title")
colnames(lineTable) <- c("Character", "Sentiment", "Vader", "Text", "Season", "Episode_No_Overall", "Episode_Title")
colnames(episodeTable) <- c("Episode_Title", "Episode_No_Overall", "Season", "Episode_No_perSeason", "Air_Date", "Edge_Density", "Reciprocity", "Diameter", "Assortativity","Betweenness_1", "Betweenness_2", "Betweenness_3", "Betweenness_4", "Betweenness_5", "Eigenvector_1", "Eigenvector_2", "Eigenvector_3", "Eigenvector_4", "Eigenvector_5", "No_Nodes")
colnames(sentimentTable) <- c("Character", "Sentiment", "Vader","Season", "Episode_Overall", "Episode_Title")

#sorting
dialogTable <-dialogTable[order(dialogTable$Episode_No_Overall),]
episodeTable <-episodeTable[order(episodeTable$Episode_No_Overall),]
lineTable <-lineTable[order(lineTable$Episode_No_Overall),]
sentimentTable <-sentimentTable[order(sentimentTable$Episode_Overall),]
#sentimentTable <-sentimentTable[order(sentimentTable$'Character'),] #problem-gelöst: pro episode sollte jeder name nur einmal vorkommen, leider kommt bspw. "Adrien" in Stormy-Weather und anderen mehrfach vor!

#trim Characters, already done in cleaning script
#dialogTable$From <- trimws(dialogTable$From, which = c("both"))
#dialogTable$To <- trimws(dialogTable$To, which = c("both"))
#sentimentTable$Character <- trimws(sentimentTable$Character, which = c("both"))

#Sentiment pro Character over whole Series
sentimentCharacterOverSeries <-sentimentTable %>%
  group_by(Character) %>%
  summarize(Sentiment_Over_Series = mean(Sentiment, na.rm = TRUE), Frequency = n())%>% arrange(desc(Frequency))
#Vader pro Character over whole serie
VaderCharacterOverSeries <-sentimentTable %>%
  group_by(Character) %>%
  summarize(Sentiment_Over_Series = mean(Vader, na.rm = TRUE), Frequency = n())%>% arrange(desc(Frequency))

#Sentiment pro Character over Seasons
sentimentCharacterOverSeasons <-sentimentTable %>%
  group_by(Character, Season) %>%
  summarize(Sentiment_Over_Series = mean(Sentiment, na.rm = TRUE), Frequency = n()) %>% 
  arrange(desc(Season))
#Vader pro Character over Seasons
VaderCharacterOverSeasons <-sentimentTable %>%
  group_by(Character, Season) %>%
  summarize(Sentiment_Over_Series = mean(Vader, na.rm = TRUE), Frequency = n()) %>% 
  arrange(desc(Season))

#igraph over whole series
    series_igraph <- graph_from_adjacency_matrix(table(dialogTable[, 1], dialogTable[, 2]), weighted=TRUE) 
    series_eigen_centrality <- eigen_centrality(series_igraph, directed=F)$vector
    series_eigen_centrality<-data.frame(series_eigen_centrality)
    series_eigen_centrality <- series_eigen_centrality %>% as.data.frame() %>% arrange(desc(series_eigen_centrality))
    series_betweenness <- betweenness(series_igraph, directed=T) 
    series_betweenness<-data.frame(series_betweenness)
    series_betweenness <- series_betweenness %>% as.data.frame() %>% arrange(desc(series_betweenness))
    #V(sociogram_s1)
    #E(sociogram_series)
    #plot(series_sociogram_igraph, edge.arrow.size=.4,vertex.label=NA)

#igraph over seasons
    dialogtable_s1 <-subset(dialogTable, Season==1)
    s1_igraph<-graph_from_adjacency_matrix(table(dialogtable_s1[, 1], dialogtable_s1[, 2]), weighted=TRUE) 
    s1_eigen_centrality <- eigen_centrality(s1_igraph, directed=F)$vector
    s1_eigen_centrality<-data.frame(s1_eigen_centrality)
    s1_eigen_centrality <- s1_eigen_centrality %>% as.data.frame() %>% arrange(desc(s1_eigen_centrality))
    s1_betweenness <- betweenness(s1_igraph, directed=T) 
    s1_betweenness<-data.frame(s1_betweenness)
    s1_betweenness <- s1_betweenness %>% as.data.frame() %>% arrange(desc(s1_betweenness))
    
    dialogtable_s2 <-subset(dialogTable, Season==2)
    s2_igraph<-graph_from_adjacency_matrix(table(dialogtable_s2[, 1], dialogtable_s2[, 2]), weighted=TRUE) 
    s2_eigen_centrality <- eigen_centrality(s2_igraph, directed=F)$vector
    s2_eigen_centrality<-data.frame(s2_eigen_centrality)
    s2_eigen_centrality <- s2_eigen_centrality %>% as.data.frame() %>% arrange(desc(s2_eigen_centrality))
    s2_betweenness <- betweenness(s2_igraph, directed=T) 
    s2_betweenness<-data.frame(s2_betweenness)
    s2_betweenness <- s2_betweenness %>% as.data.frame() %>% arrange(desc(s2_betweenness))
    
    dialogtable_s3 <-subset(dialogTable, Season==3)
    s3_igraph<-graph_from_adjacency_matrix(table(dialogtable_s3[, 1], dialogtable_s3[, 2]), weighted=TRUE) 
    s3_eigen_centrality <- eigen_centrality(s3_igraph, directed=F)$vector
    s3_eigen_centrality<-data.frame(s3_eigen_centrality)
    s3_eigen_centrality <- s3_eigen_centrality %>% as.data.frame() %>% arrange(desc(s3_eigen_centrality))
    s3_betweenness <- betweenness(s3_igraph, directed=T) 
    s3_betweenness<-data.frame(s3_betweenness)
    s3_betweenness <- s3_betweenness %>% as.data.frame() %>% arrange(desc(s3_betweenness))
    
    dialogtable_s4 <-subset(dialogTable, Season==4)
    s4_igraph<-graph_from_adjacency_matrix(table(dialogtable_s4[, 1], dialogtable_s4[, 2]), weighted=TRUE) 
    s4_eigen_centrality <- eigen_centrality(s4_igraph, directed=F)$vector
    s4_eigen_centrality<-data.frame(s4_eigen_centrality)
    s4_eigen_centrality <- s4_eigen_centrality %>% as.data.frame() %>% arrange(desc(s4_eigen_centrality))
    s4_betweenness <- betweenness(s4_igraph, directed=T) 
    s4_betweenness<-data.frame(s4_betweenness)
    s4_betweenness <- s4_betweenness %>% as.data.frame() %>% arrange(desc(s4_betweenness))


#looking up one character
    sentimentCharacterOverSeasons_M <-sentimentTable %>%
      group_by(Character, Season) %>%
      summarize(Sentiment_Over_Series = mean(Sentiment, na.rm = TRUE), Frequency = n()) %>% 
      arrange(desc(Season))%>% #bish hierher
      filter(Character == 'Marinette') #plus filter
    
    plot_S_Marinette_series <- sentimentCharacterOverSeasons_M %>%
      tail(10) %>%
      ggplot( aes(x=Season, y=Sentiment_Over_Series)) +
      geom_line( color="grey") +
      geom_point(shape=21, color="black", fill="#69b3a2", size=3) +
      ylim(-1,1)
    #plot_S_Marinette_series + labs(title = "Time Evolution of mean sentiment over seasons", subtitle = "Marinette")

#subsets with missing values in episode table
    missing_entries_episode_table <- episodeTable[rowSums(is.na(episodeTable)) > 0,]
    #missing_entries_episode_table99999 <- episodeTable[episodeTable$Season %like% "99999", ]

#------------------------------------------------------------------------
#aufräumen
rm(i, incl_nt, neu_set)
print("<End of Analyzing-Script>")
#-------next: gender_role------------------------------------------------


