##zielverzeichnis muss beinhalten: processed, plots, dialogtable
source("./main.R")
source("./metadata scripts/metainfo_series_miraculous_Martin.R")
# source("./analyze scripts/sentimentAI - initiate_v3.R")


process_transcript<-function(filepath){
  #filepath<-"./data/miraculous/processed/Stormy Weather.csv"
  filetext <- read.csv(filepath, header = F, sep="|")
  #filetext <- readtext(filepath)#old
  
  #transcript_lines <- str_split(filetext, "\\n")[[1]]#old
  #characterName <- strsplit(transcript_lines, ":::")#old

  #### DIAGRAMS ####
  #script <- matrix(unlist(characterName), ncol = 2, nrow=length(characterName), byrow = T) # file format suitable for sentiment analysis and graph
  script<-filetext
  sentimentMiraculous <- sentiment_score(script[, 2])
  sentimentProCharacter <- aggregate(sentimentMiraculous, by = list(script[, 1]), FUN = mean)
  #sentimentProCharacter <- aggregate(sentimentMiraculous, by = list(script[, 1]), FUN = length)
  
  characters <- script[, 1]
  characters <- str_trim(characters) #ist gut, bezieht sich aber nur auf die liste characters, sollte sich aber auch auf "list(script[, 1])" beziehen
  
  nNodes <- table(characters)
  pairs <- matrix(NA, ncol = 2, nrow = length(characters))
  pairs[, 1] <- characters[1:length(characters)]
  pairs[, 2] <- characters[c(2:(length(characters)),1)]
  ep_edges <- as.character(str_split(unlist(pairs), ","))
  
  ep_sociogram_igraph<-graph_from_adjacency_matrix(table(pairs[, 1], pairs[, 2]), weighted=TRUE) 
  
  ###EpisodeTable: ep_title | ep_no | season | ep_per_season | air_date | ep_edge_density_value | ep_reciprocity_value | ep_diameter_value
  ep_df_values <- data.frame(matrix(ncol = 7, nrow = 0))
  colnames(ep_df_values) <- data.frame("no", "season", "ep_per_season", "air_date", "ep_edge_density_value", "ep_reciprocity_value", "ep_diameter_value")
  ep_title <- gsub(".csv", "", filepath)
  ep_title <- gsub("./data/miraculous/processed/", "", ep_title)
  subset <- season_ep_list[season_ep_list$title %like% ep_title, ] 
  ep_no <- paste(subset[1, 1])
  ep_per_season<-paste(subset[1, 2])
  air_date<-paste(subset[1, 4])
  season<-paste(subset[1, 5])
  #problem: wenn season NA ist, führt dies zu Problemen bei Berechnung, s. Sentiment von Marinette über dies Seasons
  #problem: was tun, wenn ep_no,ep_per_season, air_date, season "NA" ist => ersetzen mit "99999" sinnvoll?
    ep_no<-gsub("NA", "99999",ep_no, perl = TRUE)
    ep_per_season<-gsub("NA", "99999",ep_per_season, perl = TRUE)
    air_date<-gsub("NA", "99999",air_date, perl = TRUE)
    season<-gsub("NA", "99999",season, perl = TRUE)
    #season[is.na(season)] <- 99999
  #end:missing data from metadata
  ep_edge_density_value<-edge_density(ep_sociogram_igraph) #Anzahl an Verbindungen im Verhältnis zu Anzahl aller möglichen Verbindungen; The density of a graph is the ratio of the number of edges and the number of possible edges.
  ep_reciprocity_value<-reciprocity(ep_sociogram_igraph) #Aussage wird getätigt, Antwort an diese Person auf Episodenebene
  ep_diameter_value<-diameter(ep_sociogram_igraph, directed=T)
  ep_assortativity_value<-assortativity_degree(ep_sociogram_igraph, directed = TRUE)
  ep_df_values <- cbind(ep_title, ep_no, season, ep_per_season, air_date, ep_edge_density_value, ep_reciprocity_value, ep_diameter_value, ep_assortativity_value) #erweitern
  write.table(ep_df_values,"./data/miraculous/tables/episodes.csv", row.names = F, append = T, col.names = F, sep = "|") #erweitern #todo:
  
  
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
  sentimentProCharacter<- c(sentimentProCharacter, season, ep_no, ep_title)
  #sentimentProCharacter<- c(sentimentProCharacter, season, ep_no, ep_title, ep_degree_in, ep_degree_out, ep_degree_all, ep_closeness, ep_eigen_centrality, ep_betweenness, ep_hub_score,ep_authority_score,ep_rank_score)
  write.table(sentimentProCharacter,"./data/miraculous/tables/sentiment.csv", row.names = F, append = T, col.names = F, sep = "|")
  
  
  #todo
  #-betweenness 
  
  ### DialogTable for all eps: From | To | Sentiment | Text | season | ep_no | ep_title 
  dialogTable <- data.frame(pairs[, 1], pairs[, 2], sentimentMiraculous[1:(length(sentimentMiraculous))], script[, 2][1:(length(script[, 2]))], season, ep_no, ep_title)
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

# Coming soon
files <- list.files(path='./data/miraculous/processed', full.names=TRUE, recursive=TRUE, include.dirs=FALSE, pattern = ".*.csv",)
allCharacters <- vector(mode = "list", length = length(files))
i <- 0
for (file in files){
  characters <- process_transcript(file)
  i <- i+1
  allCharacters[[i]] <- unique(characters)
}
subset(table(unlist(allCharacters)), table(unlist(allCharacters))>20)
dialogTable <- read.csv("./data/miraculous/tables/dialogs.csv", sep="|")
episodeTable <- read.csv("./data/miraculous/tables/episodes.csv", sep="|")
sentimentTable <- read.csv("./data/miraculous/tables/sentiment.csv", sep="|")
lineTable <- dialogTable
lineTable[2] <- NULL
write.table(lineTable,"./data/miraculous/tables/lines.csv", row.names = F, append = T, col.names = F, sep = "|")
colnames(dialogTable) <- c("From", "To", "Sentiment", "Text","Season", "Episode Overall", "Episode Title")
colnames(lineTable) <- c("Character", "Sentiment", "Text", "Season", "Episode Overall", "Episode Title")
colnames(episodeTable) <- c("Episode Title", "Episode Overall", "Season", "Season's Episode", "Air Date", "Edge Density", "Reciprocity", "Diameter", "Assortativity")
colnames(sentimentTable) <- c("Character", "Sentiment","Season", "Episode Overall", "Episode Title")

#sorting
dialogTable <-dialogTable[order(dialogTable$'Episode Overall'),]
episodeTable <-episodeTable[order(episodeTable$'Episode Overall'),]
lineTable <-lineTable[order(lineTable$'Episode Overall'),]
sentimentTable <-sentimentTable[order(sentimentTable$'Episode Overall'),]
#sentimentTable <-sentimentTable[order(sentimentTable$'Character'),] #problem: pro episode sollte jeder name nur einmal vorkommen, leider kommt bspw. "Adrien" in Stormy-Weather und anderen mehrfach vor!

#trim Characters
dialogTable$From <- trimws(dialogTable$From, which = c("both"))
dialogTable$To <- trimws(dialogTable$To, which = c("both"))
lineTable$To <- trimws(lineTable$Character, which = c("both"))
sentimentTable$Character <- trimws(sentimentTable$Character, which = c("both"))

#Sentiment pro Character over whole Series
sentimentCharacterOverSeries <-sentimentTable %>%
  group_by(Character) %>%
  summarize(Sentiment_Over_Series = mean(Sentiment, na.rm = TRUE), Frequency = n())%>% arrange(desc(Frequency))

#Sentiment pro Character over Seasons
sentimentCharacterOverSeasons <-sentimentTable %>%
  group_by(Character, Season) %>%
  summarize(Sentiment_Over_Series = mean(Sentiment, na.rm = TRUE), Frequency = n()) %>% 
  arrange(desc(Season))

#looking up one character
sentimentCharacterOverSeasons_M <-sentimentTable %>%
  group_by(Character, Season) %>%
  summarize(Sentiment_Over_Series = mean(Sentiment, na.rm = TRUE), Frequency = n()) %>% 
  arrange(desc(Season))%>% #bish hierher
  filter(Character == 'Marinette') #plus filter

plot_S_Marinette_series <- sentimentCharacterOverSeasons_M %>%
  tail(10) %>%
  ggplot( aes(x=Frequency, y=Sentiment_Over_Series)) + #problem aufgrund von missing data
  geom_line( color="grey") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) +
  ylim(-1,1)
plot_S_Marinette_series + labs(title = "Time Evolution of mean sentiment over seasons", subtitle = "Marinette")

#subsets with missing values in episode table
missing_entries_episode_table <- episodeTable[rowSums(is.na(episodeTable)) > 0,]
missing_entries_episode_table99999 <- episodeTable[episodeTable$Season %like% "99999", ]

#processing - name
#dub<-table(lineTable$Character)
#dub<-data.frame(dub)

#cleanup
#rm(characterName, ep_sociogram_plot, filetext, pairs, script, sentimentProCharacter, sociogram_intermediate, characters, ep_sentiment_plot, file, file_intermediate, filepath, files, myEdges, nNodes, sentimentMiraculous, title_intermediate, transcript_lines)