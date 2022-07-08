##zielverzeichnis muss beinhalten: processed, plots, dialogtable
source("./main.R")
source("./analyze scripts/sentimentAI - initiate.R")

process_transcript<-function(filepath){
  #filepath<-file
  filetext <- readtext(filepath)
  transcript_lines <- str_split(filetext, "\\n")[[1]]
  characterName <- strsplit(transcript_lines, ":::")
  file_intermediate<-strsplit(file, "/")[[1]]
  title_intermediate<-file_intermediate[[5]]
  series<-file_intermediate[[3]]

  str(characterName)
  script <- matrix(unlist(characterName), ncol = 2, nrow=length(characterName), byrow = T) # file format suitable for sentiment analysis and graph
  sentimentAdventuretime <- sentiment_score(script[, 2])
  sentimentProCharacter <- aggregate(sentimentAdventuretime, by = list(script[, 1]), FUN = mean)
  sentimentProCharacter <- aggregate(sentimentAdventuretime, by = list(script[, 1]), FUN = length)
  
  characters <- script[, 1]
  
  nNodes <- table(characters)
  pairs <- matrix(NA, ncol = 2, nrow = length(characters)-1)
  pairs[, 1] <- characters[1:(length(characters)-1)]
  pairs[, 2] <- characters[2:length(characters)]
  myEdges <- as.character(str_split(unlist(pairs), ","))
  
  table(pairs[, 1], pairs[, 2])
  
  #SociogramPlot: format: series_titel.txt_sociogram
  
  sociogram_intermediate <- graph(c(myEdges))
  ep_sociogram_plot<-assign(paste(series, title_intermediate,"sociogram", sep="_"),sociogram_intermediate)
  plot(ep_sociogram_plot, main = "plot: sociogram", sub = paste(series, title_intermediate,"sociogram", sep="_"))

  #SentimentPlot (sentiment scores/lines): format: series_titel.txt_sentiment
  ep_sentiment_plot<-assign(paste(series, title_intermediate,"sentiment", sep="_"),sentimentAdventuretime)
  ts.plot(ep_sentiment_plot, main = "plot: sentiment score per lines (time)", sub = paste(series, title_intermediate,"sentiment", sep="_"), xlab="line", ylab="sentiment score")
  #ts.plot(sentimentAdventuretime, main = file)s
  
  #write to pdf
  pdf(file=paste("./data/adventuretime/plots/", series, "_", title_intermediate, ".pdf", sep=""))
  # 2. Create the plot
  plot(ep_sociogram_plot, main = "plot: sociogram", sub = paste(series, title_intermediate,"sociogram", sep="_"))
  ts.plot(ep_sentiment_plot, main = "plot: sentiment score per lines (time)", sub = paste(series, title_intermediate,"sentiment", sep="_"), xlab="line", ylab="sentiment score")
  # 3. Close the file
  dev.off()
  #end write
  return(assign(paste(series, title_intermediate,"sociogram", sep="_"),sociogram_intermediate))
}

# Coming soon
files <- list.files(path='./data/adventuretime/processed', full.names=TRUE, recursive=TRUE, include.dirs=FALSE)
for (file in files){
  process_transcript(file)
}

#cleanup
#rm(characterName, ep_sociogram_plot, filetext, pairs, script, sentimentProCharacter, sociogram_intermediate, characters, ep_sentiment_plot, file, file_intermediate, filepath, files, myEdges, nNodes, sentimentAdventuretime, title_intermediate, transcript_lines) 
