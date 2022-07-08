get_transcripts_urls <- function(overview_urls, prefix) {
  transcript_urls <- c()
  for(url in overview_urls){
    # get all links from overview_page
    overview_html <- read_html(url)
    links <- html_attr(html_nodes(overview_html, "a"), "href")
    for(link in links){
      if(!is.na(link)){
        # check if the link leads to a transcript page
        if(str_detect(link, regex("wiki/.*/Transcript"))){
          if(!startsWith(link, "http")){
            link <- paste(prefix, link, sep="")
          }
          transcript_urls <- c(transcript_urls, link)
        }
      }
    }
  }
  return(transcript_urls)
}

get_transcript <- function(url, path){
  url <- paste(url,'?action=edit', sep="")
  transcript_html <- read_html(url) 
  transcript <- transcript_html %>%
    html_nodes("#wpTextbox1") %>% html_text()
  writeLines(transcript, path)
}

clean_transcript <- function(filepath){
  serie <- "Adventure Time"
  epTitle <- tail(str_split(filepath, "/")[[1]], n=1)
  epTitle <- gsub("_", " ", epTitle)
  epTitle <- gsub(".txt", "", epTitle)
  season <- NaN
  airDate <- NaN
  filetext <- readtext(filepath)
  transcript_lines <- str_split(filetext, "\\n")[[1]]
  cleaned_transcript <- c()
  for (line in transcript_lines){
    if(str_starts(line, "\\|.*season")){
      season <- str_extract(line, "[0-9]+")
    } 
    if(str_starts(line, "\\|.*airdate")){
      airDate <- str_extract(line, "[0-9]{4}")
    }
    line <- gsub("\\{\\{L\\|", "", line)
    line <- gsub('}}', "", line)
    line <- gsub("\\[\\[", "", line)
    line <- gsub("\\]\\]", "", line)
    line <- gsub("\\[''[^\\]]+\\'']", "", line, perl= TRUE)
    line <- gsub("\\|", "::", line)
    if(str_starts(line, "[^:]+:.+")){
      cleaned_transcript <- append(cleaned_transcript, line)
    }
  }
  if (length(cleaned_transcript) < 40){
    file.move(filepath, "./data/adventuretime/abandoned/")
    return ()
  }
  lines <- length(cleaned_transcript)
  file.move(filepath, "./data/adventuretime/raw/")
  filepath <- gsub("adventuretime", "adventuretime/processed", filepath)
  writeLines(cleaned_transcript, filepath)
  episodeCorpus <- c(serie, season, epTitle, airDate, lines)
  return (episodeCorpus)
}

process_transcript<-function(filepath){
  countingTable <- matrix(ncol=6)
  colnames(countingTable)<- c("Name", "Lines", "Sentiment", "SentenceCount", "WordCount", "WordLength")
  cdf <- as.data.frame(countingTable)
  charTable <- matrix(ncol=6)
  colnames(charTable)<- c("Name", "Lines", "Sentiment", "SentenceCount", "AvgWordLength", "AvgWordsPerSentence")
  lines <- 0
  sentiment <- 0
  sentenceCounter <- 0
  wordsCounter <- 0
  wordLength <- 0
 
  #require(stringi)
  #ui <- rep("a", length(filepath))
  #for(i in 1:length(filepath)){ui[i] <- as.character(stri_enc_detect(filepath)[[i]]$Encoding[1])} # tries to guess, which is the correct encoding
   
  #filetext <- readtext(filepath, encoding = ui)
  filetext <- readtext(filepath)
  transcript_lines <- str_split(filetext, "\\n")[[1]]
  name <- strsplit(transcript_lines, "[:]")
  #problemLinien <- which(mapply(function(k) length(name[[k]]), 1:length(transcript_lines))>2)
  #for(j in 1:length(problemLinien)) {name[[j]][2] <- paste(name[[problemLinien[j]]][-1], collapse =" ")}

  str(name)
  script <- matrix(unlist(name), ncol = 2, nrow=length(name), byrow = T) # file format suitable for sentiment analysis and graph
  sentimentAdventurous <- sentiment_score(script[, 2])
  sentmentProCharacter <- aggregate(sentimentAdventurous, by = list(script[, 1]), FUN = mean)
  sentmentProCharacter <- aggregate(sentimentAdventurous, by = list(script[, 1]), FUN = length)
  
  install.packages("igraph")
  require(igraph)
  
  characters <- script[, 1]
  
  nNodes <- table(characters)
  pairs <- matrix(NA, ncol = 2, nrow = length(characters)-1)
  pairs[, 1] <- characters[1:(length(characters)-1)]
  pairs[, 2] <- characters[2:length(characters)]
  myEdges <- as.character(str_split(unlist(pairs), ","))
  
  table(pairs[, 1], pairs[, 2])
  
  
  g1 <- graph(c(myEdges))
  plot(g1)
  
  ts.plot(sentimentAdventurous)
  
  for (line in transcript_lines){
    name <- strsplit(line, "[:]")[[1]][1]
    name <- toupper(name)
    name <- trimws(name)
    dialog <- gsub("^[^:]*:", "", line)
    # entry exists
    if(name%in%charactersInEpTable[, 1]){
      entry <- filter(cdf, Name == toupper(name))
      entry$Lines <- entry$Lines +1
      # TODO real sentiment
      entry$Sentiment <- entry$Sentiment + 0
      entry$SentenceCount <- entry$SentenceCount +nsentence(dialog)
      entry$WordCount <- entry$WordCount + str_count(dialog, '\\w+')
      for (word in strsplit(dialog, "[[:punct:]]")[[1]]){
        entry$WordLength <- entry$WordLength + str_length(word)
      }
    }
    else{
      lines <- 1
      # TODO real sentiment
      sentiment <- 0
      sentenceCount <- nsentence(dialog)
      wordCount <- str_count(dialog, '\\w+')
      for (word in strsplit(dialog, "[[:punct:]]")[[1]]){
        wordLength <- str_length(word)
      }
      rbind(cdf, c(name, lines, sentiment, sentenceCount, wordCount, wordLength))
    }
  }
  #forEach line in counterTable
  for(row in 1:nrow(cdf)){
    sentiment <- cdf[row, "Sentiment"] / cdf[row, "Lines"]
    avgWordLength <- cdf[row, "WordLength"] / cdf[row, "WordCounter"]
    avgWordsPerSentence <- cdf[row, "WordCounter"] / cdf[row, "SentenceCounter"]
    charTable <- rbind(charTable,c(cdf$name, cdf$lines, sentiment, cdf$sentenceCount, avgWordLength, avgWordsPerSentence))
  }
  return (dialogTable)
}

episodeCorpusMatrix <- matrix(ncol=5)
characterInAllEpTable <- data.frame()
adventuretime_overview_urls<-c('https://adventuretime.fandom.com/wiki/Category:Transcripts')
adventuretime_prefix <- 'https://adventuretime.fandom.com/'
adventuretime_transcript_urls <- get_transcripts_urls(adventuretime_overview_urls, adventuretime_prefix)
for(i in 1:length(adventuretime_transcript_urls)){
  Sys.sleep(1)
  filename <- paste('.\\data\\adventuretime\\',str_match(adventuretime_transcript_urls[i], "wiki/\\s*(.*?)\\s*/Transcript")[2], sep = "")
  get_transcript(adventuretime_transcript_urls[i], paste(filename, ".txt", sep = ""))
}
files <- list.files(path='./data/adventuretime/', full.names=TRUE, recursive=TRUE, include.dirs=FALSE)
for (file in files){
  episodeCorpus <- clean_transcript(file)
  episodeCorpusMatrix <- rbind(episodeCorpusMatrix, episodeCorpus)
}

# Coming soon
files <- list.files(path='./data/adventuretime/processed', full.names=TRUE, recursive=TRUE, include.dirs=FALSE)
for (file in files){
  charactersEpDf <- process_transcript(file)
  # TODO: Combine Characters
  characterInAllEpTable <- rbind(characterInAllEpTable, charactersInEpTable)
}

## testecke
# setwd("E:/Wood/lehre/Graz/Empirical_seminary/Series/ES/ES/")

files <- list.files(path='./data/adventuretime/processed', full.names=TRUE, recursive=TRUE, include.dirs=FALSE)
filepath <- files[5]
countingTable <- matrix(ncol=6)
colnames(countingTable)<- c("Name", "Lines", "Sentiment", "SentenceCount", "WordCount", "WordLength")
cdf <- as.data.frame(countingTable)
charTable <- matrix(ncol=6)
colnames(charTable)<- c("Name", "Lines", "Sentiment", "SentenceCount", "AvgWordLength", "AvgWordsPerSentence")
lines <- 0
sentiment <- 0
sentenceCounter <- 0
wordsCounter <- 0
wordLength <- 0
filetext <- readtext(filepath)
transcript_lines <- str_split(filetext, "\\n")[[1]]
for (line in transcript_lines){
  name <- strsplit(line, "[:]")[[1]][1]
  name <- toupper(name)
  name <- trimws(name)
  dialog <- gsub("^[^:]*:", "", line)
  # entry exists
  if(name%in%countingTable[, 1]){
    entry <- filter(cdf, Name == toupper(name))
    entry$Lines <- entry$Lines +1
    # TODO real sentiment
    entry$Sentiment <- entry$Sentiment + 0
    entry$SentenceCount <- entry$SentenceCount +nsentence(dialog)
    entry$WordCount <- entry$WordCount + str_count(dialog, '\\w+')
    for (word in strsplit(dialog, "[[:punct:]]")[[1]]){
      entry$WordLength <- entry$WordLength + str_length(word)
    }
  }
  else{
    lines <- 1
    # TODO real sentiment
    sentiment <- 0
    sentenceCount <- nsentence(dialog)
    wordCount <- str_count(dialog, '\\w+')
    for (word in strsplit(dialog, "[[:punct:]]")[[1]]){
      wordLength <- str_length(word)
    }
    rbind(cdf, c(name, lines, sentiment, sentenceCount, wordCount, wordLength))
  }
}
#forEach line in counterTable
for(row in 1:nrow(cdf)){
  sentiment <- cdf[row, "Sentiment"] / cdf[row, "Lines"]
  avgWordLength <- cdf[row, "WordLength"] / cdf[row, "WordCounter"]
  avgWordsPerSentence <- cdf[row, "WordCounter"] / cdf[row, "SentenceCounter"]
  characterTable <- rbind(characterTable,c(cdf$name, cdf$lines, sentiment, cdf$sentenceCount, avgWordLength, avgWordsPerSentence))
}
