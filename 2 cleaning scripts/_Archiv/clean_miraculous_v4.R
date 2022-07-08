##zielverzeichnis muss beinhalten: raw_proscessed dateien in root; leere verzeichnisse: raw, abandoned, processed
source("./main.R")

clean_transcript <- function(filepath){
  filetext <- readtext(filepath)
  transcript <- str_split(filetext, "\\n")[[1]]
  transcript_lines <- c()
  for (line in transcript){
    transcript_lines <- append(transcript_lines, line)
  }
  cleaned_transcript <- c()
  ### TODO: Regex for cleaning transcripts
  for (line in transcript_lines){
    #line <- gsub("\\[\\[[^\\|^\\]]+\\|", "",line, perl = TRUE) # make [[x|y]] to y]]
    #line <- gsub("\\[[^\\]]+\\]", "", line, perl= TRUE) # delete evertin in [Sinle brackets]
    line <- gsub("'''", "", line)
    line <- gsub("''", "", line)
    line <- gsub("\\[[^\\|]+\\|", "",line, perl = TRUE)
    line <- gsub("\\([^\\)]+\\)", "", line, perl= TRUE)
    line <- gsub("\\{[^\\}]+\\}", "", line, perl= TRUE) # delete every line in {Single brackets} '- funzt
    #line <- gsub("\\D+\\:+\\ ", "", line, perl= TRUE) # delete es: '- funzt nicht
    line <- gsub("\\[", "", line)
    line <- gsub("\\]", "", line)
    line <- gsub("\\<[^\\table^\\}]+\\}", "", line, perl= TRUE) # delete evertin in {Single brackets} '- funzt
    #song1 - ok
    line <- gsub("\\<[^\\>]+\\>", " ", line, perl= TRUE)
    line <- gsub("\\♫", "", line)
    line <- gsub("Transformation Sequence", "", line)
    line <- gsub("\\♪", "", line)
    line <- gsub("-->", "", line)
    line <- gsub("--", "", line)
    #ausprobieren, 3x und 2x leerzeichen - ok
    line <- gsub("   ", "", line)
    line <- gsub("  ", "", line)
    line <- sub(':',':::', line)
    #v4
    line <- sub('Adrian','Adrien', line)
    line <- sub('Andre','André', line)
    line <- sub('Andrè','André', line)
    line <- sub("August's mom","August's mother", line)
    line <- sub("August's mom","August's mother", line)
    line <- sub("Chloe","Chloé", line)
    line <- sub("Myléne","Mylène", line)
    line <- sub("Najda","Nadja", line)
    line <- sub("Natalie","Nathalie", line)
    line <- sub("Officer Rogers","Officer Roger", line)
    line <- sub("Robustus","Robostus", line)
    line <- sub("Rytym","Rythm", line)
    line <- sub("Santa","Santa Claus", line)
    line <- sub("The rest of the kwamis","The rest of the Kwamis", line)
    line <- sub("Hawkmoth","Hawk Moth", line)
    line <- sub("All three","All Three", line)
    line <- sub("boy","Boy", line)
    line <- sub("bus driver","Bus driver", line)
    line <- sub("Civillian","Civilian", line)
    line <- sub("Dider Roustan","Didier Roustan", line)
    line <- sub("Tom'","Tom", line)
    line <- sub("Veronique","Véronique", line)
    line <- sub(" ) Marinette","Marinette", line)
    #Delte all white spaces
    line <- gsub(".*:::", paste(str_trim(sub(":::.*", "",line)), ":::", sep = ""), line)
    if(str_starts(line, "[^:]+:::.+")){
      if(!str_starts(line, "Category") & (!str_starts(line, "es:")) & (!str_starts(line, "pl:")) & (!str_starts(line, "fr:")) & (!str_starts(line, "ja:")) & (!str_starts(line, "de:")) & (!str_starts(line, "\\{"))){
      #if(!str_starts(line, "Category") & (!str_starts(line, "es:")) & (!str_starts(line, "pl:")) & (!str_starts(line, "fr:")) & (!str_starts(line, "ja:")) & (!str_starts(line, "de:")){ 
        cleaned_transcript <- append(cleaned_transcript, line)
      } 
    }
  }
  if (length(cleaned_transcript) < 20){
    file.move(filepath, "./data/miraculous/abandoned/")
    return ()
  }
  file.move(filepath, "./data/miraculous/raw/")
  filepath <- gsub("miraculous", "miraculous/processed", filepath)
  writeLines(cleaned_transcript, filepath)
}

files <- list.files(path='./data/miraculous/', full.names=TRUE, pattern = ".*.txt", include.dirs=FALSE)
for (file in files){
  clean_transcript(file)
}