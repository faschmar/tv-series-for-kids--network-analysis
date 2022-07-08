##zielverzeichnis muss beinhalten: raw_proscessed dateien in root; leere verzeichnisse: raw, abandoned, processed
source("./main.R")

clean_transcript <- function(filepath){
  filetext <- readtext(filepath)
  transcript <- str_split(filetext, "\\n")[[1]]
  transcript_lines <- c()
  for (line in transcript){
    breakpointA <- str_split(line, "<br />")[[1]]
    for (bkA in breakpointA){
      breakpointB <- str_split(bkA, "<br>")[[1]]
      for(bkB in breakpointB){
        transcript_lines <- append(transcript_lines, bkB)
      }
    }
  }
  cleaned_transcript <- c()
  ### TODO: Regex for cleaning transcripts
  for (line in transcript_lines){
    line <- gsub("'''", "", line)
    line <- gsub('"', "", line)
    line <- gsub("<br>", "", line)
    line <- gsub("</b>", "", line)
    line <- gsub("<b>", "", line) 
    line <- gsub("''\\[[^\\]]+\\]''", "", line, perl= TRUE)
    line <- gsub("''", "", line) 
    line <- gsub("\\[[^\\]]+\\]]", "", line, perl= TRUE) # delete line in double in [[brackets]]
    line <- gsub("\\[[^\\]]+\\]", "", line, perl= TRUE) # delete line in single in [ brackets]
    line <- gsub("\\{[^\\}]+\\}}", "", line, perl= TRUE) # delete every line in {Single brackets} '- funzt
    line <- gsub("<br/>", "", line)
    line <- gsub(" / ", ". ", line)
    line <- gsub("<u>", "", line) 
    line <- gsub("</u>", "", line)
    line <- gsub("♫<i>", "", line) 
    line <- gsub("</i>♫", "", line)
    line <- gsub("<i>", "", line) 
    line <- gsub("</i>", "", line)
    line <- gsub("\\♫ ", "", line)
    line <- gsub("\\♫", "", line) #zuletzt
    
    line <- sub(':',':::', line)
    if(str_starts(line, "[^:]+:::.+")){
      if(!str_starts(line, "Category") & (!str_starts(line, "es:")) & (!str_starts(line, "pl:")) & (!str_starts(line, "fr:")) & (!str_starts(line, "de:"))){ 
        cleaned_transcript <- append(cleaned_transcript, line)
      } 
    }
  }
  if (length(cleaned_transcript) < 20){
    file.move(filepath, "./data/theloudhouse/abandoned")
    return ()
  }
  file.move(filepath, "./data/theloudhouse/raw/")
  filepath <- gsub("theloudhouse", "theloudhouse/processed", filepath)
  writeLines(cleaned_transcript, filepath)
}

files <- list.files(path='./data/theloudhouse/', full.names=TRUE, recursive=TRUE, include.dirs=FALSE)
for (file in files){
  clean_transcript(file)
}
