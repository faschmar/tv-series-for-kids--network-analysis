clean_transcript <- function(filepath){
  filetext <- readtext(filepath)
  transcript_lines <- str_split(filetext, "\\n")[[1]]
  cleaned_transcript <- c()
  for (line in transcript_lines){
    line <- gsub("\\[\\[[^\\|^\\]]+\\|", "",line, perl = TRUE) # make [[x|y]] to y]]
    line <- gsub("\\[\\[", "", line)
    line <- gsub("\\]\\]", "", line)
    line <- gsub("\\[[^\\]]+\\]", "", line, perl= TRUE)
    line <- gsub("\\{\\{L\\|", "", line)
    line <- gsub("\\{\\{l\\|", "", line)
    line <- gsub('}}', "", line)
    line <- gsub("'''", "", line)
    line <- gsub("''", "", line)
    line <- gsub("\\♫", "", line)
    line <- gsub("\\♪", "", line)
    line <- gsub("   ", "", line)
    line <- gsub("  ", "", line)
    line <- sub(':',':::', line)
    line <- gsub("\\|", ":::", line)
    if(str_starts(line, "[^:]+:::.+")){
      if(!str_starts(line, "Category") & (!str_starts(line, "es:")) & (!str_starts(line, "pl:")) & (!str_starts(line, "fr:")) & (!str_starts(line, "de:"))){ 
        cleaned_transcript <- append(cleaned_transcript, line)
      }
    }
  }
  if (length(cleaned_transcript) < 40){
    file.move(filepath, "./data/adventuretime/abandoned/")
    return ()
  }
  file.move(filepath, "./data/adventuretime/raw/")
  filepath <- gsub("adventuretime", "adventuretime/processed", filepath)
  writeLines(cleaned_transcript, filepath)
}

files <- list.files(path='./data/adventuretime/', full.names=TRUE, recursive=TRUE, include.dirs=FALSE)
for (file in files){
  clean_transcript(file)
}