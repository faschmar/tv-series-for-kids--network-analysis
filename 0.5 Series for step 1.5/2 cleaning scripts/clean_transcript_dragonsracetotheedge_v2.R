clean_transcript <- function(filepath){
  filetext <- readtext(filepath)
  transcript <- str_split(filetext, "\\n")[[1]]
  transcript_lines <- c()
  for (line in transcript){
    transcript_lines <- append(transcript_lines, line)
  }
  cleaned_transcript <- c()
  for (line in transcript_lines){
    line <- gsub("'''", "", line)
    line <- gsub('"', "", line)
    line <- gsub(".*\\|", "", line)
    line <- gsub("\\([^\\)]+\\)", "", line, perl= TRUE)
    line <- gsub("\\[", "", line)
    line <- gsub("\\]", "", line)
    line <- gsub("''", "", line)
    line <- sub(':',':::', line)
    if(str_starts(line, "[^:]+:.+")){
      if(!str_starts(line, "Category")){
        cleaned_transcript <- append(cleaned_transcript, line)
      }
    }
  }
  if (length(cleaned_transcript) < 20){
    file.move(filepath, "./data/dragonsracetotheedge/abandoned/")
    return ()
  }
  file.move(filepath, "./data/dragonsracetotheedge/raw/")
  filepath <- gsub("dragonsracetotheedge", "dragonsracetotheedge/processed", filepath)
  writeLines(cleaned_transcript, filepath)
}

files <- list.files(path='./data/dragonsracetotheedge/', full.names=TRUE, recursive=TRUE, include.dirs=FALSE)
for (file in files){
  clean_transcript(file)
}