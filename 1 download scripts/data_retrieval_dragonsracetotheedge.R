get_transcripts_urls <- function(overview_urls, prefix) {
  transcript_urls <- c()
  for(url in overview_urls){
    # get all links from overview_page
    overview_html <- read_html(url)
    links <- html_attr(html_nodes(overview_html, "a"), "href")
    for(link in links){
      if(!is.na(link)){
        # check if the link leads to a transcript page
        if(str_detect(link, regex("wiki/.*(transcript)"))){
          #https://howtotrainyourdragon.fandom.com/wiki/A_Gruff_Separation_(transcript)
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
    if(str_starts(line, "[A-Z]*[a-z]*:.*")){
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

dragonsracetotheedge_overview_urls<-c('https://howtotrainyourdragon.fandom.com/wiki/Category:Dragons:_Race_to_the_Edge_Transcripts')
dragonsracetotheedge_prefix <- 'https://howtotrainyourdragon.fandom.com/'
dragonsracetotheedge_transcript_urls <- get_transcripts_urls(dragonsracetotheedge_overview_urls, dragonsracetotheedge_prefix)
for(i in 1:length(dragonsracetotheedge_transcript_urls)){
  Sys.sleep(1)
  filename <- paste('.\\data\\dragonsracetotheedge\\',str_match(dragonsracetotheedge_transcript_urls[i], "wiki/\\s*(.*?)\\s*(transcript)")[2], sep = "")
  get_transcript(dragonsracetotheedge_transcript_urls[i], paste(filename, ".txt", sep = ""))
}