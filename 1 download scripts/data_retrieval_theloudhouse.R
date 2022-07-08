get_transcripts_urls <- function(overview_urls, prefix) {
  transcript_urls <- c()
  for(url in overview_urls){
    # get all links from overview_page
    overview_html <- read_html(url)
    links <- html_attr(html_nodes(overview_html, "a"), "href")
    for(link in links){
      if(!is.na(link)){
        # check if the link leads to a transcript page
        if(str_detect(link, regex("wiki/.*/Script"))){
          #https://theloudhouse.fandom.com/wiki/10_Headed_Beast/Script
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
    breakpointA <- str_split(line, "<br />")[[1]]
    for (bkA in breakpointA){
      breakpointB <- str_split(bkA, "<br>")[[1]]
      for(bkB in breakpointB){
        transcript_lines <- append(transcript_lines, bkB)
      }
    }
  }
  cleaned_transcript <- c()
  for (line in transcript_lines){
    line <- gsub("'''", "", line)
    line <- gsub('"', "", line)
    line <- gsub("<br>", "", line)
    line <- gsub("</b>", "", line)
    line <- gsub("<b>", "", line) 
    line <- gsub("''\\[[^\\]]+\\]''", "", line, perl= TRUE)
    line <- gsub("''", "", line) 
    if(str_starts(line, "[A-Z]*[a-z]*:.*")){
      if(!str_starts(line, "Category")){
        cleaned_transcript <- append(cleaned_transcript, line)
      }
    }
  }
  if (length(cleaned_transcript) < 20){
    file.move(filepath, "./data/theloudhouse/abandoned/")
    return ()
  }
  file.move(filepath, "./data/theloudhouse/raw/")
  filepath <- gsub("theloudhouse", "theloudhouse/processed", filepath)
  writeLines(cleaned_transcript, filepath)
}

theloudhouse_overview_urls<-c('https://theloudhouse.fandom.com/wiki/Category:Scripts')
theloudhouse_prefix <- 'https://theloudhouse.fandom.com/'
theloudhouse_transcript_urls <- get_transcripts_urls(theloudhouse_overview_urls, theloudhouse_prefix)
for(i in 1:length(theloudhouse_transcript_urls)){
  Sys.sleep(1)
  filename <- paste('.\\data\\theloudhouse\\',str_match(theloudhouse_transcript_urls[i], "wiki/\\s*(.*?)\\s*/Script")[2], sep = "")
  get_transcript(theloudhouse_transcript_urls[i], paste(filename,".txt", sep = ""))
}
files <- list.files(path='./data/theloudhouse/', full.names=TRUE, recursive=TRUE, include.dirs=FALSE)
for (file in files){
  clean_transcript(file)
}
