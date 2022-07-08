# METAINFO

#prerequisites
# - internet connection
# - empty folders "data/miraculous"must be present

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
  filetext <- readtext(filepath)
  transcript <- str_split(filetext, "\\n")[[1]]
  transcript_lines <- c()
  for (line in transcript){
    transcript_lines <- append(transcript_lines, line)
  }
  cleaned_transcript <- c()
  for (line in transcript_lines){
    line <- gsub("'''", "", line)
    line <- gsub("''", "", line)
    line <- gsub("\\[[^\\|]+\\|", "",line, perl = TRUE)
    line <- gsub("\\([^\\)]+\\)", "", line, perl= TRUE)
    line <- gsub("\\[", "", line)
    line <- gsub("\\]", "", line)
    if(str_starts(line, "[A-Z]*[a-z]*:.*")){
      if(!str_starts(line, "Category")){
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

miraculous_overview_urls<-c('https://miraculousladybug.fandom.com/wiki/Category:Season_1_transcripts','https://miraculousladybug.fandom.com/wiki/Category:Season_2_transcripts','https://miraculousladybug.fandom.com/wiki/Category:Season_3_transcripts','https://miraculousladybug.fandom.com/wiki/Category:Season_4_transcripts','https://miraculousladybug.fandom.com/wiki/Category:Season_5_transcripts')
miraculous_prefix <- 'https://miraculousladybug.fandom.com/'
miraculous_transcript_urls <- get_transcripts_urls(miraculous_overview_urls, miraculous_prefix)
for(i in 1:length(miraculous_transcript_urls)){
  Sys.sleep(1)
  filepath <- paste('.\\data\\miraculous\\',str_match(miraculous_transcript_urls[i], "wiki/\\s*(.*?)\\s*/Transcript")[2], sep = "")
  get_transcript(miraculous_transcript_urls[i], paste(filepath, ".txt", sep = ""))
}
files <- list.files(path='./data/miraculous/', full.names=TRUE, recursive=TRUE, include.dirs=FALSE)
for (file in files){
  clean_transcript(file)
}
print("<End of Downloading-Script>")
#------------------------------------------------------------------------