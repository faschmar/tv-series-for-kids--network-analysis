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
pawpatrol_overview_urls<-c('https://pawpatrol.fandom.com/wiki/Category:Season_1_Transcripts','https://pawpatrol.fandom.com/wiki/Category:Season_2_Transcripts','https://pawpatrol.fandom.com/wiki/Category:Season_3_Transcripts','https://pawpatrol.fandom.com/wiki/Category:Season_4_Transcripts','https://pawpatrol.fandom.com/wiki/Category:Season_5_Transcripts','https://pawpatrol.fandom.com/wiki/Category:Season_6_Transcripts')
pawpatrol_prefix <- 'https://pawpatrol.fandom.com/'
pawpatrol_transcript_urls <- get_transcripts_urls(pawpatrol_overview_urls, pawpatrol_prefix)
for(i in 1:length(pawpatrol_transcript_urls)){
  Sys.sleep(1)
  get_transcript(pawpatrol_transcript_urls[i], paste('./data/pawpatrol/',str_match(pawpatrol_transcript_urls[i], "wiki/\\s*(.*?)\\s*/Transcript")[2], sep = ""))
}
