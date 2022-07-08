get_transcripts_urls <- function(overview_urls, prefix) {
  transcript_urls <- c()
  for(url in overview_urls){
    # get all links from overview_page
    overview_html <- read_html(url)
    links <- html_attr(html_nodes(overview_html, "a"), "href")
    for(link in links){
      if(!is.na(link)){
        # check if the link leads to a transcript page
        if(str_detect(link, regex("wiki/Transcripts/.*"))){
          if(!startsWith(link, "http")){
            link <- paste(prefix, link, sep="")
          }
          transcript_urls <- c(transcript_urls, link)
        }
        #https://mlp.fandom.com/wiki/Transcripts/Owl%27s_Well_That_Ends_Well
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

mylittlepony_overview_urls <- c('https://mlp.fandom.com/wiki/Friendship_is_Magic_animated_media')
mylittlepony_prefix <- 'https://mlp.fandom.com/'
mylittlepony_transcript_urls <- get_transcripts_urls(mylittlepony_overview_urls, mylittlepony_prefix)
for(i in 1:length(mylittlepony_transcript_urls)){
  Sys.sleep(1)
  get_transcript(mylittlepony_transcript_urls[i],paste('./data/mylittlepony/', gsub("^.*/", "", mylittlepony_transcript_urls[i]), sep = ""))
}
