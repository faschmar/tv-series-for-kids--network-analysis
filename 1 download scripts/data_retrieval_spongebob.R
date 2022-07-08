get_transcripts_urls <- function(overview_urls, prefix) {
  transcript_urls <- c()
  for(url in overview_urls){
    # get all links from overview_page
    overview_html <- read_html(url)
    links <- html_attr(html_nodes(overview_html, "a"), "href")
    for(link in links){
      if(!is.na(link)){
        # check if the link leads to a transcript page
        if(str_detect(link, regex("wiki/.*/transcript"))){
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

spongebob_overview_urls <- c('https://spongebob.fandom.com/wiki/List_of_transcripts')
spongebob_prefix <- 'https://spongebob.fandom.com/'
spongebob_transcript_urls <- get_transcripts_urls(spongebob_overview_urls, spongebob_prefix)
for(i in 1:length(spongebob_transcript_urls)){
  Sys.sleep(1)
  get_transcript(spongebob_transcript_urls[i], paste('.\\data\\spongebob\\',str_match(spongebob_transcript_urls[i], "wiki/\\s*(.*?)\\s*/transcript")[2], sep = ""))
}
