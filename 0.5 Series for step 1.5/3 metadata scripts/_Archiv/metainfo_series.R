library(htmltab)
#-----EINTRAGUNGEN-------------------------------------------------------
prefix_series = "spongebob"
anzahlseasons=14
link="https://en.wikipedia.org/wiki/List_of_SpongeBob_SquarePants_episodes#Season_1_(1999%E2%80%932001)"
#------------------------------------------------------------------------

#-----Table aus WIKIPEDIA Erstellen--------------------------------------
prefix = paste(prefix_series, "_meta_s", sep='')

for(i in 1:anzahlseasons) { 
  nam <- paste(prefix, i, sep = "")
  assign(nam, htmltab(link,1+i))}
#------------------------------------------------------------------------
#
filenames= list.files("./data/spongebob/raw_renamed", full.names=FALSE)
#
for(u in filenames)
  {
  print(u)
  inner_join(spongebob_meta_s1, by=u)
  }
#

spongebob_meta_s1[, c(1, 2, 3)] #ersten 3 columns anschauen
spongebob_meta_s1[, (colnames(spongebob_meta_s1) %in% c('Title'))] #column abschauen
filter(spongebob_meta_s1, Title=="Help")
spongebob_meta_s1 %>% filter(spongebob_meta_s1, Title=="Help")

spongebob_meta_s1 %>% filter(title=="\"Help Wanted\"")

library(dplyr)
x<-as_tibble(spongebob_meta_s1)
x %>% filter(No.overall == "1a")
x %>% filter(Title == "\"Help Wanted\")

