library(htmltab)
#-----EINTRAGUNGEN-------------------------------------------------------
prefix_series = "spongebob"
anzahlseasons=14
link="https://en.wikipedia.org/wiki/List_of_SpongeBob_SquarePants_episodes#Season_1_(1999%E2%80%932001)"
#------------------------------------------------------------------------

#-----Table aus WIKIPEDIA Erstellen--------------------------------------
prefix = paste(prefix_series, "_meta_s", sep='')

for(i in 1:anzahlseasons) { 
  nam <- paste(prefix, i-1, sep = "")
  assign(nam, htmltab(link,i))}

#tidy up-----------------------------------------------------------------
#delete ever other row  
  spongebob_meta_overview<-spongebob_meta_s0
  rm("spongebob_meta_s0")
#------------------------------------------------------------------------
#
filenames= list.files("./data/spongebob/raw_renamed", full.names=FALSE)
#------------------------------------------------------------------------