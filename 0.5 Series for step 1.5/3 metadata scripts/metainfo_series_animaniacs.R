library(htmltab)
#-----EINTRAGUNGEN-------------------------------------------------------
prefix_series = "animaniacs"
anzahlseasons=7
link="https://en.wikipedia.org/wiki/List_of_Animaniacs_episodes"
#------------------------------------------------------------------------

#-----Table aus WIKIPEDIA Erstellen--------------------------------------
prefix = paste(prefix_series, "_meta_s", sep='')

for(i in 1:anzahlseasons) { 
  nam <- paste(prefix, i, sep = "")
  assign(nam, htmltab(link,1+i))}

#tidy up-----------------------------------------------------------------
animaniacs_meta_0_movie <-animaniacs_meta_s7
rm("animaniacs_meta_s7")
animaniacs_meta_0_movie= animaniacs_meta_0_movie[seq(1, nrow(animaniacs_meta_0_movie), 2), ]
animaniacs_meta_0_special <-animaniacs_meta_s6
rm("animaniacs_meta_s6")
#------------------------------------------------------------------------
#
filenames= list.files("./data/animaniacs/raw_renamed", full.names=FALSE)
#------------------------------------------------------------------------