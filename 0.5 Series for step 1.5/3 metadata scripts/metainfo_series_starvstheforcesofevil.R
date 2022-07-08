library(htmltab)
#-----EINTRAGUNGEN-------------------------------------------------------
prefix_series = "starvstheforcesofevil"
anzahlseasons=4
link="https://en.wikipedia.org/wiki/List_of_Star_vs._the_Forces_of_Evil_episodes"
#------------------------------------------------------------------------

#-----Table aus WIKIPEDIA Erstellen--------------------------------------
prefix = paste(prefix_series, "_meta_s", sep='')

for(i in 1:anzahlseasons) { 
  nam <- paste(prefix, i, sep = "")
  assign(nam, htmltab(link,1+i))}

#tidy up-----------------------------------------------------------------
  starvstheforcesofevil_meta_s1= starvstheforcesofevil_meta_s1[seq(1, nrow(starvstheforcesofevil_meta_s1), 2), ]
  starvstheforcesofevil_meta_s2= starvstheforcesofevil_meta_s2[seq(1, nrow(starvstheforcesofevil_meta_s2), 2), ]
  starvstheforcesofevil_meta_s3= starvstheforcesofevil_meta_s3[seq(1, nrow(starvstheforcesofevil_meta_s3), 2), ]
  starvstheforcesofevil_meta_s4= starvstheforcesofevil_meta_s4[seq(1, nrow(starvstheforcesofevil_meta_s4), 2), ]
#------------------------------------------------------------------------
#
filenames= list.files("./data/starvstheforcesofevil/raw_renamed", full.names=FALSE)
#------------------------------------------------------------------------