library(htmltab)
#-----EINTRAGUNGEN-------------------------------------------------------
prefix_series = "miraculous"
anzahlseasons=5
link="https://en.wikipedia.org/wiki/List_of_Miraculous:_Tales_of_Ladybug_%26_Cat_Noir_episodes"
#------------------------------------------------------------------------

#-----Table aus WIKIPEDIA Erstellen--------------------------------------
prefix = paste(prefix_series, "_meta_s", sep='')

for(i in 1:anzahlseasons) { 
  nam <- paste(prefix, i, sep = "")
  assign(nam, htmltab(link,1+i))}

#tidy up-----------------------------------------------------------------
#delete useless tables 
#delete ever other row  
  miraculous_meta_s0= miraculous_meta_s0[seq(1, nrow(miraculous_meta_s0), 2), ]
  miraculous_meta_s1= miraculous_meta_s1[seq(1, nrow(miraculous_meta_s1), 2), ]
  miraculous_meta_s2= miraculous_meta_s2[seq(1, nrow(miraculous_meta_s2), 2), ]
  miraculous_meta_s3= miraculous_meta_s3[seq(1, nrow(miraculous_meta_s3), 2), ]
  miraculous_meta_s4= miraculous_meta_s4[seq(1, nrow(miraculous_meta_s4), 2), ]
  #miraculous_meta_s5= miraculous_meta_s5[seq(1, nrow(miraculous_meta_s5), 2), ] - dont!
#------------------------------------------------------------------------
#
filenames= list.files("./data/miraculous/raw_renamed", full.names=FALSE)
#------------------------------------------------------------------------