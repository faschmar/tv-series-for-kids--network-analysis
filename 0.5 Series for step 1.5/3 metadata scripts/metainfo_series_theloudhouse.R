library(htmltab)
#-----EINTRAGUNGEN-------------------------------------------------------
prefix_series = "theloudhouse"
anzahlseasons=10
link="https://en.wikipedia.org/wiki/List_of_The_Loud_House_episodes"
#------------------------------------------------------------------------

#-----Table aus WIKIPEDIA Erstellen--------------------------------------
prefix = paste(prefix_series, "_meta_s", sep='')

for(i in 1:anzahlseasons) { 
  nam <- paste(prefix, i, sep = "")
  assign(nam, htmltab(link,1+i))}

#tidy up-----------------------------------------------------------------
theloudhouse_meta_s1= theloudhouse_meta_s1[seq(1, nrow(theloudhouse_meta_s1), 2), ]
theloudhouse_meta_s2= theloudhouse_meta_s2[seq(1, nrow(theloudhouse_meta_s2), 2), ]
theloudhouse_meta_s3= theloudhouse_meta_s3[seq(1, nrow(theloudhouse_meta_s3), 2), ]
theloudhouse_meta_s4= theloudhouse_meta_s4[seq(1, nrow(theloudhouse_meta_s4), 2), ]
theloudhouse_meta_s5= theloudhouse_meta_s5[seq(1, nrow(theloudhouse_meta_s5), 2), ]
theloudhouse_meta_s6= theloudhouse_meta_s6[seq(1, nrow(theloudhouse_meta_s6), 2), ]
theloudhouse_meta_s7= theloudhouse_meta_s7[seq(1, nrow(theloudhouse_meta_s7), 2), ]
theloudhouse_meta_s8= theloudhouse_meta_s8[seq(1, nrow(theloudhouse_meta_s8), 2), ]
theloudhouse_meta_s9= theloudhouse_meta_s9[seq(1, nrow(theloudhouse_meta_s9), 2), ]
theloudhouse_meta_s10= theloudhouse_meta_s10[seq(1, nrow(theloudhouse_meta_s10), 2), ]
theloudhouse_meta_0_special <-theloudhouse_meta_s5
rm("theloudhouse_meta_s5")
theloudhouse_meta_s5 <-theloudhouse_meta_s6
rm("theloudhouse_meta_s6")
theloudhouse_meta_0_specials <- theloudhouse_meta_s7
rm("theloudhouse_meta_s7")
theloudhouse_meta_s6 <-theloudhouse_meta_s8
rm("theloudhouse_meta_s8")
theloudhouse_meta_0_movies <-theloudhouse_meta_s9
rm("theloudhouse_meta_s9")
theloudhouse_meta_0_shorts <-theloudhouse_meta_s10
rm("theloudhouse_meta_s10")
#------------------------------------------------------------------------
#
filenames= list.files("./data/theloudhouse/raw_renamed", full.names=FALSE)
#------------------------------------------------------------------------