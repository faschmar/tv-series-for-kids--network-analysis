library(htmltab)
#-----EINTRAGUNGEN-------------------------------------------------------
prefix_series = "mylittlepony"
anzahlseasons=14
link="https://en.wikipedia.org/wiki/List_of_My_Little_Pony:_Friendship_Is_Magic_episodes"
#------------------------------------------------------------------------

#-----Table aus WIKIPEDIA Erstellen--------------------------------------
prefix = paste(prefix_series, "_meta_s", sep='')

for(i in 1:anzahlseasons) { 
  nam <- paste(prefix, i, sep = "")
  assign(nam, htmltab(link,1+i))}

#tidy up-----------------------------------------------------------------
#delete useless tables 
mylittlepony_meta_0_movie <-mylittlepony_meta_s8
rm("mylittlepony_meta_s8")
mylittlepony_meta_s8 <-mylittlepony_meta_s9
rm("mylittlepony_meta_s8")
mylittlepony_meta_s10= mylittlepony_meta_s10[seq(1, nrow(mylittlepony_meta_s10), 2), ]
mylittlepony_meta_s8_holiday_special <-mylittlepony_meta_s10
rm("mylittlepony_meta_s10")
mylittlepony_meta_s9 <-mylittlepony_meta_s11
rm("mylittlepony_meta_s11")
mylittlepony_meta_s12= mylittlepony_meta_s12[seq(1, nrow(mylittlepony_meta_s12), 2), ]
mylittlepony_meta_s9_rainbow_roadtrip <-mylittlepony_meta_s12
rm("mylittlepony_meta_s12")
mylittlepony_meta_s13= mylittlepony_meta_s13[seq(1, nrow(mylittlepony_meta_s13), 2), ]
mylittlepony_meta_0_animated_shorts <-mylittlepony_meta_s13
rm("mylittlepony_meta_s13")
mylittlepony_meta_s14= mylittlepony_meta_s14[seq(1, nrow(mylittlepony_meta_s14), 2), ]
mylittlepony_meta_0_clip_shorts <-mylittlepony_meta_s14
rm("mylittlepony_meta_s14")
#------------------------------------------------------------------------
#
filenames= list.files("./data/mylittlepony/raw_renamed", full.names=FALSE)
#------------------------------------------------------------------------