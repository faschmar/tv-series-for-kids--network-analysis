library(htmltab)
#-----EINTRAGUNGEN-------------------------------------------------------
prefix_series = "adventuretime"
anzahlseasons=15
link="https://en.wikipedia.org/wiki/List_of_Adventure_Time_episodes"
#------------------------------------------------------------------------

#-----Table aus WIKIPEDIA Erstellen--------------------------------------
prefix = paste(prefix_series, "_meta_s", sep='')

for(i in 1:anzahlseasons) { 
  nam <- paste(prefix, i-1, sep = "")
  assign(nam, htmltab(link,1+i))}

#tidy up-----------------------------------------------------------------
adventuretime_meta_0_pilot <-adventuretime_meta_s0
adventuretime_meta_0_pilot= adventuretime_meta_0_pilot[seq(1, nrow(adventuretime_meta_0_pilot), 2), ]
rm("adventuretime_meta_s0")
adventuretime_meta_0_specials_diamonds_and_lemons <-adventuretime_meta_s11
rm("adventuretime_meta_s11")
adventuretime_meta_0_specials_diamonds_and_lemons= adventuretime_meta_0_specials_diamonds_and_lemons[seq(1, nrow(adventuretime_meta_0_specials_diamonds_and_lemons), 2), ]
adventuretime_meta_0_specials_the_wand <-adventuretime_meta_s12
adventuretime_meta_0_specials_the_wand= adventuretime_meta_0_specials_the_wand[seq(1, nrow(adventuretime_meta_0_specials_the_wand), 2), ]
rm("adventuretime_meta_s12")
adventuretime_meta_0_specials_graybles_allsorts <-adventuretime_meta_s13
adventuretime_meta_0_specials_graybles_allsorts= adventuretime_meta_0_specials_graybles_allsorts[seq(1, nrow(adventuretime_meta_0_specials_graybles_allsorts), 2), ]
rm("adventuretime_meta_s13")
adventuretime_meta_0_specials_frog_seasons <-adventuretime_meta_s14
rm("adventuretime_meta_s14")
#------------------------------------------------------------------------
#
filenames= list.files("./data/adventuretime/raw_renamed", full.names=FALSE)
#------------------------------------------------------------------------