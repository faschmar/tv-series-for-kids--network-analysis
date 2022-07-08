library(htmltab)
#-----EINTRAGUNGEN-------------------------------------------------------
prefix_series = "amazingworldofgumball"
anzahlseasons=9
link="https://en.wikipedia.org/wiki/List_of_The_Amazing_World_of_Gumball_episodes"
#------------------------------------------------------------------------

#-----Table aus WIKIPEDIA Erstellen--------------------------------------
prefix = paste(prefix_series, "_meta_s", sep='')

for(i in 1:anzahlseasons) { 
  nam <- paste(prefix, i-1, sep = "")
  assign(nam, htmltab(link,1+i))}

#tidy up-----------------------------------------------------------------
amazingworldofgumball_meta_0_pilot <-amazingworldofgumball_meta_s0
rm("amazingworldofgumball_meta_s0")
amazingworldofgumball_meta_0_special_darwins_yearbook<-amazingworldofgumball_meta_s7
rm("amazingworldofgumball_meta_s7")
amazingworldofgumball_meta_0_special_the_gumball_chronicles <-amazingworldofgumball_meta_s8
rm("amazingworldofgumball_meta_s8")

#delete ever other row  
amazingworldofgumball_meta_0_pilot= amazingworldofgumball_meta_0_pilot[seq(1, nrow(amazingworldofgumball_meta_0_pilot), 2), ]
amazingworldofgumball_meta_0_special_the_gumball_chronicles= amazingworldofgumball_meta_0_special_the_gumball_chronicles[seq(1, nrow(amazingworldofgumball_meta_0_special_the_gumball_chronicles), 2), ]

#------------------------------------------------------------------------
#
filenames= list.files("./data/amazingworldofgumball/raw_renamed", full.names=FALSE)
#------------------------------------------------------------------------