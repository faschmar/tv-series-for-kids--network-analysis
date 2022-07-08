library(htmltab)
#-----EINTRAGUNGEN-------------------------------------------------------
prefix_series = "gravityfalls"
anzahlseasons=19
link="https://en.wikipedia.org/wiki/List_of_Gravity_Falls_episodes"
#------------------------------------------------------------------------

#-----Table aus WIKIPEDIA Erstellen--------------------------------------
prefix = paste(prefix_series, "_meta_s", sep='')

for(i in 1:anzahlseasons) { 
  nam <- paste(prefix, i-1, sep = "")
  assign(nam, htmltab(link,1+i))}

#tidy up-----------------------------------------------------------------
gravityfalls_meta_0_pilot <-gravityfalls_meta_s0
rm("gravityfalls_meta_s0")
gravityfalls_meta_0_shorts <-gravityfalls_meta_s3
rm("gravityfalls_meta_s3")
rm("gravityfalls_meta_s4")
rm("gravityfalls_meta_s5")
rm("gravityfalls_meta_s6")
rm("gravityfalls_meta_s7")
rm("gravityfalls_meta_s8")
rm("gravityfalls_meta_s9")
rm("gravityfalls_meta_s10")
rm("gravityfalls_meta_s11")
rm("gravityfalls_meta_s12")
rm("gravityfalls_meta_s13")
rm("gravityfalls_meta_s14")
rm("gravityfalls_meta_s15")
rm("gravityfalls_meta_s16")
rm("gravityfalls_meta_s17")
gravityfalls_meta_0_bts_special <-gravityfalls_meta_s18
rm("gravityfalls_meta_s18")
gravityfalls_meta_0_bts_special= gravityfalls_meta_0_bts_special[seq(1, nrow(gravityfalls_meta_0_bts_special), 2), ]

#------------------------------------------------------------------------
#
filenames= list.files("./data/gravityfalls/raw_renamed", full.names=FALSE)
#------------------------------------------------------------------------