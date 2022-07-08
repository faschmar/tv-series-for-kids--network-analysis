library(htmltab)
#-----EINTRAGUNGEN-------------------------------------------------------
prefix_series = "stevenuniverse"
anzahlseasons=12
link="https://en.wikipedia.org/wiki/List_of_Steven_Universe_episodes"
#------------------------------------------------------------------------

#-----Table aus WIKIPEDIA Erstellen--------------------------------------
prefix = paste(prefix_series, "_meta_s", sep='')

for(i in 1:anzahlseasons) { 
  nam <- paste(prefix, i-1, sep = "")
  assign(nam, htmltab(link,1+i))}

#tidy up-----------------------------------------------------------------
#delete ever other row  
  stevenuniverse_meta_s0= stevenuniverse_meta_s0[seq(1, nrow(stevenuniverse_meta_s0), 2), ]
  stevenuniverse_meta_s1= stevenuniverse_meta_s1[seq(1, nrow(stevenuniverse_meta_s1), 2), ]
  stevenuniverse_meta_s2= stevenuniverse_meta_s2[seq(1, nrow(stevenuniverse_meta_s2), 2), ]
  stevenuniverse_meta_s3= stevenuniverse_meta_s3[seq(1, nrow(stevenuniverse_meta_s3), 2), ]
  stevenuniverse_meta_s4= stevenuniverse_meta_s4[seq(1, nrow(stevenuniverse_meta_s4), 2), ]
  stevenuniverse_meta_s5= stevenuniverse_meta_s5[seq(1, nrow(stevenuniverse_meta_s5), 2), ]
  stevenuniverse_meta_s6= stevenuniverse_meta_s6[seq(1, nrow(stevenuniverse_meta_s6), 2), ]
  stevenuniverse_meta_s7= stevenuniverse_meta_s7[seq(1, nrow(stevenuniverse_meta_s7), 2), ]
  stevenuniverse_meta_s8= stevenuniverse_meta_s8[seq(1, nrow(stevenuniverse_meta_s8), 2), ]
  stevenuniverse_meta_s9= stevenuniverse_meta_s9[seq(1, nrow(stevenuniverse_meta_s9), 2), ]
  stevenuniverse_meta_s10= stevenuniverse_meta_s10[seq(1, nrow(stevenuniverse_meta_s10), 2), ]
  stevenuniverse_meta_s11= stevenuniverse_meta_s11[seq(1, nrow(stevenuniverse_meta_s11), 2), ]

#tidy up2
  stevenuniverse_meta_0_pilot <-stevenuniverse_meta_s0
  rm("stevenuniverse_meta_s0")
  stevenuniverse_meta_0_movie <-stevenuniverse_meta_s6
  rm("stevenuniverse_meta_s6")
  stevenuniverse_meta_0_StevenUniverseFuture <-stevenuniverse_meta_s7
  rm("stevenuniverse_meta_s7")
  stevenuniverse_meta_s2_shorts <-stevenuniverse_meta_s8
  rm("stevenuniverse_meta_s8")
  stevenuniverse_meta_s4_shorts <-stevenuniverse_meta_s9
  rm("stevenuniverse_meta_s9")
  stevenuniverse_meta_0_DoveSelfEsteemProject_shorts <-stevenuniverse_meta_s10
  rm("stevenuniverse_meta_s10")  
  stevenuniverse_meta_0_TheCrystalGemsSayBeAntiRacist_shorts <-stevenuniverse_meta_s11
  rm("stevenuniverse_meta_s11")  
#------------------------------------------------------------------------
#
filenames= list.files("./data/stevenuniverse/raw_renamed", full.names=FALSE)
#------------------------------------------------------------------------