library(htmltab)
#-----EINTRAGUNGEN-------------------------------------------------------
prefix_series = "peppapig"
anzahlseasons=11
link="https://en.wikipedia.org/wiki/List_of_Peppa_Pig_episodes"
#------------------------------------------------------------------------

#-----Table aus WIKIPEDIA Erstellen--------------------------------------
prefix = paste(prefix_series, "_meta_s", sep='')

for(i in 1:anzahlseasons) { 
  nam <- paste(prefix, i-1, sep = "")
  assign(nam, htmltab(link,1+i))}

#tidy up-----------------------------------------------------------------
#delete ever other row  
#peppapig_meta_s0= peppapig_meta_s0[seq(1, nrow(peppapig_meta_s0), 2), ]
peppapig_meta_s1= peppapig_meta_s1[seq(1, nrow(peppapig_meta_s1), 2), ]
peppapig_meta_s2= peppapig_meta_s2[seq(1, nrow(peppapig_meta_s2), 2), ]
peppapig_meta_s3= peppapig_meta_s3[seq(1, nrow(peppapig_meta_s3), 2), ]
peppapig_meta_s4= peppapig_meta_s4[seq(1, nrow(peppapig_meta_s4), 2), ]
peppapig_meta_s5= peppapig_meta_s5[seq(1, nrow(peppapig_meta_s5), 2), ]
peppapig_meta_s6= peppapig_meta_s6[seq(1, nrow(peppapig_meta_s6), 2), ]
peppapig_meta_s7= peppapig_meta_s7[seq(1, nrow(peppapig_meta_s7), 2), ]
peppapig_meta_s8= peppapig_meta_s8[seq(1, nrow(peppapig_meta_s8), 2), ]
peppapig_meta_s9= peppapig_meta_s9[seq(1, nrow(peppapig_meta_s9), 2), ]
peppapig_meta_s10= peppapig_meta_s10[seq(1, nrow(peppapig_meta_s10), 2), ]


#delete useless tables 
  peppapig_meta_0_overview <-peppapig_meta_s0
  rm("peppapig_meta_s0")

  peppapig_meta_s2_christmas_special <-peppapig_meta_s3
  rm("peppapig_meta_s3")
  peppapig_meta_s3 <-peppapig_meta_s4
  rm("peppapig_meta_s4")
  peppapig_meta_s4 <-peppapig_meta_s5
  rm("peppapig_meta_s5")
  peppapig_meta_0_specials <-peppapig_meta_s6
  rm("peppapig_meta_s6")
  peppapig_meta_s5 <-peppapig_meta_s7
  rm("peppapig_meta_s7")
  peppapig_meta_0_movie <-peppapig_meta_s8
  rm("peppapig_meta_s8")
  peppapig_meta_s6 <-peppapig_meta_s9
  rm("peppapig_meta_s9")
  peppapig_meta_s7 <-peppapig_meta_s10
  rm("peppapig_meta_s10")
#------------------------------------------------------------------------
#
filenames= list.files("./data/peppapig/raw_renamed", full.names=FALSE)
#------------------------------------------------------------------------