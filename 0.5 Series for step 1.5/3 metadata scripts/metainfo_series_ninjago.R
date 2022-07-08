library(htmltab)
#-----EINTRAGUNGEN-------------------------------------------------------
prefix_series = "ninjago"
anzahlseasons=27
link="https://en.wikipedia.org/wiki/List_of_Ninjago_episodes"
#------------------------------------------------------------------------

#-----Table aus WIKIPEDIA Erstellen--------------------------------------
prefix = paste(prefix_series, "_meta_s", sep='')

for(i in 1:anzahlseasons) { 
  nam <- paste(prefix, i-1, sep = "")
  assign(nam, htmltab(link,1+i))}

#tidy up-----------------------------------------------------------------
ninjago_meta_0_pilot_episodes <-ninjago_meta_s0
rm("ninjago_meta_s0")
ninjago_meta_s1= ninjago_meta_s1[seq(1, nrow(ninjago_meta_s1), 2), ]
ninjago_meta_s2= ninjago_meta_s2[seq(1, nrow(ninjago_meta_s2), 2), ]
#ninjago_meta_s3= ninjago_meta_s3[seq(1, nrow(ninjago_meta_s3), 2), ] -- dont!!
ninjago_meta_s4= ninjago_meta_s4[seq(1, nrow(ninjago_meta_s4), 2), ]
ninjago_meta_s5= ninjago_meta_s5[seq(1, nrow(ninjago_meta_s5), 2), ]
ninjago_meta_s6= ninjago_meta_s6[seq(1, nrow(ninjago_meta_s6), 2), ]
ninjago_meta_s7= ninjago_meta_s7[seq(1, nrow(ninjago_meta_s7), 2), ]
ninjago_meta_0_special <-ninjago_meta_s7
rm("ninjago_meta_s7")
ninjago_meta_s8= ninjago_meta_s8[seq(1, nrow(ninjago_meta_s8), 2), ]
ninjago_meta_s9= ninjago_meta_s9[seq(1, nrow(ninjago_meta_s9), 2), ]
ninjago_meta_s7 <-ninjago_meta_s8
rm("ninjago_meta_s8")
ninjago_meta_s8 <-ninjago_meta_s9
rm("ninjago_meta_s9")
ninjago_meta_s9 <-ninjago_meta_s10
rm("ninjago_meta_s10")
ninjago_meta_s10 <-ninjago_meta_s11
rm("ninjago_meta_s11")
ninjago_meta_s9= ninjago_meta_s9[seq(1, nrow(ninjago_meta_s9), 2), ]
ninjago_meta_s10= ninjago_meta_s10[seq(1, nrow(ninjago_meta_s10), 2), ]
ninjago_meta_s11 <-ninjago_meta_s12
rm("ninjago_meta_s12")
#ninjago_meta_s11= ninjago_meta_s11[seq(1, nrow(ninjago_meta_s11), 2), ] - dont!!
ninjago_meta_s13= ninjago_meta_s13[seq(1, nrow(ninjago_meta_s13), 2), ]
ninjago_meta_s14= ninjago_meta_s14[seq(1, nrow(ninjago_meta_s14), 2), ]
ninjago_meta_s15= ninjago_meta_s15[seq(1, nrow(ninjago_meta_s15), 2), ]
ninjago_meta_s16= ninjago_meta_s16[seq(1, nrow(ninjago_meta_s16), 2), ]
ninjago_meta_s20= ninjago_meta_s20[seq(1, nrow(ninjago_meta_s20), 2), ]
ninjago_meta_s21= ninjago_meta_s21[seq(1, nrow(ninjago_meta_s21), 2), ]
ninjago_meta_s22= ninjago_meta_s22[seq(1, nrow(ninjago_meta_s22), 2), ]
ninjago_meta_s23= ninjago_meta_s23[seq(1, nrow(ninjago_meta_s23), 2), ]
ninjago_meta_s24= ninjago_meta_s24[seq(1, nrow(ninjago_meta_s24), 2), ]
ninjago_meta_s25= ninjago_meta_s25[seq(1, nrow(ninjago_meta_s25), 2), ]
ninjago_meta_s26= ninjago_meta_s26[seq(1, nrow(ninjago_meta_s26), 2), ]
ninjago_meta_s12 <-ninjago_meta_s13
rm("ninjago_meta_s13")
ninjago_meta_s13 <-ninjago_meta_s14
rm("ninjago_meta_s14")
ninjago_meta_0_miniseries <-ninjago_meta_s15
rm("ninjago_meta_s15")
ninjago_meta_s14 <-ninjago_meta_s16
rm("ninjago_meta_s16")
ninjago_meta_0_Webisodes <-ninjago_meta_s17
rm("ninjago_meta_s17")
ninjago_meta_0_chen_mini_movies <-ninjago_meta_s18
rm("ninjago_meta_s18")
ninjago_meta_0_tall_tales <-ninjago_meta_s19
rm("ninjago_meta_s19")
ninjago_meta_0_wus_teas <-ninjago_meta_s20
rm("ninjago_meta_s20")
ninjago_meta_0_Decoded <-ninjago_meta_s21
rm("ninjago_meta_s21")
ninjago_meta_0_tales_monastery_mpinjitzu <-ninjago_meta_s22
rm("ninjago_meta_s22")
ninjago_meta_0_prime_empire_original_shorts <-ninjago_meta_s23
rm("ninjago_meta_s23")
ninjago_meta_0_reimagined <-ninjago_meta_s24
rm("ninjago_meta_s24")
ninjago_meta_0_virtues_of_spinjitzu <-ninjago_meta_s25
rm("ninjago_meta_s25")
ninjago_meta_0_lego_ninjago_movie <-ninjago_meta_s26
rm("ninjago_meta_s26")
#------------------------------------------------------------------------
#
filenames= list.files("./data/ninjago/raw_renamed", full.names=FALSE)
#------------------------------------------------------------------------