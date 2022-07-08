library(htmltab)
#-----EINTRAGUNGEN-------------------------------------------------------
prefix_series = "pawpatrol"
anzahlseasons=16
link="https://en.wikipedia.org/wiki/List_of_PAW_Patrol_episodes"
#------------------------------------------------------------------------

#-----Table aus WIKIPEDIA Erstellen--------------------------------------
prefix = paste(prefix_series, "_meta_s", sep='')

for(i in 1:anzahlseasons) { 
  nam <- paste(prefix, i-1, sep = "")
  assign(nam, htmltab(link,1+i))}

#tidy up-----------------------------------------------------------------
#delete ever other row  
#pawpatrol_meta_s0= pawpatrol_meta_s0[seq(1, nrow(pawpatrol_meta_s0), 2), ]
#pawpatrol_meta_s1= pawpatrol_meta_s1[seq(1, nrow(pawpatrol_meta_s1), 2), ]
#pawpatrol_meta_s2= pawpatrol_meta_s2[seq(1, nrow(pawpatrol_meta_s2), 2), ]
#pawpatrol_meta_s3= pawpatrol_meta_s3[seq(1, nrow(pawpatrol_meta_s3), 2), ]
pawpatrol_meta_s4= pawpatrol_meta_s4[seq(1, nrow(pawpatrol_meta_s4), 2), ]
pawpatrol_meta_s5= pawpatrol_meta_s5[seq(1, nrow(pawpatrol_meta_s5), 2), ]
pawpatrol_meta_s6= pawpatrol_meta_s6[seq(1, nrow(pawpatrol_meta_s6), 2), ]
pawpatrol_meta_s7= pawpatrol_meta_s7[seq(1, nrow(pawpatrol_meta_s7), 2), ]
pawpatrol_meta_s8= pawpatrol_meta_s8[seq(1, nrow(pawpatrol_meta_s8), 2), ]
pawpatrol_meta_s9= pawpatrol_meta_s9[seq(1, nrow(pawpatrol_meta_s9), 2), ]
pawpatrol_meta_s10= pawpatrol_meta_s10[seq(1, nrow(pawpatrol_meta_s10), 2), ]
pawpatrol_meta_s11= pawpatrol_meta_s11[seq(1, nrow(pawpatrol_meta_s11), 2), ]
#pawpatrol_meta_s12= pawpatrol_meta_s12[seq(1, nrow(pawpatrol_meta_s12), 2), ]
pawpatrol_meta_s13= pawpatrol_meta_s13[seq(1, nrow(pawpatrol_meta_s13), 2), ]
pawpatrol_meta_s14= pawpatrol_meta_s14[seq(1, nrow(pawpatrol_meta_s14), 2), ]
#pawpatrol_meta_s15= pawpatrol_meta_s15[seq(1, nrow(pawpatrol_meta_s15), 2), ]


#delete useless tables 
rm("pawpatrol_meta_s0")
rm("pawpatrol_meta_s1")
rm("pawpatrol_meta_s2")
pawpatrol_meta_0_overview <-pawpatrol_meta_s3
rm("pawpatrol_meta_s3")


  pawpatrol_meta_s1 <-pawpatrol_meta_s4
  rm("pawpatrol_meta_s4")
  pawpatrol_meta_s2 <-pawpatrol_meta_s5
  rm("pawpatrol_meta_s5")
  pawpatrol_meta_s3 <-pawpatrol_meta_s6
  rm("pawpatrol_meta_s6")
  pawpatrol_meta_s4 <-pawpatrol_meta_s7
  rm("pawpatrol_meta_s7")
  pawpatrol_meta_s5 <-pawpatrol_meta_s8
  rm("pawpatrol_meta_s8")
  pawpatrol_meta_s6 <-pawpatrol_meta_s9
  rm("pawpatrol_meta_s9")
  pawpatrol_meta_s7 <-pawpatrol_meta_s10
  rm("pawpatrol_meta_s10")
  pawpatrol_meta_s8 <-pawpatrol_meta_s11
  rm("pawpatrol_meta_s11")
  pawpatrol_meta_s9 <-pawpatrol_meta_s12
  rm("pawpatrol_meta_s12")
  pawpatrol_meta_0_specials <-pawpatrol_meta_s13
  rm("pawpatrol_meta_s13")
  pawpatrol_meta_0_original_5s <-pawpatrol_meta_s14
  rm("pawpatrol_meta_s14") 
  pawpatrol_meta_0_movies <-pawpatrol_meta_s15
  rm("pawpatrol_meta_s15") 
#------------------------------------------------------------------------
#
filenames= list.files("./data/pawpatrol/raw_renamed", full.names=FALSE)
#------------------------------------------------------------------------
  library(rlist)
  library(pipeR)

Matching
find
get column and get row
season = table name episode ist column - 1



# declare a DataFrame
data_frame <- pawpatrol_meta_s1

# declaring the vector
vec <- c('Pups Make a Splash')
vec2 <- paste('"',vec)

# getting the subset DataFrame after 
# checking values if belonging to vector
sub_df <- data_frame[data_frame$Title %in% vec,]

print ("Resultant DataFrame")
print (sub_df)