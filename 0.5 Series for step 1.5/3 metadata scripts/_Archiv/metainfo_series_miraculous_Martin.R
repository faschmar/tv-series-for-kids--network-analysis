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
  #miraculous_meta_s0= miraculous_meta_s0[seq(1, nrow(miraculous_meta_s0), 2), ]
  miraculous_meta_s1= miraculous_meta_s1[seq(1, nrow(miraculous_meta_s1), 2), ]
  miraculous_meta_s2= miraculous_meta_s2[seq(1, nrow(miraculous_meta_s2), 2), ]
  miraculous_meta_s3= miraculous_meta_s3[seq(1, nrow(miraculous_meta_s3), 2), ]
  miraculous_meta_s4= miraculous_meta_s4[seq(1, nrow(miraculous_meta_s4), 2), ]
  #miraculous_meta_s5= miraculous_meta_s5[seq(1, nrow(miraculous_meta_s5), 2), ] - dont!
  
  miraculous_meta_s1[4] <- NULL   
  miraculous_meta_s1[4] <- NULL   
  miraculous_meta_s1[5] <- NULL   
  miraculous_meta_s1[5] <- NULL
  miraculous_meta_s1[5] <- NULL  
  miraculous_meta_s1[5] <- NULL  
  
  miraculous_meta_s2[4] <- NULL   
  miraculous_meta_s2[4] <- NULL   
  miraculous_meta_s2[5] <- NULL   
  miraculous_meta_s2[5] <- NULL
  miraculous_meta_s2[5] <- NULL  
  miraculous_meta_s2[5] <- NULL  
  
  miraculous_meta_s3[4] <- NULL   
  miraculous_meta_s3[4] <- NULL   
  miraculous_meta_s3[5] <- NULL   
  miraculous_meta_s3[5] <- NULL
  miraculous_meta_s3[5] <- NULL  
  miraculous_meta_s3[5] <- NULL  
  
  miraculous_meta_s4[4] <- NULL   
  miraculous_meta_s4[4] <- NULL   
  miraculous_meta_s4[5] <- NULL   
  miraculous_meta_s4[5] <- NULL
  miraculous_meta_s4[5] <- NULL  
  miraculous_meta_s4[5] <- NULL  
  
  
  miraculous_meta_s5[4] <- NULL   
  miraculous_meta_s5[4] <- NULL   
  miraculous_meta_s5[5] <- NULL   
  miraculous_meta_s5[5] <- NULL
  miraculous_meta_s5[5] <- NULL  
  miraculous_meta_s5[5] <- NULL  
  
  
  miraculous_meta_s1<- mutate(miraculous_meta_s1, season = "1")
  miraculous_meta_s2<- mutate(miraculous_meta_s2, season = "2")
  miraculous_meta_s3<- mutate(miraculous_meta_s3, season = "3")
  miraculous_meta_s4<- mutate(miraculous_meta_s4, season = "4")
  miraculous_meta_s5<- mutate(miraculous_meta_s5, season = "5")
  
  colnames(miraculous_meta_s1) <- c("no. overall","no. season","title","original air date","season")
  colnames(miraculous_meta_s2) <- c("no. overall","no. season","title","original air date","season")
  colnames(miraculous_meta_s3) <- c("no. overall","no. season","title","original air date","season")
  colnames(miraculous_meta_s4) <- c("no. overall","no. season","title","original air date","season")
  colnames(miraculous_meta_s5) <- c("no. overall","no. season","title","original air date","season")
  
  season_ep_list <- rbind(miraculous_meta_s1, miraculous_meta_s2, miraculous_meta_s3, miraculous_meta_s4, miraculous_meta_s5)
  rm(miraculous_meta_s1, miraculous_meta_s2, miraculous_meta_s3, miraculous_meta_s4, miraculous_meta_s5)
  
#------------------------------------------------------------------------
#
filenames_list_txt= list.files("./data/miraculous/processed", full.names=FALSE)
filenames_list <-c()
for (filename in filenames_list_txt){
  filename <- gsub(".txt", "", filename)
  filenames_list <- append(filenames_list, filename)
}
rm(filename, i, link, nam, prefix, prefix_series, anzahlseasons)
#data_subset <- season_ep_list["Lady Wifi", ] #suchen?
#------------------------------------------------------------------------