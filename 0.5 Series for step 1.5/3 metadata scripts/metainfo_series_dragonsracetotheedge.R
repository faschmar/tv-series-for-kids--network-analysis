library(htmltab)
#-----EINTRAGUNGEN-------------------------------------------------------
prefix_series = "dragonsracetotheedge"
anzahlseasons=12
link="https://en.wikipedia.org/wiki/List_of_DreamWorks_Dragons_episodes"
#------------------------------------------------------------------------

#-----Table aus WIKIPEDIA Erstellen--------------------------------------
prefix = paste(prefix_series, "_meta_s", sep='')

for(i in 1:anzahlseasons) { 
  nam <- paste(prefix, i, sep = "")
  assign(nam, htmltab(link,1+i))}

#delete wrongly downloaded tables (other series
  rm("dragonsracetotheedge_meta_s1", "dragonsracetotheedge_meta_s2", "dragonsracetotheedge_meta_s9")

#tidy up
  dragonsracetotheedge_meta_s1_1 <-dragonsracetotheedge_meta_s3
  rm("dragonsracetotheedge_meta_s3")
  dragonsracetotheedge_meta_s2_1 <-dragonsracetotheedge_meta_s4
  rm("dragonsracetotheedge_meta_s4")
  dragonsracetotheedge_meta_s3_1 <-dragonsracetotheedge_meta_s5
  rm("dragonsracetotheedge_meta_s5")
  dragonsracetotheedge_meta_s4_1 <-dragonsracetotheedge_meta_s6
  rm("dragonsracetotheedge_meta_s6")
  dragonsracetotheedge_meta_s5_1 <-dragonsracetotheedge_meta_s7
  rm("dragonsracetotheedge_meta_s7")
  dragonsracetotheedge_meta_s6_1 <-dragonsracetotheedge_meta_s8
  rm("dragonsracetotheedge_meta_s8")

dragonsracetotheedge_meta_s1= dragonsracetotheedge_meta_s1_1[seq(1, nrow(dragonsracetotheedge_meta_s1_1), 2), ]
dragonsracetotheedge_meta_s2= dragonsracetotheedge_meta_s2_1[seq(1, nrow(dragonsracetotheedge_meta_s2_1), 2), ]
dragonsracetotheedge_meta_s3= dragonsracetotheedge_meta_s3_1[seq(1, nrow(dragonsracetotheedge_meta_s3_1), 2), ]
dragonsracetotheedge_meta_s4= dragonsracetotheedge_meta_s4_1[seq(1, nrow(dragonsracetotheedge_meta_s4_1), 2), ]
dragonsracetotheedge_meta_s5= dragonsracetotheedge_meta_s5_1[seq(1, nrow(dragonsracetotheedge_meta_s5_1), 2), ]
dragonsracetotheedge_meta_s6= dragonsracetotheedge_meta_s6_1[seq(1, nrow(dragonsracetotheedge_meta_s6_1), 2), ]

rm(list = ls()[grepl("_1", ls())])
#------------------------------------------------------------------------
#
filenames= list.files("./data/dragonsracetotheedge/raw_renamed", full.names=FALSE)
#------------------------------------------------------------------------
