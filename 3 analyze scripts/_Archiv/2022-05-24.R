table(episodeTable$Betweenness_3)

table(episodeTable$Eigenvector_3)

cor(episodeTable[,c(2,6,7,8,9,20)])
#add occuring nodes

aggregate(episodeTable$Edge_Density, by=list(episodeTable$Betweeness_1), FUN=mean)


x <-episodeTable %>%
  group_by(Betweenness_1) %>%
  summarize(x = mean(Edge_Density, na.rm = TRUE))
colnames(episodeTable)[20]<-"nNodes"


y <-episodeTable %>%
  group_by(Eigenvector_1) %>%
  summarize(x = mean(Assortativity, na.rm = TRUE))
