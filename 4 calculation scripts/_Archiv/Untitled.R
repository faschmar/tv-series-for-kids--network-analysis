character_lookup1 <- read.csv("./data/miraculous/lookup/lookupn.csv", sep=";", header = F)
colnames(character_lookup1) <- c("Character", "Gender", "Role", "Role_Category")


character_lookup1 <-character_lookup1 %>%
  group_by(Role_Category) %>%
  summarize(Frequency=n())%>% arrange(desc(Frequency))
view(character_lookup1)

character_lookup<-character_lookup1
character_lookup1 <-character_lookup1[order(character_lookup1$Character),]


missing_entries_episode_table <- lineTable_gender_role[rowSums(is.na(lineTable_gender_role)) > 0,]
view(missing_entries_episode_table)

missing_entries_episode_table <-missing_entries_episode_table  %>%left_join(character_lookup, by='Character')




write.table(character_lookup1,"./data/miraculous/tables/lookup.csv", row.names = F, append = F, col.names = F, sep = "|")
