# GENDER, ROLES

#prerequisites
# - download script executed
# - cleaning script executed
# - analyzing script executed
# - folder "./data/miraculous/lookup/lookup.csv"

source("./main.R")
character_lookup <- read.csv("./data/miraculous/lookup/lookup.csv", sep="|", header = F)
colnames(character_lookup) <- c("Character", "Gender", "Role", "Role_Category")

#dataframe of all characters occuring in episodeTable$Betweenness 1 to 5
    character_betweenness<-c()
    character_betweenness<-append(episodeTable$Betweenness_1,episodeTable$Betweenness_2)
    character_betweenness<-append(character_betweenness,episodeTable$Betweenness_3)
    character_betweenness<-append(character_betweenness,episodeTable$Betweenness_4)
    character_betweenness<-append(character_betweenness,episodeTable$Betweenness_5)
    character_betweenness<-data.frame(character_betweenness)
    character_betweenness <-character_betweenness %>%
      group_by(character_betweenness) %>%
      summarize(Frequency=n())%>% arrange(desc(Frequency))
    colnames(character_betweenness) <- data.frame("Character", "Frequency")
    write.table(character_betweenness,"./data/miraculous/tables/gender_betweeness.csv", row.names = F, append = F, col.names = T, sep = "|")
    #match mit gender, role
    character_betweenness<-character_betweenness %>%left_join(character_lookup, by='Character')
    
    
#dataframe of all characters occuring in episodeTable$Eigenvector 1 to 5
    character_eigenvector<-c()
    character_eigenvector<-append(episodeTable$Eigenvector_1,episodeTable$Eigenvector_2)
    character_eigenvector<-append(character_eigenvector,episodeTable$Eigenvector_3)
    character_eigenvector<-append(character_eigenvector,episodeTable$Eigenvector_4)
    character_eigenvector<-append(character_eigenvector,episodeTable$Eigenvector_5)
    character_eigenvector<-data.frame(character_eigenvector)
    character_eigenvector <-character_eigenvector %>%
      group_by(character_eigenvector) %>%
      summarize(Frequency=n())%>% arrange(desc(Frequency))
    colnames(character_eigenvector) <- data.frame("Character", "Frequency")
    write.table(character_eigenvector,"./data/miraculous/tables/gender_eigenvector.csv", row.names = F, append = F, col.names = T, sep = "|")
    #match mit gender
    character_eigenvector<-character_eigenvector %>% left_join(character_lookup, by='Character')

#allcharacters over series
  characters_series <- lineTable %>% #all characters aus 
    group_by(Character) %>%
    summarize(Frequency=n())%>% arrange(desc(Frequency))
  characters_series <-characters_series  %>% left_join(character_lookup, by='Character')

#allcharacters per season
  characters_season <- lineTable %>% #all characters aus 
    group_by(Character, Season) %>%
    summarize(Frequency=n())%>% arrange(desc(Frequency))
  characters_season <-characters_season  %>% left_join(character_lookup, by='Character')
  
#show differences between characters_series and lookup
  #genderna<-characters_series %>% filter(is.na(Gender))
  #write characters without gender
    #write.table(genderna,"./data/miraculous/lookup/genderna.csv", row.names = F, append = F, col.names = F, sep = ";")
    #write.table(character_lookup1,"./data/miraculous/lookup/lookup.csv", row.names = F, append = F, col.names = T, sep = "|")
    
#gender frequencys
    characters_series_gender <- characters_series %>% #all characters aus 
    group_by(Gender) %>%
    summarize(Frequency=n())%>% arrange(desc(Frequency))

#role frequencys
  characters_series_role <- characters_series %>% #all characters aus 
  group_by(Role) %>%
  summarize(Frequency=n())%>% arrange(desc(Frequency))

#role_category frequencys
  characters_series_role_category <- characters_series %>% #all characters aus 
    group_by(Role_Category) %>%
    summarize(Frequency=n())%>% arrange(desc(Frequency))
  
#join gender and role with lineTable
  lineTable_gender_role<-lineTable %>%left_join(character_lookup, by='Character')
  
#join gender and role with dialogTable
  colnames(character_lookup) <- c("From", "Gender_From", "Role_From", "Role_Category_From") #muss für join lineTable - FROM #problem - zahl zw. dialogtable und dialogtable_gender_role stimmt nicht überein
  dialogTable_gender_role<-dialogTable %>%left_join(character_lookup, by='From') 
  colnames(character_lookup) <- c("To", "Gender_To", "Role_To", "Role_Category_To") #muss für join lineTable - TO
  dialogTable_gender_role<-dialogTable_gender_role %>%left_join(character_lookup, by='To') 
  colnames(character_lookup) <- c("Character", "Gender", "Role", "Role_Category") # soll für character_lookup
  
  #better with merge?
  #outerJoin: dialogTable_gender_role<-merge(dialogTable, character_lookup, by = "From") #better?
  #LeftOuterJoin: dialogTable_gender_role<-merge(x = dialogTable, y = character_lookup, by = "From", all.x = TRUE) #better?
  #dialogTable_gender_role<-dialogTable
  #dialogTable_gender_role<-merge(x = dialogTable, y = character_lookup_new, by = "From", all = F) #better?
  
  
  #join gender and role with sentimentTable
  sentimentTable_gender_role<-sentimentTable %>%left_join(character_lookup, by='Character')
  
  
print("<End of Script>")
#--------next: calculation------------------------------------------------------------
