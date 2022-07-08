# CALCULATIONs VIII - 3a - Characters
#prerequisites
# - download script executed
# - cleaning script executed
# - analyzing script executed
# - gender_role script executed
# - calculations_miraculous_pre executed
# - calculations_miraculous_wordcount

# Descriptives Martin
source("./main.R")
#-----------------------------------------


#Characteristics: Gender
      view(characters_series_gender) #anzahl pro gender
      desc_gender_by_series<-characters_season_mf %>% 
        group_by(Gender) %>%
        summarize(Frequency=n())%>% arrange(desc(Frequency))
      desc_gender_by_series <-desc_gender_by_series[order(desc_gender_by_series$Gender),]
      view(desc_gender_by_series)
    #crosstab
      desc_gender_by_series<-xtabs(~characters_series_mf$Gender)
      desc_gender_by_series
      summary(desc_gender_by_series)
    
      chi2_observed_gender<-table(characters_season_mf$Gender)
      chi2_expected_gender<-c(1/2, 1/2)
      chisq.test(x = chi2_observed_gender, p = chi2_expected_gender, simulate.p.value = TRUE)
      #difference between frequ between genders
    
    #Anzahl an Frequenzen nach Geschlecht pro Season (plot)
      desc_gender_by_season<-characters_season_mf %>% 
        group_by(Season, Gender) %>%
        summarize(Frequency=n())%>% arrange(desc(Frequency))
      desc_gender_by_season <-desc_gender_by_season[order(desc_gender_by_season$Season, desc_gender_by_season$Gender),]
      view(desc_gender_by_season)
      
      interaction.plot(x.factor = desc_gender_by_season$Season, trace.factor = desc_gender_by_season$Gender, 
                       response = desc_gender_by_season$Frequency,
                       trace.label = "Gender",
                       type = "b", legend = TRUE, 
                       xlab = "Season", ylab="Frequency", main="Frequency of gender & season",
                       pch=c(1,19), ylim = range(1:120),col = c("#00AFBB", "#E7B800"))
      #crosstab (new)
        desc_gender_by_season<-xtabs(~characters_season_mf$Season + characters_season_mf$Gender)
        desc_gender_by_season
        summary(desc_gender_by_season)
    
    #In allen Season vorkommenden Charactern #45 Charaktere (inkl. genderless)
      desc_reoccurring_gender_by_series<-character_seasons_sentiment_time_complete %>% 
        group_by(Gender, Role) %>%
        summarize(Frequency=n())%>% arrange(desc(Frequency))
      desc_reoccurring_gender_by_series <-desc_reoccurring_gender_by_series[order(desc_reoccurring_gender_by_series$Gender),]
      view(desc_reoccurring_gender_by_series)
      
      view(character_seasons_sentiment_time_complete)
    
#Characteristics: Role
    View(characters_series_role)

#Characteristics: Role_Category
    View(characters_series_role_category)
    
    desc_gender_by_rolecategory<-characters_series_mf %>% 
      group_by(Gender, Role_Category) %>%
      summarize(Frequency=n())%>% arrange(desc(Frequency))
    desc_gender_by_rolecategory<-desc_gender_by_rolecategory[order(desc_gender_by_rolecategory$Role_Category, desc_gender_by_rolecategory$Gender),]
    view(desc_gender_by_rolecategory)
    
    #search for specific group
    subset<-subset(lineTable_gender_mf, Gender == "female" & Role == "main" & Role_Category=="no_villain")
    view(subset)
    rm(subset)

  #todo:   
    # Create the barplot - todo
    ggplot(data=df_cumsum, aes(x=dose, y=len, fill=supp)) +
      geom_bar(stat="identity")+
      geom_text(aes(y=label_ypos, label=len), vjust=1.6, 
                color="white", size=3.5)+
      scale_fill_brewer(palette="Paired")+
      theme_minimal()
    
    
#-------Descriptives Martin-END----------------------------------
