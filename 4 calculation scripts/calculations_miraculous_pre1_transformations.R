# CALCULATIONs PRE I
#prerequisites
  # - download script executed
  # - cleaning script executed
  # - analyzing script executed
  # - gender_role script executed

#PREREQUISITES for further calculations

source("./main.R")
#-----------------------------------------

#Descriptives
        #dialogTable_gender_role$ID<-1:nrow(dialogTable_gender_role) #ID hinzufügen
        summary(dialogTable_gender_role) #overview
        #Sentiment over seasons
        dialogTable_gender_role %>%
          group_by(Season) %>%
          get_summary_stats(Sentiment, type = "mean_sd")
        #Vader over seasons
        dialogTable_gender_role %>%
          group_by(Season) %>%
          get_summary_stats(Vader, type = "mean_sd")
        
#visuals        
        #visuals of sentiment
        ggdensity(dialogTable_gender_role, x = "Sentiment", 
                  fill = "#0073C2FF", color = "#0073C2FF",
                  add = "mean", rug = TRUE, title = "plot of (all) Sentiment.Ai Scores")
        hist(dialogTable_gender_role$Sentiment, col='steelblue', main='histogram of (all) Sentiment.Ai Scores')
        #visuals of Vader
        ggdensity(dialogTable_gender_role, x = "Vader", 
                  fill = "#0073C2FF", color = "#0073C2FF",
                  add = "mean", rug = TRUE, title = "plot of (all) Vader Scores")
        hist(dialogTable_gender_role$Vader, col='steelblue', main='histogram of (all) Vader Scores')

#-----------------------------------------
#Assumption of normal distribution
  #Sentiment
      #Shapiro-Wilk
        #interpretation: no normal-distribution - Shapiro-Wilk is very sensitive with higher sample sizes - looking at QQ plot instead
        dialogTable_gender_role %>%
          group_by(Gender_From, Gender_To, Role_From, Role_To) %>%
          shapiro_test(Sentiment)
        
      #QQ Plot  
        #interpretation: also with QQ Plots a normal distribution of "Sentiment" cannot be assumed
        ggqqplot(dialogTable_gender_role, "Sentiment", facet.by = "Gender_From", main='untransformed Sentiment-data')
    
      #tranformation of Sentiment data to fit normal distribution
        #step1 - adding constant to avoid negative values (add min of Sentiment to all values)
        min(dialogTable_gender_role$Sentiment)
        dialogTable_gender_role$Sentiment_addedconstant <- dialogTable_gender_role$Sentiment+0.835 #adding smalles value
        min(dialogTable_gender_role$Sentiment_addedconstant)
        #step2 - actual transformations after all values are positiv
        #log
        dialogTable_gender_role$Sentiment_transformed_log <- log10(dialogTable_gender_role$Sentiment_addedconstant)
        which(is.na(dialogTable_gender_role$Sentiment_transformed_log)) # are there NA in log-transformed values?
        summary(dialogTable_gender_role$Sentiment_transformed_log)
        hist(dialogTable_gender_role$Sentiment_transformed_log, col='steelblue', main='log-transformed Sentiment-data')
        ggqqplot(dialogTable_gender_role, "Sentiment_transformed_log", facet.by="Gender_From", main='log-transformed Sentiment-data')
        #sqrt
        dialogTable_gender_role$Sentiment_transformed_sqrt <- sqrt(dialogTable_gender_role$Sentiment_addedconstant)
        which(is.na(dialogTable_gender_role$Sentiment_transformed_sqrt)) # are there NA in log-transformed values?
        summary(dialogTable_gender_role$Sentiment_transformed_sqrt)
        hist(dialogTable_gender_role$Sentiment_transformed_sqrt, col='steelblue', main='sqrt-transformed Sentiment-data')
        ggqqplot(dialogTable_gender_role, "Sentiment_transformed_sqrt", facet.by="Gender_From", main='sqrt-transformed Sentiment-data')
        #cuberoot
        dialogTable_gender_role$Sentiment_transformed_cuberoot <- dialogTable_gender_role$Sentiment_addedconstant^(1/3)
        which(is.na(dialogTable_gender_role$Sentiment_transformed_cuberoot)) # are there NA in log-transformed values?
        summary(dialogTable_gender_role$Sentiment_transformed_cuberoot)
        hist(dialogTable_gender_role$Sentiment_transformed_cuberoot, col='steelblue', main='cuberoot-transformed Sentiment-data')
        ggqqplot(dialogTable_gender_role, "Sentiment_transformed_cuberoot", facet.by="Gender_From", main='cuberoot-transformed Sentiment-data')
        #z-transformation
        dialogTable_gender_role$Sentiment_transformed_z<-scale(dialogTable_gender_role$Sentiment)
        which(is.na(dialogTable_gender_role$Sentiment_transformed_z)) # are there NA in log-transformed values?
        summary(dialogTable_gender_role$Sentiment_transformed_z)
        hist(dialogTable_gender_role$Sentiment_transformed_z, col='steelblue', main='z-transformed Sentiment-data')
        ggqqplot(dialogTable_gender_role, "Sentiment_transformed_z", facet.by="Gender_From", main='cuberoot-transformed Sentiment-data')
        #box-cox (ad,cvm,pt, lt,jb not normally distributed; sf,sw, ac, mle not possible due to sample size )
        Sentiment_transformed_boxcox=boxcoxnc(dialogTable_gender_role$Sentiment, method = "ad", lambda = seq(-3,3,0.01), lambda2 = 0.835, plot = TRUE, alpha = 0.05, verbose = TRUE)
        Sentiment_transformed_boxcox$lambda.hat
        Sentiment_transformed_boxcox$p.value
        Sentiment_transformed_boxcox$tf.data 
        confInt(Sentiment_transformed_boxcox)
        #box-cox transformation returns non normally distributed values
        
        dialogTable_gender_role %>%
          group_by(Gender_From, Gender_To) %>%
          shapiro_test(Sentiment_transformed_z)
        
  #Vader
      #Shapiro-Wilk
        #interpretation: no normal-distribution - Shapiro-Wilk is very sensitive with higher sample sizes - looking at QQ plot instead
        dialogTable_gender_role %>%
          group_by(Gender_From, Gender_To) %>%
          shapiro_test(Vader)
        
      #QQ Plot  
        #interpretation: also with QQ Plots a normal distribution of "Vader" can hardly be assumed
        ggqqplot(dialogTable_gender_role, "Vader", facet.by = "Gender_From", main='untransformed Vader-data') 
        
      #tranformation of Vader data to fit normal distribution
        #step1 - adding constant to avoid negative values (add min of Sentiment to all values)
        min(dialogTable_gender_role$Vader)
        dialogTable_gender_role$Vader_addedconstant <- dialogTable_gender_role$Vader+0.962 #adding smalles value
        min(dialogTable_gender_role$Vader_addedconstant)
        #step2 - actual transformations after all values are positiv
        #log
        dialogTable_gender_role$Vader_transformed_log <- log10(dialogTable_gender_role$Vader_addedconstant)
        which(is.na(dialogTable_gender_role$Vader_transformed_log)) # are there NA in log-transformed values?
        summary(dialogTable_gender_role$Vader_transformed_log)
        hist(dialogTable_gender_role$Vader_transformed_log, col='steelblue', main='log-transformed Vader-data')
        ggqqplot(dialogTable_gender_role, "Vader_transformed_log", facet.by="Gender_From", main='log-transformed Vader-data')
        #sqrt
        dialogTable_gender_role$Vader_transformed_sqrt <- sqrt(dialogTable_gender_role$Vader_addedconstant)
        which(is.na(dialogTable_gender_role$Vader_transformed_sqrt)) # are there NA in log-transformed values?
        summary(dialogTable_gender_role$Vader_transformed_sqrt)
        hist(dialogTable_gender_role$Vader_transformed_sqrt, col='steelblue', main='sqrt-transformed Vader-data')
        ggqqplot(dialogTable_gender_role, "Vader_transformed_sqrt", facet.by="Gender_From", main='sqrt-transformed Vader-data') 
        #cuberoot
        dialogTable_gender_role$Vader_transformed_cuberoot <- dialogTable_gender_role$Vader_addedconstant^(1/3)
        which(is.na(dialogTable_gender_role$Vader_transformed_cuberoot)) # are there NA in log-transformed values?
        summary(dialogTable_gender_role$Vader_transformed_cuberoot)
        hist(dialogTable_gender_role$Vader_transformed_cuberoot, col='steelblue', main='cuberoot-transformed Vader-data')
        ggqqplot(dialogTable_gender_role, "Vader_transformed_cuberoot", facet.by="Gender_From",main='cuberoot-transformed Vader-data') 
        #z-transformation
        dialogTable_gender_role$Vader_transformed_z<-scale(dialogTable_gender_role$Vader)
        which(is.na(dialogTable_gender_role$Vader_transformed_z)) # are there NA in log-transformed values?
        summary(dialogTable_gender_role$Vader_transformed_z)
        hist(dialogTable_gender_role$Vader_transformed_z, col='steelblue', main='z-transformed Vader-data')
        ggqqplot(dialogTable_gender_role, "Vader_transformed_z", facet.by="Gender_From",main='cuberoot-transformed Vader-data') 
        #box-cox - boxcoxnc {AID}
        #(ad,cvm,pt, lt,jb not normally distributed; sf,sw, ac, mle not possible due to sample size )
        Vader_transformed_boxcox=boxcoxnc(dialogTable_gender_role$Vader, method = "ac", lambda = seq(-3,3,0.01), lambda2 = 0.962, plot = TRUE, alpha = 0.05, verbose = TRUE)
        Vader_transformed_boxcox$lambda.hat
        Vader_transformed_boxcox$p.value
        Vader_transformed_boxcox$tf.data 
        confInt(Vader_transformed_boxcox)
        #box-cox transformation returns non normally distributed values
        
        
        dialogTable_gender_role %>%
          group_by(Gender_From, Gender_To) %>%
          shapiro_test(Vader_transformed_log)
        
        dialogTable_gender_role %>%
          group_by(Gender_From, Gender_To) %>%
          shapiro_test(Vader_transformed_sqrt)
        
        dialogTable_gender_role %>%
          group_by(Gender_From, Gender_To) %>%
          shapiro_test(Vader_transformed_cuberoot)

        #intepretation - visually non of the transformed values is normal distributed, untransformed data might fits best

#--------------------------------------------------   
        
#Assumption of sphericity: Mauchly’s test - for repeated ANOVAS
    #needed to run ANOVAS with repeated measures; should also be computed when running the repeated measures ANOVA
    #Sentiment & Gender
        res.aov <- anova_test(data = dialogTable_gender_role, dv = Sentiment, wid = ID, between = Gender_From)
        get_anova_table(res.aov)
        res.aov <- anova_test(data = dialogTable_gender_role, dv = Sentiment, wid = ID, between = Gender_To)
        get_anova_table(res.aov)
        #sphericity violated
    #Vader & Gender
        res.aov <- anova_test(data = dialogTable_gender_role, dv = Vader, wid = ID, between = Gender_From)
        get_anova_table(res.aov)
        res.aov <- anova_test(data = dialogTable_gender_role, dv = Vader, wid = ID, between = Gender_To)
        get_anova_table(res.aov)
        #sphericity violated

#--------------------------------------------------          
        
#rmA: Transform sentimentTable to character_sentiment_time/character_vader_time
        if (exists("character_sentiment_time")) {
          rm(character_sentiment_time)
          print("file deleted: character_sentiment_time will be created from scratch")
        } else{print("nothing to delete: character_sentiment_time will be created from scratch")}
        if (exists("character_vader_time")) {
          rm(character_vader_time)
          print("file deleted: character_vader_time will be created from scratch")
        } else{print("nothing to delete: character_vader_time will be created from scratch")}
        
        ep_no_list<-unique(sentimentTable$Episode_Overall) #alle vorhandenen ep nummern holen
        #colnames(characters_ep)<-c("Character", paste("Sentiment_Ep_",i), paste("Vader_Ep_",i))
        
        character_sentiment_time<-characters[1]
        character_sentiment_time<-data.frame(character_sentiment_time)
        colnames(character_sentiment_time)<-c("Character")
        
        character_vader_time<-characters[1]
        character_vader_time<-data.frame(character_vader_time)
        colnames(character_vader_time)<-c("Character")
        
        for(i in ep_no_list) {  
          #sentiment
          characters_ep_sentiment<-subset(sentimentTable, Episode_Overall == i, select = c(Character, Sentiment))
          season<-subset(sentimentTable, Episode_Overall == i, select = c(Season))
          season<-unique(season)
          colnames(characters_ep_sentiment)<-c("Character", paste("Sentiment_s",season,"_ep_",i, sep = ""))
          character_sentiment_time <- merge(character_sentiment_time, characters_ep_sentiment,by = 'Character', all=TRUE)
          #vader
          characters_ep_vader<-subset(sentimentTable, Episode_Overall == i, select = c(Character, Vader))
          colnames(characters_ep_vader)<-c("Character", paste("Vader_s",season,"_ep_",i, sep = ""))
          character_vader_time <- merge(character_vader_time, characters_ep_vader,by = 'Character', all=TRUE)
        }
        
    #join gender and role 
        character_sentiment_time<-character_sentiment_time %>%left_join(character_lookup, by='Character')
        character_sentiment_time <-character_sentiment_time[order(character_sentiment_time$Character),]
        
        character_vader_time<-character_vader_time %>%left_join(character_lookup, by='Character')
        character_vader_time <-character_vader_time[order(character_vader_time$Character),]
        
        #view(character_sentiment_time)
        #length(character_sentiment_time)
        #view(character_vader_time)
        #length(character_vader_time)
        
    #find duplicates
        #double_sentiment_time<-subset(character_sentiment_time, duplicated(Character))
        write.table(character_sentiment_time,"./data/miraculous/tables/character_sentiment_time.csv", row.names = F, append = F, col.names = T, sep = "|")
        write.table(character_vader_time,"./data/miraculous/tables/character_vader_time.csv", row.names = F, append = F, col.names = T, sep = "|")
    
    #umbenennen
        character_episodes_sentiment_time <- character_sentiment_time
        character_episodes_vader_time <- character_vader_time 
    #erstellen von sentiment pro character pro season
    #sentiment    
        character_seasons_sentiment_time <- character_episodes_sentiment_time$Character
        character_seasons_sentiment_time <- data.frame(character_seasons_sentiment_time)
        colnames(character_seasons_sentiment_time)<-c("Character")
        #
        s1_time<- character_episodes_sentiment_time %>% dplyr:: select(contains("s1"))
        s1_mean_time<-rowMeans(s1_time, na.rm = T)
        s1_mean_time<-data.frame(s1_mean_time)
        character_seasons_sentiment_time<-cbind(character_seasons_sentiment_time, s1_mean_time)
        #
        s2_time<- character_episodes_sentiment_time %>% dplyr:: select(contains("s2"))
        s2_mean_time<-rowMeans(s2_time, na.rm = T)
        s2_mean_time<-data.frame(s2_mean_time)
        character_seasons_sentiment_time<-cbind(character_seasons_sentiment_time, s2_mean_time)
        #
        s3_time<- character_episodes_sentiment_time %>% dplyr:: select(contains("s3"))
        s3_mean_time<-rowMeans(s3_time, na.rm = T)
        s3_mean_time<-data.frame(s3_mean_time)
        character_seasons_sentiment_time<-cbind(character_seasons_sentiment_time, s3_mean_time)
        #
        s4_time<- character_episodes_sentiment_time %>% dplyr:: select(contains("s4"))
        s4_mean_time<-rowMeans(s4_time, na.rm = T)
        s4_mean_time<-data.frame(s4_mean_time)
        character_seasons_sentiment_time<-cbind(character_seasons_sentiment_time, s4_mean_time)
      #vader  
        character_seasons_vader_time <- character_episodes_vader_time$Character
        character_seasons_vader_time <- data.frame(character_seasons_vader_time)
        colnames(character_seasons_vader_time)<-c("Character")
        #
        s1_time<- character_episodes_vader_time %>% dplyr:: select(contains("s1"))
        s1_mean_time<-rowMeans(s1_time, na.rm = T)
        s1_mean_time<-data.frame(s1_mean_time)
        character_seasons_vader_time<-cbind(character_seasons_vader_time, s1_mean_time)
        #
        s2_time<- character_episodes_vader_time %>% dplyr:: select(contains("s2"))
        s2_mean_time<-rowMeans(s2_time, na.rm = T)
        s2_mean_time<-data.frame(s2_mean_time)
        character_seasons_vader_time<-cbind(character_seasons_vader_time, s2_mean_time)
        #
        s3_time<- character_episodes_vader_time %>% dplyr:: select(contains("s3"))
        s3_mean_time<-rowMeans(s3_time, na.rm = T)
        s3_mean_time<-data.frame(s3_mean_time)
        character_seasons_vader_time<-cbind(character_seasons_vader_time, s3_mean_time)
        #
        s4_time<- character_episodes_vader_time %>% dplyr:: select(contains("s4"))
        s4_mean_time<-rowMeans(s4_time, na.rm = T)
        s4_mean_time<-data.frame(s4_mean_time)
        character_seasons_vader_time<-cbind(character_seasons_vader_time, s4_mean_time)
        colnames(character_seasons_sentiment_time)<-c("Character", "s1_mean_sentiment", "s2_mean_sentiment", "s3_mean_sentiment", "s4_mean_sentiment")
        colnames(character_seasons_vader_time)<-c("Character", "s1_mean_vader", "s2_mean_vader", "s3_mean_vader", "s4_mean_vader")
        rm(s1_time,s2_time,s3_time,s4_time,s1_mean_time,s2_mean_time,s3_mean_time,s4_mean_time)
       
        #gender_role matching
        character_seasons_sentiment_time<-character_seasons_sentiment_time %>%left_join(character_lookup, by='Character')
        #character_seasons_sentiment_time <-character_seasons_sentiment_time[order(character_seasons_sentiment_time$Role),]

        character_seasons_vader_time<-character_seasons_vader_time %>%left_join(character_lookup, by='Character')
        #character_seasons_vader_time <-character_seasons_vader_time[order(character_seasons_vader_time$Role),]
        
        #NAN durch NA ersetzen
        character_seasons_sentiment_time$s1_mean_sentiment[is.nan(character_seasons_sentiment_time$s1_mean_sentiment)]<-NA
        character_seasons_sentiment_time$s2_mean_sentiment[is.nan(character_seasons_sentiment_time$s2_mean_sentiment)]<-NA
        character_seasons_sentiment_time$s3_mean_sentiment[is.nan(character_seasons_sentiment_time$s3_mean_sentiment)]<-NA
        character_seasons_sentiment_time$s4_mean_sentiment[is.nan(character_seasons_sentiment_time$s4_mean_sentiment)]<-NA
        
        character_seasons_vader_time$s1_mean_vader[is.nan(character_seasons_vader_time$s1_mean_vader)]<-NA
        character_seasons_vader_time$s2_mean_vader[is.nan(character_seasons_vader_time$s2_mean_vader)]<-NA
        character_seasons_vader_time$s3_mean_vader[is.nan(character_seasons_vader_time$s3_mean_vader)]<-NA
        character_seasons_vader_time$s4_mean_vader[is.nan(character_seasons_vader_time$s4_mean_vader)]<-NA
        
        #view(character_seasons_sentiment_time)  
        #view(character_seasons_vader_time)  
        
        #######
    #---aufräumen
        rm(ep_no_list, i, characters_ep_sentiment, characters_ep_vader, season, character_vader_time,character_sentiment_time )


#----END OF PRE (NEXT: ANOVAS)------------------------
