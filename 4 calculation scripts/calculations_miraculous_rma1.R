# CALCULATIONs 5

#prerequisites
# - download script executed
# - cleaning script executed
# - analyzing script executed
# - gender_role script executed
# - calculations_miraculous_pre executed

source("./main.R")

#Sentiment (untransformed)
    attach(character_seasons_sentiment_time)
    par(mfrow=c(1,4))
    hist(s1_mean_sentiment,prob=T)
    hist(s2_mean_sentiment, prob=T)
    hist(s3_mean_sentiment,prob=T)
    hist(s4_mean_sentiment,prob=T)

  #try1 - complete cases nach transponieren (690 characters)
    #character_seasons_sentiment_time_B<-matrix(nrow=2760,ncol=4,c(rep(1:690,4), rep(character_seasons_sentiment_time$Character,4),character_seasons_sentiment_time$s1_mean_sentiment, character_seasons_sentiment_time$s2_mean_sentiment,character_seasons_sentiment_time$s3_mean_sentiment,character_seasons_sentiment_time$s4_mean_sentiment,rep(1,690),rep(2,690),rep(3,690),rep(4,690)))
    #character_seasons_sentiment_time_B<-data.frame(character_seasons_sentiment_time_B)
    #colnames(character_seasons_sentiment_time_B)<-c("ID", "Character", "Mean_Sentiment", "Season")
    #character_seasons_sentiment_time_B$Season<-factor(character_seasons_sentiment_time_B$Season)
    #character_seasons_sentiment_time_B$Mean_Sentiment <- as.numeric(character_seasons_sentiment_time_B$Mean_Sentiment)
    #attach(character_seasons_sentiment_time_B)
    #view(character_seasons_sentiment_time)
  #problem: NAs
  #reduce to complete cases only
    #character_seasons_sentiment_time_B<-character_seasons_sentiment_time_B[complete.cases(character_seasons_sentiment_time_B), ]
  #problem - still missing data when running rmANOVA - todo
    #run rmANOVA (rma1)
    #rma1<-ezANOVA(data=character_seasons_sentiment_time_B,dv=.(Mean_Sentiment),wid=.(ID),within=.(Season),type=3)
    
    
  #try2 - complete cases vor transponieren (45 characters)
    #reduce to complete cases only
    character_seasons_sentiment_time_complete<-character_seasons_sentiment_time[complete.cases(character_seasons_sentiment_time), ]
    #transponieren
    character_seasons_sentiment_time_complete_B<-matrix(nrow=180,ncol=4,c(rep(1:45,4), rep(character_seasons_sentiment_time_complete$Character,4),character_seasons_sentiment_time_complete$s1_mean_sentiment, character_seasons_sentiment_time_complete$s2_mean_sentiment,character_seasons_sentiment_time_complete$s3_mean_sentiment,character_seasons_sentiment_time_complete$s4_mean_sentiment,rep(1,45),rep(2,45),rep(3,45),rep(4,45)))
    character_seasons_sentiment_time_complete_B<-data.frame(character_seasons_sentiment_time_complete_B)
    colnames(character_seasons_sentiment_time_complete_B)<-c("ID", "Character", "Mean_Sentiment", "Season")
    character_seasons_sentiment_time_complete_B$Season<-factor(character_seasons_sentiment_time_complete_B$Season)
    character_seasons_sentiment_time_complete_B$Mean_Sentiment <- as.numeric(character_seasons_sentiment_time_complete_B$Mean_Sentiment)
    attach(character_seasons_sentiment_time_complete_B)
    view(character_seasons_sentiment_time_complete_B)
    #run rmANOVA (rma1)
    rma1<-ezANOVA(data=character_seasons_sentiment_time_complete_B,dv=.(Mean_Sentiment),wid=.(Character),within=.(Season),type=3)
    rma1 
    #Spericity is sign - so assumption has not been met! -> Sphericity Correction
    #GG Episilon is higher than 0.75 -> use of Huynh-Feldt is recommended; 
    #Huynh-Feldt Epsilon is => 0.093 - Season has no influence on sentiment (untransformed)

# #Vader (untransformed)
#     attach(character_seasons_vader_time)
#     par(mfrow=c(1,4))
#     hist(s1_mean_vader,prob=T)
#     hist(s2_mean_vader, prob=T)
#     hist(s3_mean_vader,prob=T)
#     hist(s4_mean_vader,prob=T)
#     
#     
#     #try2 - complete cases vor transponieren (45 characters)
#     #reduce to complete cases only
#     character_seasons_vader_time_complete<-character_seasons_vader_time[complete.cases(character_seasons_vader_time), ]
#     #transponieren
#     character_seasons_vader_time_complete_B<-matrix(nrow=180,ncol=4,c(rep(1:45,4), rep(character_seasons_vader_time_complete$Character,4),character_seasons_vader_time_complete$s1_mean_vader, character_seasons_vader_time_complete$s2_mean_vader,character_seasons_vader_time_complete$s3_mean_vader,character_seasons_vader_time_complete$s4_mean_vader,rep(1,45),rep(2,45),rep(3,45),rep(4,45)))
#     character_seasons_vader_time_complete_B<-data.frame(character_seasons_vader_time_complete_B)
#     colnames(character_seasons_vader_time_complete_B)<-c("ID", "Character", "Mean_Vader", "Season")
#     character_seasons_vader_time_complete_B$Season<-factor(character_seasons_vader_time_complete_B$Season)
#     character_seasons_vader_time_complete_B$Mean_Vader <- as.numeric(character_seasons_vader_time_complete_B$Mean_Vader)
#     attach(character_seasons_vader_time_complete_B)
#     view(character_seasons_vader_time_complete_B)
#     #run rmANOVA (rma1)
#     rma1v<-ezANOVA(data=character_seasons_vader_time_complete_B,dv=.(Mean_Vader),wid=.(Character),within=.(Season),type=3)
#     rma1v
#     #Spericity is not sign - so assumption has  been met! -> No Sphericity Correction
#     #ANOVA Effect interpreted => 0.272 - Season has no influence on vaders (untransformed)   

    