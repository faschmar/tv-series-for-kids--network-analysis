# CALCULATIONs VII

#prerequisites
# - download script executed
# - cleaning script executed
# - analyzing script executed
# - gender_role script executed
# - calculations_miraculous_pre executed

source("./main.R")
#-------------------------------------------
# values: dialogTable_gender_role
#   Sentiment
#   Sentiment_transformed_log
#   Sentiment_transformed_sqrt
#   Sentiment_transformed_cuberoot
#   Sentiment_transformed_z

#   Vader
#   Vader_transformed_log
#   Vader_transformed_sqrt
#   Vader_transformed_cuberoot
#   Vader_transformed_z
#-------------------------------------------

#Kruskal-Wallis-Test3 (kwt3)
#Sentiment 
        #AV: Sentiment.ai-Scores (untransformed)
        #UV: Role_Cat_From

  describeBy(dialogTable_gender_role$Sentiment,dialogTable_gender_role$Role_Category_From)
  kruskal.test(dialogTable_gender_role$Sentiment,dialogTable_gender_role$Role_Category_From)
  #Kein Unterschied in Sentiment zw. Role_Category_From Gruppen
  
  #Sentiment 
  #AV: Sentiment.ai-Scores (untransformed)
  #UV: Role_Cat_To
  
  describeBy(dialogTable_gender_role$Sentiment,dialogTable_gender_role$Role_Category_To)
  kruskal.test(dialogTable_gender_role$Sentiment,dialogTable_gender_role$Role_Category_To)
  #Unterschied in Role_Cat_To
  pairwise.wilcox.test(dialogTable_gender_role$Sentiment,dialogTable_gender_role$Role_Category_To, p.adjust="bonferroni")
  eta_squared <- (11.007 - 3 + 1)/(19261 - 3) # Effektstärke
  eta_squared
  sqrt(0.0004677017/(1-0.0004677017)) #schwacher Effect: Ab 0,1 ist es ein schwacher Effekt, ab 0,25 ein mittlerer und ab 0,4 ein starker Effekt.
  #Unterschiede in MW zw. Female_From - Genderless_From (genderless höher)
  #                       Female_From - Male_From       (male höher)
  
  #Vader
  #AV: Sentiment.ai-Scores (untransformed)
  #UV: Role_Cat_From
  
  describeBy(dialogTable_gender_role$Vader,dialogTable_gender_role$Role_Category_From)
  kruskal.test(dialogTable_gender_role$Vader,dialogTable_gender_role$Role_Category_From)
  #Unterschied
  pairwise.wilcox.test(dialogTable_gender_role$Vader,dialogTable_gender_role$Role_Category_From, p.adjust="bonferroni")
  #no_villians sign höheres Vader als villian
  
  describeBy(dialogTable_gender_role$Vader,dialogTable_gender_role$Role_Category_To)
  kruskal.test(dialogTable_gender_role$Vader,dialogTable_gender_role$Role_Category_To)
  #Unterschied
  pairwise.wilcox.test(dialogTable_gender_role$Vader,dialogTable_gender_role$Role_Category_To, p.adjust="bonferroni")
  #mit nicht villains wird feundlicher gesprochen als mit villains
  
  #plots
    #to do
  
#-END KWT2--------------------------------------
  