# CALCULATIONs IV

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

#ANOVA4: two-way-anova (twa4)
        #AV: Sentiment.ai-Scores over time (added constant, log transformed)
        #UV: Gender-From, Gender-To, Role_Category_From, Role_Category_To
  
twa4s <- aov(Sentiment_transformed_log ~ Gender_From * Gender_To*Role_Category_From*Role_Category_To, data=dialogTable_gender_role)
  #twa4s <- aov(Sentiment_transformed_log ~ Gender_From * Gender_To+Role_Category_From+Role_Category_To+Role_Category_From*Role_Category_To, data=dialogTable_gender_role)
  Anova(twa4s, type=3)
  summary(twa4s)
  
  twa4s_pht1 <- dialogTable_gender_role %>% #post-hoc-test -Gender_From
    pairwise_t_test(
      Sentiment_transformed_log ~ Gender_From, paired = F,
      p.adjust.method = "bonferroni")
  twa4s_pht1
  
  twa4s_pht2 <- dialogTable_gender_role %>% #post-hoc-test - Role_Category_From
    pairwise_t_test(
      Sentiment_transformed_log ~ Role_Category_To, paired = F,
      p.adjust.method = "bonferroni")
  twa4s_pht2
  
  
  twa4s_pht3<-TukeyHSD(twa4s)
  twa4s_pht3
  
  #plots
    #to do

#Vader  
  #ANOVA4: two-way-anova (twa4)
      #AV: Vader-Scores (untransformed)
       #UV: Gender-From, Gender-To, Role_Category_From, Role_Category_To
  
  twa4v <- aov(Vader ~ Gender_From * Gender_To * Role_Category_From * Role_Category_To, data=dialogTable_gender_role)
  #twa4s <- aov(Sentiment_transformed_log ~ Gender_From * Gender_To+Role_Category_From+Role_Category_To+Role_Category_From*Role_Category_To, data=dialogTable_gender_role)
  Anova(twa4v, type=3)
  summary(twa4v)
  
  twa4v_pht1 <- dialogTable_gender_role %>% #post-hoc-test -Gender_From
    pairwise_t_test(
      Vader ~ Gender_To, paired = F,
      p.adjust.method = "bonferroni")
  twa4v_pht1
  
  twa4v_pht2 <- dialogTable_gender_role %>% #post-hoc-test - Role_Category_From
    pairwise_t_test(
      Vader ~ Role_Category_From, paired = F,
      p.adjust.method = "bonferroni")
  twa4v_pht2
  
  twa4v_pht3 <- dialogTable_gender_role %>% #post-hoc-test - Role_Category_From
    pairwise_t_test(
      Vader ~ Role_Category_To, paired = F,
      p.adjust.method = "bonferroni")
  twa4v_pht3
  
  #plots
    #to do

#----END ANOVA4------------------------

  
        
        