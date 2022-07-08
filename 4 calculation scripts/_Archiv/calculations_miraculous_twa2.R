# CALCULATIONs II

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

#ANOVA2: two-way-anova (twa2)
#Sentiment
  #AV: Sentiment.ai-Scores (added constant, log transformed)
  #UV: Gender-From, Gender-To
  
  twa2s <- aov(Sentiment_transformed_log ~ Role_From * Role_To, data=dialogTable_gender_role)
  Anova(twa2s, type=3)
  summary(twa2s)
  
  twa2s_pht1 <- dialogTable_gender_role %>% #post-hoc-test - Role_From
    pairwise_t_test(
      Sentiment_transformed_log ~ Role_From, paired = F,
      p.adjust.method = "bonferroni")
  twa2s_pht1
  
  dialogTable_gender_role %>% #crosstable twa2_pht1
    group_by(Role_From) %>%
    get_summary_stats(Sentiment_transformed_log, type = "mean_sd")
  #interpretation twa2_pht1: s. overview - results
  
  twa2s_pht2 <- dialogTable_gender_role %>% #post-hoc-test - Role_To
    pairwise_t_test(
      Sentiment_transformed_log ~ Role_To, paired = F,
      p.adjust.method = "bonferroni")
  twa2s_pht2
  
  dialogTable_gender_role %>% #crosstable twa2_pht1
    group_by(Role_From) %>%
    get_summary_stats(Sentiment, type = "median")
  #interpretation twa2_pht2: s. overview - results
  
  #insert graph
  
  
  twa2s_pht3<-TukeyHSD(twa2s) # interaction
  twa2s_pht3
  str(twa2s_pht3)
  #interpretation twa2_pht3: s. overview - results
  
  #plots
  interaction.plot(x.factor = dialogTable_gender_role$Role_From, trace.factor = dialogTable_gender_role$Role_To, 
                   response = dialogTable_gender_role$Sentiment_transformed_log, fun = mean, 
                   type = "b", legend = TRUE, 
                   xlab = "Role_From", ylab="log_transformed sentiment.ai",
                   pch=c(1,19), col = c("#00AFBB", "#E7B800"))
  #boxplot(Sentiment_transformed_log ~ Role_From * Role_To, data=dialogTable_gender_role, frame = FALSE, col = c("#00AFBB", "#E7B800"), ylab="Log Transformed Sentiment")
  
#Vader
  #AV: Vader-Scores (untransformed)
  #UV: Gender-From, Gender-To
  
  twa2v <- aov(Vader ~ Role_From * Role_To, data=dialogTable_gender_role)
  Anova(twa2v, type=3)
  summary(twa2v)
  
  twa2v_pht1 <- dialogTable_gender_role %>% #post-hoc-test - Role_From
    pairwise_t_test(
      Vader ~ Role_From, paired = F,
      p.adjust.method = "bonferroni")
  twa2v_pht1
  
  dialogTable_gender_role %>% #crosstable twa2_pht1
    group_by(Role_From) %>%
    get_summary_stats(Vader, type = "mean_sd")
  #interpretation twa2v_pht1: s. overview - results
  
  twa2v_pht2 <- dialogTable_gender_role %>% #post-hoc-test - Role_To
    pairwise_t_test(
      Vader ~ Role_To, paired = F,
      p.adjust.method = "bonferroni")
  twa2v_pht2
  
  dialogTable_gender_role %>% #crosstable twa2_pht1
    group_by(Role_To) %>%
    get_summary_stats(Vader, type = "mean_sd")
  #interpretation twa2_pht2: s. overview - results

  #plots
    #to do
  
#--END ANOVA2---------------------------