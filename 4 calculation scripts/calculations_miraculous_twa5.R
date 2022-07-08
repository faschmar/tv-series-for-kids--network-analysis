# ANOVA V

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
#   Sentiment_transformed_z
#-------------------------------------------

#ANOVA5: two-way-anova (twa5)
#Sentiment 
#AV: Sentiment.ai-Scores (added constant, log transformed)
#UV: Gender-From, Gender-To, Role-Category-From, Role-Category-To

twa5 <- aov(Sentiment_transformed_z ~ Gender_From * Gender_To * Role_Category_From * Role_Category_To, data=dialogTable_gender_mf)
Anova(twa5, type=3)
summary(twa5)

twa5_pht1 <- dialogTable_gender_mf %>% #post-hoc-test
  pairwise_t_test(
    Sentiment_transformed_z ~ Gender_From, paired = F, #ändern
    p.adjust.method = "bonferroni")
twa5_pht1

twa5_pht2 <- dialogTable_gender_mf %>% #post-hoc-test
  pairwise_t_test(
    Sentiment_transformed_z ~ Role_Category_From, paired = F, #ändern
    p.adjust.method = "bonferroni")
twa5_pht2

twa5_pht3 <- dialogTable_gender_mf %>% #post-hoc-test
  pairwise_t_test(
    Sentiment_transformed_z ~ Role_Category_To, paired = F, #ändern
    p.adjust.method = "bonferroni")
twa5_pht3

twa5_pht4<-TukeyHSD(twa5)
twa5_pht4

#table nicht mit transformierten Daten
dialogTable_gender_mf %>% #crosstable
  group_by(Gender_From) %>%   #ändern
  get_summary_stats(Sentiment, type = "mean_sd")

dialogTable_gender_mf %>% #crosstable
  group_by(Role_Category_From) %>%   #ändern
  get_summary_stats(Sentiment, type = "mean_sd")

dialogTable_gender_mf %>% #crosstable
  group_by(Role_Category_To) %>%   #ändern
  get_summary_stats(Sentiment, type = "mean_sd")
#interaction
dialogTable_gender_mf %>% #crosstable
  group_by(Gender_From,Role_Category_From) %>%   #ändern
  get_summary_stats(Sentiment, type = "mean_sd")

dialogTable_gender_mf %>% #crosstable
  group_by(Role_Category_From,Role_Category_To) %>%   #ändern
  get_summary_stats(Sentiment, type = "mean_sd")

dialogTable_gender_mf %>% #crosstable
  group_by(Gender_To, Role_Category_From,Role_Category_To) %>%   #ändern
  get_summary_stats(Sentiment, type = "mean_sd")

#plots
#to do

#-END ANOVA5--------------------------------------
