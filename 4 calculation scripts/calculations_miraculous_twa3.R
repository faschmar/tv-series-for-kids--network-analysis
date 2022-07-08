# ANOVA III

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

#ANOVA2: two-way-anova (twa1)
#Sentiment 
#AV: Sentiment.ai-Scores (added constant, log transformed)
#UV: Role-Category-From, Role-Category-To

twa3 <- aov(Sentiment_transformed_z ~ Role_Category_From * Role_Category_To, data=dialogTable_gender_mf)
Anova(twa3, type=3)
summary(twa3)

#if necessary
twa3_pht1 <- dialogTable_gender_mf %>% #post-hoc-test
  pairwise_t_test(
    Sentiment_transformed_z ~ Role_Category_To, paired = F, #ändern
    p.adjust.method = "bonferroni")
twa3_pht1

twa3_pht2<-TukeyHSD(twa3)
twa3_pht2

#table nicht mit transformierten Daten
dialogTable_gender_mf %>% #crosstable
  group_by(Role_Category_To) %>%   #ändern
  get_summary_stats(Sentiment, type = "mean_sd")

dialogTable_gender_mf %>% #crosstable
  group_by(Role_Category_From, Role_Category_To) %>%   #ändern
  get_summary_stats(Sentiment, type = "mean_sd")

#plots
#to do


#-END ANOVA3--------------------------------------
