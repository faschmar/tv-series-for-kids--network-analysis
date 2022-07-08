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

#ANOVA5: two-way-anova (twa7)
#Sentiment 
#AV: Sentiment.ai-Scores (added constant, log transformed)
#UV: Gender_From, Gender_To, Word_Count

twa7 <- aov(Sentiment_transformed_z ~ Gender_From * Gender_To * WCount, data=dialogTable_gender_mf)
Anova(twa7, type=3)
summary(twa7)
#nur Gender From Sign.

twa7_pht1 <- dialogTable_gender_mf %>% #post-hoc-test
  pairwise_t_test(
    Sentiment_transformed_z ~ Gender_From, paired = F, #ändern
    p.adjust.method = "bonferroni")
twa7_pht1


#table nicht mit transformierten Daten
dialogTable_gender_mf %>% #crosstable
  group_by(Gender_From) %>%   #ändern
  get_summary_stats(Sentiment, type = "mean_sd")

#plots
#to do

#-END ANOVA7--------------------------------------
