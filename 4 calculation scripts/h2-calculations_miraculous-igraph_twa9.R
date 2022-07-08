# ANOVA IX

#prerequisites
# - download script executed
# - cleaning script executed
# - analyzing script executed
# - gender_role script executed
# - calculations_miraculous_pre executed
# - h2-calculations_miraculous_igraph executed

source("./main.R")
#-------------------------------------------
# values: genderSozioTable
#   Sentiment
#   Sentiment_transformed_z
#-------------------------------------------

#--Both Gender ---------
#ANOVA9a: two-way-anova (twa9a)
  #Sentiment 
  #AV: Sentiment.ai-Scores (added constant, log transformed)
  #UV: From, To (containing both gender): "Main" containing m&f characters (top 10)

twa9a <- aov(Sentiment_transformed_z ~ From * To, data=genderSozioTable_mf)
Anova(twa9a, type=3)
summary(twa9a)

twa9a_pht1 <- genderSozioTable_mf %>% #post-hoc-test
  pairwise_t_test(
    Sentiment_transformed_z ~ From, paired = F, #ändern
    p.adjust.method = "bonferroni")
twa9a_pht1

#table nicht mit transformierten Daten
genderSozioTable_mf %>% #crosstable
  group_by(From) %>%   #ändern
  get_summary_stats(Sentiment, type = "mean_sd")

#--Gender: female ---------
#ANOVA9b: two-way-anova (twa9b)
  #Sentiment 
  #AV: Sentiment.ai-Scores (added constant, log transformed)
  #UV: From, To (containing female): "Main" containing female characters (top 10)

twa9b <- aov(Sentiment_transformed_z ~ From * To, data=genderSozioTable_f)
Anova(twa9b, type=3)
summary(twa9b)

twa9b_pht1 <- genderSozioTable_f %>% #post-hoc-test
  pairwise_t_test(
    Sentiment_transformed_z ~ From, paired = F, #ändern
    p.adjust.method = "bonferroni")
twa9b_pht1

#table nicht mit transformierten Daten
genderSozioTable_f %>% #crosstable
  group_by(From) %>%   #ändern
  get_summary_stats(Sentiment, type = "mean_sd")

#--Gender: female ---------
#ANOVA9c: two-way-anova (twa9c)
  #Sentiment 
  #AV: Sentiment.ai-Scores (added constant, log transformed)
  #UV: From, To (containing male): "Main" containing male characters (top 10)

twa9c <- aov(Sentiment_transformed_z ~ From * To, data=genderSozioTable_m)
Anova(twa9c, type=3)
summary(twa9c)

twa9c_pht1 <- genderSozioTable_f %>% #post-hoc-test
  pairwise_t_test(
    Sentiment_transformed_z ~ From, paired = F, #ändern
    p.adjust.method = "bonferroni")
twa9c_pht1

#table nicht mit transformierten Daten
genderSozioTable_m %>% #crosstable
  group_by(From) %>%   #ändern
  get_summary_stats(Sentiment, type = "mean_sd")


#plots
#to do

#-END ANOVA5--------------------------------------
