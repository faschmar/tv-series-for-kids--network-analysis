# ANOVA II

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
#UV: Role-From, Role-To

twa2 <- aov(Sentiment_transformed_z ~ Role_From * Role_To, data=dialogTable_gender_mf)
Anova(twa2, type=3)
summary(twa2)

twa2_pht1 <- dialogTable_gender_mf %>% #post-hoc-test
  pairwise_t_test(
    Sentiment_transformed_z ~ Role_From, paired = F, #ändern
    p.adjust.method = "bonferroni")
twa2_pht1

twa2_pht2 <- dialogTable_gender_mf %>% #post-hoc-test
  pairwise_t_test(
    Sentiment_transformed_z ~ Role_To, paired = F, #ändern
    p.adjust.method = "bonferroni")
twa2_pht2


twa2_pht3<-TukeyHSD(twa2)
twa2_pht3
plot(twa2_pht3)
twa2_pht3<-as.data.frame(tidy(twa2_pht3)) %>% 
  filter(adj.p.value < .05)
view(twa2_pht3)

#table nicht mit transformierten Daten
dialogTable_gender_mf %>% #crosstable twa1_pht1
  group_by(Role_To) %>%   #ändern
  get_summary_stats(Sentiment, type = "mean_sd")

dialogTable_gender_mf %>% #crosstable twa1_pht1
  group_by(Role_From) %>%   #ändern
  get_summary_stats(Sentiment, type = "mean_sd")

dialogTable_gender_mf %>% #crosstable twa1_pht1
  group_by(Role_From, Role_To) %>%   #ändern
  get_summary_stats(Sentiment, type = "mean_sd")


#plots
#to do

#-END ANOVA2--------------------------------------
