# ANOVA VI - Laura

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

#ANOVA6: two-way-anova (twa6)
#Sentiment 
#AV: Sentiment.ai-Scores (added constant, log transformed)
#UV: Role-From (subset: main, villain), Role-To (subset: main, villain))

#Subset Beginn
#gender elimination (genderless) from dialogtable_gender_role => dialogTable_gender_mf
dialogTable_gender_role_mv<-dialogTable_gender_role[dialogTable_gender_role$Role_From != "friends_other", ] 
dialogTable_gender_role_mv<-dialogTable_gender_role_mv[dialogTable_gender_role_mv$Role_From != "kwami", ] 
dialogTable_gender_role_mv<-dialogTable_gender_role_mv[dialogTable_gender_role_mv$Role_From != "multiple", ]

dialogTable_gender_role_mv<-dialogTable_gender_role_mv[dialogTable_gender_role_mv$Role_To != "friends_other", ]
dialogTable_gender_role_mv<-dialogTable_gender_role_mv[dialogTable_gender_role_mv$Role_To != "kwami", ] 
dialogTable_gender_role_mv<-dialogTable_gender_role_mv[dialogTable_gender_role_mv$Role_To != "multiple", ]

#find out if subset worked - just main&villain groups
dialogTable_gender_role_mv %>%
group_by(Role_From, Role_To) %>%
  dplyr::summarize(Frequency=n()) %>% 
  arrange(desc(Frequency))  

twa6 <- aov(Sentiment_transformed_z ~ Role_From * Role_To, data=dialogTable_gender_role_mv)
Anova(twa6, type=3)
summary(twa6)

#effect size:
eta_squared(car::Anova(twa6, type = 3))

twa6_pht1 <- dialogTable_gender_role_mv %>% #post-hoc-test
  pairwise_t_test(
    Sentiment_transformed_z ~ Role_From, paired = F, 
    p.adjust.method = "bonferroni")
twa6_pht1
#interpretation: main are showing more "stress" when talking to villains, as villains when they talking to main.
#(F=4.590, p=0.0322)


twa6_pht2<-TukeyHSD(twa6)
twa6_pht2

#table nicht mit transformierten Daten
dialogTable_gender_role_mv %>% #crosstable twa1_pht1
  group_by(Role_From) %>%   #ändern
  get_summary_stats(Sentiment, type = "mean_sd")

#interaction: crosstable to
dialogTable_gender_role_mv %>% #crosstable twa1_pht1
  group_by(Role_From,Role_To) %>%   #ändern
  get_summary_stats(Sentiment, type = "mean_sd")

#plots
#to do
#Anova input
twa6_bar_chart <- dialogTable_gender_role_mv %>% #crosstable twa1_pht1
  group_by(Role_From,Role_To) %>%   #ändern
  get_summary_stats(Sentiment, type = "mean_sd")
ggplot(twa6_bar_chart, aes(x = Role_From, y = mean, fill = Role_To)) +
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,position=position_dodge(.9))+
  ylab('Sentiment')

#-END ANOVA6--------------------------------------
