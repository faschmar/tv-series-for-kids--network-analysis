# ANOVA IV

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

#ANOVA4: two-way-anova (twa4)
#Sentiment 
#AV: Sentiment.ai-Scores (added constant, log transformed)
#UV: Gender-From, Gender-To, Role-From, Role-To

twa4 <- aov(Sentiment_transformed_z ~ Gender_From * Gender_To * Role_From * Role_To, data=dialogTable_gender_mf_r)
Anova(twa4, type=3)
summary(twa4)

#Gender From
twa4_pht1 <- dialogTable_gender_mf_r %>% #post-hoc-test
  pairwise_t_test(
    Sentiment_transformed_z ~ Gender_From, paired = F, #ändern
    p.adjust.method = "bonferroni")
twa4_pht1

#Role From
twa4_pht2 <- dialogTable_gender_mf_r %>% #post-hoc-test
  pairwise_t_test(
    Sentiment_transformed_z ~ Role_From, paired = F, #ändern
    p.adjust.method = "bonferroni")
twa4_pht2

#Role To
twa4_pht3 <- dialogTable_gender_mf_r %>% #post-hoc-test
  pairwise_t_test(
    Sentiment_transformed_z ~ Role_To, paired = F, #ändern
    p.adjust.method = "bonferroni")
twa4_pht3

twa4_pht4<-TukeyHSD(twa4)
plot(twa4_pht4)
twa4_pht4<-as.data.frame(tidy(twa4_pht4)) %>% 
  filter(adj.p.value < .05)
view(twa4_pht4)

#table nicht mit transformierten Daten
dialogTable_gender_mf_r %>% #crosstable 
  group_by(Gender_From) %>%   #ändern
  get_summary_stats(Sentiment, type = "mean_sd")

dialogTable_gender_mf_r %>% #crosstable 
  group_by(Role_From) %>%   #ändern
  get_summary_stats(Sentiment, type = "mean_sd")

dialogTable_gender_mf_r %>% #crosstable 
  group_by(Role_To) %>%   #ändern
  get_summary_stats(Sentiment, type = "mean_sd")
#interaction
dialogTable_gender_mf_r %>% #crosstable 
  group_by(Gender_From, Role_From) %>%   #ändern
  get_summary_stats(Sentiment, type = "mean_sd")

dialogTable_gender_mf_r %>% #crosstable 
  group_by(Role_From, Role_To) %>%   #ändern
  get_summary_stats(Sentiment, type = "mean_sd")

dialogTable_gender_mf_r %>% #crosstable 
  group_by(Gender_To, Role_From, Role_To) %>%   #ändern
  get_summary_stats(Sentiment, type = "mean_sd")

#plots
data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))}

#twa4_pht1_plot
          twa4_pht1_plot<-ggplot(dialogTable_gender_mf_r, aes(x = Gender_From, y = Sentiment, fill=Gender_From)) + 
            geom_violin(trim=FALSE) +
            theme(text=element_text(size=25))+
            theme(
              legend.position="none",
              plot.title = element_text(size=11)
            )+
            scale_color_brewer(palette="Dark2")+
            theme(legend.position="bottom")+
            stat_summary(fun.data=data_summary)+
            geom_boxplot(width=0.1)+
            #ggtitle("**Basic boxplot**") +
            xlab("")
          
          twa4_pht1_plot<-twa4_pht1_plot+ stat_summary(fun.data=data_summary) + 
            geom_boxplot(width=0.1) +theme(text=element_text(size=25))
          twa4_pht1_plot
        
#twa4_pht2_plot
          twa4_pht2_plot<-ggplot(dialogTable_gender_mf_r, aes(x = Role_From, y = Sentiment, fill=Role_From)) + 
            geom_violin(trim=FALSE) +
            theme(text=element_text(size=25))+
            theme(
              legend.position="none",
              plot.title = element_text(size=11)
            )+
            theme(legend.position="bottom")+
            stat_summary(fun.data=data_summary)+
            geom_boxplot(width=0.1)+
            #ggtitle("**Basic boxplot**") +
            xlab("")
          
          twa4_pht2_plot<-twa4_pht2_plot+ stat_summary(fun.data=data_summary) + 
            geom_boxplot(width=0.1)+theme(text=element_text(size=12))+ylim(-1.2,1.2)+scale_color_brewer(palette="Set3")
          twa4_pht2_plot

          
#twa4_pht3_plot
          twa4_pht3_plot<-ggplot(dialogTable_gender_mf_r, aes(x = Role_To, y = Sentiment, fill=Role_To)) + 
            geom_violin(trim=FALSE) +
            theme(text=element_text(size=25))+
            theme(
              legend.position="none",
              plot.title = element_text(size=11)
            )+
            theme(legend.position="bottom")+
            stat_summary(fun.data=data_summary)+
            geom_boxplot(width=0.1)+
            #ggtitle("**Basic boxplot**") +
            xlab("")
          
          twa4_pht3_plot<-twa4_pht3_plot+ stat_summary(fun.data=data_summary) + 
            geom_boxplot(width=0.1)+theme(text=element_text(size=12))+ylim(-1.2,1.2)+scale_color_brewer(palette="Set3")
          twa4_pht3_plot

#-END ANOVA4--------------------------------------
