# ANOVA 1 

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

#ANOVA1: two-way-anova (twa1)
#Sentiment 
        #AV: Sentiment.ai-Scores (added constant, log transformed)
        #UV: Gender-From, Gender-To

  twa1 <- aov(Sentiment_transformed_z ~ Gender_From * Gender_To, data=dialogTable_gender_mf)
  Anova(twa1, type=3)
  summary(twa1)
  
  #if necessary
  twa1_pht1 <- dialogTable_gender_mf %>% #post-hoc-test
    pairwise_t_test(
      Sentiment_transformed_z ~ Gender_From, paired = F,
      p.adjust.method = "bonferroni")
  twa1_pht1

  #table nicht mit transformierten Daten
  dialogTable_gender_mf %>% #crosstable twa1_pht1
    group_by(Gender_From) %>%
    get_summary_stats(Sentiment, type = "mean_sd")

  #effect size:
    eta_squared(car::Anova(twa1, type = 3))
    #very small effect sizes
    twa1_pht2<-TukeyHSD(twa1)
    multcompLetters4(twa1,twa1_pht2)
    
  #plots
    
    #https://statdoe.com/scatterplot-for-two-factors-in-r/
    #http://www.sthda.com/english/wiki/ggplot2-violin-plot-quick-start-guide-r-software-and-data-visualization
      
    data_summary <- function(x) {
      m <- mean(x)
      ymin <- m-sd(x)
      ymax <- m+sd(x)
      return(c(y=m,ymin=ymin,ymax=ymax))}
    
     twa1_pht1_plot<-ggplot(dialogTable_gender_mf, aes(x = Gender_From, y = Sentiment, fill=Gender_From)) + 
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
     
     twa1_pht1_plot<-twa1_pht1_plot+ stat_summary(fun.data=data_summary) + 
                      geom_boxplot(width=0.1)+ylim(-1.2,1.2) +
       theme(text=element_text(size=25))
     twa1_pht1_plot
       
#-END ANOVA1--------------------------------------
  