# CALCULATIONs

#prerequisites
# - download script executed
# - cleaning script executed
# - analyzing script executed
# - gender_role script executed

source("./main.R")

#CALCULATIONS WITH GENDER "MALE" AND "FEMALE" - "GENDERLESS" DELETED for ANOVAS (dialogs in "From" and "To" with kwawis and multiple people)

#prerequisites
  #subset with gender: "male" & "female" only, "genderless" deleted: result -> dialogTable_gender_mf
      dialogTable_gender_role$id<-1:nrow(dialogTable_gender_role) #ID hinzufügen
      dialogTable_gender_mf <- dialogTable_gender_role[ which(dialogTable_gender_role$Gender_From=='male'| dialogTable_gender_role$Gender_From=='female'), ]
      dialogTable_gender_mf <- dialogTable_gender_mf[ which(dialogTable_gender_mf$Gender_To=='male'| dialogTable_gender_mf$Gender_To=='female'), ]
       #find out if subset worked - just male&female groups
      dialogTable_gender_role %>%
        group_by(Gender_From, Gender_To) %>%
        summarize(Frequency=n())%>% arrange(desc(Frequency))  

#Descriptives
        summary(dialogTable_gender_mf) #overview
    
        dialogTable_gender_role %>%
          group_by(Gender_From, Gender_To, Season) %>%
          get_summary_stats(Sentiment, type = "mean_sd")
        dialogTable_gender_role %>%
          group_by(Season) %>%
          get_summary_stats(Sentiment, type = "mean_sd")
        
        plot_gender_from <- ggboxplot(dialogTable_gender_role, x = "Gender_From", y = "Sentiment", add = "point")
        plot_gender_from
        plot_gender_to <- ggboxplot(dialogTable_gender_role, x = "Gender_To", y = "Sentiment", add = "point")
        plot_gender_to
        
        #histogram 
        ggdensity(dialogTable_gender_role, x = "Sentiment", 
                  fill = "#0073C2FF", color = "#0073C2FF",
                  add = "mean", rug = TRUE)
        
        dialogTable_gender_role %>%
          group_by(Season) %>% identify_outliers(Sentiment)
        
    #Assumption of normal distribution: "SentimentAI" - dialogTable_gender_mf$Sentiment
        #graphs of Sentiment
        ggdensity(dialogTable_gender_role, x = "Sentiment", 
                  fill = "#0073C2FF", color = "#0073C2FF",
                  add = "mean", rug = TRUE)
        hist(dialogTable_gender_role$Sentiment, col='steelblue', main='untransformed Sentiment-data')
        
        #Shapiro-Wilk
        #interpretation: no normal-distribution - Shapiro-Wilk is very sensitive with higher sample sizes - looking at QQ plot instead
        dialogTable_gender_mf %>%
          group_by(Gender_From, Gender_To) %>%
          shapiro_test(Sentiment)
        
        #interpretation: also with QQ Plots a normal distribution of "Sentiment" cannot be assumed
        ggqqplot(dialogTable_gender_mf, "Sentiment_transformed_log", facet.by = "Gender_From") 
    
        #tranformation of data to fit normal distribution
        log_y <- log10(dialogTable_gender_mf$Sentiment_transformed)
        sqrt_y <- sqrt(dialogTable_gender_mf$Sentiment_transformed)
        dialogTable_gender_mf$Sentiment_transformed <- dialogTable_gender_mf$Sentiment+0.822 #adding smalles value
        min(dialogTable_gender_mf$Sentiment)
        min(dialogTable_gender_mf$Sentiment_transformed)
        #log
        dialogTable_gender_mf$Sentiment_transformed_log <- log10(dialogTable_gender_mf$Sentiment_transformed)
        which(is.na(dialogTable_gender_mf$Sentiment_transformed_log)) # are there NA in log-transformed values?
        summary(dialogTable_gender_mf$Sentiment_transformed_log)
        hist(dialogTable_gender_mf$Sentiment_transformed_log, col='steelblue', main='log-transformed Sentiment-data')
        ggqqplot(dialogTable_gender_mf, "Sentiment_transformed_log", facet.by="Gender_From") 
        #sqrt
        dialogTable_gender_mf$Sentiment_transformed_sqrt <- sqrt(dialogTable_gender_mf$Sentiment_transformed)
        which(is.na(dialogTable_gender_mf$Sentiment_transformed_sqrt)) # are there NA in log-transformed values?
        summary(dialogTable_gender_mf$Sentiment_transformed_sqrt)
        hist(dialogTable_gender_mf$Sentiment_transformed_sqrt, col='steelblue', main='sqrt-transformed Sentiment-data')
        ggqqplot(dialogTable_gender_mf, "Sentiment_transformed_sqrt", facet.by="Gender_From") 
        #cuberoot
        dialogTable_gender_mf$Sentiment_transformed_cuberoot <- dialogTable_gender_mf$Sentiment_transformed^(1/3)
        which(is.na(dialogTable_gender_mf$Sentiment_transformed_cuberoot)) # are there NA in log-transformed values?
        summary(dialogTable_gender_mf$Sentiment_transformed_cuberoot)
        hist(dialogTable_gender_mf$Sentiment_transformed_cuberoot, col='steelblue', main='cuberoot-transformed Sentiment-data')
        ggqqplot(dialogTable_gender_mf, "Sentiment_transformed_cuberoot", facet.by="Gender_From") 

        dialogTable_gender_mf %>%
          group_by(Gender_From, Gender_To) %>%
          shapiro_test(Sentiment_transformed_log)
        
        #Normal distribution can be assumed at most with transformation: log(Sentiment + min.Sentiment-0.822) 
        
    #Assumption of sphericity: Mauchly’s test - for repeated ANOVAS
        #needed?
        res.aov <- anova_test(data = dialogTable_gender_mf, dv = Sentiment, wid = id, between = Gender_From)
        get_anova_table(res.aov)
        res.aov <- anova_test(data = dialogTable_gender_mf, dv = Sentiment, wid = id, between = Gender_To)
        get_anova_table(res.aov)

#ANOVAs
#-------------------------------------------

#ANOVA1: two-way-anova (twa1)
        #AV: Sentiment.ai-Scores (added constant, log transformed)
        #UV: Gender-From, Gender-To
  
  twa1 <- aov(Sentiment_transformed_log ~ Gender_From * Gender_To, data=dialogTable_gender_mf)
  Anova(twa1, type=3)
  summary(twa1)
  
  twa1_pht1 <- dialogTable_gender_mf %>% #post-hoc-test
    pairwise_t_test(
      Sentiment_transformed_log ~ Gender_From, paired = F,
      p.adjust.method = "bonferroni")
  twa1_pht1
  
  dialogTable_gender_mf %>% #crosstable twa1_pht1
    group_by(Gender_From) %>%
    get_summary_stats(Sentiment_transformed_log, type = "mean_sd")

  #interpretation twa1: diff of Sentiment_transformed_log - scores between m and f in Gender_From; no other effect (Gender_to, Interaction): FEMALES SHOW sign. LOWER SENTIMENT THAN MALES

#-------------------------------------------

#ANOVA2: two-way-anova (twa2)
  #AV: Sentiment.ai-Scores (added constant, log transformed)
  #UV: Gender-From, Gender-To
  
  twa2 <- aov(Sentiment_transformed_log ~ Role_From * Role_To, data=dialogTable_gender_mf)
  Anova(twa2, type=3)
  summary(twa2)
  
  boxplot(Sentiment_transformed_log ~ Role_From * Role_To, data=dialogTable_gender_mf, frame = FALSE, 
          col = c("#00AFBB", "#E7B800"), ylab="Tooth Length")
  
  interaction.plot(x.factor = dialogTable_gender_mf$Role_From, trace.factor = dialogTable_gender_mf$Role_To, 
                   response = dialogTable_gender_mf$Sentiment_transformed_log, fun = mean, 
                   type = "b", legend = TRUE, 
                   xlab = "Role_From", ylab="log_transformed sentiment.ai",
                   pch=c(1,19), col = c("#00AFBB", "#E7B800"))
  
  twa2_pht1 <- dialogTable_gender_mf %>% #post-hoc-test - Role_From
    pairwise_t_test(
      Sentiment_transformed_log ~ Role_From, paired = F,
      p.adjust.method = "bonferroni")
  twa2_pht1
  
  dialogTable_gender_mf %>% #crosstable twa2_pht1
    group_by(Role_From) %>%
    get_summary_stats(Sentiment_transformed_log, type = "mean_sd")
  #interpretation twa2_pht1: s. overview - results
  
  twa2_pht2 <- dialogTable_gender_mf %>% #post-hoc-test - Role_To
    pairwise_t_test(
      Sentiment_transformed_log ~ Role_To, paired = F,
      p.adjust.method = "bonferroni")
  twa2_pht2
  
  dialogTable_gender_mf %>% #crosstable twa2_pht1
    group_by(Role_To) %>%
    get_summary_stats(Sentiment_transformed_log, type = "mean_sd")
  #interpretation twa2_pht2: s. overview - results
  
  twa2_pht3<-TukeyHSD(twa2) # interaction
  #interpretation twa2_pht3: s. overview - results
  
#-------------------------------------------

#ANOVA3: two-way-anova (twa3)
        #AV: Sentiment.ai-Scores over time (added constant, log transformed)
        #UV: Gender-From, Gender-To, Role_From, Role_To
  
  twa3 <- aov(Sentiment_transformed_log ~ Role_Category_From * Role_Category_To, data=dialogTable_gender_mf)
  Anova(twa3, type=3)
  summary(twa3)
  
  twa3_pht1 <- dialogTable_gender_mf %>% #post-hoc-test - Role_Category_To
    pairwise_t_test(
      Sentiment_transformed_log ~ Role_Category_To, paired = F,
      p.adjust.method = "bonferroni")
  twa3_pht1
  
  dialogTable_gender_mf %>% #crosstable twa3_pht1
    group_by(Role_Category_To) %>%
    get_summary_stats(Sentiment_transformed_log, type = "mean_sd")
  #interpretation twa3_pht1: s. overview - results
  
  twa3_pht2<-TukeyHSD(twa3) # interaction

#-------------------------------------------

#ANOVA4: two-way-anova (twa4)
        #AV: Sentiment.ai-Scores over time (added constant, log transformed)
        #UV: Gender-From, Gender-To, Role_Category_From, Role_Category_To
  twa4 <- aov(Sentiment_transformed_log ~ Gender_From * Gender_To+Role_Category_From+Role_Category_To+Role_Category_From*Role_Category_To, data=dialogTable_gender_mf)
  Anova(twa4, type=3)
  summary(twa4)
  
  twa4_pht1 <- dialogTable_gender_mf %>% #post-hoc-test -Gender_From
    pairwise_t_test(
      Sentiment_transformed_log ~ Gender_From, paired = F,
      p.adjust.method = "bonferroni")
  twa4_pht1
  
  twa4_pht2 <- dialogTable_gender_mf %>% #post-hoc-test - Role_Category_From
    pairwise_t_test(
      Sentiment_transformed_log ~ Role_Category_From, paired = F,
      p.adjust.method = "bonferroni")
  twa4_pht2
  
  twa4_pht3 <- dialogTable_gender_mf %>% #post-hoc-test - Role_Category_To
    pairwise_t_test(
      Sentiment_transformed_log ~ Role_Category_To, paired = F,
      p.adjust.method = "bonferroni")
  twa4_pht3
  
  twa4_pht4<-TukeyHSD(twa4)
#-------------------------------------------

  
        
        