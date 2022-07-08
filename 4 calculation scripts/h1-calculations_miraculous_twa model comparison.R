# ANOVA VII - ANOVA Model Comparision

#prerequisites
# - download script executed
# - cleaning script executed
# - analyzing script executed
# - gender_role script executed
# - calculations_miraculous_pre executed
# - calculations_miraculous_twa1, _twa4, _twa5 executed

source("./main.R")
#-------------------------------------------
# values: dialogTable_gender_role
#   Sentiment
#   Sentiment_transformed_z
#-------------------------------------------

#ANOVA1, ANOVA4, ANOVA5 should be compared - twa1, twa4, twa5
#https://www.scribbr.com/statistics/anova-in-r/
#-------------------------------------------

#model fit: Akaike information criterion (AIC) 
  AIC_model.set <- list(twa1, twa2, twa3, twa4, twa5)
  AIC_model.names <- c("twa1: Gender", "twa2: Role","twa3: Role_Category", "twa4: Gender_Role", "twa5: Gender_Role_Category")
  aictab(AIC_model.set, modnames = AIC_model.names)

#best fit: Gender_Role!

#Check for homoscedasticity of winner: twa4
    par(mfrow=c(2,2))
    plot(twa4)
    par(mfrow=c(1,1))

    
#bootsampling for best fitted model (twa4)

    #bootstrapping (FoCSS) - I
    # obtaining M bootstrap samples and calculate mean value of each bootstrap sample
    bootstrap_twa4_m <- 10000 
    bootstrap_twa4_slopes <- numeric(bootstrap_twa4_m)
    bootstrap_twa4_intercepts <- numeric(bootstrap_twa4_m)
    for(i in 1:bootstrap_twa4_m)
    {
      bootstrap_sample <- sample(nrow(dialogTable_gender_mf$Sentiment_transformed_z), replace = T)
      #bootmodel <- ?aov(Sentiment_transformed_z ~ Gender_From[bootstrap_sample] * Gender_To[bootstrap_sample] * Role_From[bootstrap_sample] * Role_To[bootstrap_sample], data=dialogTable_gender_mf)
      bootstrap_twa4_model <- lm(dialogTable_gender_mf$Sentiment_transformed_z[bootstrap_sample] ~ dialogTable_gender_mf$Gender_From[bootstrap_sample] * dialogTable_gender_mf$Gender_To[bootstrap_sample] * dialogTable_gender_mf$Role_From[bootstrap_sample] * dialogTable_gender_mf$Role_To[bootstrap_sample])
      #summary.aov(bootmodel)
      bootstrap_twa4_slopes[i] <- bootstrap_twa4_model$coefficients[2]
      bootstrap_twa4_intercepts[i] <- bootstrap_twa4_model$coefficients[1]
    }
    median(bootstrap_twa4_slopes)
    sd(bootstrap_twa4_slopes)
    hist(bootstrap_twa4_slopes)
    quantile(bootstrap_twa4_slopes, probs = c(0.025, 0.975))

    bootstrap_twa4_coefs <- rbind(bootstrap_twa4_intercepts, bootstrap_twa4_slopes)
  
    confint(twa4)
    confint(bootstrap_twa4_model)
    ##wie weiter?
    
#bootsampling for (twa1) - Gender From/To (to be sure)
    bootstrap_twa1 <- ANOVA.boot(Sentiment_transformed_z~as.factor(Gender_From)*as.factor(Gender_To), data=dialogTable_gender_mf, B=10000, seed=9999)
    bootstrap_twa1$`p-values`  #bootstrap p-values  - Ergebni bestätigt 
    summary(twa1) #to compare to bootstrap
#---aufräumen--------------------------------
    rm(i, bootstrap_twa4_m, bootstrap_sample)
    
#-END ANOVA - Model Comparision END---------
