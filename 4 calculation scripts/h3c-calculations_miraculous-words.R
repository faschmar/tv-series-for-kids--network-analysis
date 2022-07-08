# CALCULATIONs IX - H2.2 Calculations (number of words spoken)
#prerequisites
# - download script executed
# - cleaning script executed
# - analyzing script executed
# - gender_role script executed
# - calculations_miraculous_pre I executed
# - calculations_miraculous_pre II executed
# - desc-calculations_miraculous_descriptives executed

# H2.2 Calculations (number of words spoken)
source("./main.R")
#---------------------------------------------------------------------------------------------
#H2.2 Number of words spoken (lines per gender)
#---------------------------------------------------------------------------------------------
#WCount per gender
      #http://www.sthda.com/english/wiki/ggplot2-histogram-plot-quick-start-guide-r-software-and-data-visualization
    #plot
      hist(lineTable_gender_mf$WCount, col='steelblue', main='histogram of (all) Sentiment.Ai Scores')
      
      #mean
      #mu <- ddply(lineTable_gender_mf, "Gender", summarise, grp.mean=mean(WCount))
      #head(mu)
      plot_WCount_g<-ggplot(lineTable_gender_mf, aes(x=WCount, fill=Gender, color=Gender)) + 
                      geom_histogram(binwidth=15, alpha=0.5, position="dodge")+
                      theme(legend.position="right")+
                      labs(title="Histogram plot of word count per gender over lines",x="Word Count", y = "No. of lines")+
                      theme_classic()
      #plot_WCount_g<-plot_WCount_g+ geom_vline(aes(xintercept=mean(WCount)), color="blue", linetype="dashed", size=0,5) #mean
      #plot_WCount_g<-plot_WCount_g+scale_color_brewer(palette="Dark2")
      #mean - plot_WCount_g<-plot_WCount_g+geom_histogram(fill="white", position="dodge")+geom_vline(data=mu, aes(xintercept=grp.mean, color=Gender),linetype="dashed")+theme(legend.position="top")
      plot_WCount_g
      
      #density
      ggplot(lineTable_gender_mf, aes(x=WCount,fill=Gender, color=Gender)) + 
        geom_histogram(aes(y=..density..), colour="black", fill="white")+
        geom_density(alpha=.2, fill="#FF6666")
     #plot End
      
      #mean
      desc_wcount_by_gender<-lineTable_gender_mf %>% 
        group_by(Gender) %>%
        summarize(Mean = mean(WCount, na.rm=TRUE))
      desc_wcount_by_gender<-desc_wcount_by_gender[order(desc_wcount_by_gender$Gender),]
      view(desc_wcount_by_gender)

     #t-test
      describeBy(lineTable_gender_mf$WCount, lineTable_gender_mf$Gender)
      t.test(lineTable_gender_mf$WCount~lineTable_gender_mf$Gender, var.equal=T)
      cohensD(lineTable_gender_mf$WCount~lineTable_gender_mf$Gender) 
      #t-test not sign.
#---------------------------------------------------------------------------------------------
#WCount per gender & season   
      desc_wcount_by_gender_season<-lineTable_gender_mf %>% 
        group_by(Gender, Season) %>%
        summarize(Mean = mean(WCount, na.rm=TRUE))
      desc_wcount_by_gender_season<-desc_wcount_by_gender_season[order(desc_wcount_by_gender_season$Gender, desc_wcount_by_gender_season$Season),]
      view(desc_wcount_by_gender_season)

      interaction.plot(x.factor = desc_wcount_by_gender_season$Season, trace.factor = desc_wcount_by_gender_season$Gender, 
                       response = desc_wcount_by_gender_season$Mean, 
                       type = "b", legend = TRUE, ylim = range(1:13, na.rm = TRUE),
                       xlab = "Season", ylab="Line Frequency", main="lines per gender & season",
                       pch=c(1,19), col = c("#00AFBB", "#E7B800"))

#WCount per gender_From/To
      
#from gender

  describeBy(dialogTable_gender_mf$WCount, dialogTable_gender_mf$Gender_From)      

      desc_words_by_gender_from<-dialogTable_gender_mf %>% 
        group_by(Gender_From, WCount) %>%
        summarize(Frequency=n())%>% arrange(desc(Frequency))
      desc_words_by_gender_from<-desc_words_by_gender_from[order(desc_words_by_gender_from$Gender_From),]
      view(desc_words_by_gender_from) 
      
      plot_WCount_gfrom<-ggplot(dialogTable_gender_mf, aes(x=WCount, fill=Gender_From, color=Gender_From)) + 
        geom_histogram(binwidth=15, alpha=0.5, position="dodge")+
        scale_fill_manual("Gender speaking",values=c("#E69F00","#56B4E9"))+
        scale_color_manual("Gender speaking",values=c("#E69F00","#56B4E9"))+
        theme(legend.position="right")+
        theme(text=element_text(size=21))+
        #labs(title="Histogram plot of word count per gender_from over lines",x="Word count", y = "Frequency of lines with respective word count")+
        labs(x="Word count", y = "Frequency of lines with respective word count")+
        theme_classic()+
        ylim(0, 5000)
      
      
     #plot_WCount_gfrom<- plot_WCount_gfrom + scale_y_continuous(trans = "log10")
      plot_WCount_gfrom
      
#to gender
      
      describeBy(dialogTable_gender_mf$WCount, dialogTable_gender_mf$Gender_To)
      
      desc_words_by_gender_to<-dialogTable_gender_mf %>% 
        group_by(Gender_To, WCount) %>%
        summarize(Frequency=n())%>% arrange(desc(Frequency))
      desc_words_by_gender_to<-desc_words_by_gender_from[order(desc_words_by_gender_from$Gender_To),]
      view(desc_words_by_gender_to) 
      
      plot_WCount_gto<-ggplot(dialogTable_gender_mf, aes(x=WCount, fill=Gender_To, color=Gender_To)) + 
        geom_histogram(binwidth=15, alpha=0.5, position="dodge")+
        scale_fill_manual("Gender spoken to",values=c("#009E73","#D55E00"))+
        scale_color_manual("Gender spoken to",values=c("#009E73","#D55E00"))+
        theme(legend.position="right")+
        #labs(title="Histogram plot of word count per gender_to over lines",x="Word count", y = "Frequency of lines with respective word count")+
        labs(x="Word Count", y = "Frequency of lines with respective word count")+
        theme_classic()+
        ylim(0, 5000)
      plot_WCount_gto

#to both
      
      describeBy(dialogTable_gender_mf$WCount, list(dialogTable_gender_mf$Gender_From,dialogTable_gender_mf$Gender_To))
      
      desc_words_by_gender_both<-dialogTable_gender_mf %>% 
        group_by(Gender_From, Gender_To, WCount) %>%
        summarize(Frequency=n())%>% arrange(desc(Frequency))
      desc_words_by_gender_both<-desc_words_by_gender_both[order(desc_words_by_gender_both$Gender_From),]
      view(desc_words_by_gender_both) 
    
      
      plot_WCount_gboth<-ggarrange(plot_WCount_gfrom, plot_WCount_gto + rremove("x.text"),common.legend=F, legend ="bottom",
                labels = c("A", "B"),
                ncol = 2, nrow = 1)
      plot_WCount_gboth
      
      plot_WCount_gboth <- plot_WCount_gboth + guides(fill=guide_legend(title="New Legend Title"))
      annotate_figure(plot_WCount_gboth,
                      top = text_grob("Visualizing mpg", color = "red", face = "bold", size = 14),
                      bottom = text_grob("Data source: \n mtcars data set", color = "blue",
                                         hjust = 1, x = 1, face = "italic", size = 10),
                      #left = text_grob("Figure arranged using ggpubr", color = "green", rot = 90),
                      #right = "I'm done, thanks :-)!",
                      fig.lab = "Figure 1", fig.lab.face = "bold"
      )
      


#---------------------------------------------------------------------------------------------    
      # #WCount Outliers - not necessary - all relevant
      #             describeBy(lineTable_gender_mf$WCount, lineTable_gender_mf$Gender)
      #             
      #             lineTable_gender_mf %>%
      #               identify_outliers("WCount")
      #             
      #         #outliers by groups
      #             lineTable_gender_mf %>% 
      #               group_by(Gender) %>%
      #               identify_outliers("WCount")
      #             
      #           #outliers by groups - visual
      #             hist(lineTable_gender_mf$WCount,
      #                  xlab = "WCount",
      #                  main = "Histogram of WCount",
      #                  breaks = sqrt(nrow(lineTable_gender_mf)))
      #             
      #             ggplot(lineTable_gender_mf) +
      #               aes(x = WCount) +
      #               geom_histogram(bins = 30L, fill = "#0c4c8a") +
      #               theme_minimal()
      #             
      #             ggbetweenstats(lineTable_gender_mf,
      #                            Gender, WCount, outlier.tagging = TRUE)
      #             
      #             #boxplot(lineTable_gender_role$WCount,ylab = "WCount")
      #             
      #             #boxplot.stats(lineTable_gender_role$WCount)$out
#-------H2.2 Calculations-END----------------------------------

