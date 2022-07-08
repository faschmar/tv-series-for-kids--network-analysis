# CALCULATIONs IX - H2.1 Calculations (number of speech prompts)
#prerequisites
# - download script executed
# - cleaning script executed
# - analyzing script executed
# - gender_role script executed
# - calculations_miraculous_pre I executed
# - calculations_miraculous_pre II executed
# - desc-calculations_miraculous_descriptives executed

# H2.1 Calculations (number of speech prompts)
source("./main.R")
#---------------------------------------------------------------------------------------------
#H2.1 Number of speech prompts (lines per gender)

      desc_lines_by_gender<-lineTable_gender_mf %>% 
        group_by(Gender) %>%
        summarize(Frequency=n())%>% arrange(desc(Frequency))
      desc_lines_by_gender<-desc_lines_by_gender[order(desc_lines_by_gender$Gender),]
      view(desc_lines_by_gender)
      chi2_observed_gender<-table(dialogTable_gender_mf$Gender_From)
      chi2_expected_gender<-c(1/2, 1/2)
      chisq.test(x = chi2_observed_gender, p = chi2_expected_gender, simulate.p.value = TRUE)
      #dt <- as.table(as.matrix(dialogTable_gender_mf))
      #balloonplot(t(dt), main ="housetasks", xlab ="", ylab="",
#---------------------------------------------------------------------------------------------
#Lines per gender & Role
        desc_lines_by_gender_role<-lineTable_gender_mf_r %>% 
          group_by(Gender, Role) %>%
          summarize(Frequency=n())%>% arrange(desc(Frequency))
        desc_lines_by_gender_role<-desc_lines_by_gender_role[order(desc_lines_by_gender_role$Gender, desc_lines_by_gender_role$Role),]
        view(desc_lines_by_gender_role)

      #crosstab + chi2
        desc_lines_by_gender_role<-xtabs(~lineTable_gender_mf_r$Gender + ~lineTable_gender_mf_r$Role)
        desc_lines_by_gender_role
        summary(desc_lines_by_gender_role)
        
        chi2_intermediate = structure(list(A = c(4631L, 4099L), B = c(5584L, 3248L), c = c(750L, 1560L)),
                                      .Names = c("friends_other", "main", "villain"), class = "data.frame", row.names = c("female", "male"))
        chisq.test(chi2_intermediate)
        chisq.posthoc.test(chi2_intermediate)

      #table
        colnames(desc_lines_by_gender_role)<-c("Gender","Role", "No. of Lines")
        desc_lines_by_gender_role %>% #table: Lines per gender & Role
          gt(
          ) %>% 
          gt_theme_espn() %>%
          tab_header(title = md("**Speech Prompts**"), subtitle = md("Number of Lines by gender*role")) 
      #or
        chi2_intermediate %>% #table: Lines per gender & Role
          gt(rownames_to_stub = T, rowname_col = "name" 
          ) %>% 
          gt_theme_espn() %>%
          tab_row_group(label = "Gender", rows = c("female", "male")) %>%
          tab_spanner(label = "Role", columns = c("friends_other", "main", "villain")) %>%
          tab_header(title = md("**Speech Prompts**"), subtitle = md("Number of lines by gender*role"))
      #or
          tbl_summary(desc_lines_by_gender_role)
#---------------------------------------------------------------------------------------------
#Lines per gender & season (plot)
        desc_lines_by_gender_season<-lineTable_gender_mf %>% 
          group_by(Gender, Season) %>%
          summarize(Frequency=n())%>% arrange(desc(Frequency))
        desc_lines_by_gender_season<-desc_lines_by_gender_season[order(desc_lines_by_gender_season$Gender, desc_lines_by_gender_season$Season),]
        view(desc_lines_by_gender_season)
        
        desc_lines_by_gender_season<-xtabs(~lineTable_gender_mf_r$Gender + ~lineTable_gender_mf_r$Season)
        desc_lines_by_gender_season
        summary(desc_lines_by_gender_season)

      #chi2_intermediate
        chi2_intermediate = structure(list(1 = c(2774L, 2178L), 2 = c(2797L, 2059L), 3 = c(2574L, 2301L), 4 = c(2820L, 2369L)),
                                      .Names = c("S1", "S2", "S3", "S4"), class = "data.frame", row.names = c("female", "male"))
        chisq.test(chi2_intermediate)
        chisq.posthoc.test(chi2_intermediate)

      #plot
        interaction.plot(x.factor = desc_lines_by_gender_season$Season, trace.factor = desc_lines_by_gender_season$Gender, 
                 response = desc_lines_by_gender_season$Frequency, 
                 trace.label = "Gender",
                 type = "b", legend = TRUE, 
                 xlab = "Season", ylab="Line Frequency", main="lines per gender & season",
                 pch=c(1,19), ylim = range(1:3000), col = c("#00AFBB", "#E7B800"))
#---------------------------------------------------------------------------------------------
#Lines per gender & season - relative to number of female/male characters
      desc_lines_by_gender_season<-lineTable_gender_mf %>% 
        group_by(Season, Gender) %>%
        summarize(Frequency=n())%>% arrange(desc(Frequency))
      desc_lines_by_gender_season<-desc_lines_by_gender_season[order(desc_lines_by_gender_season$Gender, desc_lines_by_gender_season$Season),]
      view(desc_lines_by_gender_season)

      desc_gender_by_season<-characters_season_mf %>% 
        group_by(Season, Gender) %>%
        summarize(Frequency=n())%>% arrange(desc(Frequency))
      desc_gender_by_season <-desc_gender_by_season[order(desc_gender_by_season$Gender, desc_gender_by_season$Season),]
      view(desc_gender_by_season)

      desc_rel_lines_by_gender_season <-left_join(desc_lines_by_gender_season, desc_gender_by_season, by = c("Season" = "Season", "Gender" = "Gender"))
      colnames(desc_rel_lines_by_gender_season)<-c("Season", "Gender","Number_of_lines", "Number_of_characters")
      view(desc_rel_lines_by_gender_season)
      desc_rel_lines_by_gender_season$Relative_number_of_lines <- with(desc_rel_lines_by_gender_season, Number_of_lines/Number_of_characters) #relative Häufigkeit berechnen

      describeBy(desc_rel_lines_by_gender_season$Relative_number_of_lines, list(desc_rel_lines_by_gender_season$Gender,desc_rel_lines_by_gender_season$Season))
      
      interaction.plot(x.factor = desc_rel_lines_by_gender_season$Season, trace.factor = desc_rel_lines_by_gender_season$Gender, 
                       response = desc_rel_lines_by_gender_season$Relative_number_of_lines, 
                       trace.label = "Gender",
                       type = "b", legend = TRUE, 
                       xlab = "Season", ylab="Line Frequency", main="relative Number of Lines (=Lines/No of Characters) by gender & season",
                       pch=c(1,19), col = c("#00AFBB", "#E7B800"))

      describeBy(desc_rel_lines_by_gender_season$Relative_number_of_lines, list(desc_rel_lines_by_gender_season$Gender,desc_rel_lines_by_gender_season$Season))
      
      x<-qplot(x = Season, y = Relative_number_of_lines, color=Gender, data = desc_rel_lines_by_gender_season) +
        geom_line(aes(group = Gender),size=2, show.legend = F) +
        geom_point(size=4) +
        #labs(title = "Relative number of lines by gender & season",
        #     subtitle = "(lines divided by characters gender over seasons)") +
        theme_classic()+
        labs(x="Season", y = "Relative line frequency")+
        scale_y_continuous(limits = c(0,100))
      
      qplot(x = Season, y = Number_of_lines, color=Gender, data = desc_rel_lines_by_gender_season) +
        geom_line(aes(group = Gender))+
        geom_point() +
        labs(title = "Absolute number of lines by gender & season",
             subtitle = "") +
        theme_classic()+
        labs(x="Season", y = "Absolute line frequency")+
        scale_y_continuous(limits = c(0,3000))
      
#histogramm mit ggplot2 von Anzahl der Characteren
      
      y<-qplot(x = Season, y = Frequency, color=Gender, data = desc_gender_by_season) +
        geom_line(aes(group = Gender),size=2, show.legend = F)+
        geom_point(size=4) +
        #labs(title = "Number of characters by gender & season",
        #     subtitle = "") +
        theme_classic()+
        labs(x="Season", y = "Character frequency")+
        scale_y_continuous(limits = c(0,100))
      
      
      z<-ggarrange(x, y + rremove("x.text"),common.legend=T, legend ="bottom",
                                   labels = c("A", "B"),
                                   ncol = 2, nrow = 1)
      z
      
#-------H2.1 Calculations (number of speech prompts)-END----------------------------------
