# CALCULATIONs IX - H2.2 Calculations (number of questions asked (sending/receiving per gender)
#prerequisites
# - download script executed
# - cleaning script executed
# - analyzing script executed
# - gender_role script executed
# - calculations_miraculous_pre I executed
# - calculations_miraculous_pre II executed
# - desc-calculations_miraculous_descriptives executed

# H2.3 Calculations (number of questions
source("./main.R")
#---------------------------------------------------------------------------------------------
#H2.3 Number of questions asked (sending/receiving per gender)
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------
#Questions per gender_From
    #from gender
      desc_questions_by_gender_from<-dialogTable_gender_mf %>% 
        group_by(Gender_From, Question) %>%
        summarize(Frequency=n())%>% arrange(desc(Frequency))
      desc_questions_by_gender_from<-desc_questions_by_gender_from[order(desc_questions_by_gender_from$Gender_From),]
      view(desc_questions_by_gender_from)

    #crosstab + chi2
      desc_questions_by_gender_from<-xtabs(~dialogTable_gender_mf$Gender_From + dialogTable_gender_mf$Question)
      desc_questions_by_gender_from
      summary(desc_questions_by_gender_from)
      #there is a difference - but between only female & male in lines overall (question_yes + question_no)

    #between gender&questions
      chi2_intermediate = structure(list(A = c(8551L, 6922L), B = c(1469L, 1298L)),
                                    .Names = c("Is_Question: No", "Is_Question: Yes"), class = "data.frame", row.names = c("female", "male"))
      chisq.test(chi2_intermediate)
      chisq.posthoc.test(chi2_intermediate)

#to gender
      desc_questions_by_gender_to<-dialogTable_gender_mf %>% 
        group_by(Gender_To, Question) %>%
        summarize(Frequency=n())%>% arrange(desc(Frequency))
      desc_questions_by_gender_to<-desc_questions_by_gender_to[order(desc_questions_by_gender_to$Gender_To),]
      view(desc_questions_by_gender_to)

    #crosstab + chi2
      desc_questions_by_gender_to<-xtabs(~dialogTable_gender_mf$Gender_To + dialogTable_gender_mf$Question)
      desc_questions_by_gender_to
      summary(desc_questions_by_gender_to)
      #there is a difference

    #between groups gender&questions - all groups differ
    chi2_intermediate = structure(list(A = c(8383L, 7090L), B = c(1630L, 1137L)),
                                  .Names = c("Yes", "No"), class = "data.frame", row.names = c("female", "male"))
    chisq.test(chi2_intermediate)
    chisq.posthoc.test(chi2_intermediate)

    #table
      colnames(desc_questions_by_gender_to)<-c("Gender_To","Question (Y/N)", "No. of Lines")
      
      chi2_intermediate %>% #table: Gender_to*question 
        gt(rownames_to_stub = T, rowname_col = "name"
        ) %>% 
        gt_theme_espn()%>%
        tab_row_group(label = "Gender being asked", rows = c("female", "male")) %>%
        tab_spanner(label = "Line is a question", columns = c("Yes", "No")) %>%
        tab_header(title = md("**Questions**"), subtitle = md("Number of lines by gender_received*question"))
      #or
      tbl_summary(desc_questions_by_gender_to)
    

#from/to gender
    desc_questions_by_gender_both<-dialogTable_gender_mf %>% 
      group_by(Gender_From, Gender_To, Question) %>%
      summarize(Frequency=n())%>% arrange(desc(Frequency))
    desc_questions_by_gender_both<-desc_questions_by_gender_both[order(desc_questions_by_gender_both$Gender_From, desc_questions_by_gender_both$Gender_To),]
    view(desc_questions_by_gender_both)

    #delete Question_No - so there are only 4 groups
      desc_questions_by_gender_both<-desc_questions_by_gender_both[desc_questions_by_gender_both$Question != "question_no", ]

    #crosstab + chi2
      desc_questions_by_gender_both<-xtabs(~dialogTable_gender_mf$Gender_From + dialogTable_gender_mf$Gender_To)
      desc_questions_by_gender_both
      summary(desc_questions_by_gender_both)
      #there is a difference between groups

    #between gender&questions - all groups differ
      chi2_intermediate = structure(list(A = c(5123L, 4890L), B = c(4897L, 3330L)),
                                    .Names = c("towards female", "towards male"), class = "data.frame", row.names = c("by female", "by male"))
      chisq.test(chi2_intermediate)
      chisq.posthoc.test(chi2_intermediate)
      #all groups differ!

    #table
      desc_questions_by_gender_both$Question<-NULL
      colnames(desc_questions_by_gender_both)<-c("Gender_From","Gender_To", "No. of Questions")
      desc_questions_by_gender_both

      chi2_intermediate %>% #table: question between gender (From/To)
        gt(rownames_to_stub = T, rowname_col = "name") %>% 
        gt_theme_espn() %>%
        tab_row_group(label = "Question asked", rows = c("by female", "by male")) %>%
        tab_spanner(label = "Question received", columns = c("towards female", "towards male")) %>%
        tab_header(title = md("**Questions**"), subtitle = md("Number of questions asked/received by gender"))
      
      #or
      tbl_summary(desc_questions_by_gender_both)
#---------------------------------------------------------------------------------------------

#-------H2.2 Calculations-END----------------------------------
