# CALCULATIONs PRE II
#prerequisites
# - download script executed
# - cleaning script executed
# - analyzing script executed
# - gender_role script executed
# - calculations_miraculous_pre1 executed

# Word Count & Questions

source("./main.R")
#-----------------------------------------
#linetable_gender_role
  for(i in 1:nrow(lineTable_gender_role)) { 
    #i=27
  lineTable_gender_role$ID<-1:nrow(lineTable_gender_role)
  linetext<-subset(lineTable_gender_role, ID == i, select = c(Text))
  linelenght<-length(strsplit(as.character(linetext), " ")[[1]])-1
  lineTable_gender_role$WCount[i] = linelenght
  } 

#dialogTable_gender_role
for(i in 1:nrow(dialogTable_gender_role)) { 
  #i=27
  #dialogTable_gender_role$ID<-1:nrow(dialogTable_gender_role)
  linetext<-subset(dialogTable_gender_role, ID == i, select = c(Text))
  linelenght<-length(strsplit(as.character(linetext), " ")[[1]])-1
  dialogTable_gender_role$WCount[i] = linelenght
} 
#---Questions

#extract only questions
#questionLines <- lineTable_gender_role[which(grepl('\\?$',lineTable$Text)),]
#table(questionLines$Gender)
#view(questionLines)

#add questions column to dialogTable_gender_role
    for(j in 1:nrow(dialogTable_gender_role)) { 
      #j=29
      linetext<-subset(dialogTable_gender_role, ID == j, select = c(Text)) #get id und text
      #question_yn<-length(strsplit(as.character(linetext), " ")[[1]])-1 #ist "?" in text enthalten
      #question_yn<-which(grepl('\\?$',linetext))
      question_yn<-str_count(linetext, '\\?$')
      #dialogTable_gender_role$Question[j] = "question"
      if (question_yn >= 1) {
        #print("question_yes")
        dialogTable_gender_role$Question[j] = "question_yes"
      } else {
        #print("question_no")
        dialogTable_gender_role$Question[j] = "question_no"
      }
      
    } 

#add questions column to line_gender_role
for(j in 1:nrow(lineTable_gender_role)) { 
  #j=29
  linetext<-subset(lineTable_gender_role, ID == j, select = c(Text)) #get id und text
  #question_yn<-length(strsplit(as.character(linetext), " ")[[1]])-1 #ist "?" in text enthalten
  #question_yn<-which(grepl('\\?$',linetext))
  question_yn<-str_count(linetext, '\\?$')
  #dialogTable_gender_role$Question[j] = "question"
  if (question_yn >= 1) {
    #print("question_yes")
    lineTable_gender_role$Question[j] = "question_yes"
  } else {
    #print("question_no")
    lineTable_gender_role$Question[j] = "question_no"
  }
  
} 



#---aufräumen
  rm(linelenght, linetext, i, j, question_yn)

#----END OF WORD COUNT ------------------------
  
