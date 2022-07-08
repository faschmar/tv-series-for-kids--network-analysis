#Mittelwerte pro gruppe ausgeben
dialogTable_gender_role %>%
  group_by(Gender_From, Gender_To, Season) %>%
  get_summary_stats(Sentiment, type = "mean_sd")

dialogTable_gender_role %>%
  group_by(Season) %>%
  get_summary_stats(Sentiment, type = "mean_sd")

#plots davon
plot_gender_from <- ggboxplot(dialogTable_gender_role, x = "Gender_From", y = "Sentiment", add = "point")
plot_gender_from
plot_gender_to <- ggboxplot(dialogTable_gender_role, x = "Gender_To", y = "Sentiment", add = "point")
plot_gender_to


#plots
#barplot (in colours)
#sentiment
neutral <- length(which(dialogTable_gender_role$Sentiment >= -0.1 & dialogTable_gender_role$Sentiment <= 0.1))
positive <- length(which(dialogTable_gender_role$Sentiment > 0.1))
negative <- length(which(dialogTable_gender_role$Sentiment< -0.1))
#neutral <- length(which(dialogTable_gender_role$Sentiment == 0))
#positive <- length(which(dialogTable_gender_role$Sentiment > 0))
#negative <- length(which(dialogTable_gender_role$Sentiment< -0))
Sentiment <- c("Negative","Neutral","Positive")
Lines <- c(negative,neutral,positive)
output <- data.frame(Sentiment,Lines)
output$Sentiment<-factor(output$Sentiment,levels=Sentiment)
ggplot(output, aes(x=Sentiment,y=Lines))+
  geom_bar(stat = "identity", aes(fill = Sentiment))+
  ggtitle("Barplot of all Sentiment-scores over all seasons")
#vader
neutral <- length(which(dialogTable_gender_role$Vader == 0))
positive <- length(which(dialogTable_gender_role$Vader > 0))
negative <- length(which(dialogTable_gender_role$Vader< 0))
Vader <- c("Negative","Neutral","Positive")
Lines <- c(negative,neutral,positive)
output <- data.frame(Vader,Lines)
output$Sentiment<-factor(output$Vader,levels=Vader)
ggplot(output, aes(x=Vader,y=Lines))+
  geom_bar(stat = "identity", aes(fill = Vader))+
  ggtitle("Barplot of all Vader-scores over all seasons")


plot(dialogTable_gender_role$Sentiment,pch=16)
ggplot(dialogTable_gender_role,aes(x=Gender_To, y=Sentiment))+geom_point()


#questions?

linetext<-c("Hello")
line_is_question<-str_extract_all(linetext, "(?<=^|\\?|\\.|!| )\\w[a-zA-Z ]+\\?")
line_is_question<-stringr::str_match_all(linetext, "(?<=\\s|^)([\\w\\s\\d\\(\\)\\[\\];:,]+\\?)")[[1]][,2]


contains_question_mark(linetext, convert_NA_to_FALSE = TRUE)


if linetext.endswith("?"):
  print (linetext`, "is a question")

import re
p = re.compile( r"\w+\?\s*" )
if p.search( question_text ):
  print `question_text`, "contains a question"

stringr::str_count(linetext, "\\?")

