#problem_done: ♫<i>Ooooooooh</i>♫
#line <- c("♫<i>Ooooooooh</i>♫")
#problem_done: <u>is</u>
#line <- c("<u>is</u>")
#problem_done: <i>Mi-mi-mi-mi-mi!</i> 
#line <- c("<i>Mi-mi-mi-mi-mi!</i>")
#problem_done: ♫
#line <- c("♫")
#problem_done: ♫ Today's the big day!  Just got to chill the Duck Liver Pâté.  I'll show him that I'm a star with my yummy take on caviar. ♫ 
#line <- c("♫ Today's the big day!  Just got to chill the Duck Liver Pâté.  I'll show him that I'm a star with my yummy take on caviar. ♫") 
#problem_done: [[es:De Vuelta al Negro/Transcripción]]
#line <- c("[[es:De Vuelta al Negro/Transcripción]]")
#problem_done: [[Category:The Loud House Season 2 Scripts]]
#line <- c("[[Category:The Loud House Season 2 Scripts]]")
#problem_done: [pops up, holding a holly berry]
#line <- c("[pops up, holding a holly berry]")
#problem_done: [[id:A Fridge Too Far/Transkrip]]
#line <- c("[[id:A Fridge Too Far/Transkrip]]")
#problem_done: {{DEFAULTSORT:Bug's Strife, A/Script}}
#line <- c("{{DEFAULTSORT:Bug's Strife, A/Script}}")
#problem_done:<br/>
line <- c("Note: Scenes cut out from certain reruns are highlighted in <font color=red>red</font>.<br/>Police Officer: Police! Open up!<br/><br/>")


line
line <- gsub("\\[[^\\]]+\\]]", "", line, perl= TRUE) # delete line in double in [[brackets]]
line <- gsub("\\{[^\\}]+\\}}", "", line, perl= TRUE) # delete every line in {Single brackets} '- funzt
line <- gsub("<br/>", "", line)
line <- gsub(" / ", "", line)
#" / " 11 Louds A Leapin'.txt

#<br/> Head Poet's Anxiety.txt, Friendzy


line <- gsub("<u>", "", line) 
line <- gsub("</u>", "", line)
line <- gsub("♫<i>", "", line) 
line <- gsub("</i>♫", "", line)
line <- gsub("<i>", "", line) 
line <- gsub("</i>", "", line)
line <- gsub("\\♫ ", "", line)
line <- gsub("\\♫", "", line) #zuletzt


#line <- gsub("\\[\\[[^\\♫^\\]]+\\♫", "",line, perl = TRUE) # make [[x|y]] to y]]
#line <- gsub("\\♫[^\\♫]+\\♫", "", line, perl= TRUE) # delete every line in {Single brackets} '- funzt
#ok
##line1 <- gsub("es:[^\\n]+\\n", "x", line1, perl= TRUE) # delete every line in {Single brackets} '- funzt
#line1 <- gsub("es:[$Transcripción]+Transcripción", "x", line1, perl= TRUE) # delete every line in {Single brackets} '- funzt
#line1 <- gsub("-n", "1", line)
#line1 <- gsub("\\<[^\\>]+\\>", " ", line, perl= TRUE) # nicht so schlecht - songs
#line <- gsub("\\♫", "", line) #zuletzt
#line <- gsub("\\<[^\\♫]+\\♫", "", line, perl= TRUE) # delete evertin in {Single brackets} '- nicht so schlecht - songs
#<table> </table>
line



#song1 - ok
#line <- gsub("\\<[^\\>]+\\>", "", line, perl= TRUE) # nicht so schlecht - songs
#line <- gsub("\\♫", "", line)
