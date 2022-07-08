#song1♫
#line <- c('<table><tr valign="top"><td nowrap>Rose: ♫</td><td nowrap>I only want them to be mine, mine, mine.<br />Dreaming about them in the nights!<br />You know I love unicorns<br />And nothing makes me feel better! ♫</td></tr></table>')
#song2♫
#line <- c('♫ La Befana comes at night, on her shaky broom in flight. She gives candy to those who are dandy. ♫ ')
#song3 -rapping
#line <- c('<!-- Rapping template: <table><tr valign="top"><td nowrap>Timetagger: </td><td nowrap>  </td></tr></table> -->')

line1 <- c("es:Copi-Gato/Transcripción")
#pl:Kotowtór/Transkrypt
#fr:Le Chevalier Noir/Transcription

#line <- c('<center> Transformation Sequence <br> Cat Noir: Power up! </br> </center>')
  
line1
#line <- gsub("\\[\\[[^\\♫^\\]]+\\♫", "",line, perl = TRUE) # make [[x|y]] to y]]
#line <- gsub("\\{[^\\}]+\\}", "", line, perl= TRUE) # delete every line in {Single brackets} '- funzt
#ok
##line1 <- gsub("es:[^\\n]+\\n", "x", line1, perl= TRUE) # delete every line in {Single brackets} '- funzt
line1 <- gsub("es:[$Transcripción]+Transcripción", "x", line1, perl= TRUE) # delete every line in {Single brackets} '- funzt
#line1 <- gsub("-n", "1", line)
#line1 <- gsub("\\<[^\\>]+\\>", " ", line, perl= TRUE) # nicht so schlecht - songs
#line <- gsub("\\♫", "", line)
#line <- gsub("\\<[^\\♫]+\\♫", "", line, perl= TRUE) # delete evertin in {Single brackets} '- nicht so schlecht - songs
#<table> </table>
line1



#song1 - ok
#line <- gsub("\\<[^\\>]+\\>", "", line, perl= TRUE) # nicht so schlecht - songs
#line <- gsub("\\♫", "", line)
