text <- c(
  #"<center> Transformation Sequence <br />Marinette: Tikki, spots on! Ha! </center>",
  #"<table><tr valign="top"><td nowrap>Alya: </td><td nowrap>Alya's on the line, I rise and shine<br />When I pick up the mic like this game is mine<br />Fake MC's can't catch my flow<br />'Cause I whip up the beat with each word I throw!</td></tr></table> ",
  #"<!-- Rapping template: <table><tr valign="top"><td nowrap>Timetagger: </td><td nowrap>  </td></tr></table> -->",
  #"<table><tr valign="top"><td nowrap>Timetagger: </td><td nowrap>Yo-yo! I'm Timetagger from the future, man<br />I criss-cross time with a spraypaint can<br />Looks like your time's up at last<br />'Cause I'm gonna send you way back to the past</td></tr></table> ",
  "♫I only want them to be mine, mine, mine.Dreaming about them in the nights!You know I love unicornsAnd nothing makes me feel better! ♫",
  "I only want them to be mine, mine, mine.Dreaming about them in the nights!You know I love unicornsAnd nothing makes me feel better! ",
  "the resturant served human flesh",
  "the resturant is my favorite!",
  "the resturant is my favourite!",
  "this restront is my FAVRIT innit!",
  "the resturant was my absolute favorite until they gave me food poisoning",
  "This fantastic app freezes all the time!",
  "I learned so much on my trip to Hiroshima museum last year!",
  "What happened to the people of Hiroshima in 1945",
  "I had a blast on my trip to Nagasaki",
  "The blast in Nagasaki",
  "I love watching scary horror movies",
  "This package offers so much more nuance to sentiment analysis!",
  "you remind me of the babe. What babe? The babe with the power! What power? The power of voodoo. Who do? You do. Do what? Remind me of the babe!",
  "may the force be with you"
)

# sentiment.ai
sentiment.ai.score <- sentiment_score(text)

# From Sentiment Analysis
sentimentAnalysis.score <- analyzeSentiment(text)$SentimentQDAP

# From sentimentr
sentimentr.score <- sentiment_by(get_sentences(text), 1:length(text))$ave_sentiment


example_en <- data.table(target = text, 
                      sentiment.ai = sentiment.ai.score,
                      sentimentAnalysis = sentimentAnalysis.score,
                      sentimentr = sentimentr.score)