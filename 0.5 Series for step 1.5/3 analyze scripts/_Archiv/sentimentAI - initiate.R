# Load the package
library(sentiment.ai)
library(SentimentAnalysis)
library(sentimentr)
library(data.table)

#require(sentiment.ai)
#require(SentimentAnalysis)
#require(sentimentr)
#library(data.table)



# Only if it's your first ever time
#install_sentiment.ai(
#  envname = "r-sentiment-ai",
#  method = c("conda"),
#  gpu = FALSE,
#  python_version = "3.8.10",
#  modules = list(numpy = "1.19.5", sentencepiece = "0.1.95", tensorflow = "2.4.1",
#                 tensorflow_hub = "0.12.0", `tensorflow-text` = "2.4.3"),
#  fresh_install = TRUE,
#  restart_session = TRUE
#)


# Initiate the model: This will create the sentiment.ai.embed model
# Do this so it can be reused without recompiling - especially on GPU!
init_sentiment.ai(
  model = c("en.large"),
  #model = c(multi.large"),
  #model = c("en"), #less ressources
  #model = c("multi"), #less ressources
  envname = "r-sentiment-ai",
  method = c("conda"),
  silent = FALSE
)

