
###### reading the data
answers <- read.csv("D:/Ashkan/Projects/iZone Proposal/Data/test data/test_Answers.csv", header = TRUE)
questions <- read.csv("D:/Ashkan/Projects/iZone Proposal/Data/test data/test_Questions.csv", header = TRUE)
tags <- read.csv("D:/Ashkan/Projects/iZone Proposal/Data/test data/test_Tags.csv", header = TRUE)
######

##################
# remove HTML tags and URLs
##################
#remove URLS
answers$CleanBody <- gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", answers$Body)
questions$CleanBody <- gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", questions$Body)

# remove HTML tags
cleanFun <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}
answers$CleanBody <- cleanFun(answers$Body)
questions$CleanBody <- cleanFun(questions$Body)
#################

########################################################
##### fix some errors, expand the stop-word list
########################################################
# #fix up 1) differences between us and aussie english 2) general errors
# docs <- tm_map(docs, content_transformer(gsub),
#                pattern = "organiz", replacement = "organ")
# 
# #define and eliminate all custom stopwords
# myStopwords <- c("can", "say", "great", "lot")
# docs <- tm_map(docs, removeWords, myStopwords)
# #inspect a document as a check
# writeLines(as.character(docs[[30]]))
########################################################


#####################################################
### Sentiment Analysis
#http://blog.kaggle.com/2017/10/05/data-science-101-sentiment-analysis-in-r-tutorial/
#####################################################
library(tidyverse)
library(tidytext)
library(glue)
library(stringr)

cleanText <- function(text){
  #Clean Text
  text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)","",text)
  text = gsub("http[^[:blank:]]+", "", text)
  text = gsub("@\\w+", "", text)
  text = gsub("[ \t]{2,}", "", text)
  text = gsub("^\\s+|\\s+$", "", text)
  text = gsub("\\n", "", text)
  text <- gsub('\\d+', '', text)
  text = gsub("[[:punct:]]", " ", text)
  
  return(text)
}

#GetSentiment(answers$Body[2])
GetSentiment <- function(text){
  #text <- cleanText(text)
  
  # tokenize
  tokens <- data_frame(text = text) %>% unnest_tokens(word, text)
  
  result <- merge(tokens, get_sentiments("bing"), by = "word") 
  result <- result %>%
    count(sentiment) # count the # of positive & negative words
  
  result <- t(result)
  colnames(result) = result[1, ] # the first row will be the header
  result = result[-1, ]      
  
  result["sentiment"] <- (as.integer(result["positive"]) - as.integer(result["negative"])) / (as.integer(result["positive"]) + as.integer(result["negative"]))
  return(as.double(result["sentiment"]))
}


########
#### apply it to all the rows in the dataset
## Answers
for(i in 1:nrow(answers)) {
  #row <- answers[i,]
  answers[i, "CleanBody"] <- cleanText(answers[i, "CleanBody"])
  answers[i, "Sentiment"] <- GetSentiment(answers[i, "CleanBody"])
}
##
## Questions
for(i in 1:nrow(questions)) {
  questions[i, "CleanBody"] <- cleanText(questions[i, "CleanBody"])
  questions[i, "Sentiment"] <- GetSentiment(questions[i, "CleanBody"])
}
##
########

######## Write the results to CSV
write.csv(answers, file = "D:/Ashkan/Projects/iZone Proposal/Demo/Results/SentimentAnalysis/Sentiment_answers.csv", row.names=FALSE)
write.csv(questions, file = "D:/Ashkan/Projects/iZone Proposal/Demo/Results/SentimentAnalysis/Sentiment_questions.csv", row.names=FALSE)
########


######## write to the SQL tables
#install.packages("sqldf")
#install.packages("XLConnect")
library(sqldf)

####### update 
#dbSendQuery(conn = db, "UPDATE test SET Location='test' WHERE SchID=1")

## Answers
for(i in 1:nrow(answers)) {
  db <- dbConnect(SQLite(), dbname="D:/Ashkan/Databases/iPEERS/iPEERS.db")
  
  print(answers[i, "Id"])
  updateQuery <- sprintf("UPDATE Answers SET Sentiment='%s', CleanBody='%s' WHERE Id=%d", 
                        answers[i, "Sentiment"], answers[i, "CleanBody"], answers[i, "Id"])
#  print(updateQuery)
  dbClearResult(dbSendQuery(conn = db, updateQuery))
  dbDisconnect(db) 
}
##

## Questions
for(i in 1:nrow(questions)) {
  db <- dbConnect(SQLite(), dbname="D:/Ashkan/Databases/iPEERS/iPEERS.db")
  
  print(questions[i, "Id"])
  updateQuery <- sprintf("UPDATE Questions SET Sentiment='%s', CleanBody='%s' WHERE Id=%d", 
                         questions[i, "Sentiment"], questions[i, "CleanBody"], questions[i, "Id"])
  #  print(updateQuery)
  dbClearResult(dbSendQuery(conn = db, updateQuery))
  dbDisconnect(db) 
}
##
##########
#dbListTables(db)
#dbDisconnect(db) 


