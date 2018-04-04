
###### reading the data
answers <- read.csv("D:/Ashkan/Projects/iZone Proposal/Demo/Results/SentimentAnalysis/Sentiment_Answers.csv", header = TRUE)
questions <- read.csv("D:/Ashkan/Projects/iZone Proposal/Demo/Results/SentimentAnalysis/Sentiment_Questions.csv", header = TRUE)
tags <- read.csv("D:/Ashkan/Projects/iZone Proposal/Data/test data/test_Tags.csv", header = TRUE)
######

# load the NLP library to do text mining
library(tm)

#########################
## Preprocessing Answers
#########################
#inspect a particular document in corpus
#  writeLines(as.character(dfCorpus[[30]]))

preprocess <- function(text){
  # create a corpus from the given answers
  dfCorpus = Corpus(VectorSource(text)) 

  #Transform to lower case
  dfCorpus <-tm_map(dfCorpus, content_transformer(tolower))
  
  #remove potentially problematic symbols
  toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, "", x))})
  dfCorpus <- tm_map(dfCorpus, toSpace, "-")
  dfCorpus <- tm_map(dfCorpus, toSpace, "'")
  dfCorpus <- tm_map(dfCorpus, toSpace, "`")
  dfCorpus <- tm_map(dfCorpus, toSpace, "\"")
  
  #remove punctuation
  dfCorpus <- tm_map(dfCorpus, removePunctuation)
  #Strip digits
  dfCorpus <- tm_map(dfCorpus, removeNumbers)
  #remove stopwords
  dfCorpus <- tm_map(dfCorpus, removeWords, stopwords("english"))
  #remove whitespace
  dfCorpus <- tm_map(dfCorpus, stripWhitespace)
  
  return(dfCorpus)
}

answers_corpus <- preprocess(answers$CleanBody)
questions_corpus <- preprocess(questions$CleanBody)

#Good practice to check every now and then
writeLines(as.character(answers_corpus[[30]]))
writeLines(as.character(questions_corpus[[30]]))


####### saving the preprocessed body
for(i in 1:nrow(answers)) {
  answers[i, "PreprocessBody"] <- as.character(answers_corpus[[i]])
}
for(i in 1:nrow(questions)) {
  questions[i, "PreprocessBody"] <- as.character(questions_corpus[[i]])
}
#######

######## Write the results to CSV
write.csv(answers, file = "D:/Ashkan/Projects/iZone Proposal/Demo/Results/SentimentAnalysis/2_Sentiment_answers.csv", row.names=FALSE)
write.csv(questions, file = "D:/Ashkan/Projects/iZone Proposal/Demo/Results/SentimentAnalysis/2_Sentiment_questions.csv", row.names=FALSE)
########


###############################
### creating the document-term matrix
### the input to LDA
###############################
#####Create document-term matrix
answers_dtm <- DocumentTermMatrix(answers_corpus)
questions_dtm <- DocumentTermMatrix(questions_corpus)

#convert rownames to answer ids
rownames(answers_dtm) <- answers$Id
rownames(questions_dtm) <- questions$Id

#collapse matrix by summing over columns
answers_freq <- colSums(as.matrix(answers_dtm))
questions_freq <- colSums(as.matrix(questions_dtm))

#length should be total number of terms
length(answers_freq)
length(questions_freq)

#create sort order (descending)
answers_ord <- order(answers_freq, decreasing=TRUE)
questions_ord <- order(questions_freq, decreasing=TRUE)
#List all terms in decreasing order of freq and write to disk
answers_freq[answers_ord]
questions_freq[questions_ord]

write.csv(answers_freq[answers_ord], "D:/Ashkan/Projects/iZone Proposal/Demo/Results/SentimentAnalysis/3_WordFreq_answers.csv")
write.csv(questions_freq[questions_ord], "D:/Ashkan/Projects/iZone Proposal/Demo/Results/SentimentAnalysis/3_WordFreq_questions.csv")
####

####creating term matrix with TF-IDF weighting
answers_tfidf <-DocumentTermMatrix(answers_corpus,control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))
questions_tfidf <-DocumentTermMatrix(questions_corpus,control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))

#convert rownames to answer ids
rownames(answers_tfidf) <- answers$Id
rownames(questions_tfidf) <- questions$Id

#collapse matrix by summing over columns
answers_freq <- colSums(as.matrix(answers_tfidf))
questions_freq <- colSums(as.matrix(questions_tfidf))

#length should be total number of terms
length(answers_freq)
length(questions_freq)

#create sort order (descending)
answers_ord <- order(answers_freq, decreasing=TRUE)
questions_ord <- order(questions_freq, decreasing=TRUE)
#List all terms in decreasing order of freq and write to disk
answers_freq[answers_ord]
questions_freq[questions_ord]

write.csv(answers_freq[answers_ord], "D:/Ashkan/Projects/iZone Proposal/Demo/Results/SentimentAnalysis/3_TFIDF_answers.csv")
write.csv(questions_freq[questions_ord], "D:/Ashkan/Projects/iZone Proposal/Demo/Results/SentimentAnalysis/3_TFIDF_questions.csv")
#####
#or compute cosine distance among documents
#dissimilarity(answers_tfidf, method = "cosine")
################################


##################################
#### Finding the Optimal Number of Topics
##################################
# minimization:
#   Arun2010 [1]
#   CaoJuan2009 [2]
# maximization:
#   Deveaud2014 [3]
#   Griffiths2004 [4,5]

#install.packages("ldatuning")
library("ldatuning")

#load topic models library
library(topicmodels)

answers_NoTopics <- FindTopicsNumber(
  answers_dtm,
  topics = seq(from = 2, to = 15, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
)

questions_NoTopics <- FindTopicsNumber(
  questions_dtm,
  topics = seq(from = 2, to = 15, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
)

FindTopicsNumber_plot(answers_NoTopics)  ### 12 to 15 topics for answers
FindTopicsNumber_plot(questions_NoTopics) ### 13 to 15 topics for questions

write.csv(answers_NoTopics, "D:/Ashkan/Projects/iZone Proposal/Demo/Results/SentimentAnalysis/4_NoTopics_answers.csv")
write.csv(questions_NoTopics, "D:/Ashkan/Projects/iZone Proposal/Demo/Results/SentimentAnalysis/4_NoTopics_questions.csv")
##################################



##################################
#### APPROACH-1:  Topic Modeling
##################################
#Set parameters for Gibbs sampling
burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE

#Number of topics
k <- 13

# remove rows with zero entry
#rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
#dtm.new   <- dtm[rowTotals> 0, ]           #remove all docs without words
##

#Run LDA using Gibbs sampling
answers_ldaOut <- LDA(answers_dtm,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
questions_ldaOut <- LDA(questions_dtm,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))

#write out results
############docs to topics
answers_ldaOut.topics <- as.matrix(topics(answers_ldaOut))
questions_ldaOut.topics <- as.matrix(topics(questions_ldaOut))

for(i in 1:nrow(answers)) {
  answers[i, "LDA_Topic"] <- answers_ldaOut.topics[i]
}
for(i in 1:nrow(questions)) {
  questions[i, "LDA_Topic"] <- questions_ldaOut.topics[i]
}

write.csv(answers, "D:/Ashkan/Projects/iZone Proposal/Demo/Results/SentimentAnalysis/5_GibbsLDA13_answers.csv")
write.csv(questions, "D:/Ashkan/Projects/iZone Proposal/Demo/Results/SentimentAnalysis/5_GibbsLDA13_questions.csv")

#top 30 terms in each topic
answers_ldaOut.terms <- as.matrix(terms(answers_ldaOut,30))
questions_ldaOut.terms <- as.matrix(terms(questions_ldaOut,30))
write.csv(answers_ldaOut.terms, "D:/Ashkan/Projects/iZone Proposal/Demo/Results/SentimentAnalysis/6_GibbsLDA13_TopicTerms_answers.csv")
write.csv(questions_ldaOut.terms, "D:/Ashkan/Projects/iZone Proposal/Demo/Results/SentimentAnalysis/6_GibbsLDA13_TopicTerms_questions.csv")

#probabilities associated with each topic assignment
answers_topicProbabilities <- as.data.frame(answers_ldaOut@gamma)
questions_topicProbabilities <- as.data.frame(questions_ldaOut@gamma)
#convert rownames to answer ids
rownames(answers_topicProbabilities) <- answers$Id
rownames(questions_topicProbabilities) <- questions$Id

write.csv(answers_topicProbabilities, "D:/Ashkan/Projects/iZone Proposal/Demo/Results/SentimentAnalysis/7_GibbsLDA13_TopicProbabilities_answers.csv")
write.csv(questions_topicProbabilities, "D:/Ashkan/Projects/iZone Proposal/Demo/Results/SentimentAnalysis/7_GibbsLDA13_TopicProbabilities_questions.csv")
#############

############ Find relative importance of top 2 topics
answers_topic1ToTopic2 <- lapply(1:nrow(answers_dtm),function(x)
  sort(answers_topicProbabilities[x,])[k]/sort(answers_topicProbabilities[x,])[k-1])
#write to file
# write.csv(topic1ToTopic2,file=paste("LDAGibbs",k,"Topic1ToTopic2.csv"))
# write.csv(topic2ToTopic3,file=paste("LDAGibbs",k,"Topic2ToTopic3.csv"))
############

############### Find topic tags for generated topics
questions_topics <- read.csv("D:/Ashkan/Projects/iZone Proposal/Demo/Results/SentimentAnalysis/5_GibbsLDA13_questions.csv", header = TRUE)
questions_topics_terms <- read.csv("D:/Ashkan/Projects/iZone Proposal/Demo/Results/SentimentAnalysis/6_GibbsLDA13_TopicTerms_questions.csv", header = TRUE)

tags <- tags[tags$Id %in% questions_topics$Id,]
questions_topic_ids <- questions_topics[, c("Id", "LDA_Topic")]

questions_topic_tags <- merge(questions_topic_ids, tags)
write.csv(questions_topic_tags, "D:/Ashkan/Projects/iZone Proposal/Demo/Results/SentimentAnalysis/9_TopicsTags_questions.csv")

questions_topic_tags <- questions_topic_tags[, c("LDA_Topic", "Tag")]

# remove duplicates
questions_topic_tags <- questions_topic_tags[!duplicated(questions_topic_tags), ]
write.csv(questions_topic_tags, "D:/Ashkan/Projects/iZone Proposal/Demo/Results/SentimentAnalysis/9_TopicsTags_NoDuplicates_questions.csv")
###############
###############################################################################



#####################################################
### APPROACH-2: TF-IDF and Similarity
#####################################################
### check and apply tf-idf 
#install.packages("lsa")
library('lsa')

doc_similarity <- function(corpus, docIDs, query_ratio = 0.01, tfidf = FALSE,
                           removePunctuation = TRUE, removeNumbers = TRUE, stopwords = TRUE){
  
  if(tfidf){
    tdm <- TermDocumentMatrix(corpus, control = list(weighting =
                                                       function(x)
                                                         weightTfIdf(x, normalize = FALSE),
                                                     removePunctuation = removePunctuation, 
                                                     removeNumbers = removeNumbers,
                                                     stopwords = stopwords))
  }
  else{
    tdm <- TermDocumentMatrix(corpus, control = list(removePunctuation = removePunctuation, 
                                                     removeNumbers = removeNumbers,
                                                     stopwords = stopwords))
  }
  
  tdm_mat = as.matrix(tdm)
  colnames(tdm_mat) = docIDs #answers$Id     #c(names(news_list),names(queries_list))
  
  #normalizing the term document matrix
  tfidf_mat <- scale(tdm_mat, center = FALSE,scale = sqrt(colSums(tdm_mat^2)))
  
  N.query <- length(docIDs) * query_ratio  #10
  N.docs <- length(docIDs) - N.query #990
  
  #seperating query tdm matrix and content tdm matrix
  query.vectors <- tfidf_mat[, (N.docs + 1):(N.docs+N.query)]
  tfidf_mat <- tfidf_mat[, 1:N.docs]
  
  #calculating the similarity scores
  doc.scores <- t(query.vectors) %*% tfidf_mat
  
  return(data.frame(doc.scores)) #querylist = queries_list,doc.scores)
}

# tf
answers_tf_doc_similarity <- doc_similarity(answers_corpus, answers$Id, query_ratio = 0.012)
questions_tf_doc_similarity <- doc_similarity(questions_corpus, questions$Id, query_ratio = 0.05)
write.csv(answers_tf_doc_similarity, "D:/Ashkan/Projects/iZone Proposal/Demo/Results/SentimentAnalysis/8_tf_docSimilarities_answers.csv")
write.csv(questions_tf_doc_similarity, "D:/Ashkan/Projects/iZone Proposal/Demo/Results/SentimentAnalysis/8_tf_docSimilarities_questions.csv")

# tfidf
answers_tfidf_doc_similarity <- doc_similarity(answers_corpus, answers$Id, query_ratio = 0.012, tfidf = TRUE)
questions_tfidf_doc_similarity <- doc_similarity(questions_corpus, questions$Id, query_ratio = 0.05, tfidf = TRUE)
write.csv(answers_tfidf_doc_similarity, "D:/Ashkan/Projects/iZone Proposal/Demo/Results/SentimentAnalysis/8_tfidf_docSimilarities_answers.csv")
write.csv(questions_tfidf_doc_similarity, "D:/Ashkan/Projects/iZone Proposal/Demo/Results/SentimentAnalysis/8_tfidf_docSimilarities_questions.csv")

#https://feeds.feedburner.com/DataPerspective
#####################################################

# library(slam)
# cosine_sim <- tcrossprod_simple_triplet_matrix(answers_dtm, questions_dtm)/sqrt(row_sums(answers_dtm^2) %*% t(row_sums(questions_dtm^2)))
# test <- as.data.frame(answers_dtm)
# 
# install.packages('proxy') # Let's be honest, you've never heard of this before.
# library('proxy') # Library of similarity/dissimilarity measures for 'dist()'
# test <- dist(inspect(answers_dtm), method="cosine")
# 
# test <- inspect(answers_dtm[1:3, 1:7630])
# 
# library(tm)
# tm::dissimilarity(answers_dtm, method = "cosine")

#test_tdm$v[1]

#temp <- inspect(test_tdm[, 1:2])
#temp <- as.matrix(test_tdm[1:7630, 1:3])
###########################################################################################
###########################################################################################




###############################################################################
############# Word Cloud
###############################################################################
# Load
#library("SnowballC")
library("wordcloud")
library("RColorBrewer")

answers_termfreq <- read.csv("D:/Ashkan/Projects/iZone Proposal/Demo/Results/SentimentAnalysis/3_WordFreq_answers.csv", header = TRUE)
colnames(answers_termfreq) <- c("word", "freq")
questions_termfreq <- read.csv("D:/Ashkan/Projects/iZone Proposal/Demo/Results/SentimentAnalysis/3_TFIDF_questions.csv", header = TRUE)
colnames(questions_termfreq) <- c("word", "freq")

wcloud <- function(df, min.freq = 1){
  set.seed(1234)
  wordcloud(words = df$word, freq = df$freq, min.freq = min.freq,
            max.words=200, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"))
}

wcloud(answers_termfreq)
wcloud(questions_termfreq)
################################################################################



















