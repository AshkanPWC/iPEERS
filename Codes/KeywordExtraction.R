###### reading the data
questions <- read.csv("D:/Ashkan/Projects/iZone Proposal/Demo/Results/SentimentAnalysis/Sentiment_Questions.csv", header = TRUE)
tags <- read.csv("D:/Ashkan/Projects/iZone Proposal/Data/test data/test_Tags.csv", header = TRUE)
######

library(udpipe)
library(textrank)


## 1) annotate the text

questions$KeywordBody <- gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", questions$Body)
cleanFun <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}
questions$KeywordBody <- cleanFun(questions$Body)


ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)
x <- udpipe_annotate(ud_model, x = questions$KeywordBody)
x <- as.data.frame(x)


## 2) Extracting only nouns
stats <- subset(x, upos %in% "NOUN")
stats <- txt_freq(x = stats$lemma) #lemmatization
stats <- stats[-c(12, 56, 119),]   # remove weird scripts

library(lattice)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 30), col = "cadetblue", main = "Most occurring nouns", xlab = "Freq")

### write the file
write.csv(stats, file = "D:/Ashkan/Projects/iZone Proposal/Demo/Results/KeywordExtraction/1_KE_FrequentNouns_questions.csv", row.names=FALSE)
###


## 3) Collocation & co-occurrences
# Collocation (words following one another)
# stats <- keywords_collocation(x = x, 
#                               term = "token", group = c("doc_id", "paragraph_id", "sentence_id"),
#                               ngram_max = 4)
# ## Co-occurrences: How frequent do words occur in the same sentence, in this case only nouns or adjectives
# stats <- cooccurrence(x = subset(x, upos %in% c("NOUN", "ADJ")), 
#                       term = "lemma", group = c("doc_id", "paragraph_id", "sentence_id"))

## Co-occurrences: How frequent do words follow one another
stats <- cooccurrence(x = x$lemma, 
                      relevant = x$upos %in% c("NOUN", "ADJ"))
stats <- stats[-c(1, 2, 3, 9, 15, 26, 43, 95, 96),]   # remove weird scripts

### write the file
write.csv(stats, file = "D:/Ashkan/Projects/iZone Proposal/Demo/Results/KeywordExtraction/2_KE_FrequentNounsFollow_questions.csv", row.names=FALSE)
###
## Co-occurrences: How frequent do words follow one another even if we would skip 2 words in between
# stats <- cooccurrence(x = x$lemma, 
#                       relevant = x$upos %in% c("NOUN", "ADJ"), skipgram = 2)


#install.packages("ggraph")
library(igraph)
library(ggraph)
library(ggplot2)
wordnetwork <- head(stats, 100)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "pink") +
  geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
  theme_graph(base_family = "Arial Narrow") +
  theme(legend.position = "none") +
  labs(title = "Cooccurrences, words follow one another", subtitle = "Nouns & Adjective (Top-100)")
###


## 4) Textrank (word network ordered by Google Pagerank)
library(wordcloud)
library("RColorBrewer")
stats <- textrank_keywords(x$lemma, 
                           relevant = x$upos %in% c("NOUN", "ADJ"), 
                           ngram_max = 3, sep = " ")
stats <- subset(stats$keywords, ngram > 1 & freq >= 3)
stats <- stats[-c(1, 2, 4, 7, 24, 25, 26),]   # remove weird scripts
wordcloud(words = stats$keyword, freq = stats$freq, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Set1"))
### write the file
write.csv(stats, file = "D:/Ashkan/Projects/iZone Proposal/Demo/Results/KeywordExtraction/3_KE_ngrams2_questions.csv", row.names=FALSE)
###
###


## 5) Rapid Automatic Keyword Extraction: RAKE
stats <- keywords_rake(x = x, 
                       term = "token", group = c("doc_id", "paragraph_id", "sentence_id"),
                       relevant = x$upos %in% c("NOUN", "ADJ"),
                       ngram_max = 4)
stats <- stats[-c(1, 5, 24, 26, 54),]   # remove weird scripts
stats <- subset(stats, freq > 2)

wordcloud(words = stats$keyword, freq = stats$freq, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Paired"))
### write the file
write.csv(stats, file = "D:/Ashkan/Projects/iZone Proposal/Demo/Results/KeywordExtraction/4_KE_RAKE_ngramMax4_questions.csv", row.names=FALSE)
###


### 6) Frequent Phrases
## Simple noun phrases (a adjective+noun, pre/postposition, optional determiner and another adjective+noun)
x$phrase_tag <- as_phrasemachine(x$upos, type = "upos")
stats <- keywords_phrases(x = x$phrase_tag, term = x$token, 
                          pattern = "(A|N)+N(P+D*(A|N)*N)*", 
                          is_regex = TRUE, ngram_max = 4, detailed = FALSE)
stats <- subset(stats, ngram > 2)
head(subset(stats, ngram > 2))
###


### 7) Use dependency parsing output to get the nominal subject and the adjective of it
stats <- merge(x, x, 
               by.x = c("doc_id", "paragraph_id", "sentence_id", "head_token_id"),
               by.y = c("doc_id", "paragraph_id", "sentence_id", "token_id"),
               all.x = TRUE, all.y = FALSE, 
               suffixes = c("", "_parent"), sort = FALSE)
stats <- subset(stats, dep_rel %in% "nsubj" & upos %in% c("NOUN") & upos_parent %in% c("ADJ"))
stats$term <- paste(stats$lemma_parent, stats$lemma, sep = " ")
stats <- txt_freq(stats$term)

wordcloud(words = stats$key, freq = stats$freq, min.freq = 2, max.words = 100,
          random.order = FALSE, colors = brewer.pal(6, "Dark2"))
###


















