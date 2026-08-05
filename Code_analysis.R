#Import packages
install.packages("tm")
install.packages("NLP")
install.packages("stringr")
install.packages("topicmodels")
install.packages("tidytext")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("SnowballC")
install.packages("textstem")
install.packages("reshape2")
install.packages("readxl")
install.packages("stringr")
install.packages("gsub") #doesn't work

#Import libraries
library(tm)
library(NLP)
library(stringr)
library(topicmodels)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(SnowballC)


library(textstem)
library(reshape2)
library(readxl)
library(stringr)
library(lexicon)
library(gsub) #doesn't work

#Import dataset

data <- read.csv("Translated_data_cleaned4.csv", sep = ";")
View(data)

# replace strange characters



df <- data.frame(Changes = data$Changes)

#df <- data.frame(data) |>
#  filter(Filter == 1) |>   #filter by the comments that were classified as relevant
#  select(comment)




#Lowercase words
df$Changes <- tolower(df$Changes)



#remove single words 
df$Changes <- gsub(pattern = "\\b[A-z]\\b{1}", replace = " ", df$Changes) 
#Remove whitespace
df$Changes <- stripWhitespace(df$Changes)
#Lematize terms in its dictionary form
custom_dict <- hash_lemmas
custom_dict["parking"] <- "parking"  # Add exception for "parking"
#df$Changes <- lemmatize_strings(df$Changes, dictionary = lexicon::hash_lemmas)
df$Changes <- lemmatize_strings(df$Changes, dictionary = custom_dict)

#Remove much
df$Changes <- gsub(pattern = "much", replace = " ", df$Changes) 

#Remove little
df$Changes <- gsub(pattern = "little", replace = " ", df$Changes) 

#Remove especially
df$Changes <- gsub(pattern = "especially", replace = " ", df$Changes) 

#Remove even
df$Changes <- gsub(pattern = "even", replace = " ", df$Changes) 

#Remove make
df$Changes <- gsub(pattern = "make", replace = " ", df$Changes) 

#Remove NA
df$Changes <- gsub(pattern = "\\bNA\\b", replace = " ", df$Changes)

# Remove words for bigrams

adicional_stopwords <- c("don", stopwords("en"))
text_bigram <- removeWords(df$Changes, adicional_stopwords)

head(text_bigram)

#############################################################################

#Bigrams

install.packages("quanteda")
install.packages("igraph")
install.packages("ggraph")
install.packages("tidyverse")
install.packages("tidyr")
install.packages("broom")
install.packages("tidytext")


library(quanteda)
library(igraph)
library(ggraph)
library(tidyverse)
library(tidyr)
library(broom)
library(tidytext)
library(lexicon)


#Create dataframe
df_corpus <- data.frame(text_bigram)

#Create bigrams by separating words in sequences of 2. 
bigrams_df <- df_corpus %>%
  unnest_tokens(output = bigram,
                input = text_bigram,
                token = "ngrams",
                n = 2)

#Count bigrams
bigrams_df %>%
  count(bigram, sort = TRUE)

btm <- bigrams_df %>%
  count(bigram, sort = TRUE)


#Remove stopwords in case it wasn't in the beginning.
#data("stop_words") 

#Separate words into two columns
bigrams_separated <- bigrams_df %>%
  separate(bigram, c("word1", "word2"), sep = " ")

#Remove stopwords
#bigrams_filtered <- bigrams_separated %>%
#  filter(!word1 %in% stop_words$word) %>%
#  filter(!word2 %in% stop_words$word)

#Count the number of times two words are always together
bigram_counts <- bigrams_separated %>%
  count(word1, word2, sort = TRUE)

#Create network of bigrams

bigram_network <- bigram_counts %>%
  filter(n > 11) %>% #filter for the most common combinations of bigrams that appear at least 15 times.
  graph_from_data_frame()


set.seed(2016)

a <- grid::arrow(type = "closed", length = unit(.06, "inches"))

ggraph(bigram_network, layout = "fr") +   
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.03, 'inches')) +
  geom_node_point(color = "orange", size = 1) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 0.3, size = 3) +
  theme_void()

##############################################################################
# Create biterm topic model
install.packages("data.table")
install.packages("udpipe")
install.packages("dplyr")
install.packages("BTM")


library(data.table)
library(udpipe)
library(dplyr)
library(BTM)

#Arrange document to format required for the udpipe 

df_tm <- mutate(df, doc_id =row_number())

colnames(df_tm) <- c("text", "doc_id")

df_tm <- df_tm |> 
  relocate(doc_id, text)

#Data cleaning

#Remove punctuation
df_tm$text <- gsub(pattern = "\\W", replace = " ", df_tm$text)
#Take out urls
df_tm$text <- str_replace_all(df_tm$text, "(http\\S+)", "")
#Lowercase words
df_tm$text <- tolower(df_tm$text)
#remove single words 
df_tm$text <- gsub(pattern = "\\b[A-z]\\b{1}", replace = " ", df_tm$text) 
#Lematize terms in its dictionary form
#df_tm$text <- lemmatize_strings(df_tm$text, dictionary = lexicon::hash_lemmas)

custom_dict <- hash_lemmas
custom_dict["parking"] <- "parking"  # Add exception for "parking"
#df$Changes <- lemmatize_strings(df$Changes, dictionary = lexicon::hash_lemmas)
df_tm$text <- lemmatize_strings(df_tm$text, dictionary = custom_dict)

#Remove much
df_tm$text <- gsub(pattern = "much", replace = " ", df_tm$text) 

#Remove little
df_tm$text <- gsub(pattern = "little", replace = " ", df_tm$text) 

#Remove especially
df_tm$text <- gsub(pattern = "especially", replace = " ", df_tm$text) 

#Remove even
df_tm$text <- gsub(pattern = "even", replace = " ", df_tm$text) 

#Remove make
df_tm$text <- gsub(pattern = "make", replace = " ", df_tm$text) 



adicional_stopwords <- c("don, wasn", stopwords("en"))

#remove stopwords
df_tm$text <- removeWords(df_tm$text, adicional_stopwords)



#Remove whitespace
df_tm$text <- stripWhitespace(df_tm$text)


biterm_data_tm <- udpipe(df_tm, "english", trace = 10)
biterms <- as.data.table(biterm_data_tm)
biterms <- biterms[, cooccurrence(x = lemma,
                                  relevant = upos %in% c("NOUN",
                                                         "ADJ",
                                                         "PROPN") & 
                                    !lemma %in% stopwords("en"),
                                  skipgram = 5),
                   by = list(doc_id)]

# Build BTM
set.seed(588)
traindata <- subset(biterm_data_tm, upos %in% c("NOUN", "ADJ", "PROPN"))
traindata <- traindata[, c("doc_id", "lemma")]
model <- BTM(traindata, k = 4, 
             beta = 0.01, 
             iter = 1000,
             window = 15,
             biterms = biterms, 
             trace = 100)

## Inspect the model - topic frequency + conditional term probabilities
model$theta

topicterms <- terms(model, top_n = 25)
topicterms <- data.frame(topicterms)


#plot topicterms
install.packages("textplot")
install.packages("ggraph")
install.packages("concaveman")
library(textplot)
library(ggraph)
library(concaveman)
plot(model, top_n = 20, title ="",
     labels = paste(round(model$theta *
                            100, 2), "%", sep = ""))

plot(model, top_n = 25, title ="",
     labels = c("Urban traffic dynamics (33.44 %)","Public space and maintenance (20.22 %)", "Public transport accessibility (27.41 %)","Neighbourhoods and social dynamics (18.92 %)", "Public transport accessibility (27.41 %)"))

model$theta

topicterms <- terms(model, top_n = 25)
topicterms <- data.frame(topicterms)

head(topicterms)

model$theta

#coherence score
#terms_matrix <- terms(model, top_n = 10)
#doc_topic_probs <- predict(model, newdata = dtm, type = "mix")$topics

install.packages("text2vec")
library(text2vec)

# Extract the top terms for each topic
top_terms <- terms(model, top_n = 10)  # Adjust top_n as needed
#top_terms_list <- lapply(1:ncol(top_terms), function(i) top_terms[, i])

top_terms_list <- lapply(top_terms, function(df) df$token)
# Preprocess the original text data to match the BTM model requirements
preprocessed_text <- df_tm$text  # Use your cleaned and processed text data

# Create a Document-Term Matrix (DTM) using text2vec
vectorizer <- text2vec::vocabulary(preprocessed_text) %>%
  text2vec::prune_vocabulary(term_count_min = 1)

# Tokenize the preprocessed text
tokens <- text2vec::itoken(preprocessed_text, progressbar = FALSE)

# Create vocabulary
vocab <- text2vec::create_vocabulary(tokens)

# Prune vocabulary (if needed)
vocab <- text2vec::prune_vocabulary(vocab, term_count_min = 10)

# Create a Document-Term Matrix (DTM)
dtm <- text2vec::create_dtm(it = tokens, 
                            vectorizer = text2vec::vocab_vectorizer(vocab))

dtm <- text2vec::create_dtm(it = text2vec::itoken(preprocessed_text, progressbar = FALSE), 
                            vectorizer = text2vec::vocab_vectorizer(vectorizer))

# Compute coherence score for each topic
#coherence_scores <- sapply(top_terms_list, function(topic_terms) {
#  text2vec::topic_coherence(topic_terms, dtm, top_n = 10)
#})


top_terms_mat <- do.call(cbind, top_terms_list)
colnames(top_terms_mat) <- paste0("topic_", seq_len(ncol(top_terms_mat)))


vocab <- create_vocabulary(tokens)
vectorizer <- vocab_vectorizer(vocab)

tcm <- create_tcm(tokens, vectorizer, skip_grams_window = 15L)

coherence_scores <- text2vec::coherence(
  x = top_terms_mat,
  tcm = tcm,
  n_doc_tcm = length(preprocessed_text),
  smooth = 1e-6
)
# Average coherence score across all topics
average_coherence <- mean(coherence_scores)

# Output coherence scores and average
coherence_scores
average_coherence





#Coherence score via UMass coherence
library(Matrix)

dtm_bin <- dtm
dtm_bin@x[dtm_bin@x > 0] <- 1

topic_umass_coherence <- function(topic_terms, dtm_bin, eps = 1) {
  topic_terms <- intersect(topic_terms, colnames(dtm_bin))
  
  if (length(topic_terms) < 2) return(NA_real_)
  
  scores <- c()
  
  for (m in 2:length(topic_terms)) {
    w_m <- topic_terms[m]
    docs_wm <- dtm_bin[, w_m] > 0
    
    for (l in 1:(m - 1)) {
      w_l <- topic_terms[l]
      docs_wl <- dtm_bin[, w_l] > 0
      
      d_wl <- sum(docs_wl)
      d_wm_wl <- sum(docs_wm & docs_wl)
      
      score <- log((d_wm_wl + eps) / d_wl)
      scores <- c(scores, score)
    }
  }
  
  mean(scores)
}

btm_coherence <- sapply(top_terms_list, topic_umass_coherence, dtm_bin = dtm_bin)

btm_coherence
mean(btm_coherence, na.rm = TRUE)

# Word cloud --------------------------------------------------------------

install.packages("wordcloud")
library(wordcloud)
install.packages("RColorBrewer")
library(RColorBrewer)
install.packages("wordcloud2")
library(wordcloud2)

install.packages("dplyr")
library(dplyr)

install.packages("tm")
library(tm)

#Create a vector containing only the text
text <- data$Changes

# Create a corpus  
docs <- Corpus(VectorSource(text))

#clean text data
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))

extra_stopwords <- c("due", "currently", "can", "also", "none", "cant", "get", "still", "etc", "often", "like", "instead", "much", "dont", "now", "will", "just", "know", "idea", "think")
docs <- tm_map(docs, removeWords, extra_stopwords)

#create document-term-matrix
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df_cloud <- data.frame(word = names(words),freq=words)

#Generate word cloud
set.seed(1234) # for reproducibility 
wordcloud(words = df_cloud$word, freq = df_cloud$freq, min.freq = 3,           max.words=100, random.order=FALSE, rot.per=0.35,            colors=brewer.pal(8, "Dark2"))

View(df_cloud)



# LDA ---------------------------------------------------------------------

install.packages(c("topicmodels", "slam"))
library(topicmodels)
library(slam)
k <- length(top_terms_list)
k
set.seed(1234)

# controle: documents met minstens 1 term
row_totals <- Matrix::rowSums(dtm)
valid_docs <- row_totals > 0

table(valid_docs)

dtm_nonempty <- dtm[valid_docs, ]
dtm_lda <- slam::as.simple_triplet_matrix(dtm_nonempty)

#dtm_lda <- slam::as.simple_triplet_matrix(dtm)




lda_model <- LDA(
  dtm_lda,
  k = k,
  method = "Gibbs",
  control = list(
    seed = 1234,
    burnin = 1000,
    iter = 2000,
    thin = 100
  )
)

lda_top_terms_mat <- terms(lda_model, 10)
lda_top_terms_list <- lapply(seq_len(ncol(lda_top_terms_mat)), function(i) lda_top_terms_mat[, i])

lda_top_terms_list

library(Matrix)

dtm_bin <- dtm_nonempty
dtm_bin@x[dtm_bin@x > 0] <- 1

topic_umass_coherence <- function(topic_terms, dtm_bin, eps = 1) {
  topic_terms <- intersect(topic_terms, colnames(dtm_bin))
  
  if (length(topic_terms) < 2) return(NA_real_)
  
  scores <- c()
  
  for (m in 2:length(topic_terms)) {
    w_m <- topic_terms[m]
    docs_wm <- dtm_bin[, w_m] > 0
    
    for (l in 1:(m - 1)) {
      w_l <- topic_terms[l]
      docs_wl <- dtm_bin[, w_l] > 0
      
      d_wl <- sum(docs_wl)
      
      if (d_wl == 0) next  
      
      d_wm_wl <- sum(docs_wm & docs_wl)
      
      score <- log((d_wm_wl + eps) / d_wl)
      scores <- c(scores, score)
    }
  }
  
  if (length(scores) == 0) return(NA_real_)
  
  mean(scores)
}

lda_coherence <- sapply(
  lda_top_terms_list,
  topic_umass_coherence,
  dtm_bin = dtm_bin
)

lda_coherence
mean(lda_coherence, na.rm = TRUE)






btm_coherence
mean(btm_coherence, na.rm = TRUE)

comparison <- data.frame(
  model = c("BTM", "LDA"),
  mean_umass_coherence = c(
    mean(btm_coherence, na.rm = TRUE),
    mean(lda_coherence, na.rm = TRUE)
  )
)

comparison

topic_comparison <- rbind(
  data.frame(model = "BTM", topic = seq_along(btm_coherence), coherence = btm_coherence),
  data.frame(model = "LDA", topic = seq_along(lda_coherence), coherence = lda_coherence)
)

topic_comparison