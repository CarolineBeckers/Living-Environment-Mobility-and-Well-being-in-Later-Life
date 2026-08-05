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

#select neighbourhoods from cluster 2

library(dplyr)

selected_rows <- data %>%
  filter(Neighbourhood %in% c("Brugse Poort  Rooigem", "Watersportbaan  Ekkergem", "Oud Gentbrugge", "Ledeberg", "Gentbrugge", "MoscouVogelhoek","Mariakerke", "Nieuw Gent  UZ" ))

View(selected_rows)
# replace strange characters



df <- data.frame(Comments = selected_rows$Comments_neighbourhood)



library(dplyr)

# Keep rows where not all columns are empty
df <- df %>% filter(rowSums(. == "") < ncol(.))





head(df, n = 15)

#df <- data.frame(data) |>
#  filter(Filter == 1) |>   #filter by the comments that were classified as relevant
#  select(comment)

colnames(df)  # Check if 'Comments_Ghent' is a valid column name
summary(df$Comments)  # Check the contents of the column
head(df$Comments, n=90)

#Finding invalid characters such as ë
for (i in seq_along(df$Comments)) {
  tryCatch({
    tolower(df$Comments[i])
  }, error = function(e) {
    cat("Error at row:", i, " - ", df$Comments[i], "\n")
  })
}





#Lowercase words
df$Comments <- tolower(df$Comments)



#remove single words 
df$Comments <- gsub(pattern = "\\b[A-z]\\b{1}", replace = " ", df$Comments) 
#Remove white spaces
df$Comments <- stripWhitespace(df$Comments)
#Lematize terms in its dictionary form
custom_dict <- hash_lemmas
custom_dict["parking"] <- "parking"  # Add exception for "parking"
#df$Changes <- lemmatize_strings(df$Changes, dictionary = lexicon::hash_lemmas)
df$Comments <- lemmatize_strings(df$Comments, dictionary = custom_dict)

#Remove much
df$Comments <- gsub(pattern = "much", replace = " ", df$Comments) 

#Remove little
df$Comments <- gsub(pattern = "little", replace = " ", df$Comments) 

#Remove especially
df$Comments <- gsub(pattern = "especially", replace = " ", df$Comments) 

#Remove even
df$Comments <- gsub(pattern = "even", replace = " ", df$Comments) 

#Remove make
df$Comments <- gsub(pattern = "make", replace = " ", df$Comments) 

#Remove NA
df$Comments <- gsub(pattern = "\\bNA\\b", replace = " ", df$Comments)

# Remove words for bigrams

adicional_stopwords <- c("don", stopwords("en"))
text_bigram <- removeWords(df$Comments, adicional_stopwords)

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

#Remove De Lijn
df_tm$text <- gsub(pattern = "de lijn", replace = "delijn", df_tm$text) 

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
model <- BTM(traindata, k = 3, 
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

head(topicterms, n = 10)

model$theta

#coherence score
#terms_matrix <- terms(model, top_n = 10)
#doc_topic_probs <- predict(model, newdata = dtm, type = "mix")$topics

install.packages("text2vec")
library(text2vec)

# Extract the top terms for each topic
top_terms <- terms(model, top_n = 10)  # Adjust top_n as needed
top_terms_list <- lapply(1:ncol(top_terms), function(i) top_terms[, i])

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
vocab <- text2vec::prune_vocabulary(vocab, term_count_min = 1)

# Create a Document-Term Matrix (DTM)
dtm <- text2vec::create_dtm(it = tokens, 
                            vectorizer = text2vec::vocab_vectorizer(vocab))

dtm <- text2vec::create_dtm(it = text2vec::itoken(preprocessed_text, progressbar = FALSE), 
                            vectorizer = text2vec::vocab_vectorizer(vectorizer))

# Compute coherence score for each topic
coherence_scores <- sapply(top_terms_list, function(topic_terms) {
  text2vec::topic_coherence(topic_terms, dtm, top_n = 10)
})

# Average coherence score across all topics
average_coherence <- mean(coherence_scores)

# Output coherence scores and average
coherence_scores
average_coherence

