#########################################################################
### Prediction of Online Engagement for Sports Apparel Industry  ########
###               Extracting keywords from Comments                   ###
#########################################################################

#### TO BE RUN ONLY AFTER TWEET_EXTRACTION_FINAL IS RUN #################

setwd("C:/Users/moetoKompiutarche/Documents/IESEG/Social Media Analytics/Group Project")

# Read in the final basetable to perform topic analysis on the tweets 
tmls_topic <- read.csv('basetable.csv', stringsAsFactors = FALSE)

# installing all the packages needed
for (i in c('SnowballC','slam','tm','RWeka','Matrix', 'NLP', 'rJava', 'openNLPdata', 'dplyr')){
  if (!require(i, character.only=TRUE)) install.packages(i, repos = "http://cran.us.r-project.org")
  require(i, character.only=TRUE)
}

#### CLEANING DATA AND CORPUS
# Change the enconding for later use in udpipe
comments_text <- sapply(tmls_topic$text,function(x) iconv(x, 'utf8', 'ascii', ""))

# Transform data frame into a corpus to use tm package
comments_topic <- VCorpus(VectorSource(comments_text))

# Function to to use transfmer to remove any extra information we wont use
gsubtransfo <- content_transformer(function(x,from, to) gsub(from, to, x))
# Remove the mentions "@..."
comments_topic <- tm_map(comments_topic, gsubtransfo, "@\\w+",  "")
# Remove the links "https\\....."
comments_topic <- tm_map(comments_topic, gsubtransfo, "http[s]?:\\/\\/(\\/|[[:alnum:]]|\\.)+","")
# Remove hashtags
comments_topic <- tm_map(comments_topic, gsubtransfo, "#\\w+",  "")
# Remove all the punctuations
comments_topic <- tm_map(comments_topic, removePunctuation)
# Remove all the numbers
comments_topic <- tm_map(comments_topic, removeNumbers)
# Remove the white space
comments_topic <- tm_map(comments_topic, stripWhitespace)
# Convert to lowercase
comments_topic <- tm_map(comments_topic,content_transformer(tolower))

# Transform the clean corpus into a data.frame
myDf <- data.frame(text=unlist(sapply(comments_topic, `[`, "content")), 
                        stringsAsFactors=F)

# Create ID with the index
if (!require("zoo")) install.packages("zoo", quiet=TRUE) ; require("zoo")
myDf$document <- index(myDf)

# Install packages
if (!require("udpipe")) install.packages("udpipe", quiet=TRUE) ; require("udpipe")
if (!require("stringr")) install.packages("stringr", quiet=TRUE) ; require("stringr")


#### Lematization: use the udpipe package to annotate each word of the tweets
# Use first two lines for initial download; use third line for every subsequent run to save time
ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)
#ud_model <- udpipe_load_model("english-ewt-ud-2.3-181115.udpipe")

x <- udpipe_annotate(ud_model, x = myDf$text, doc_id = myDf$document)
x <- as.data.frame(x)

#### Combine lemmas for each document and calculate metrics for basetable
lemmatized <- x %>%
  group_by(doc_id) %>%
  summarize(text_lemma = paste0(lemma, collapse = " "),
            word_total = n() - length(upos[upos == "PUNCT"]) - length(upos[upos == "X"]),
            noun_freq = length(upos[upos == "NOUN"])/word_total,
            verb_freq = length(upos[upos == "VERB"])/word_total)

lemmatized$doc_id <- as.numeric(gsub("doc", "", lemmatized$doc_id))
lemmatized <- arrange(lemmatized, doc_id)

#### Now that lemmas have been extracted, remove stop words from enlgish dictionnay + some words that will not be informative for the topic analysis
forremoval <- c(stopwords('english'),"justdoit","heretocreate","uff","flexweave","wewill","uf","iwill","uufef","bemorehuman",
                "curry","tweetus","feelthefloatride","amp","ua","omnitalk","week","columbia","nano","ufe","ufa",
                "phantom","unleashchao","reebok","cop")

# Ensure that negators are not removed
forremoval <- forremoval[which(!forremoval %in% c("no", "not", "nor"))]

keep_tf <- logical(nrow(x))

for (stop_w in 1:nrow(x)){
  if (x$token[stop_w] %in% forremoval){
    keep_tf[stop_w] <- FALSE
  }else{
    keep_tf[stop_w] <- TRUE
  }
}

x_no_stop <- x[keep_tf, ]

#### Write x to csv for use in keywords
write.csv(x_no_stop, file = "tweet_lemmas.csv")

## Define the identifier at which we will build a topic model
x_no_stop$topic_level_id <- unique_identifier(x_no_stop, fields = c("doc_id", "paragraph_id", "sentence_id"))
## Get a data.frame with 1 row per id/lemma
# keeping only the noun for the purpose of the topic analysis
dtf <- subset(x_no_stop, upos %in% c("NOUN", "VERB"))
dtf <- document_term_frequencies(dtf, document = "topic_level_id", term = "lemma")
head(dtf)


# convert back to a corpus
dtf_corp <- VCorpus(VectorSource(dtf))

#### CREATING DTM

## Create a document term matrix (dtm) for the topic modelling
dtm <- document_term_matrix(x = dtf)

## Remove words which do not occer very often (remove words that occurs less than 50 times)
dtm_clean <- dtm_remove_lowfreq(dtm, minfreq = 50)
#head(dtm_colsums(dtm_clean))
# keep only term with high tfidf
dtm_clean <- dtm_remove_tfidf(dtm_clean, top = 50)

#### RUNNING LDA MODEL

# Install and load the topicmodels package in R to be able to use the LDA model
if (!require("topicmodels")) install.packages("topicmodels", quiet=TRUE) ; require("topicmodels")

# Tune to get the optimal number of topics
if (!require("ldatuning")) install.packages("ldatuning", quiet=TRUE) ; require("ldatuning")

result <- FindTopicsNumber(
  dtm_clean,
  topics = seq(from = 2, to = 11, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
)

# Plot the results
FindTopicsNumber_plot(result)

# Use the LDA method and Gibbs sampling with 5 topics 
tweets_lda <- LDA(dtm, k = 5,method="gibbs",control = list(nstart = 5, burnin = 2000, best = TRUE, seed = 2:6) )
tweets_lda

# Use tidytext, we are getting the words that determine the topic (represented by Beta value)
if (!require("tidytext")) install.packages("tidytext", quiet=TRUE) ; require("tidytext")
tweet_topics <- tidy(tweets_lda, matrix = "beta")

# Top terms per topic
top_tweet_terms <- tweet_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_tweet_terms

# Visualise the top terms per topics
if (!require("ggplot2")) install.packages("ggplot2", quiet=TRUE) ; require("ggplot2")

topics <- top_tweet_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
topics

# Now that we have the words per topic, we will determine the topics per tweet(documents) represented by gamma value
tweet_documents <- tidy(tweets_lda, matrix = "gamma")
tweet_documents

# Get the highest gamma value for each doc id(tweets)
tweet_classifications <- tweet_documents %>%
  group_by(document) %>%
  top_n(1, gamma) %>%
  ungroup() %>%
  arrange(gamma)
tweet_classifications

# Spread the data to have one columne per topic (5 columns)
if (!require("tidyr")) install.packages("tidyr", quiet=TRUE) ; require("tidyr")
tweets_topic_basetable <- spread(tweet_classifications,topic, gamma)
tweets_topic_basetable$document <- as.numeric(tweets_topic_basetable$document)
tweets_topic_basetable <- tweets_topic_basetable[order(tweets_topic_basetable$document),]

# Update column names
topic_col_names <- character(ncol(tweets_topic_basetable)-1)

for (i in 1:ncol(tweets_topic_basetable) - 1){
  topic_col_names[i] <- paste0("topic_", i)
}

colnames(tweets_topic_basetable)[2:ncol(tweets_topic_basetable)] <- topic_col_names

# Create dummy variable, 1 if topic present within document, 0 otherwise
for (col_n in topic_col_names){
  tweets_topic_basetable[,col_n] <- ifelse(!is.na(tweets_topic_basetable[,col_n]), 1, 0)
}


#### Merge the lemmatized text with the topic analysis
topic_final <- merge(lemmatized,tweets_topic_basetable, by.x = "doc_id", by.y = "document")
topic_final <- merge(tmls_topic, topic_final, by.x = "X", by.y = "doc_id")

# Export to CSV
write.csv(topic_final, file = "basetable_topic.csv")
