#########################################################################
### Prediction of Online Engagement for Sports Apparel Industry  ########
###               Extracting keywords from Comments                   ###
#########################################################################

#### TO BE RUN ONLY AFTER SENTIMENT_ANALYSIS_FINAL IS RUN ###############

### Install & load the required packages and data

for (i in c('udpipe','textrank','wordcloud','lattice', 'ggplot2')){
  if (!require(i, character.only=TRUE)) install.packages(i, repos = "http://cran.us.r-project.org")
  require(i, character.only=TRUE)
}

setwd("C:/Users/moetoKompiutarche/Documents/IESEG/Social Media Analytics/Group Project")

# Read in basetable and sentiment dictionary
basetable <- read.csv('basetable_topic_SA.csv', stringsAsFactors = FALSE)
tweet_lemmas <- read.csv('tweet_lemmas.csv', stringsAsFactors = FALSE)

# Remove automatic and extraneous index value
basetable <- basetable[,-1]

# Checking different approaches to keyword extraction. We are beginning with extracting 
# the most common nouns, followed by word networks using the TextRank algorithm.

### A. Extracting keywords based on the nouns (From the Parts of Speech Tagging)

stats_1 <- subset(tweet_lemmas, upos %in% "NOUN")
stats_1 <- txt_freq(x = stats_1$lemma)
stats_1$key <- factor(stats_1$key, levels = rev(stats_1$key))

stats_1 <- stats_1[c(1, 3:2426),]

p<-ggplot(data=head(stats_1,30), aes(y=freq, x = key)) +
  geom_bar(stat="identity")
p + coord_flip() + labs(
  x="Term", y = "Frequency") + theme_minimal() +  ggtitle("Most frequently occuring Nouns") + theme(plot.title =element_text(color="black", hjust =0.5,size=12, face = 'bold'))



### B. Extracting keywords using TextRank package.The TextRank algorithm summarises text and extracts keywords by constructing a word network. 
# On top of that network the 'Google Pagerank' algorithm is applied to extract relevant words. 
# Relevant words which are following one another are combined to get keywords. 


stats_2 <- textrank_keywords(tweet_lemmas$lemma, 
                           relevant = tweet_lemmas$upos %in% c("NOUN", "ADJ"), 
                           ngram_max = 8, sep = " ")
stats_2 <- subset(stats_2$keywords, ngram > 1 & freq >= 4)

set.seed(121)
wordcloud(words = stats_2$keyword, freq = stats_2$freq)


### Create master list and check frequency in tweets
# Concatenating keywords into a character vector.
keywords <- c(as.character(stats_1[1:20,1]), stats_2[1:20,1])

# Checking whether each tweet contains a keyword and counting the number of keywords.
# The result (count) is added into a new column named keyword_count.

basetable$keyword_count <- numeric(nrow(basetable))
count <- numeric(nrow(basetable))

for(i in 1:nrow(basetable)){
    count[i] <- sum(str_detect(basetable$text[i], keywords))
    basetable$keyword_count[i]<- count[i]}

# Creating relative frequency of keywords
basetable$keyword_freq <- basetable$keyword_count/basetable$word_total

# Creating another variable indicating the presence/absence  of keywords in a tweet.
# (0 indicates absence/ 1 indicates presence).

basetable$keyword_dummy<- ifelse(basetable$keyword_count == 0, 0, 1)


### Export to CSV
write.csv(basetable, file = "basetable_topic_SA_keyword.csv")


