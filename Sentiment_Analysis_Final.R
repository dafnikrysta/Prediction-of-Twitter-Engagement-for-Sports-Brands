#########################################################################
### Prediction of Online Engagement for Sports Apparel Industry  ########
###                    Sentiment Analysis                             ###
#########################################################################

#### TO BE RUN ONLY AFTER TOPIC_ANALYSIS_FINAL IS RUN #################

# Set working directory to save intermediate tweet_basetable at the end
setwd("C:/Users/moetoKompiutarche/Documents/IESEG/Social Media Analytics/Group Project")

# Load required packages
for (i in c('XML','textreadr','rvest', 'udpipe')){
  if (!require(i, character.only=TRUE)) install.packages(i, repos = "http://cran.us.r-project.org")
  require(i, character.only=TRUE)
}

# Read in basetable and sentiment dictionary
tmls_SA <- read.csv('basetable_topic.csv', stringsAsFactors = FALSE)
dictionary <- read.csv("C:/Users/moetoKompiutarche/Documents/GitHub/SMA/Data/SentimentDictionary.csv", stringsAsFactors = FALSE)

# Remove automatic and extraneous index value
tmls_SA <- tmls_SA[,-1]

# Scrap online emoji dictionary
webpage <- read_html(URLdecode("http://kt.ijs.si/data/Emoji_sentiment_ranking/index.html"))

Emoji_html <- html_nodes(webpage,'td:nth-child(3)')
Emoji <- html_text(Emoji_html)

Emoji_Sentiment_html <- html_nodes(webpage,'td:nth-child(9)')
Emoji_Sentiment <- html_text(Emoji_Sentiment_html)

# Convert emoji unicode codepoint to Utf-8
Emoji_Utf8 <- character(length(Emoji))
for (i in 1:length(Emoji)){
  Emoji_Utf8[i] <- intToUtf8(as.integer(Emoji[i]))
}

# Convert sentiment to numeric and to dictionary scale
Emoji_Sentiment <- as.numeric(Emoji_Sentiment) * 5 + 5

# Create emoji dictionary
# enc2native function needed to ensure that characters in emoji_dict$Word are readable by R
emoji_dict <- data.frame(Word = enc2native(Emoji_Utf8), VALENCE = Emoji_Sentiment, stringsAsFactors = FALSE)

# Write emoji dictionary to CSV in case the website ever goes down
#write.csv(emoji_dict, file = "emoji_dict.csv")

# Remove tabs and mentions to ensure space between emojis and words for tokenization
tmls_SA$text <- gsub("\n", " ", tmls_SA$text)
tmls_SA$text <- gsub("@\\w+", " ", tmls_SA$text)

### Match words and emojis (in Utf-8) to sentiment
sentimentScore <- numeric(length(tmls_SA$text))

# Transform dictionary for lemmas to lowercase
dictionary$Word <- tolower(dictionary$Word)

# The correct code is this
for (i in 1:length(tmls_SA$text)){
  
  ## Find sentiment of emojis first
  # Split tweets into tokens
  token_emo <- strsplit(tmls_SA$text[i],split=" ")[[1]]
  
  # Determine which emojis are in the dictionary
  emo <- match(token_emo, emoji_dict$Word)
  present_emo <- !is.na(emo)
  
  # Find the valence of the emojis that are present
  wordvalences_emo <- emoji_dict$VALENCE[emo[present_emo]]
  
  ## Find sentiment for lemmas, including negators
  # Split tweets into tokens
  token_m <- strsplit(tmls_SA$text_lemma[i],split=" ")[[1]]
  
  # Initiate the neg vector and set the first value to 1 as there is no previous value and thus there cannot be a negative modifier
  neg <- c(1)
  
  # Determine if the previous word is negative modifier and add the values to the vector
  if (length(token_m) > 1){
    for (n in 2: length(token_m)) {
      if (token_m[n-1] %in% c("no", "not", "nor")) {
        neg[n] <- -1
      } else {
        neg[n] <- 1
      }
    }
  }
  
  # Determine which words are in the dictionary
  m <- match(token_m, dictionary$Word)
  present_m <- !is.na(m)
  
  # Find the valence of the words that are present
  wordvalences_m <- dictionary$VALENCE[m[present_m]]
  
  neg_avail <- neg[present_m]
  wordvalences_m <- wordvalences_m*neg_avail
  
  # Compute the mean valence of the entire comment
  wordvalences <- c(wordvalences_emo, wordvalences_m)
  sentimentScore[i] <- mean(wordvalences, na.rm=TRUE)
  
  ## Increase intensity (by 20%) based on availability of "!"
  if ('!' %in% token_m){
    sentimentScore[i] <- sentimentScore[i] * 1.2
  }
  
  #handle the case when none of the words is in the dictionary
  if (is.na(sentimentScore[i])) sentimentScore[i] <- 0 else sentimentScore[i] <- sentimentScore[i]
  
}

### Add sentiment score to tweet basetable
tmls_SA$sentiment_analysis <- sentimentScore

# Export updated basetable to CSV
write.csv(tmls_SA, file = "basetable_topic_SA.csv")


