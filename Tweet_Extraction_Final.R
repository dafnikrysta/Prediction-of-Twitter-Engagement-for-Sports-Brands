#########################################################################
### Prediction of Online Engagement for Sports Apparel Industry  ########
###        Tweet Extraction and Basetable Initialization              ###
#########################################################################

# Set working directory to save intermediate tweet_basetable at the end
setwd("C:/Users/moetoKompiutarche/Documents/IESEG/Social Media Analytics/Group Project")

# Load required packages
for (i in c('rtweet','lubridate','stringi','textclean', 'dplyr')){
  if (!require(i, character.only=TRUE)) install.packages(i, repos = "http://cran.us.r-project.org")
  require(i, character.only=TRUE)
}

#Connect to rtweet
create_token(
  app = "HashtagWordcloud",
  consumer_key = "aFHvBPQVP0zKvPeTbMjlTtkdd",
  consumer_secret = "l9QglsTRUwXpcRVQfDDGW9yQObNHruShNMaaHh6Yv1xLGoRr0C",
  access_token = "2830201371-LzuyGF3PKGMVX5XSg1tyXUgTxd8FmKZ19yG9lsV",
  access_secret = "D0eytQFYLjCgcILpi9blwisY2pXritTAv6S2jN9sDLJCV")

# Download Top Sports Apparel Industry Brands timeline of tweets, excluding replies
# Maximum allowed by Twitter is 3200 and replies filtered after the tweets have been collected
tmls <- get_timelines(c("Nike", "adidas", "Columbia1938", "UnderArmour", "Reebok"), n = 3200, exclude_replies = TRUE)

# Twitter refuses to give us anything older than August 26 2017 for Nike
# Could possibly do it with web scraping and rvest but is it ethical?
# https://www.analyticsvidhya.com/blog/2017/03/beginners-guide-on-web-scraping-in-r-using-rvest-with-hands-on-knowledge/
# tmls_origin <- get_timelines("Nike", n = 3200, exclude_replies = TRUE)
# tmls_origin_2 <- get_timelines("Nike", n = 3200, exclude_replies = TRUE, max_id = 901565055359008768)
# tmls_origin_3 <- get_timelines("Nike", n = 3200, exclude_replies = TRUE, max_id = 901565055359008768)

########### Data Exploration and Variable Creation  ###########

# Explore the variables and the first 20 rows
# View(tmls[1:20,])

# View distribution of brands
table(tmls$screen_name)

# Earliest tweet in database total and by brand
min(tmls$created_at)

# library("dplyr")
# tmls %>%
# group_by(screen_name) %>%
#   summarise(
#     min_date = min(created_at)
#   )

# Remove all non-english posts
tmls <- tmls[tmls$lang == "en",]

#### Type of Post
# 90 quoted retweets out of ~2700 tweets and retweets and convert to dummy
table(tmls$is_quote)
tmls$is_quote <- ifelse(tmls$is_quote == TRUE, 1, 0)

# 275 (not quoted) retweets out of ~2700 tweets and retweets and convert to dummy
table(tmls$is_retweet)
tmls$is_retweet <- ifelse(tmls$is_retweet == TRUE, 1, 0)

# The rest of the posts are original posts. No dummy variable created for this category as it is the last one


#### Media in Tweet
# Presence of url
tmls$url_present <- ifelse(!is.na(tmls$urls_url), 1, 0)

# Types of media
# Unfortunately, the dataframe returned does not specify which tweets have photos and which have videos
unique(tmls$media_type)

# Presence of video (753 posts with videos)
tmls$video <- ifelse(grepl("video", tmls$media_expanded_url) | grepl("video", tmls$media_url), 1, 0)
# dim(tmls[grepl("video", tmls$media_expanded_url) | grepl("video", tmls$media_url),])

# Presence of photo (1288 posts with photos)
tmls$photo <- ifelse(!is.na(tmls$media_url) & tmls$video == 0, 1, 0)
# dim(tmls[!is.na(tmls$media_url) & tmls$video == 0,])


#### Date Formating and Generalizing
# Determine day of the month
# Possibly make into dummy variables for regression?
tmls$WD <- weekdays(tmls$created_at)

weekday_names <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

for (i in 1:length(weekday_names)){
  tmls[ncol(tmls) + 1] <- ifelse(tmls$WD == weekday_names[i], 1, 0)
  colnames(tmls)[ncol(tmls)] <- weekday_names[i]
}

# Extract year of post
tmls$year <- year(tmls$created_at)

# Extract month of post
tmls$month <- months(tmls$created_at)
table(tmls$month)
month_names <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November")

# Create dummy variables for all months, except December
for (i in 1:length(month_names)){
  tmls[ncol(tmls) + 1] <- ifelse(tmls$month == month_names[i], 1, 0)
  colnames(tmls)[ncol(tmls)] <- month_names[i]
}

# Extract hour (UTC timezone)
tmls$hour <- hour(tmls$created_at)

# Extract time of day and create dummy variables
# 4 to 11:59 o'clock is morning, 12 to 19:59 is afternoon and 20 to 3:59 is the night
# Again, morning is not coded because it is the third out of three categories
tmls$afternoon <- ifelse(tmls$hour >= 12 & tmls$hour <= 19, 1, 0)
tmls$evening <- ifelse(tmls$hour <= 3 | tmls$hour >= 20, 1, 0)

# Convert date column to date for Shiny slider
tmls$created_at <- as.Date(tmls$created_at)


#### Count Community References
# Count mentions
# regex for mentions: @\w
tmls$mentions_count <- stri_count_regex(tmls$text, "@\\w")
tmls$mentions_presence <- ifelse(tmls$mentions_count == 0, 0, 1)
table(tmls$mentions_count)

# Count hastags
# regex for hashtags: #\w
tmls$hashtag_count <- stri_count_regex(tmls$text, "#\\w")
tmls$hashtag_presence <- ifelse(tmls$hashtag_count == 0, 0, 1)

# Count emojis
# regex for emojis: U\\+\\w+
# enc2native from textclean package used in order tol print the unicode version of the emojis
# enc2utf8 could be useful for SentimentR package later
tmls$emoji_count <- stri_count_regex(enc2native(tmls$text), "U\\+\\w+")
tmls$emoji_presence <- ifelse(tmls$emoji_count == 0, 0, 1)
table(tmls$emoji_count)

#### Test for skewness in variables
if (!require("e1071")) install.packages("e1071", quiet=TRUE) ; require("e1071")

# Skewness is 35, 29, 4.5, 0.77, 3.41 and 0.70 respectively meaning that these variables
# (except hastag_count and display_text_width) are highly skewed
skewness(tmls$favorite_count)
skewness(tmls$retweet_count)
skewness(tmls$mentions_count)
skewness(tmls$hashtag_count)
skewness(tmls$emoji_count)
skewness(tmls$display_text_width)

# View histograms
hist(tmls$favorite_count)
hist(tmls$retweet_count)
hist(tmls$display_text_width) # more or less normally distributed
hist(tmls$mentions_count)
hist(tmls$hashtag_count) # more or less normally distributed
hist(tmls$emoji_count)

#### Try various methods to correct skewness
# Find the square root of the variables
tmls$favorite_sqrt <- sqrt(tmls$favorite_count)
tmls$retweet_sqrt <- sqrt(tmls$retweet_count)
tmls$mentions_sqrt <- sqrt(tmls$mentions_count)
tmls$emoji_sqrt <- sqrt(tmls$emoji_count)

# Find the log of the variables
tmls$favorite_log <- log(tmls$favorite_count)
tmls$retweet_log <- log(tmls$retweet_count)
tmls$mentions_log <- log(tmls$mentions_count)
tmls$emoji_log <- log(tmls$emoji_count)

# Winsorize numeric variables with large values to decrease the effect of outliers

var_org <- c("favorite_count", "retweet_count", "mentions_count", "emoji_count")
var_wins <- c("favorite_winsorize", "retweet_winsorize", "mentions_winsorize", "emoji_winsorize")

for (i in 1:length(var_org)) {
  tmls[,var_wins[i]] <- tmls[,var_org[i]]
}

# need to use pull to get the values of the column, instead of a list
for(var in var_wins) {
  UL <- mean(pull(tmls, var))+3*sd(pull(tmls, var))
  tmls[which(tmls[, var] > UL) , var] <- UL
}

#### Check the effect of the three strategies
for (skew_var in 121:132){
  tmls[tmls[,skew_var] == -Inf, skew_var] <- 0
}

# For favorite and retweet, log seemed to have the best results.
# For mentions and emojis, sqrt seemed to have the best results.
skewness(tmls$favorite_sqrt)
skewness(tmls$favorite_log)
skewness(tmls$favorite_winsorize)

skewness(tmls$retweet_sqrt)
skewness(tmls$retweet_log)
skewness(tmls$retweet_winsorize)

skewness(tmls$mentions_sqrt)
skewness(tmls$mentions_log)
skewness(tmls$mentions_winsorize)

skewness(tmls$emoji_sqrt)
skewness(tmls$emoji_log)
skewness(tmls$emoji_winsorize)

# View histograms
hist(tmls$favorite_log)
hist(tmls$retweet_log)
hist(tmls$mentions_sqrt)
hist(tmls$emoji_sqrt)


#### Create binary target variable with normalized data
# Values under the mean are low enagagement while values over zero are high engagement (aka target = 1)
tmls$target_fav_norm <- ifelse(tmls$favorite_log > mean(tmls$favorite_log), 1, 0)
tmls$target_RT_norm <- ifelse(tmls$retweet_log > mean(tmls$retweet_log), 1, 0)


#### Create binary target variable with winsorized data
# Values under the mean are low enagagement while values over zero are high engagement (aka target = 1)
tmls$target_fav <- ifelse(tmls$favorite_winsorize > mean(tmls$favorite_winsorize), 1, 0)
tmls$target_RT <- ifelse(tmls$retweet_winsorize > mean(tmls$retweet_winsorize), 1, 0)


#### Standardize predictor variable and keep positive to help with model interpretation later
tmls$tweet_length_scaled <- (tmls$display_text_width - mean(tmls$display_text_width)) / sd(tmls$display_text_width)
tmls$tweet_length_scaled <- tmls$tweet_length_scaled + abs(min(tmls$tweet_length_scaled))
hist(tmls$tweet_length_scaled)

# Variable to take into account the popularity of the brand
# End up not including it in the regression because it is very strongly correlated with the tweet engagement
hist(tmls$followers_count)
UL <- mean(tmls$followers_count)+3*sd(tmls$followers_count)
tmls$followers_count[tmls$followers_count > UL] <- UL

tmls$followers_count_scaled <- (tmls$followers_count - mean(tmls$followers_count)) / sd(tmls$followers_count)
tmls$followers_count_scaled <- tmls$followers_count_scaled + abs(min(tmls$followers_count_scaled))
hist(tmls$followers_count_scaled)


### Add dummy variable for questions
# Logic behind is that questions open the scene for discussions and may increase engagement
tmls$question <- numeric(length(tmls$text))
for (tweet in 1:length(tmls$text)){
  if (sum(grepl("\\?", tmls$text[tweet])) > 0){
    tmls$question[tweet] <- 1
  }
}

#### Athelete Mentions
if (!require("stringr")) install.packages("stringr", quiet=TRUE) ; require("stringr")
# The goal is to check whether the mentions of an athletes will impact the engagement 
# Create a binary variable where 1: mentions of an athlete 0: no mentions of athletes

# Extracting the mentions (@....) in a tweet
tmls$athletes <- str_extract(tmls$text,"@\\w+")
tmls$athletes <- tolower(tmls$athletes)

# Creating a dictionnary with the frequency of mentions
dic_mentions <- as.data.frame(table(tmls$athletes))
dic_mentions <- dic_mentions[order(-dic_mentions$Freq),]

# Creating a dictionary of top athletes mentionned more than 1 times in a tweet post
dic_athletes <- subset(dic_mentions,Var1 %in% c('@stephencurry30','@cameronnewton','@anthonyfjoshua','@JJWatt','@JordanSpieth','@TheRock','@lindseyvonn', 
                                                '@faithevebee','@richfroning','@Bharper3407','@katrintanja','@Dennis1SmithJr','@markjameschase',
                                                '@brandincooks','@thejudge44','@camillelbaz','@devontafreeman','@_fournette','@natashahastings',
                                                '@canelo','@jessiegraffpwr','@mistyonpointe','@pkizzire','@aly_raisman',
                                                '@ben_smith13','@claytonkersh22','@justinverlander','@michaelphelps',
                                                '@mikaelamayer1','@paulpogba','@the_footdoctor','@tiatoomey',
                                                '@daniellescott7','@harmanbrian','@jharrison9292','@jrmantia','@juliojones_11',
                                                '@missmiakang','@sloanestephens','@yusramardini','@brittanybowe','@cody_bellinger',
                                                '@dan_bailey9','@deionsanders','@eliudkipchoge','@garbimuguruza',
                                                '@angeliquekerber','@carowozniacki','@naomi_osaka_','@rogerfederer',
                                                '@serenawilliams','@andy_murray','@rafaelnadal','@simona_halep',
                                                '@tigerwoods',"@gervontaa","@j_josh11","@jharden13","@joannamma","@joelembiid",
                                                "@kingjames","@kmbappe","@kristinholte",
                                                "@kyrieirving","@marcelhirscher","@mikaelkingsbury",
                                                "@raylewis","@ryanpalmerpga","@samgirardcan","@scottpanchik",
                                                "@teddyriner","@the2kferguson"))

# matching each post to the dictionnary of athletes to check whether an athlete is mention or not
for (i in 1:length(dic_athletes$Var1)){
  athetes_mentions <- match(tmls$athletes,dic_athletes$Var1)
  presentss <-!is.na(athetes_mentions)
}

#adding the column to tmls in which it returns TRUE for mentions of athletes of FALSE otherwise
#converting to binary variable 
tmls$athletes_mentions <- presentss
tmls$athletes_mentions <- ifelse(tmls$athletes_mentions == TRUE, 1,0)

#### Create the intermediate (i.e. before Sentiment Analysis and Topic Modeling) basetable
# Filter unneeded rows (i.e. relating to the brand profile set-up, (missing) location columns, quoted RT attributes)
tweet_basetable <- tmls[, c(3:5, 7, 11:14, 89:120, 123:126, 133:139, 141)]

# Export to CSV
write.csv(tweet_basetable, file = "basetable.csv")
