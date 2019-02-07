#text analysis
library(rtweet)
library(tidytext)
library(dplyr)  #dplyr provides a flexible grammar of data manipulation
SEARCH = search_tweets(q = "#MahindraFurio", n = 10000, lang = "en",include_rts = FALSE)
head(SEARCH$text)
# REMOVE HYPERLINKS
SEARCH %>% mutate_at(c("stripped_text"), gsub("http.*","",.))
SEARCH$stripped_text <- gsub("http.*","",  SEARCH$text)
SEARCH$stripped_text <- gsub("https.*","", SEARCH$stripped_text)
head(SEARCH$stripped_text)
# remove punctuation, convert to lowercase
SEARCH_clean <- SEARCH %>% dplyr::select(stripped_text) %>% unnest_tokens(word,stripped_text)
head(SEARCH_clean)
# remove numbers
nrow(SEARCH_clean)
SEARCH_NUMBERS <-SEARCH_clean[-grep("\\b\\d+\\b", SEARCH_clean$word),]
head(SEARCH_NUMBERS)
nrow(SEARCH_NUMBERS)
# REMOVE STOP WORDS
cleaned_tweet_words <- SEARCH_NUMBERS %>% anti_join(stop_words)
nrow(cleaned_tweet_words)
head(cleaned_tweet_words)

#sentiment analysis
library(syuzhet)
#read file
word.df <- as.character(cleaned_tweet_words) # converted into character vector.
sentiment <- get_nrc_sentiment(word.df)
# obtain sentiment scores
sentiment
#plotting of sentiments scores
emo_bar = colSums(sentiment)
emo_sum = data.frame(count=emo_bar, sentiment=names(emo_bar))
emo_sum$sentiment = factor(emo_sum$sentiment, levels=emo_sum$sentiment[order(emo_sum$count, decreasing = TRUE)])
library(plotly)
p <- plot_ly(emo_sum, x=~sentiment, y=~count, type="bar", color=~sentiment) %>%
  layout(xaxis=list(title=""), showlegend=FALSE,
         title="Emotion Type for hashtag: #MahendraFurio")
p
get_nrc_sentiment('launch')
get_nrc_sentiment('spectacular')





  #***********************************************************************************************
  #***********************************************************************************************
library(rtweet)
library(ggplot2)
consumerKey = 's7pyd0tktb8as7DSHBihXwJVU'
consumerSecret = 'bdfQino2TcYaEM25c6Vj6z3U2YkfGseCrfIGkTl5cL0ciMozps'
OauthToken = '1089175947163578368-46JIAhFAaAMd5NKQ2xIYYjaYhqXtKk'
OauthSecret = 'pMTbHbOL45j4IRA0GSVNijYS0Ykm8mIPTRBczUF4jDLSe'
#SETUP TWITTER CONNECTION
create_token(app = "mytwitterapp",'s7pyd0tktb8as7DSHBihXwJVU','bdfQino2TcYaEM25c6Vj6z3U2YkfGseCrfIGkTl5cL0ciMozps','1089175947163578368-46JIAhFAaAMd5NKQ2xIYYjaYhqXtKk','pMTbHbOL45j4IRA0GSVNijYS0Ykm8mIPTRBczUF4jDLSe', set_renv = TRUE)
TWEET = search_tweets("#MahindraFurio", n = 200, include_rts = FALSE)
TWEET$text
# network analysis
FRIENDS = get_friends("@anandmahindra", n = 5000)
FRIENDS
FRIENDS_DATA = lookup_users(FRIENDS$user_id)
FRIENDS_DATA$screen_name
FOLLOWERS = get_followers("@anandmahindra", n = 5000)
FOLLOWERS
FOLLOWERS_DATA = lookup_users(FOLLOWERS$user_id)
FOLLOWERS_DATA$screen_name
TIMELINE = get_timeline("@anandmahindra", n = 100)
head(TIMELINE$text)
FAVORITES = get_favorites("@anandmahindra",n = 3000)
FAVORITES$screen_name
RETWEETER = get_retweeters(status_id = "1087849748915257344", n = 100)
RETWEETER
RETWEETER_DATA = lookup_users(RETWEETER$user_id)
RETWEETER_DATA$screen_name
# location analysis
FRIENDS = get_friends("@anandmahindra", n = 5000)
FRIENDS
FRIENDS_DATA = lookup_users(FRIENDS$user_id)
FRIENDS_DATA$location
FOLLOWERS = get_followers("@anandmahindra", n = 5000)
FOLLOWERS
FOLLOWERS_DATA = lookup_users(FOLLOWERS$user_id)
FOLLOWERS_DATA$location
TIMELINE = get_timeline("@anandmahindra", n = 100)
TIMELINE$location
TWEET = search_tweets("#MahindraFurio", n = 200, include_rts = FALSE)
TWEET$location
RETWEETER = get_retweeters(status_id = "1087849748915257344", n = 100)
RETWEETER
RETWEETER_DATA = lookup_users(RETWEETER$user_id)
RETWEETER_DATA$location
FAVORITES = get_favorites("@anandmahindra",n = 3000)
FAVORITES$location
RETWEET = get_retweets(status_id = "1087849748915257344", n = 100)
RETWEET$location
#action analysis
TWEET = search_tweets("#MahindraFurio", n = 200, include_rts = FALSE)
TWEET$text
TWEET_DATA = lookup_users(TWEET$user_id)
TWEET_DATA$screen_name
USER = users_data(TWEET)
USER
library(ggplot2)
PLOT = ts_plot(TWEET)
PLOT
RETWEET = get_retweets(status_id = "1087849748915257344", n = 100)
head(RETWEET$text)



