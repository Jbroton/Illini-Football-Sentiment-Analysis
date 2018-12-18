# This script shows how we gathered our default data
# We saved the original data to an output file called twitter_data.Rda
# Our RShiny App runs that data as default


# install.packages("twitteR")
library('twitteR')


# Original Keys from Standard Twitter API, since been regenerated
# Change to your own keys in order to get new data

consumer_key <- "oJ2IQ5HrUgTbla4uFdYe7Zm8J"
consumer_secret <-"p5Hz8PWubUdJTNTDm4pY5WYYYby0sDvgqW4R6kJhHmFSOX40ZS"
access_token <- "2189043781-rZzKPC8Yqga1FrgCWHlGjM0nADAnJvWnkUOplGo"
access_secret <- "1gA7j3v5Bhm2gYQFEqx3PKbH4azfRWc6LyCjgQ689EQ0N" 

options(httr_oauth_cache=TRUE)
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)


# Using twitteR's function searchtwitter using our keywords
# 'illinifootball', 'illini football", 'lovie smith', 'loviesmith'

tw <- searchTwitter(c("illinifootball","illini football","lovie smith","loviesmith", " -filter:retweet"), n = 1e6, retryOnRateLimit = 1e3)
d <- twListToDF(tw)

# Saved output d, as twitter_data.Rda
# This was to prevent data from changing as we wrote our report

