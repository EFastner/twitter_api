require(dplyr)
require(twitteR)

#Create oauth connection
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#Grab last 3200 tweets (max for api)
tw.all_tweets <- userTimeline(user = 'Lin_Manuel',
                               n = 3200,
                               includeRts = TRUE,
                               excludeReplies = FALSE)

df.all_tweets <- twListToDF(tw.all_tweets)

#Filter for only the gmorning tweets
df.gmorning_tweets <- 
  dplyr::filter(df.all_tweets, 
                grepl(pattern = "gmorning", 
                      x = text, 
                      ignore.case = TRUE))

#Filter for only the gmorning tweets
df.gnight_tweets <- 
  dplyr::filter(df.all_tweets, 
                grepl(pattern = "gnight", 
                      x = text, 
                      ignore.case = TRUE))

