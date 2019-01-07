require(twitteR)

#Create oauth connection
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#Search for specific terms
tw.search_terms = searchTwitter('Hockey')
df.search_terms = twListToDF(tw.search_terms)

#Search specific timeline (looks like only last 20 w/ free api)
tw.user_tweets <- userTimeline(user = 'lin_manuel',
                               n = 2000,
                               includeRts = TRUE,
                               excludeReplies = FALSE)

df.user_tweets <- twListToDF(tw.user_tweets)
