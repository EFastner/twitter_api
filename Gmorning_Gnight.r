require(plyr)
require(dplyr)
require(rtweet)

fun.find_tweet_string <- function(df, tweet_string){
  #DESCRIPTION: Finds the earliest tweet of the day that isn't a RT, quote, or reply. Assumes this is the actual gmorning tweet
  #ARGUMENTS: Expects a raw dataframe created by get_timeline in the rtweet package
  df.possible_tweets <- 
    df %>%
    dplyr::filter(df$is_retweet == FALSE &
                    df$is_quote == FALSE &
                    (is.na(df$reply_to_screen_name) | df$reply_to_screen_name == "Lin_Manuel")
    )
  
  if(tolower(tweet_string) == "gmorning"){
  df.gmorning_tweet <- 
    df.possible_tweets %>%
    dplyr::filter(df.possible_tweets$created_at == min(df.possible_tweets$created_at))
  }else if(tolower(tweet_string) == "gnight") {
    df.gmorning_tweet <- 
      df.possible_tweets %>%
      dplyr::filter(df.possible_tweets$created_at == max(df.possible_tweets$created_at))  
  }
  
  return(df.gmorning_tweet)
}

#If the tl variable doesn't exist in the current environment, query it from Twitter
if(!exists("tl")) {
  #Search a timeline
  tl <- get_timeline(
    user = "@Lin_Manuel",
    n = 5000
  )
}

#Identify the Gmorning Texts and add a column for the data
gmorning <- dplyr::filter(tl, grepl("Gmorning", tl$text))
gmorning$date <- as.Date(gmorning$created_at)

#Group the gmorning tweets by date to identify days that include multiple tweets
gmorning_dates <- 
  gmorning %>%
  group_by(date) %>%
  summarise(number_of_tweets = n()) %>%
  arrange(desc(number_of_tweets))

#Loop through dates with multiple tweets and try to identify which one is the actual Gmorning tweet
for(i in 1:nrow(gmorning_dates)){
  if(gmorning_dates[i, "number_of_tweets"] > 1){
    multi_date <- gmorning_dates[i, "date"]
    keep_tweet <- fun.find_tweet_string(filter(gmorning, date == multi_date), "gmorning")
    
    gmorning <- gmorning[(!gmorning$date == multi_date | gmorning$text == keep_tweet$text),]
  }
}

gmorning <- arrange(gmorning)


#Gnight
gnight <- dplyr::filter(tl, grepl("Gnight", tl$text))
gnight$date <- as.Date(gnight$created_at)

#Identify the Gmorning Texts and add a column for the data
gnight <- dplyr::filter(tl, grepl("Gnight", tl$text))
gnight$date <- as.Date(gnight$created_at)

#Group the gmorning tweets by date to identify days that include multiple tweets
gnight_dates <- 
  gnight %>%
  group_by(date) %>%
  summarise(number_of_tweets = n()) %>%
  arrange(desc(number_of_tweets))

#Loop through dates with multiple tweets and try to identify which one is the actual Gmorning tweet
for(i in 1:nrow(gnight_dates)){
  if(gnight_dates[i, "number_of_tweets"] > 1){
    multi_date <- gnight_dates[i, "date"]
    keep_tweet <- fun.find_tweet_string(filter(gmorning, date == multi_date), "gmorning")
    
    gmorning <- gmorning[(!gmorning$date == multi_date | gmorning$text == keep_tweet$text),]
  }
}

