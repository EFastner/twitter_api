require(stringr)
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
    df.found_tweets <- 
      df.possible_tweets %>%
      dplyr::filter(df.possible_tweets$created_at == min(df.possible_tweets$created_at))
  }else if(tolower(tweet_string) == "gnight") {
    df.found_tweets <- 
      df.possible_tweets %>%
      dplyr::filter(df.possible_tweets$created_at == max(df.possible_tweets$created_at))  
  }
  
  return(df.found_tweets)
}

#If the tl variable doesn't exist in the current environment, query it from Twitter
if(!exists("tl")) {
  #Search a timeline
  tl <- get_timeline(
    user = "@Lin_Manuel",
    n = 100
  )
}

tl$date <- as.Date(tl$created_at)
gmorning <- dplyr::filter(tl, tl$date == Sys.Date() & 
                            (str_detect(tl$text,fixed("Gmorning",ignore_case = TRUE)) | 
                               str_detect(tl$text, fixed("Good Morning", ignore_case = TRUE))))

