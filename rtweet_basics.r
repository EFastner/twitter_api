library(plyr)
library(dplyr)
library(rtweet)

# #Search all tweets
 rt <- search_tweets(
   q = "#rstats",
   n = 25,
   include_rts = FALSE
 )

#Search a timeline
 tl <- get_timeline(
   user = "@Lin_Manuel",
   n = 5000
 )

text_only <- tl$text

gmorning <- dplyr::filter(tl, grepl("Gmorning", tl$text))
gnight <- dplyr::filter(tl, grepl("Gnight", tl$text))

gmorning$date <- as.Date(gmorning$created_at)
gnight$date <- as.Date(gnight$created_at)

gmorning_dates <- 
  gmorning %>%
  group_by(date) %>%
  summarise(number_of_tweets = n()) %>%
  arrange(desc(number_of_tweets))
