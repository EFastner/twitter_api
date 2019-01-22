require(stringr)
require(plyr)
require(dplyr)
require(rtweet)

fun.get_tweets <- function(){
  return(
    get_timeline(
      user = "@Lin_Manuel",
      n = 5000))
}

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

fun.filter_date <- function(df, single_date){
  date_only <- 
    df %>%
    filter(df$date == single_date)
  
  return(date_only)
}

fun.postgres_connect <- function(db.name, db.host, db.user){
  #DECRIPTION: Creates a connection to a PostgreSQL Server
  #ARGUMENTS: 
  #db.name = The name of the database to connect to
  #db.host = The host ("localhost" or IP Address)
  #db.user = The name of the user to connect with
  
  require(RPostgreSQL)
  
  #Set the connection drive for PostgreSQL
  db.drv <- dbDriver("PostgreSQL")
  
  #Create Connection
  db.con <- 
    dbConnect(db.drv, 
              dbname = as.character(db.name), 
              host = as.character(db.host), 
              port = 5432, 
              user = as.character(db.user), 
              password = PASSWORD)
  
  return(db.con)
}

fun.append_table <- function(connection, table, values){
  
  require(RPostgreSQL)
  
  dbWriteTable(conn = connection, 
               name = as.character(table), 
               value = values,
               append = TRUE,
               row.names = FALSE)
}

#If the tl variable doesn't exist in the current environment, query it from Twitter
if(!exists("tl")) {
  #Search a timeline
  tl <- fun.get_tweets()
  tl$date <- as.Date(tl$created_at)
}

#FILTER OUT THE PREVIOUS DATE ONLY
df.single_date <- fun.filter_date(tl, Sys.Date()-1)
colnames(df.single_date) <- str_replace_all(colnames(df.single_date), "\\.", "")

#GRAB THE GMORNING TWEETS
gmorning <- dplyr::filter(df.single_date,
                          (str_detect(df.single_date$text,fixed("Gmorning",ignore_case = TRUE)) | 
                             str_detect(df.single_date$text, fixed("Good Morning", ignore_case = TRUE))))

#IF THERE IS MORE THAN 1 GMORNING TWEET, ATTEMPT TO IDENTIFY THE ACTUAL TWEET
if(nrow(gmorning) > 1){
  gmorning <- fun.find_tweet_string(gmorning, "gmorning")
}

#GRAB THE GNIGHT TWEETS
gnight <- dplyr::filter(df.single_date,
                          (str_detect(df.single_date$text,fixed("Gnight",ignore_case = TRUE)) | 
                             str_detect(df.single_date$text, fixed("Good Night", ignore_case = TRUE))))

#IF THERE IS MORE THAN 1 GNIGHT TWEET, ATTEMPT TO IDENTIFY THE ACTUAL TWEET
if(nrow(gnight) > 1){
  gnight <- fun.find_tweet_string(gnight, "gnight")
}

#GRAB ALL OF THE HAMILTHOUGHT TWEETS
hamilthoughts <- 
  df.single_date %>%
  filter(str_detect(df.single_date$text, fixed("Hamilthought", ignore_case = TRUE)))

#CREATE DATABASE CONNECTION - MAKE SURE TO ADD USERNAME AND PASSWORD INTO CODE BEFORE ACTIVATING
db.conn <- fun.postgres_connect("gmorning_gnight", PASSWORD, USERNAME)

#Add ALL 3 DATA SETS TO THEIR RELEVANT TABLES
fun.append_table(db.conn, "gmorning", gmorning)
fun.append_table(db.conn, "gnight", gnight)
fun.append_table(db.conn, "hamilthoughts", hamilthoughts)
