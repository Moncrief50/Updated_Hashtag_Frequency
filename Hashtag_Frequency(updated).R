getwd()
setwd("/work_bgfs/r/rmoncrief/08-2020")

library(stringi)
library(readr)
library(stringr)
library(cld2)
library(dplyr)
library(tm)
library(tidyr)

# cleaning out environment
rm(list = ls())
# setting files to temp folder to be read into R
temp = list.files(path = "August_New/08-2020", pattern = "*.csv", full.names = TRUE)
temp = temp[file.size(temp) > 150]

df <- read_csv(temp, col_select = c("truncated", "extended_tweet_entities_hashtags", "entities_hashtags", "retweeted_status_truncated", "retweeted_status_extended_tweet_entities_hashtags", "retweeted_status_entities_hashtags", "quoted_status_truncated", "text", "quoted_status_extended_tweet_entities_hashtags", "quoted_status_entities_hashtags", "user_id_str", "retweeted_status_user_id_str", "quoted_status_user_id_str"), show_col_types = FALSE)

df <- data.frame(df)

#first step in removing foreign language
df <- subset(df, detect_language(df$text) == "en")

#Lowering all of the hashtag columns to get them ready for manipulation/count
df$entities_hashtags <- str_to_lower(df$entities_hashtags)
df$extended_tweet_entities_hashtags <- str_to_lower(df$extended_tweet_entities_hashtags)
df$retweeted_status_entities_hashtags <- str_to_lower(df$retweeted_status_entities_hashtags)
df$retweeted_status_extended_tweet_entities_hashtags <- str_to_lower(df$retweeted_status_extended_tweet_entities_hashtags)
df$quoted_status_entities_hashtags <- str_to_lower(df$quoted_status_entities_hashtags)
df$quoted_status_extended_tweet_entities_hashtags <- str_to_lower(df$quoted_status_extended_tweet_entities_hashtags)

#Creating stop-words variable containing a list of specified stop-words
stopwords = c("covid19", "COVID19", "Corona", "corona", "Coronavirus", "coronavirus", "CORONAVIRUS", "COVID-19", "Covid-19", "COVID19.", "COVID_19", "COVID__19",
              "covid19", "COVID19,", "COVID19:", "CoronaVirus", "COVID", "coronavirus,", "COVID19...", "Covid19", "Covid_19", "covid_19", "COV...",
              "pandemic,", "COVID-19", "covid19nigeria", "coronavirusupdate", "covid_...", "coivd19.", "coronavi...", "virus", "virus-related", "covi", "Coronavirususa",
              "covid19on", "cirus", "cov", "covidー19", "covid", "coro", "covid1", "covi", "coronavi", "coronavir", "covid2019", "coronav", "cor", "coron", "coronaviru", 
              "coronavirusupdates", "coronaoutbreak", "covid19outbreak", "coronavirusoutbrea", "coronavirusp", "ital", "ice", "wuh", "coronaviruspandem", "coronavirusu", 
              "coronaviruspandemic", "coronavirusoutbreak", "coronaupdate", "coronacrisis", "coronaviruslockdown", "staya", "tru", "lot", "art", "chi", "and", "loc", "chin", "rt")

#Removing all of the stop-words from each hashtag column on the dataset.
df$entities_hashtags <- removeWords(df$entities_hashtags, words = stopwords)
df$extended_tweet_entities_hashtags <- removeWords(df$extended_tweet_entities_hashtags, words = stopwords)
df$retweeted_status_entities_hashtags <- removeWords(df$retweeted_status_entities_hashtags, words = stopwords)
df$retweeted_status_extended_tweet_entities_hashtags <- removeWords(df$retweeted_status_extended_tweet_entities_hashtags, words = stopwords)
df$quoted_status_entities_hashtags <- removeWords(df$quoted_status_entities_hashtags, words = stopwords)
df$quoted_status_extended_tweet_entities_hashtags <- removeWords(df$quoted_status_extended_tweet_entities_hashtags, words = stopwords)

#Specifying all of the columns we want to include in the columns variable
columns <- c('truncated', 'extended_tweet_entities_hashtags',
             'entities_hashtags', 'retweeted_status_truncated',
             'retweeted_status_extended_tweet_entities_hashtags',
             'retweeted_status_entities_hashtags',
             'quoted_status_truncated',
             'quoted_status_extended_tweet_entities_hashtags',
             'quoted_status_entities_hashtags')

#Creating names veriable
names <- c('tweet', 'retweet', 'quote')
#Creating seq variable
df_seq <- seq(1,9,3)

#Creating a df containing all of the hashtags from each hashtag column
truncated_df <- df[columns]

#Necessary to put here because R doesnt allow you to manipulate non-existent variables
hashtags_df <- NA

#Creating a for loop which creates a new dataframe called hashtags_df containing all of the hashtags without duplicates
remove(hashtags_df)

for (i in 1:3){
  truncated_df[is.na(truncated_df[,df_seq[i]]), df_seq[i]] == 'False'
  truncated_df[truncated_df[, df_seq[i]] == 'True', df_seq[i]+2] == ''
  if (!exists('hashtags_df')){
    hashtags_df <- truncated_df %>%
      select(columns[(df_seq[i]+1):(df_seq[i]+2)]) %>%
      unite(., !!names[[i]], c(columns[df_seq[i]+1],
                               columns[df_seq[i]+2]), sep='')
  } else {
    hashtags_df <- cbind(hashtags_df, truncated_df %>%
                           select(columns[(df_seq[i]+1):(df_seq[i]+2)]) %>%
                           unite(., !!names[[i]], c(columns[df_seq[i]+1],
                                                    columns[df_seq[i]+2]),
                                 sep=''))
  }
}

#removing no longer needed variables to conserve space
remove(truncated_df)

#Creating another new dataframe which gets rid of all delimeters except for commas which wil be needed in the next step
data <- data.frame(lapply(hashtags_df, function(x) {gsub("[[']", '', x) %>% gsub("[]]", ', ', .)}))

#Uniting tweet, retweet, and quote tweet columns and spereating each word with a comma
data <- data %>% unite(., 'tweets', c('tweet', 'retweet', 'quote'), sep=', ')
data <- data %>% transmute(tweets = strsplit(as.character(tweets), ',')) %>% unnest(tweets)
data <- data[!(data == ' ' | data == '')]
#Removing all punctuation
data <- data %>% str_replace_all('[[:punct:]]', '') %>%
  str_replace_all(' ', '') %>% trimws() %>% as.data.frame()

names(data) <- 'hashtags'

#removing any capital letters within the dataframe
data$hashtags <- sub("[A-Z/]+$", "", data$hashtags)
#removing capital letter from the begining of words
data$hashtags <- sub("^[A-Z]", "", data$hashtags)

data$hashtags <- sub("^[A-Z]", "", data$hashtags)
#removing any word that is not atleast 3 characters long
data$hashtags <- gsub('\\b\\w{1,2}\\b','', data$hashtags)
#removing any character that is not in the english alphabet
data$hashtags <- str_remove_all(data$hashtags, "[^[\\da-zA-Z ]]")

#creating a count table in decreaing order of the hashtags
num_data2 <- data$hashtags
num_data2 <- table(num_data2)
num_data2 <- sort(num_data2, decreasing = TRUE)

#remove(data)

# This counts unique users
UniqueUsers <- count(df, user_id_str)
UniqueUsers <- UniqueUsers[,-2]
UniqueUsers <- data.frame(UniqueUsers)
UniqueUsers_retweet <- count(df, retweeted_status_user_id_str)
UniqueUsers_retweet <- UniqueUsers_retweet[,-2]
UniqueUsers_retweet <- data.frame(UniqueUsers_retweet)
UniqueUsers_quote <- count(df, quoted_status_user_id_str)
UniqueUsers_quote <- UniqueUsers_quote[,-2]
UniqueUsers_quote <- data.frame(UniqueUsers_quote)

colnames(UniqueUsers_retweet) <- c("user_id_str")
colnames(UniqueUsers_quote) <- c("user_id_str")
colnames(UniqueUsers) <- c("user_id_str")

UniqueUsers2 <- rbind(UniqueUsers, UniqueUsers_retweet, UniqueUsers_quote)
nrow(UniqueUsers2)
UniqueUsers2$duplciated <- duplicated(UniqueUsers2) #checks to see if there is any duplicated users
UniqueUsers2 <- UniqueUsers2[!duplicated(UniqueUsers2[ , c("user_id_str")]),]
nrow(UniqueUsers2)

#Top 1000 hashtags divided by the number of unique users in the dataset
test <- num_data2[2:1010]/ nrow(UniqueUsers2)
test <- data.frame(test)
test$Count <- num_data2[2:1010]
#renaming the new dataframe
colnames(test) <- c("Hashtags", "Freq", "Count")
#organizing columns
test <- test[, c("Hashtags", "Count", "Freq")]
#writing the file back to my computer
write.csv(test, "/work_bgfs/r/rmoncrief/08-2020/August_New/August_df_test.csv")
