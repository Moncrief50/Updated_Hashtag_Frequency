
library(stringi)
library(readr)
library(stringr)
library(cld2)
library(dplyr)
library(tm)
library(tidyr)

# add a file reader to this
#temp = list.files(path = "03-2020/03-2020-new/03-2020/2020-03-21-07.csv", pattern = "*.csv", full.names = TRUE)
#temp = temp[file.size(temp) > 150]

df <- read.csv("/Users/robertmoncrief/Documents/Twitter Data 2020/x2go/03-2020/03-2020-new/03-2020/2020-03-21-07.csv")

df <- subset(df, detect_language(df$text) == "en")
#CSV_data <- subset (CSV_data, CSV_data$retweeted_status_lang == "en")#Gets rid of everything except english

df$entities_hashtags <- str_to_lower(df$entities_hashtags)
df$extended_tweet_entities_hashtags <- str_to_lower(df$extended_tweet_entities_hashtags)
df$retweeted_status_entities_hashtags <- str_to_lower(df$retweeted_status_entities_hashtags)
df$retweeted_status_extended_tweet_entities_hashtags <- str_to_lower(df$retweeted_status_extended_tweet_entities_hashtags)
df$quoted_status_entities_hashtags <- str_to_lower(df$quoted_status_entities_hashtags)
df$quoted_status_extended_tweet_entities_hashtags <- str_to_lower(df$quoted_status_extended_tweet_entities_hashtags)

stopwords = c("covid19", "COVID19", "Corona", "corona", "Coronavirus", "coronavirus", "CORONAVIRUS", "COVID-19", "Covid-19", "COVID19.", "COVID_19", "COVID__19",
              "covid19", "COVID19,", "COVID19:", "CoronaVirus", "COVID", "coronavirus,", "COVID19...", "Covid19", "Covid_19", "covid_19", "COV...",
              "pandemic,", "COVID-19", "covid19nigeria", "coronavirusupdate", "covid_...", "coivd19.", "coronavi...", "virus", "virus-related", "covi", "Coronavirususa",
              "covid19on", "cirus", "cov", "covidー19", "covid", "coro", "covid1", "covi", "coronavi", "coronavir", "covid2019", "coronav", "cor", "coron", "coronaviru", 
              "coronavirusupdates", "coronaoutbreak", "covid19outbreak", "coronavirusoutbrea", "coronavirusp", "ital", "ice", "wuh", "coronaviruspandem", "coronavirusu", 
              "coronaviruspandemic", "coronavirusoutbreak", "coronaupdate", "coronacrisis", "coronaviruslockdown", "staya", "tru", "lot", "art", "chi", "and", "loc", "chin")



df$entities_hashtags <- removeWords(df$entities_hashtags, words = stopwords)
df$extended_tweet_entities_hashtags <- removeWords(df$extended_tweet_entities_hashtags, words = stopwords)
df$retweeted_status_entities_hashtags <- removeWords(df$retweeted_status_entities_hashtags, words = stopwords)
df$retweeted_status_extended_tweet_entities_hashtags <- removeWords(df$retweeted_status_extended_tweet_entities_hashtags, words = stopwords)
df$quoted_status_entities_hashtags <- removeWords(df$quoted_status_entities_hashtags, words = stopwords)
df$quoted_status_extended_tweet_entities_hashtags <- removeWords(df$quoted_status_extended_tweet_entities_hashtags, words = stopwords)


columns <- c('truncated', 'extended_tweet_entities_hashtags',
             'entities_hashtags', 'retweeted_status_truncated',
             'retweeted_status_extended_tweet_entities_hashtags',
             'retweeted_status_entities_hashtags',
             'quoted_status_truncated',
             'quoted_status_extended_tweet_entities_hashtags',
             'quoted_status_entities_hashtags')

names <- c('tweet', 'retweet', 'quote')

df_seq <- seq(1,9,3)

truncated_df <- df[columns]

if(exists(hashtags_df)){remove(hashtags_df)}

for (i in 1:3){
  truncated_df[truncated_df[, df_seq[i]] == 'True', df_seq[i]+2] = ''
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
#remove(truncated_df)



hashtags_df[hashtags_df[, 'tweet'] != '' & hashtags_df[, 'retweet'] != '', 'tweet'] = ''



data <- data.frame(lapply(hashtags_df, function(x) {gsub("[[']", '', x) %>%
    gsub('[]]', '', .)}))
#remove(hashtags_df)



data <- data %>% unite(., 'tweets', c('tweet', 'retweet', 'quote'), sep=', ')
data <- data %>% mutate(tweets = strsplit(as.character(tweets), ',')) %>% unnest(tweets)
data <- data[!(data == ' ' | data == '')]
data <- data %>% tolower() %>% str_replace_all('[[:punct:]]', '') %>%
  str_replace_all(' ', '') %>% trimws() %>% as.data.frame()
names(data) <- 'hashtags'



# add stop words
num_data <- data %>% count(hashtags) %>% arrange(desc(n)) %>% filter(row_number() %in% c(1:1000))
#remove(data)
#This counts unique users
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
#Top 1000 most frequent hashtags / unique users
num_data$freq <- num_data$n[1:1000] / nrow(UniqueUsers2)

colnames(num_data) <- c("hashtags", "count", "frequency")




