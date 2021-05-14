# CSS project
# Luxuan Wang

# Data collection----------

# Collecting tweets that contain racial charge phrases of "Wuhan virus" and/or "Chinese virus" during three time periods by U.S. users.

library(academictwitteR)
bearer_token <- "" # Insert bearer token
setwd("")

#* 2020-01-18 to 2020-02-14----------
tweets1 <- get_hashtag_tweets('"chinese virus" OR "wuhan virus" OR chinesevirus OR wuhanvirus
                           has:links lang:en place_country:US', "2020-01-18T00:00:00Z", "2020-02-15T00:00:00Z", bearer_token, data_path = "data1/")

#* 2020-03-11 to 2020-04-07----------
tweets3 <- get_hashtag_tweets('"chinese virus" OR "wuhan virus" OR chinesevirus OR wuhanvirus
                           has:links lang:en place_country:US', "2020-03-11T00:00:00Z", "2020-04-08T00:00:00Z", bearer_token, data_path = "data3/")

#* 2021-03-10 to 2021-04-06----------
tweets3 <- get_hashtag_tweets('"chinese virus" OR "wuhan virus" OR chinesevirus OR wuhanvirus
                           has:links lang:en place_country:US', "2021-03-10T00:00:00Z", "2021-04-07T00:00:00Z", bearer_token, data_path = "data3/")










