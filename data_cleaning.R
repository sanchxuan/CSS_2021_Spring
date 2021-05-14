library(sparklyr)
library(dplyr)
library(jsonlite)
library(sparklyr.nested)

# 1. Read data with sparklyr ------
Sys.setenv(SPARK_HOME="/usr/lib/spark")
# Configure cluster (c3.4xlarge 30G 16core 320disk)
conf <- spark_config()
conf$'sparklyr.shell.executor-memory' <- "7g"
conf$'sparklyr.shell.driver-memory' <- "7g"
conf$spark.executor.cores <- 20
conf$spark.executor.memory <- "7G"
conf$spark.yarn.am.cores  <- 20
conf$spark.yarn.am.memory <- "7G"
conf$spark.executor.instances <- 20
conf$spark.dynamicAllocation.enabled <- "false"
conf$maximizeResourceAllocation <- "true"
conf$spark.default.parallelism <- 32
sc <- spark_connect(master = "local", config = conf, version = '2.2.0')

dat1 <- spark_read_json(sc = sc, name = "tweets1", path = "~/Desktop/1. Computational Social Science/project/data/data1/*",
                                header = TRUE, memory = FALSE)
dat1_r <- dat1 %>% mutate(date = to_date(created_at)) %>% 
  filter(date > as.Date("2020-01-17") & date < as.Date("2020-02-15")) %>% distinct()
sdf_dim(dat1_r) #[1] 361041     20

dat2 <- spark_read_json(sc = sc, name = "tweets2", path = "~/Desktop/1. Computational Social Science/project/data/data2_test/*",
                        header = TRUE, memory = FALSE)
dat2_r <- dat2 %>% mutate(date = to_date(created_at)) %>% 
  filter(date > as.Date("2020-03-10") & date < as.Date("2020-04-08")) %>% distinct()
sdf_dim(dat2_r) #[1] 3393819      17

dat3 <- spark_read_json(sc = sc, name = "tweets3", path = "~/Desktop/1. Computational Social Science/project/data/data3/*",
                        header = TRUE, memory = FALSE)
dat3_r <- dat3 %>% mutate(date = to_date(created_at)) %>% 
  filter(date > as.Date("2021-03-09") & date < as.Date("2021-04-07")) %>% distinct()
sdf_dim(dat3_r) # [1] 79665    20

# 2. filter lang = en  ----------
dat1_r <- dat1_r%>% filter(lang == 'en') %>% 
  select (-c(possibly_sensitive, attachments, context_annotations, attachments, geo, withheld))
dat2_r <- dat2_r%>% filter(lang == 'en') %>% 
  select (-c(possibly_sensitive, attachments, context_annotations, attachments, geo, withheld))
dat3_r <- dat3_r%>% filter(lang == 'en') %>% 
  select (-c(possibly_sensitive, attachments, context_annotations, attachments, geo, withheld))
sdf_schema_viewer(dat1_r)
sdf_schema_viewer(dat2_r)
sdf_schema_viewer(dat3_r)

# 3. select variables -------
# conversation_id, created_at, retweet_count, reply_count, like_count, quote_count, 
# lang, author_id, text, expanded_url,in_reply_to_user_id, tag
dat1_r <- dat1_r %>% sdf_select(author_id, conversation_id, created_at, date, id, public_metrics.like_count, public_metrics.quote_count,
                                          public_metrics.reply_count, public_metrics.retweet_count, lang, text, entities.urls, entities.hashtags,
                                          in_reply_to_user_id) %>% mutate(period = "tweets1")

dat2_r <- dat2_r %>% sdf_select(author_id, conversation_id, created_at, date, id, public_metrics.like_count, public_metrics.quote_count,
                                public_metrics.reply_count, public_metrics.retweet_count, lang, text, entities.urls, entities.hashtags,
                                in_reply_to_user_id) %>% mutate(period = "tweets2")

dat3_r <- dat3_r %>% sdf_select(author_id, conversation_id, created_at, date, id, public_metrics.like_count, public_metrics.quote_count,
                                public_metrics.reply_count, public_metrics.retweet_count, lang, text, entities.urls, entities.hashtags,
                                in_reply_to_user_id) %>% mutate(period = "tweets3")
sdf_dim(dat3_r)

dat <- union_all(dat1_r, dat2_r)
dat_r <- union_all(dat, dat3_r)
sdf_dim(dat)

# 4. expand urls --------
dat_r <- dat_r %>%
  mutate(urls = split(urls, ",")) %>%
  sdf_explode(urls)

# 5. delete twitter urls that contain facebook.com, youtube, and twitter status is photo or video. ----------
dat_r <- dat_r %>% filter(rlike(urls, '[^ ]*expanded_url[^ ]*'))
dat_r <- dat_r %>% filter(!rlike(urls, '[^ ]*www.facebook.com|youtu.be|www.youtube.com[^ ]*'))
dat_r <- dat_r %>% filter(!rlike(urls, '[^ ]*twitter.*photo|twitter.*video[^ ]*'))
dat_r %>% group_by(period) %>% count()
df = sdf_collect(dat_r) # write data from spark to R
spark_disconnect(sc)
write.csv(df, "df.csv")

# 6. Describe the dataset ----
df_r <- unique(df) %>% drop_na(expanded_url)
df_r %>% group_by(period) %>% count() #N of links
df_r %>% group_by(period) %>% distinct(id) %>%count() #N of unique tweets
df_r %>% group_by(period) %>% distinct(author_id) %>%count() #N of unique Twitter accounts (creators)

#* 5. extract Twitter username from expanded urls that are from Twitter
res <- str_match(df_r$expanded_url, "twitter.com/\\s*(.*?)\\s*/status")
df_r$username_twitterlink <- res[,2]

# extract expanded url links from the string
library(stringr)
url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"

df_r$expanded_url <- str_extract(df_r$urls, url_pattern)
head(df_r$expanded_url)

library(longurl)
urls <- distinct(df_r, expanded_url) %>% filter(str_count(expanded_url, "/") < 4)

for (i in 1:length(urls$expanded_url)){
  urls$url_d[i] <- expand_urls(urls$expanded_url[i]) %>% select(expanded_url)
}

urls$url_d <- unlist(urls$url_d)
df_r$url_d <- ifelse(df_r$expanded_url %in% urls$expanded_url,  
                   urls$url_d[match(df_r$expanded_url,urls$expanded_url)],
                   NA)
df_r$url_d <- ifelse(is.na(df_r$url_d), df_r$expanded_url, df_r$url_d)
df_r %>% group_by(period) %>% distinct(url_d) %>%count() #N of unique URLs

# 7. extract domains.----------
domain <- function(x) strsplit(gsub("http://|https://|www\\.", "", x), "/")[[c(1, 1)]]
df_r$link_host <- sapply(df_r$url_d, domain)
df_r %>% group_by(period) %>% count(link_host) %>% arrange(desc(n)) %>% group_by(period) %>% slice(1:10)

# 8. code dataset concerning rating, and fakenews----
# political ratings
allsides_data <- readr::read_csv("https://raw.githubusercontent.com/favstats/AllSideR/master/data/allsides_data.csv")
allsides_data_twitter <- allsides_data%>% drop_na(screen_name)
df_r$rating <- allsides_data_twitter$rating[match(df_r$username_twitterlink, allsides_data_twitter$screen_name)]

media_clowd <- read.csv('media.csv') #partisanship from media cloud
df_r$rating <- ifelse(df_r$link_host != 'twitter.com', 
                     media_clowd$partisanship[match(df_r$link_host, tolower(media_clowd$host))],
                     df_r$rating)
df_r$rating <- recode(df_r$rating, "left-center" = "center left", "right-center" = "center right")
df_r$rating[df_r$username_twitterlink == 'realDonaldTrump'] <- 'right'

table(df_r$rating)

# fakenews
black_sites <- as.data.frame(readLines("black_sites.txt"))
colnames(black_sites) <- 'host'
politifact_news <- read.csv('politifact_new.csv')
politifact_news$X <- tolower(politifact_news$X)

newsfact_coding <- read.csv('Domain_Codings.csv')
newsfact_coding <- newsfact_coding[-(171:999),]
newsfact_coding$Domain <- as.character(tolower(newsfact_coding$Domain))
df_r$link_host  <- as.character(tolower(df_r$link_host))

df_r$fakenews <- ifelse(df_r$link_host %in% tolower(black_sites$host), "black", NA)
df_r$fakenews <- ifelse(df_r$link_host %in% politifact_news$X,  
                         politifact_news$Type.of.site[match(df_r$link_host, politifact_news$X)],
                         df_r$fakenews)
df_r$fakenews <- ifelse(df_r$link_host %in% newsfact_coding$Domain,  
                         newsfact_coding$Likelihood..rating[match(df_r$link_host, newsfact_coding$Domain)],
                         df_r$fakenews)
df_r$fakenews[df_r$link_host=="google.com"] 
table(df_r$fakenews)
df_r$fakenews <- recode(df_r$fakenews, "fake news" = "Red",
                         "Fake news" = "Red",
                         Green = "news website",
                         "N/A" = "not a news", 
                         Satire = "Yellow", "Parody site" = "Yellow",
                         "some fake stories" = "Orange", "Some fake stories" = "Orange")
write.csv(df_r, "df_r.csv")
