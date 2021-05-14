df_r <- read.csv("df_r.csv")
df_r <- df_r[,2:22]
# Data analysis
library(tidyverse)
options(scipen = 999)

# # Subtract three datasets by key time points: January 2020, March 2020, and March 2021 ------
# tweets1 <- read.csv("tweets1_r.csv")
# tweets2 <- read.csv("tweets2_r.csv")
# tweets3 <- read.csv("tweets3_r.csv")
# 
# # Jan 2020, 20200118 to 20200214
# tweets1$created_at <- as.Date(tweets1$created_at) 
# tweets1 <- tweets1 %>% filter(created_at >= as.Date("2020-01-18") & created_at <= as.Date("2020-02-14")) %>% 
#   distinct() %>% complete(expanded_url)
# 
# # March 2020, 20200311 to 20200407
# tweets2$created_at <- as.Date(tweets2$created_at) 
# tweets2 <- tweets2 %>% filter(created_at >= as.Date("2020-03-11") & created_at <= as.Date("2020-04-07"))
# tweets2 <- unique(tweets2)
# 
# # March 2021, 20210310 to 20210406
# tweets3$created_at <- as.Date(tweets3$created_at) 
# tweets3 <- tweets3 %>% filter(created_at >= as.Date("2021-03-10") & created_at <= as.Date("2021-04-06")) %>% 
#   distinct() %>% complete(expanded_url)
# 
# names(tweets3)[names(tweets3) == 'username_twitterurl'] <- 'username_twitterlink'
# 
# common_cols <- Reduce(intersect, list(colnames(tweets1), colnames(tweets2), colnames(tweets3)))
# 
# library(dplyr)
# dat <- bind_rows(list(tweets1=tweets1[common_cols], 
#                       tweets2=tweets2[common_cols],
#                       tweets3=tweets3[common_cols]), .id = 'source') %>% select(-c(X))
# dat <- dat[!is.na(dat$expanded_url),]
# 
# # expand url
# urls <- distinct(dat, expanded_url) %>% filter(str_count(expanded_url, "/") < 4)
# 
# for (i in 7756:length(urls$expanded_url)){
# urls$url_d[i] <- expand_urls(urls$expanded_url[i]) %>% select(expanded_url)
# }
# 
# urls$url_d <- unlist(urls$url_d )
# dat$url_d <- ifelse(match(dat$expanded_url, urls$expanded_url), urls$url_d, dat$expanded_url)
# dat$url_d <- ifelse(is.na(dat$url_d), dat$expanded_url, dat$url_d)
# 
# domain <- function(x) strsplit(gsub("http://|https://|www\\.", "", x), "/")[[c(1, 1)]]
# dat$link_host <- sapply(dat$url_d, domain)
# 
# # code dataset concerning rating, and fakenews
# # political ratings
# allsides_data <- readr::read_csv("https://raw.githubusercontent.com/favstats/AllSideR/master/data/allsides_data.csv")
# allsides_data_twitter <- allsides_data%>% drop_na(screen_name)
# dat$rating <- allsides_data_twitter$rating[match(dat$username_twitterlink, allsides_data_twitter$screen_name)]
# 
# media_clowd <- read.csv('media.csv') #partisanship from media cloud
# dat$rating <- ifelse(dat$link_host != 'twitter.com', 
#                     media_clowd$partisanship[match(dat$link_host, tolower(media_clowd$host))],
#                     dat$rating)
# dat$rating <- recode(dat$rating, "left-center" = "center left", "right-center" = "center right")
# dat_r$rating[dat_r$name == 'realDonaldTrump'] <- 'right'
#   
# table(dat$rating)
# 
# # fakenews
# black_sites <- as.data.frame(readLines("black_sites.txt"))
# colnames(black_sites) <- 'host'
# politifact_news <- read.csv('politifact_new.csv')
# newsfact_coding <- read.csv('Domain_Codings.csv')
# newsfact_coding <- newsfact_coding[-(171:999),]
# newsfact_coding <- newsfact_coding %>% select(1, 2)
# politifact_news$X <- tolower(politifact_news$X)
# dat_r <- left_join(dat_r, newsfact_coding, by = c("link_host" = "Domain"))
# 
# dat_r$fakenews <- ifelse(dat_r$link_host %in% tolower(black_sites$host), "fake news", NA)
# dat_r$fakenews <- ifelse(dat_r$link_host %in% politifact_news$X,  
#                          politifact_news$Type.of.site[match(dat_r$link_host, politifact_news$X)],
#                          dat_r$fakenews)
# dat_r$fakenews <- ifelse(dat_r$link_host %in% newsfact_coding$Domain,  
#                          newsfact_coding$Likelihood..rating[match(dat_r$link_host, newsfact_coding$Domain)],
#                          dat_r$fakenews)
# table(dat_r$fakenews)
# table(dat_r$fakeness)
# dat_r$fakenews <- recode(dat_r$fakenews, "fake news" = "Red",
#                          "Fake news" = "Red",
#                          Green = "news website",
#                          "N/A" = "not a news", 
#                          Satire = "Yellow", "Parody site" = "Yellow",
#                          "some fake stories" = "Orange", "Some fake stories" = "Orange")
# 
# dat_r <-  dat_r[, 1:18]
# write.csv(dat_r, "dat_r.csv")
# 
# # 1. Describe the dataset ----
# dat_r %>% group_by(source) %>% distinct(author_id) %>% count()
# dat_r %>% group_by(source) %>% distinct(text) %>%count()


# 2. Top ten shared sources. -----
# What are the top media organizations (domain) mostly shared by unique Twitter users that used racial charge phrases in the overall period of 15 months? In the three time periods?
df_r$name <- ifelse(df_r$link_host == 'twitter.com', df_r$username_twitterlink, df_r$link_host)
df_r$name <- ifelse(is.na(df_r$name), df_r$expanded_url, df_r$name)

name_counts <- df_r %>% group_by(period) %>% distinct(author_id,.keep_all=TRUE) %>% count(name, sort=TRUE)
name_counts$name <- reorder(name_counts$name, name_counts$n)
top_names <- name_counts %>% group_by(period) %>% slice(1:10)
top_names$rating <- df_r$rating[match(top_names$name, df_r$name)]
top_names$fakenews <- df_r$fakenews[match(top_names$name, df_r$link_host)]

link_counts <- df_r %>% group_by(period) %>% distinct(author_id,.keep_all=TRUE) %>% count(link_host, sort=TRUE)
link_counts$link_host <- reorder(link_counts$link_host, link_counts$n)
top_links <- link_counts %>% group_by(period) %>% slice(1:11)
top_links$rating <- df_r$rating[match(top_links$link_host, df_r$link_host)]
top_links$fakenews <- df_r$fakenews[match(top_links$link_host, df_r$link_host)]

twitter_counts <- df_r %>% group_by(period) %>% distinct(author_id,.keep_all=TRUE) %>% count(username_twitterlink, sort=TRUE)
twitter_counts$username_twitterlink <- reorder(twitter_counts$username_twitterlink, twitter_counts$n)
top_twitter <- twitter_counts %>% group_by(period) %>% slice(1:12)
top_twitter$rating <- df_r$rating[match(top_twitter$username_twitterlink, df_r$username_twitterlink)]

library(stargazer)
stargazer(top_names, header=FALSE, type='text', summary=FALSE, title="Data Frame",digits=1)

# 3. How does their political ratings and fakeness distribute? ----
# the portion of links that are of different political ratings or contain false information in all links shared in the tweets.
ratings <- df_r %>% group_by(period) %>% count(rating) %>% mutate(percent = n/sum(n))
fakeness <- df_r %>% group_by(period) %>% count(fakenews) %>% mutate(percent = n/sum(n))

# visuallization
# Political leaning distribution of website links shared in tweets containing racial phrases of coronavirus during three periods. 
ratings_v <- ratings %>% drop_na(rating)
target1 <- c("left", "center left", "center", "center right", "right")
ratings_v$rating <- factor(ratings_v$rating, levels = target1)

# note: percentages of website links with unknown political leaning for three periods
# were not shown in this chart, but can be found in the table.
ggplot(ratings_v, aes(x = ratings_v$period, y = percent, fill=factor(rating))) +
  geom_bar(stat='identity', position = position_dodge2(preserve = "single")) +
  scale_fill_manual(values=c("steelblue4", "steelblue1", "grey", "lightcoral", "red3")) +
  theme_minimal() +   scale_x_discrete(labels=c("Late Jan 2020", "Mid March 2020", "Mid March 2021") )+
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label=n, y = percent + 0.01), 
            position = position_dodge(width=0.9),
            size = 3) +
  labs(x = "Time periods", fill = "Political bias", color = "Political bias")

# Fakenews distribution of website links shared in tweets containing racial phrases of coronavirus during three periods. 
fakeness_v <- fakeness %>% drop_na(fakenews) %>% filter(fakenews != 'news website')
fakeness_v$fakeness <- recode(fakeness_v$fakenews, 
                              "not a news" = "Not a news",
                              "Red" = "Major or frequent falsehoods",
                              "Yellow" = "Mild or rare inaccuracies",
                              "Orange" = "Moderate or occasional falsehoods")


target2 <- c("Not a news", "Mild or rare inaccuracies", "Moderate or occasional falsehoods", "Major or frequent falsehoods")
fakeness_v$fakeness <- factor(fakeness_v$fakeness, levels = target2)

# note: percentages of website links with unknown political leaning for three periods
# were not shown in this chart, but can be found in the table.
ggplot(fakeness_v, aes(x = fakeness_v$period, y = percent, fill=factor(fakeness))) + 
  geom_bar(stat='identity', position = position_dodge2(preserve = "single")) +
  scale_fill_manual(values=c("Grey", "yellow2", "orange", "red3")) +
  scale_y_continuous(labels = scales::percent)+
  theme_minimal() + 
  geom_text(aes(label=n, y = percent + 0.002), 
            position = position_dodge(width=0.9),
            size = 3) +
  scale_x_discrete(labels=c("Late Jan 2020", "Mid March 2020", "Mid March 2021"))+
  labs(x = "Time periods",
       fill = "Falsehood level",
       color = "Falsehood level")

# 4. Network analysis-----
#* nodes ----------
nodes1 <- dat_r %>% filter(period == 'tweets1') %>% 
  group_by(name) %>%         
  summarise(size = n_distinct(author_id)) 
nodes1$partisan <- dat_r$rating[match(nodes1$name,dat_r$name)]
nodes1$fakenews <- dat_r$Likelihood..rating.y[match(nodes1$name,dat_r$link_host)]
nodes1$id <- factor(nodes1$name)
nodes1 <- nodes1[c(5,1,2,3,4)]
#* edges ----------
# Links by co-tweeting patterns (how many times stories from two sites were tweeted 
# by the same person on the same day).
edges1 <- dat_r %>% filter(period == 'tweets1') %>% 
  group_by(author_id, created_at) %>% filter(n()>=2) %>% group_by(author_id, created_at) %>%
  do(data.frame(t(combn(.$name, 2)), stringsAsFactors=FALSE)) %>% filter(X1!= X2)
colnames(edges1) <- c("author_id",'created_at', 'from', 'to')
edges1 <- edges1[c(3,4, 1,2)]

# visualization
library("igraph")
net1 <- graph_from_data_frame(d=edges1, vertices=nodes1, directed=FALSE) 
Isolated = which(degree(net1)==0)
net1_r = delete.vertices(net1, Isolated)

E(net1_r)$arrow.mode <- 0
E(net1_r)$curved <- 0
E(net1_r)$width <- 0
V(net1_r)$size <- sqrt(V(net1_r)$size)
V(net1_r)$label <- V(net1_r)$name

ceb1 <- cluster_edge_betweenness(net1_r) 
plot(net1_r)
plot(ceb1, net1, vertex.label.cex = V(net1)$size/100)

# 5. Text analysis -----
library("tidytext") 
library("textclean")
library("textstem")
library("topicmodels")  
library("topicdoc") 
library(textcat)
Sys.setlocale('LC_ALL','C')
dat_TM <- df_r %>% filter(textcat(df_r$text) == 'english')
dat_TM$text <- replace_contraction(dat_TM$text)
dat_TM$text <- replace_non_ascii(dat_TM$text)
dat_TM$text <- replace_url(dat_TM$text)

dat_TM <- dat_TM %>% distinct(text, .keep_all = TRUE)

dat_tdy <- unnest_tokens(dat_TM, input = text, output = term,
                                   token = "tweets", format = "text",
                                   to_lower = TRUE, strip_punct = TRUE)
dat_tdy_r <- unnest_tokens(dat_TM, input = text, output = term,
                         token = "tweets", format = "text",
                         to_lower = TRUE, strip_punct = TRUE)
data("stop_words")
stopws <- c(stop_words$word, "covid", "covid-19", "covid19", "coronavirus", "virus","#coronavirus",
            "chinese", "china", "wuhan", "#chinesevirus", "#wuhanvirus", "rt", "#covid19",
            "#covid2019", "#chinesevirus19", "#chinavirus", "#wuhan", "#china")
dat_tdy <- dat_tdy[!(dat_tdy$term %in% stopws), ]
dat_tdy$word <- lemmatize_words(dat_tdy$term)

dat_tdy_r <- dat_tdy_r[!(dat_tdy_r$term %in% stopws), ]
dat_tdy_r$word <- lemmatize_words(dat_tdy_r$term)

#* top words by political leaning (top terms, topics)----
# only keep tweets with political ratings
top_words_poli_bias <- dat_tdy %>% drop_na(rating) %>% group_by(period, rating) %>% 
  count(word, sort=TRUE) %>%
  arrange(desc(n)) %>% 
  group_by(period, rating) %>% slice(1:10)

top_words_poli_bias$rating <- factor(top_words_poli_bias$rating, levels = target1)
top_words_poli_bias$period <- factor(top_words_poli_bias$period)
top_words_poli_bias$period <- c("Late January 2020","Mid March 2020","Mid March 2021")[top_words_poli_bias$period]

ggplot(top_words_poli_bias, aes(x = n, y = word)) + 
  geom_col(fill="tomato") + 
  geom_label(aes(label=n), size = 3) +
  theme_bw() + facet_grid(vars(period), vars(rating), scales = "free") + 
  labs(size = 10)+
  theme(strip.text = element_text(size=15))

top_words_poli_bias_r <- dat_tdy_r %>% drop_na(rating) %>% group_by(period, rating) %>% 
  count(word, sort=TRUE) %>%
  arrange(desc(n)) %>% 
  group_by(period, rating) %>% slice(1:10)

top_words_poli_bias_r$rating <- factor(top_words_poli_bias_r$rating, levels = target1)
top_words_poli_bias_r$period <- factor(top_words_poli_bias_r$period)
top_words_poli_bias_r$period <- c("Late January 2020","Mid March 2020","Mid March 2021")[top_words_poli_bias_r$period]

ggplot(top_words_poli_bias_r, aes(x = n, y = word)) + 
  geom_col(fill="tomato") + 
  geom_label(aes(label=n), size = 3) +
  theme_bw() + facet_grid(vars(period), vars(rating), scales = "free") + 
  labs(size = 10)+
  theme(strip.text = element_text(size=15))

stargazer(top_words_poli_bias_r, header=FALSE, type='text', summary=FALSE, title="Data Frame",digits=0)

#* sentiment analysis ----
#** sentiments within each periods----
nrc  <- get_sentiments("nrc")
dat_sen <- group_by(dat_tdy, period) 

nrc_cats <- c("positive", "negative", "joy", "sadness", "anger", "fear")

for(sen in nrc_cats) {
  sen_words <- nrc$word[nrc$sentiment == sen]
  dat_sen[, sen] <- dat_sen$word %in% sen_words + 0
}

tweet_sen <- summarize(dat_sen, positive = sum(positive), negative = sum(negative),
                       joy = sum(joy), sadness = sum(sadness), anger = sum(anger), 
                       fear = sum(fear), disgust = sum(disgust)) 
tweet_sen

# Convert the data format from wide to long for plotting 
sen_long <- pivot_longer(tweet_sen, cols=nrc_cats, names_to = "sentiment")
sen_long$sentiment <- factor(sen_long$sentiment, levels = nrc_cats)
sen_long
sen_long$period <- factor(sen_long$period)
sen_long$period <- c("Late January 2020","Mid March 2020","Mid March 2021")[sen_long$period]

# Plot sentiments within each period:
ggplot(sen_long, aes(x = sentiment, y = value, fill = sentiment)) +  
  facet_wrap( vars(period), nrow =3, scales = "free") + 
  geom_col() +  
  theme_bw()

#** sentiments within each media bias----
dat_sen_bias <- dat_tdy_r %>% drop_na(rating) %>% group_by(period, rating)

for(sen in nrc_cats) {
  sen_words <- nrc$word[nrc$sentiment == sen]
  dat_sen_bias[, sen] <- dat_sen_bias$word %in% sen_words + 0
}

tweet_sen_bias <- summarize(dat_sen_bias, positive = sum(positive), negative = sum(negative),
                       joy = sum(joy), sadness = sum(sadness), anger = sum(anger), 
                       fear = sum(fear)) 
test <- dat_sen_bias %>% count(period, rating) %>% select(n)

tweet_sen_bias <- left_join(tweet_sen_bias, test)

# Convert the data format from wide to long for plotting 
sen_long_bias <- pivot_longer(tweet_sen_bias, cols=nrc_cats, names_to = "sentiment")
sen_long_bias <- sen_long_bias %>% mutate(percent = value / n)
sen_long_bias$sentiment <- factor(sen_long_bias$sentiment, levels = nrc_cats)
sen_long_bias$rating <- factor(sen_long_bias$rating, levels = target1)
sen_long_bias$period <- factor(sen_long_bias$period)
sen_long_bias$period <- c("Late January 2020","Mid March 2020","Mid March 2021")[sen_long_bias$period]

# Plot sentiments within each period:
ggplot(sen_long_bias, aes(x = sentiment, y = percent, fill = sentiment)) +  
  facet_grid(vars(period), vars(rating), scales = "free") + 
  geom_col() +  
  theme_bw() + 
  # geom_text(aes(label=value, y = percent + 0.01), 
  #           position = position_dodge(width=0.9),
  #           size = 3) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# no significant difference