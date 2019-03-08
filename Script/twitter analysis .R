library(twitteR) # twitter API mining 
library(tidyverse)
library(tidytext) # text cleaning 
library(tm)
library(dplyr)
library(wordcloud) 
library(SnowballC) # stop words
library(topicmodels) #LDA
# i have created a dev account on twitter, here is the key 

consumer_key <- "3gGi0LPPT7vX3DHMm6yrNXSmF"
consumer_secret<- "7DHK1jv72UVMZov5E4hTfZhKivC45OtVfrAcnFcOfcDX5mEc8A"
access_token <- "976013431395569664-lCivXetBT0X4pak8HVJMtdLUSPzcg3P"
access_secret <- "nxVGanyiLWRSCUIFP5mvv7AQ7jKrbE2We4c6NgP3hwX5I"

#set up to authenticate
setup_twitter_oauth(consumer_key ,consumer_secret,access_token ,access_secret)

# get tweets from an account
tweets <- userTimeline("institute_irnas", n = 3200)
length(tweets)

# delete the any retweets in the returned tweets 
strip_retweets(tweets)

# get the users list 
user <- getUser("institute_irnas")
# get the user's friends(who the user follows) and followers 
friends <- user$getFriends() # who this user follows
friends_df <- twListToDF(friends) %>%
    rownames_to_column()
followers <- user$getFollowers() # the followers of this user
followers_df <- twListToDF(followers) %>%
    rownames_to_column()

# we are also interested in the social network of the followers 
# therefore we are going to extract the friends of them 

for (i in 1:length(friends)) {
    friends2 <- friends[[i]]$getFriends() # my friends' friends
    friends2_df <- twListToDF(friends2) %>%
        rownames_to_column() %>%
        mutate(friend = as.character(friends[[i]]$id))
    
    if (i == 1) {
        friends2_df_final <- friends2_df
    } else {
        friends2_df_final <- rbind(friends2_df_final, friends2_df)
    }
    print(i)
}


#combine friends and followers in a data frame 
friends_followers_df <- rbind(mutate(followers_df, type = ifelse(screenName %in% friends_df$screenName, "friend & follower", "follower")),
                              mutate(friends_df, type = ifelse(screenName %in% followers_df$screenName, "friend & follower", "friend"))) %>%
    unique()
summary(as.factor(friends_followers_df$type))

# plot the information in a bar plot, wewant to see what are the 
# top 10 langues spoken, divided by types

friends_followers_df %>%
    group_by(type) %>%
    count(lang) %>%
    top_n(10) %>%
    droplevels() %>%
    ggplot(aes(x = reorder(lang, desc(n)), y = n)) +
    facet_grid(type ~ ., scales = "free") +
    geom_bar(stat = "identity", color = "#377F97", fill = "#377F97", alpha = 0.8) +
    theme_bw() +
    theme(strip.background=element_rect(fill = "#4A9888")) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
    labs(x = "language ISO 639-1 code",
         y = "number of friends / followers",
         title = "Top 10 languages of friends and followers",
         caption = expression("Twitter friends and followers of @institute_irnas"))

# here is the top followers, we want to select top 10 followers whose 
# status update is most frequent until today 

# who is the top followers? find out which follower has the most followers 
top_fol <- followers_df %>%
    mutate(date = as.Date(created, format = "%Y-%m-%d"),
           today = as.Date("2017-06-07", format = "%Y-%m-%d"),
           days = as.numeric(today - date),
           statusesCount_pDay = statusesCount / days) %>%
    select(screenName, followersCount, statusesCount_pDay) %>%
    arrange(desc(followersCount)) %>%
    .[1:10, ]
# what are the most active tweets, with the most frequent status change? 
top_tweet <- followers_df %>%
    mutate(date = as.Date(created, format = "%Y-%m-%d"),
           today = as.Date("2017-06-07", format = "%Y-%m-%d"),
           days = as.numeric(today - date),
           statusesCount_pDay = statusesCount / days) %>%
    select(screenName, followersCount, statusesCount_pDay) %>%
    arrange(desc(statusesCount_pDay)) %>%
    .[1:10, ]

# let's combine them together? 
top_fol_tweet <- rbind(top_fol, top_tweet) %>%
    unique()
#Here we want to see if the number of followers correspond to 
# the freqency of their tweet status update 
followers_df %>%
    mutate(date = as.Date(created, format = "%Y-%m-%d"),
           today = as.Date("2017-06-07", format = "%Y-%m-%d"),
           days = as.numeric(today - date),
           statusesCount_pDay = statusesCount / days) %>%
    ggplot(aes(x = followersCount, y = statusesCount_pDay)) +
    geom_smooth(method = "lm", color = "#377F97") +
    geom_point(color = "#4A9888", alpha = 0.6) +
    #geom_text(data = top_fol_tweet, aes(label = screenName), check_overlap = TRUE, size = 2) +
    scale_x_continuous(trans='log2') +
    scale_y_continuous(trans='log2') +
    theme_bw() +
    labs(x = expression(log[2]~"number of followers"),
         y = expression(log[2]~"average nr. of tweets per day"),
         title = "Codecentric's most influential followers",
         #subtitle = "Text labels show the top 10 followers with most tweets per day and highest number of followers (screen names)",
         caption = expression(2^nd~"degree followers and tweet rate of @codecentric Twitter followers (data from July"~15^th~"2017)"))

# we can also calculate the top followers by both the number of 
# their followers, and the frequency of daily update 

top_fol2 <- followers_df %>%
    mutate(date = as.Date(created, format = "%Y-%m-%d"),
           today = as.Date("2017-06-07", format = "%Y-%m-%d"),
           days = as.numeric(today - date),
           statusesCount_pDay = statusesCount / days) %>%
    select(screenName, followersCount, statusesCount_pDay) %>%
    mutate(score = followersCount * statusesCount_pDay) %>% # a score indicating the number of their followers as well as daily updates 
    arrange(desc(score)) %>%
    .[1:100, ]

# this data frame wants to create a dataset with the top 100 followers
# with their screen name and the description of their profile. 
top_fol_tweet2 <- top_fol2 %>%
    left_join(select(followers_df, screenName, description), by = "screenName") %>%
    mutate(id = seq_along(1:n()))

# next we want to see, what are those people, we want to build a word cloud from the 
# descriptions of these followers's profiles 
#build our corpus 
Corpus_des <- Corpus(VectorSource(top_fol_tweet2$description))
#tranform to lower case 
Corpus_des <- tm_map(Corpus_des, content_transformer(tolower))
# remove URLs
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
Corpus_des<- tm_map(Corpus_des, content_transformer(removeURL))

# remove anything other than English letters or space 
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x) 
Corpus_des <- tm_map(Corpus_des, content_transformer(removeNumPunct))

# remove stop words or even user defined words 
Corpus_des <- tm_map(Corpus_des, removeWords, stopwords('english'))

# strip extra whitespace 
Corpus_des <- tm_map(Corpus_des, stripWhitespace)

# stem the document 
Corpus_des <- tm_map(Corpus_des, stemDocument)

# create a term document matrix 
tdm <- TermDocumentMatrix(Corpus_des, control = list(wordLengths = c(1, Inf)))

# now we can find the most frequent words in this corpus 
# first let's use the tm package function 
findFreqTerms(tdm, lowfreq = 10)

# the result will give us some idea about what is the cut-off point of our frequency limit 
# now we can really calculate the exact number of these terms by summing up the row numbers of tdm 
# let's lower the limit of our frequency in order to have more result 
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq > 5)
term.freq
# word cloud
word.freq <- sort(rowSums(as.matrix(tdm)), decreasing = T)
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 5)

# now let's look at the content of tweents from our subject user 
tweets.df <- twListToDF(tweets)
r_stats_text_corpus <- Corpus(VectorSource(tweets.df$text))

#clean the tweets
r_stats_text_corpus <- tm_map(r_stats_text_corpus, content_transformer(removeURL))
r_stats_text_corpus <- tm_map(r_stats_text_corpus, content_transformer(tolower)) 
r_stats_text_corpus <- tm_map(r_stats_text_corpus, content_transformer(removeNumPunct))
r_stats_text_corpus <- tm_map(r_stats_text_corpus, removeWords, stopwords('english'))
r_stats_text_corpus <- tm_map(r_stats_text_corpus, stemDocument)

# build a term document matrix 
tdm2 <- TermDocumentMatrix(r_stats_text_corpus)
findFreqTerms(tdm2, lowfreq = 10)
# this result more or less gives us what are these tweets are about 
# apparently they are talking a lot about IOT, opensource and development etc 
# let's look at the topics of these tweets with LDA 
# first let's transpose the tdm into a dtm 
dtm2 <- as.DocumentTermMatrix(tdm2)
lda <- LDA(dtm2, k = 8)
term <- terms(lda, 7)