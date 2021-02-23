###################################
# importing all necessary packages 
###################################
#install.packages("rtweet")
#install.packages("stopwords")
#install.packages("RColorBrewer")
#install.packages("widyr")
#install.packages("igraph")
library(rtweet) 
library(dplyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(stopwords)
library(tm)
library(wordcloud)  
library(RColorBrewer) #adding some color to our wordcloud
library(widyr) #for n-grams
library(tidyr)
library(igraph)
library(ggraph)
library(dplyr)
library(tidytext)

##############################
# create library from Twitter
##############################
########## first library: sblv ##########
sblv <- search_tweets(
  "#SBLV" , n = 3000, lang= "en", include_rts = FALSE
)

########## second library: sbhts ##########
sbhts <- search_tweets(
  "#SuperBowl + #halftimeshow" , n = 3000, lang= "en", include_rts = FALSE
)

########## third library: sbcomm ##########
sbcomm <- search_tweets(
  "#superbowlcommercials" , n = 3000, lang= "en", include_rts = FALSE
)

#######################
# data cleaning part 1 
#######################

########## sblv ##########
#Removing the URLs 
sblv$stripped_text <- gsub("http.*", "", sblv$text)

#tokenizing: tidying the text
sblv_tweets_clean <- sblv %>% 
  dplyr::select(stripped_text) %>%
  unnest_tokens(word, stripped_text)

########## halftimeshow ##########
#Removing the URLs 
sbhts$stripped_text <- gsub("http.*", "", sbhts$text)

#tokenizing: tidying the text
sbhts_tweets_clean <- sbhts %>% 
  dplyr::select(stripped_text) %>%
  unnest_tokens(word, stripped_text)

########## super bowl commercial ##########
#Removing the URLs 
sbcomm$stripped_text <- gsub("http.*", "", sbcomm$text)

#tokenizing: tidying the text
sbcomm_tweets_clean <- sbcomm %>% 
  dplyr::select(stripped_text) %>%
  unnest_tokens(word, stripped_text)

#####################################
# data cleaning part 2 and Workcloud
#####################################

########## first library ##########

#my list of words to not include
unnecessary_words1 <- c("congratulations", "winning", "game", "team", "congrats", "fans", "football", "season", "time", "watch", "victory", "2021", "superbowl", "bowl", "super", "superbowllv", "sblv", "superbowlsunday", "superbowl2021", "city")

#creating a Wordcloud
sblv_tweets_clean %>% 
  count(word, sort=TRUE) %>%
  anti_join(stop_words) %>% #exclude stopwords
  filter(!word %in% unnecessary_words1) %>% #excluding the unnecessary words
  filter(nchar(word)>3) %>% #only including words that have at least 3 chr
  with(wordcloud(word, #add the word
                 n,  #word-count
                 main= "Wordcloud for  #SBLV", 
                 scale=c(4,0.5),
                 use.r.layout=FALSE,
                 max.words = 25, #limit the numer of words
                 colors=brewer.pal(8, "Set1")))
       

########## second library ##########
#my list of words to not include
unnecessary_words2 <- c("superbowlads" , "game" , "commercials" , "superbowllv", "sblv", "superbowlsunday", "spot", "superbowl2021", "2021" , "it's" , "superbowl", "bowl", "super", "superbowlhalftimeshow", "performing game", "love", "half", "performance", "people", "time", "halftimeshow", "halftime", "weeknd", "superbowlweeknd", "commercial", "night", "watch")

#creating a Wordcloud
sbcomm_tweets_clean %>% 
  count(word, sort=TRUE) %>%
  anti_join(stop_words) %>% #exclude stopwords
  filter(!word %in% unnecessary_words2) %>% #excluding the unnecessary words
  filter(nchar(word)>3) %>% #only including words that have at least 3 chr
  with(wordcloud(word, #add the word
                 n,  #word-count
                 scale=c(5,0.5),
                 use.r.layout=FALSE,
                 max.words = 25, #limit the numer of words
                 colors=brewer.pal(8, "Set1")))

##################
# create bi-grams
##################
data("stop_words")

#as we create bi-grams, we will notice unnecessary words again, therefore we decide to filter it again
unnecessary_words3 <- c("superbowlads" , "game" , "commercials" , "superbowllv", "sblv", "superbowlsunday", "spot", "superbowl2021", "2021" , "it's" , "superbowl", "bowl", "super", "superbowlhalftimeshow", "performing game", "love", "half", "performance", "people", "time", "halftimeshow", "halftime", "weeknd", "superbowlweeknd", "commercial", "night", "watch")

#creating the bigrams
sblv_paired_words <- sblv %>% 
  dplyr::select(stripped_text) %>%
  unnest_tokens(paired_words, stripped_text, token="ngrams", n=2) #bigrams

#word-pair counts
sblv_paired_words %>%
  count(paired_words, sort=TRUE)

#separating to filter
sblv_separated_words <- sblv_paired_words %>% 
  separate(paired_words, c("word1", "word2"), sep = " ") 

#exclude the stop words from n-grams
sblv_tweets_filter <- sblv_separated_words %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(nchar(word1)>3) %>%
  filter(!word1 %in% unnecessary_words3) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word2 %in% unnecessary_words4) %>%
  filter(nchar(word2)>3) 

#view the filtered word pairs
sblv_word_counts <- sblv_tweets_filter %>%
  count(word1, word2, sort=TRUE)

#visualize bigrams
sblv_word_counts %>% 
  filter(n>17) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes()) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label= name), vjust= 1.8, size= 5) 

###########################################
# explore Half Time Show most tweets words
###########################################

unnecessary_words4 <- c("superbowllv", "sblv", "superbowlsunday", "superbowl2021", "2021" , "superbowl", "bowl", "super", "superbowlhalftimeshow", "performing game", "love", "half", "performance", "people", "time", "halftimeshow", "halftime", "weeknd", "superbowlweeknd", "pepsi", "abel", "weekend", "game")

#plotting the frequency plot for the words mentioned under the hashtag
sbhts_tweets_clean %>% 
  count(word, sort=TRUE) %>%
  top_n(200) %>%
  anti_join(stop_words) %>%
  filter(!word %in% unnecessary_words4) %>% #excluding the unnecessary words
  filter(nchar(word)>3) %>% #only including words that have at least 3 chr
  mutate(word= reorder(word, n)) %>%
  ggplot(aes(x=word, y = n)) +
  geom_bar(stat= "identity",
           fill="blue") +  #coloring the bars blue
  xlab(NULL) + #removing the x-axis label
  ylab(NULL) + #removing the y-axis label
  ggtitle("What words were most frequent words about half time show tweets?")+
  theme(plot.title = element_text(size=5))+ #changing the text size
  coord_flip() 

######################################################
# explore audiences' feeling for Super Bowl this year
######################################################

unnecessary_words5 <- c("2021", "superbowl", "bowl", "super")

#sentiment: NRC
sblv_tweets_clean %>%
  inner_join(get_sentiments("nrc")) %>% #using the nrc sentiment
  count(word, sentiment, sort=TRUE) %>% 
  anti_join(stop_words) %>%
  distinct() %>%
  filter(!word %in% unnecessary_words5) %>% #excluding the unnecessary words
  filter(nchar(word)>3) %>%
  group_by(sentiment) %>%
  top_n(5) %>% #showing the top 3 words for every emotion
  ungroup() %>%
  ggplot(aes(word, n, fill=sentiment))+
  geom_col(show.legend = FALSE) + #hiding the legend
  facet_wrap(~sentiment, scales="free_y") + #adding multiple graphs
  xlab(NULL) +
  ylab("Emotions about the Super Bowl 2021")+
  theme(axis.text.x=element_text(size=6, angle=45)) +
  coord_flip()

