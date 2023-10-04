#load libraries
library(tidytext)
library(textdata)
library(readxl)
library(dplyr)
library(stringr)
library(tibble)
library(ggplot2)

#getting sentiments from 3 seperate libraries/dictionaries
get_sentiments('afinn')
get_sentiments('bing')
get_sentiments('nrc')

#manually select the data file
data2 <- read_xlsx(file.choose())

#separate the tweets in to the words
Data_by_word <- data2 %>% mutate(linenumber = row_number()) %>% unnest_tokens(word,text()
                                                                              
                                                                              
#load stop words - meaningless words(not positive or negative, for example I, am you, to etc )
#use stop words dataset fromtidytext pacakage
data("stop_words") 

#remove curse words - can't report to exces with curse words
# a - curse words , b - type variable
a <- c('fuck','shit','dick','ass','damm','asshole','bitch')
b <- c('CURSE','CURSE','CURSE','CURSE','CURSE','CURSE','CURSE',)
# join the variables to create a data frame
curse_words <- data.frame(a,b)
#column names
colnames(curse_words) <- c('word','lexican')

#append the curse words with the stop words
stop_words<-rbind(stop_words,curse_words)

#remove stops words
Cleansed_Data <- Data_by_word
Cleansed_Data <- Cleansed_Data %>% anti_join(stop_words)

#count most commom words and show top 10 by default
Cleansed_Data %>% count(word, sort = TRUE)

#Graph the most common words
p <- Cleansed_Data %>% count(word, sort = TRUE) %>% filter(n>13) %>% mutate(word = recorder(word,n)) %>%
  ggplot(aes(word,n)) + geom_col() + xlab('words') + ylab('count') + coord_flip()
p + ggtitle('Most popular words')


#graph the most common words without first two i.e.
p <- Cleansed_Data %>% count(word, sort = TRUE) %>% filter(n>13, word != "safety" , word != "replying") %>% mutate(word = recorder(word,n)) %>%
  ggplot(aes(word,n)) + geom_col() + xlab('words') + ylab('count') + coord_flip()
p + ggtitle('Most popular words')


#create a word cloud based upon most popular words
library(wordcloud)
Cleansed_Data %>% 
  filter(word != "safeway" , word != "store", word != "stores") %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 30))

#break out the word cloud by positive/negative using sentiments
library((reshape2))
Cleansed_Data %>%
  inner_join(get_sentiments('afinn')) %>%
  inner_join(get_sentiments('bing')) %>%
  inner_join(get_sentiments('nrc')) %>%
  anti_join(stop_words) %>%
  count(word ~ sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n" , fill = 0) %>%
  comparison.cloud(colors = c("red", "blue"), max.words = 50)

#create dataframe with count and value of words
sentiments <- Cleansed_Data %>% inner_join(get_sentiments("bing")) %>%
count(word, sentiment, sort = TRUE)
sentimentsPros <- subset(sentiments, sentiment=="positive")
sentimentsPros <- subset(sentiments, sentiment=="negative")
  

#sentiment occurrence column is n
Positive_Score <- aggregate(n ~ sentiment, data = SentimentPos, sum)
Negative_Score <- aggregate(n ~ sentiment, data = SentimentNeg, sum)

RatioPosNeg <- (Positive_Score$n/Negative_Score$n)

#print score positive/negative
RatioPosNeg

