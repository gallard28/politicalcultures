#Title: Literature Review on Elazar - Data Mining Script####
#Author: Grant A. Allard
#Date
#Purpose: Data mine abstracts for insights about their content


#Set up and libraries####
sessionInfo()

#Libraries
library(dplyr)
library(stringr)
library(tidytext)
library(tm)
library(purrr)
library(RColorBrewer)
library(wordcloud)
library(topicmodels)
library(ggplot2)

#Load Data
library_raw<-read.csv('Allard_Elazar Library.csv')

#Structure of Data
str(library_raw)

#Conver to df
library_df<-as_data_frame(library_raw)

#Need to clean data
library_df$Author<-as.character(library_df$Author)
library_df$Title<-as.character(library_df$Title)
library_df$Publication.Title<-as.character(library_df$Publication.Title)
library_df$Abstract.Note<-as.character(library_df$Abstract.Note)

#Missing abstracts
nrow(library_df[is.na(library_df$Abstract.Note),])

#Analysis####
#Subset data to remove articles I wrote annotated abstracts for
remove<-library_df %>% 
  filter(str_detect(Author, "Caughey") |str_detect(Author, "Simmons") |str_detect(Author, "Amat") | str_detect(Author, "Fossum")| str_detect(Author, "Csehi")| str_detect(Author, "Vollaard") | str_detect(Author, "Fabbrini") )

library_df2<-library_df[!(library_df$Key %in% remove$Key),]

#Wordcloud####
abstracts<-Corpus(VectorSource(library_df2$Abstract.Note))

#Special character handling
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
abstracts <- tm_map(abstracts, toSpace, "/")
abstracts <- tm_map(abstracts, toSpace, "@")
abstracts <- tm_map(abstracts, toSpace, "\\|")

#Clean Text
# Convert the text to lower case
abstracts <- tm_map(abstracts, content_transformer(tolower))
# Remove numbers
abstracts <- tm_map(abstracts, removeNumbers)
# Remove english common stopwords
abstracts <- tm_map(abstracts, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
abstracts <- tm_map(abstracts, removeWords, c("article", "yet", "political")) 
# Remove punctuations
abstracts <- tm_map(abstracts, removePunctuation)
# Eliminate extra white spaces
abstracts <- tm_map(abstracts, stripWhitespace)

#TDM 
abstracts_tdm<-TermDocumentMatrix(abstracts)
abstracts_m<-as.matrix(abstracts_tdm)
abstracts_v<-sort(rowSums(abstracts_m),decreasing=TRUE)
abstracts_d<-data.frame(word= names(abstracts_v), frequency=abstracts_v)

#Wordcloud
set.seed(1234)
wordcloud(words = abstracts_d$word, freq = abstracts_d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#LDA on coprupus####
#Create a DTM 
abstracts_dtm<-DocumentTermMatrix(abstracts)

#LDA on abstracts####
abstracts_lda<-LDA(abstracts_dtm, k=12, control=list(seed=1234))

#topics
abstracts_topics<-tidy(abstracts_lda, matrix="beta")

#top terms
abstracts_top_terms<-abstracts_topics %>% 
  group_by(topic) %>% 
  top_n(10,beta) %>% 
  ungroup() %>% 
  arrange(topic,-beta)

#Plot
abstracts_top_terms %>% 
  mutate(term= reorder(term,beta)) %>% 
  ggplot(aes(term, beta, fill= factor(topic)))+
  geom_col(show.legend =FALSE)+
  facet_wrap(~topic, scales="free")+
  coord_flip()


#Calculate perplexity for topics
n_topics<- c(2,3,4,5, 6, 7, 8, 9, 10,11, 12)
abstracts_lda_compare<- n_topics %>% 
  map(LDA, x=abstracts_dtm, control = list(seed=1234))

abstracts_lda_compare_df<-data_frame(k = n_topics, 
           perplex= map_dbl(abstracts_lda_compare, perplexity))
abstracts_lda_compare_df
abstracts_lda_compare_df %>% 
  ggplot(aes(k, perplex))+
  geom_point()+
  geom_line()

#Focus on Topic 3
abstracts_top_terms[abstracts_top_terms$topic==3,]

#Focus on Topic 8
abstracts_top_terms[abstracts_top_terms$topic==8,]


#Now let's see if we can put the abstracts back together####
by_abstract<-library_df2 %>% 
  select(Title, Abstract.Note) %>% 
  group_by(Title)
 
by_abstract

#Split into words
by_abstract_words<- by_abstract %>% 
  unnest_tokens(word, Abstract.Note)

by_abstract_words

#Find Document-Word Counts
word_counts <- by_abstract_words %>% 
  anti_join(stop_words) %>% 
  count(Title, word, sort=TRUE) %>% 
  ungroup()
word_counts

#Cast into Chapters#
by_abstract_dtm<-word_counts %>% 
  cast_dtm(Title, word, n)
by_abstract_dtm

#Create LDA Object
by_abstract_lda<-LDA(by_abstract_dtm, k=12, control=list(seed=1234))
by_abstract_lda

#Create Matrix
by_abstract_topics<- tidy(by_abstract_lda, matrix="beta")
by_abstract_topics

#Create table
top_topics<-by_abstract_topics %>% 
  group_by(topic) %>% 
  top_n(5, beta) %>% 
  ungroup()
top_topics

#Visualize Results 
top_topics %>% 
  mutate(term = reorder(term,beta)) %>% 
  ggplot(aes(term, beta, fill=factor(topic)))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~topic, scales="free")+
  coord_flip()

#Calculate perplexity for topics
n_topics<- c(2,3,4,5, 6, 7, 8, 9, 10,11, 12)
abstracts_lda_compare<- n_topics %>% 
  map(LDA, x=by_abstract_dtm, control = list(seed=1234))

abstracts_lda_compare_df<-data_frame(k = n_topics, 
                                     perplex= map_dbl(abstracts_lda_compare, perplexity))
abstracts_lda_compare_df
abstracts_lda_compare_df %>% 
  ggplot(aes(k, perplex))+
  geom_point()+
  geom_line()

#Classifying Abstracts by Topic####
by_abstract_gamma<-tidy(by_abstract_lda, matrix="gamma")
by_abstract_gamma

by_abstract_gamma %>% 
  mutate(title= reorder(document, gamma * topic)) %>% 
  ggplot(aes(factor(topic), gamma))+
  geom_boxplot()+
  facet_wrap(~document)

#Export to Excel to Create Table
write.csv(by_abstract_gamma, "abstract classification")









