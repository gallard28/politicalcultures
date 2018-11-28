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



#LDA on abstracts####
