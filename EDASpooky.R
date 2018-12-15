library
?datatable
library(tidyverse)

library(DT)
# library('wordcloud')
# library(igraph)
# library(ggraph)

# 
library(caret)
library(dtplyr)
library(dplyr)
library(data.table)
library(ggplot2)

###for text analytics
library(koRpus)
library(SnowballC)
library(tidytext)
library(stringr)
library(stringi)
library(tm)
library(wordnet)
library(wordcloud)
library(nnet)
library(mxnet)
unzip(file.choose())
trainOrig = fread(file.choose())
testOrig = fread(file.choose())
View(head(trainOrig))
summary(trainOrig)

# id                text              author         
# Length:19579       Length:19579       Length:19579      
# Class :character   Class :character   Class :character  
# Mode  :character   Mode  :character   Mode  :character 

glimpse(trainOrig)


Variables: 3
# $ id     <chr> "id26305", "id17569", "id11008", "id27763", "id12958", "id2296...
# $ text   <chr> "This process, however, afforded me no means of ascertaining t...
# $ author <chr> "EAP", "HPL", "EAP", "MWS", "HPL", "MWS", "EAP", "EAP", "EAP",...


trainOrig %>% distinct(author)
# 
# author
# 1:    EAP
# 2:    HPL
# 3:    MWS

# microbenchmark(nchar(trainOrig$text),str_length(trainOrig$text),stri_length(trainOrig$text),str_count(trainOrig$text))
trainOrig = trainOrig %>% mutate(len = stri_length(text))
testOrig = testOrig %>% mutate(len = stri_length(text))


trainOrig %>% group_by(author) %>% 
      summarise(MedianByAuthor = median(len)) %>%
      arrange(MedianByAuthor) %>% ggplot(mapping = aes(x=author,y=MedianByAuthor))+
      geom_bar(stat = "identity",fill="blue") +
      geom_text(mapping = aes(x = author,y= MedianByAuthor,label = MedianByAuthor),
                color = "orange",nudge_y = 5)

        
trainMutated = trainOrig %>% unnest_tokens(Words,text)
testMutated = testOrig %>% unnest_tokens(Words,text)
as.tibble(trainMutated)

# initDict()
# synonyms("Understand","VERB")
# getLemma("Understanding")
# 
# 
# filter = getTermFilter("StartsWithFilter", "Under", TRUE)
# terms = getIndexTerms("NOUN",)
# sapply(terms, getLemma)
# class(terms)
# str(terms)
# getDict()
# setDict("C://Program Files (x86)//WordNet//2.1//dict")

countVsWordlength = trainMutated %>% group_by(id,author) %>% summarise(count = n()) %>% 
  ggplot()+geom_histogram(mapping = aes(x=count),binwidth = 10)+xlim(c(0,150))+facet_wrap(~author)+
  xlab("Word length")
ggsave("countVsWordlength.jpg",plot = countVsWordlength)


###TODO: word cloud for each author put this in a loop with filter for each author
trainMutated = trainMutated %>% as.tibble() %>% 
  anti_join(stop_words,by = c("Words" = "word")) %>% 
  group_by(Words,author) %>% 
   summarise(wordcount = n()) %>% 
   arrange(desc(wordcount)) %>% head(30)%>% with(wordcloud(Words,wordcount,max.words=10)) 
# 
#  Words author wordcount
#  <chr>  <chr>     <int>
#    1    life    MWS       329
#  2    love    MWS       273
#  3   heart    MWS       262
# trainOrig[id == "id00001",]  
 
#  trainMutated = trainMutated %>% left_join(trainMutated %>% group_by(id,author,Words) %>% 
#    summarise(Wordcount = n()) %>% ungroup(),by = c("id" = "id","author" = "author","Words" = "Words"))
# 
#  trainMutated %>% bind_tf_idf(Words,id,Wordcount) %>% View()
# 
# max(trainMutated$len)
# trainMutated[id== "id27184",] %>% group_by(Words,Wordcount) %>% filter(Words == "a") 
#   summarise(sumterms = sum(Wordcount)) %>% filter(Words == "a") %>% mutate(tf = freq/Wordcount) %>%
#   arrange(Words) %>% filter(Words == "i")
#   
# nrow(trainMutated[id== "id27184",] )


trainMutTFIDF = trainMutated %>% group_by(author,id,Words) %>% 
  summarise(Wordcount = n()) %>% 
    bind_tf_idf(Words,id,Wordcount) 


trainMutTFIDF %>% ungroup() %>%  arrange(desc(tf_idf)) %>% View()


# Checking the tfidf
# trainOrig %>% distinct(id) %>% summarise(n())
# View(stop_words)
# 19579
# trainMutated %>% filter(Words == "mountebanks") 
# log(19579/1)

# data("AssociatedPress", package = "topicmodels")
# ap_td <- tidy(AssociatedPress)
# # the output here ap_td should look very very similar to your tibble above
# ap_td %>% cast_dtm(document, term, count)

rm(df)
?count

  train_dtm <- trainOrig[,-c("len","author")] %>% head(1000) %>% 
    unnest_tokens(Words, text) %>% as.tibble() %>% 
    mutate(Words = stemDocument(Words)) %>% 
    group_by(id,Words) %>% summarise(n = n()) %>% ungroup() 
  
  
castDTM = train_dtm %>% arrange(id)%>% cast_dtm(id,Words,n)
  View(castDTM)
  as.matrix(castDTM)[ ,1:10]
  train_dtm =  trainMutated %>% mutate(Words = stemDocument(Words))

    testDf = trainMutated %>% as.data.frame()
    testDf[, -c("len")]
  
 dataForTrain = cbind(as.data.frame(as.matrix(castDTM)),as.data.frame(trainOrig[1:1000,c("author")]))  
head(dataForTrain)
colnames(dataForTrain) = c(colnames(dataForTrain[,1:4363]),"MainAuthor")


spooky.nnet = nnet(MainAuthor~., data = dataForTrain, size=1, maxit=500)
  
  
  
  