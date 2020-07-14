require("tm")
library(qdap)
library(tidyr)
library(dplyr)
require(readr)
require(lubridate)
require(stringr)
library(textstem)
library(tidytext)
library(topicmodels)
library(tidyverse)
require(LDAvis)
require(slam)
setwd("C:/Users/VivoBook/Desktop/study/china_steel/text_mini/LDAvis/weather")
var_stopwords<-stopwords()

weather_news<-read_csv("C:/Users/VivoBook/Desktop/study/china_steel/join/data_318/weather_news2012.csv"
                       ,col_names = T,
                       ,col_types = cols( date = col_character(), title = col_character(),body =col_character())
                       ,locale = locale(encoding = "Big-5"))
original_data=weather_news

original_data$date<-ymd(original_data$date)
original_data$date=original_data$date+1
id=which(original_data$date>=ymd("2011/01/01"))
original_data=original_data[id,]
id=which(original_data$date<=ymd("2020/03/19"))
original_data=original_data[id,]
news_range<-range(original_data$date)
original_data$body<-str_replace_all(original_data$body,"[[:punct:]]","")
original_data$news_id=c(1:nrow(original_data))

tmp=original_data %>%unnest_tokens(word, body) %>%
  filter(!grepl("[^a-zA-Z]",word))%>%
  mutate(word=lemmatize_strings(word))%>% #還原詞幹
  group_by(word,news_id)%>%
  summarise(count=n()) 
id=which(tmp$word%in%var_stopwords)

tfm=tmp[-id,]
dtm<-cast_dtm(tfm,news_id,word,count)
news_time=original_data$date
id=which(news_time<=ymd("2017/08/31"))
train_id=which(dtm$dimnames$Docs%in%id)
train_dtm=dtm[train_id,]
test_dtm=dtm[-train_id,]
K=5
for(K in 3:12){
  m_lda <- LDA(train_dtm, k = K, control = list(seed = 1234))# k means class
  m_lda
  fitted=m_lda
  doc_term=train_dtm
  topicmodels_json_ldavis <- function(fitted, doc_term){
    ls_LDA = function (phi)
    {
      jensenShannon <- function(x, y) {
        m <- 0.5 * (x + y)
        lhs <- ifelse(x == 0, 0, x * (log(x) - log(m+1e-16)))
        rhs <- ifelse(y == 0, 0, y * (log(y) - log(m+1e-16)))
        0.5 * sum(lhs) + 0.5 * sum(rhs)
      }
      dist.mat <- proxy::dist(x = phi, method = jensenShannon)
      pca.fit <- stats::cmdscale(dist.mat, k = 2)
      data.frame(x = pca.fit[, 1], y = pca.fit[, 2])
    }
    
    # Find required quantities
    phi <- as.matrix(posterior(fitted)$terms)
    theta <- as.matrix(posterior(fitted)$topics)
    vocab <- colnames(phi)
    term_freq <- slam::col_sums(doc_term)
    
    # Convert to json
    json_lda <- LDAvis::createJSON(phi = phi, theta = theta,
                                   vocab = vocab,
                                   doc.length = as.vector(table(doc_term$i)),
                                   term.frequency = term_freq, mds.method = ls_LDA)
    
    return(json_lda)
  }
  ##
  json_res <- topicmodels_json_ldavis(m_lda,train_dtm)
  
  
  lda_dir =  paste0(K,"_ldavis")
  LDAvis::serVis(json_res, out.dir =lda_dir, open.browser = F)
  print(K)
}

LDAvis::serVis(json_res, open.browser = T)
