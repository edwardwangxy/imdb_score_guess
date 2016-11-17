# data source: https://www.kaggle.com/deepmatrix/imdb-5000-movie-dataset

library(rvest)
library(RColorBrewer)
library(SnowballC)
library(tm)
library(wordcloud)
library(XML)

#selecing subset from dataset
set_review_num = 500
imdb_data <- read.csv("movie_metadata.csv")
imdb_data_select <- imdb_data[order(-imdb_data$imdb_score),c("imdb_score","movie_imdb_link","movie_title","num_user_for_reviews")]
imdb_data_score = rep(NA, 9)
for(i in 2:9)
{
  i=2
  imdb_data_score[i] <- subset(imdb_data_select, num_user_for_reviews >= i)
}
source("imdb_review_scraping_func.R")
review_test = myimdb.reviews(imdb_data_select[1,2])
