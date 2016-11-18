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
imdb_data_select <- subset(imdb_data_select, num_user_for_reviews >= set_review_num)
pb <- txtProgressBar(0,9, style = 3)
for(i in 2:9)
{
  var_name = paste("imdb_score_",i,sep = "")
  imdb_data_score <- subset(imdb_data_select, imdb_score >=i)
  imdb_data_score <- subset(imdb_data_score, imdb_score <i+1)
  imdb_data_score$movie_imdb_link <- lapply(imdb_data_score$movie_imdb_link, as.character)
  assign(var_name, imdb_data_score)
  setTxtProgressBar(pb, i)
  Sys.sleep(0.5)
}
close(pb)

#start achieving all reviews for each score level
source("imdb_review_scraping_func.R")
pages_each_movie = 5
max_movies_pick = 3
pb2 <- txtProgressBar(min = 0, max = 9, char = "=", style = 3)
for(n in 2:9)
{
  link_list_name = paste("imdb_score_",n,sep="")
  for(i in 1:min(nrow(get(link_list_name)),max_movies_pick))
  {
    if(i ==1)
    {
      review_final = c("")
    }
    review_test = myimdb.rangereviews(get(link_list_name)$movie_imdb_link[[i]], range = pages_each_movie)
    review_final = c(review_final, review_test)
  }
  review_name = paste("score_",n,"_reviews", sep = "")
  assign(review_name, review_final)
  rm(list = c(link_list_name))
  setTxtProgressBar(pb2, n)
}
close(pb2)

#create term table for different score
source("imdb_score_clean_func.R")
source("imdb_score_term_func.R")

K_input = 30
pb3 <- txtProgressBar(min = 0, max = 9, char = "=", style = 3)
for(i in 2:9)
{
  review_name = paste("score_",i,"_reviews",sep = "")
  term_name = paste("score_",i,"_term",sep = "")
  clean_reviews = imdb_score_clean_func(get(review_name))
  table = imdb_score_term_func(clean_reviews, K=K_input)
  assign(term_name, table)
#  rm(list = c(review_name))
  setTxtProgressBar(pb3, n)
}
close(pb3)



