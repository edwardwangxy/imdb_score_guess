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
dict_save_location <- "dictionary"
source("imdb_review_scraping_func.R")
pages_each_movie = 50
max_movies_pick = 50
total_movie_count = 0
progress_bar_counting = 0
for(t in 2:9)
{
  link_list_name = paste("imdb_score_",n,sep="")
  total_movie_count = total_movie_count + min(nrow(get(link_list_name)),max_movies_pick)
}
pb2 <- txtProgressBar(min = 0, max = total_movie_count, char = "=", style = 3)
for(n in 2:9)
{
  link_list_name = paste("imdb_score_",n,sep="")
  for(i in 1:min(nrow(get(link_list_name)),max_movies_pick))
  {
    progress_bar_counting = progress_bar_counting + 1
    if(i ==1)
    {
      raw_save_list = c(NULL)
      review_final = list(NULL)
    }
    review_test = myimdb.rangereviews(get(link_list_name)$movie_imdb_link[[i]], range = pages_each_movie)
    setTxtProgressBar(pb2, progress_bar_counting)
    review_final = c(review_final, review_test)
  }
  review_name = paste("score_",n,"_reviews", sep = "")
  raw_save_list = c(raw_save_list, review_name)
  assign(review_name, review_final)
  rm(list = c(link_list_name))
}
close(pb2)
rawdata_name <- sprintf("rawdata%s%s.Rda",substring(Sys.time(),12,13),substring(Sys.time(),15,16))
save(list = raw_save_list,file=sprintf("%s/rawdata/%s", dict_save_location,rawdata_name))

#create term table for different scores and save to dictionary
source("imdb_score_clean_func.R")
source("imdb_score_term_func.R")
dictionary_name <- sprintf("dict%s%s.Rda",substring(Sys.time(),12,13),substring(Sys.time(),15,16))
dict_save_location <- "dictionary"
K_input = 50
pb3 <- txtProgressBar(min = 0, max = 9, char = "=", style = 3)
objects_name_to_save = c(NULL)
for(i in 2:9)
{
  review_name = paste("score_",i,"_reviews",sep = "")
  term_name = paste("score_",i,"_term",sep = "")
  clean_reviews = imdb_score_clean_func(get(review_name))
  table = imdb_score_term_func(clean_reviews, K=K_input)
  assign(term_name, table)
  rm(list = c(review_name))
  objects_name_to_save = c(objects_name_to_save,term_name)
  setTxtProgressBar(pb3, n)
}
close(pb3)
save(list = objects_name_to_save,file=sprintf("%s/%s", dict_save_location,dictionary_name))
#start guessing the score
source("imdb_guess_review_func.R")
test_web_url = "http://www.imdb.com/title/tt1211837/?pf_rd_m=A2FGELUUNOQJNL&pf_rd_p=2240084082&pf_rd_r=1VGTP0PDM2J06YZFWVAN&pf_rd_s=center-1&pf_rd_t=15506&pf_rd_i=moviemeter&ref_=chtmvm_tt_1" #<westword> imdb_url
grab_pages = 50
review_prob = c(NULL)
try_score_review = myimdb.rangereviews(test_web_url, grab_pages)
for(i in 2:9)
{
  i=9
  term_name = paste("score_",i,"_term",sep="")
  review_term_prob = guess_imdb_review_score(try_score_review, get(term_name))
  review_prob = c(review_prob,review_term_prob)
}
guess_table = cbind(2:9,review_prob)
colnames(guess_table)=c("score","prob")
guess_table[,1][which.max(guess_table[,2])]
