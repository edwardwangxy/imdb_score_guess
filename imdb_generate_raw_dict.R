library(rvest)
library(RColorBrewer)
library(SnowballC)
library(tm)
library(wordcloud)
library(XML)

#########################################################################
#selecing subset from dataset
set_review_num = 100
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


generate_raw_dict <- function(page, movie)
{
  dict_save_location <- "dictionary"
  source("imdb_review_scraping_func.R")
  pages_each_movie = page
  max_movies_pick = movie
  total_movie_count = 0
  progress_bar_counting = 0
  for(n in 2:9)
  {
    link_list_name = paste("imdb_score_",n,sep="")
    total_movie_count = total_movie_count + min(nrow(get(link_list_name)),max_movies_pick)
  }
  pb2 <- txtProgressBar(min = 0, max = total_movie_count, char = "=", style = 3)
  raw_save_list = c(NULL)
  for(n in 2:9)
  {
    link_list_name = paste("imdb_score_",n,sep="")
    for(i in 1:min(nrow(get(link_list_name)),max_movies_pick)) #do not achieve more than available movies
    {
      progress_bar_counting = progress_bar_counting + 1
      if(i ==1) #create empty list for later use
      {
        review_final = list(NULL)
        review_score = list(NULL)
      }
      review_test = myimdb.rangereviews(get(link_list_name)$movie_imdb_link[[i]], range = pages_each_movie)
      setTxtProgressBar(pb2, progress_bar_counting)
      review_score = c(review_test, get(link_list_name)$imdb_score[i])
      review_final = rbind(review_final, review_score)
    }
    review_name = paste("score_",n,"_reviews", sep = "")
    raw_save_list = c(raw_save_list, review_name, link_list_name)
    review_final = review_final[-1,] #get rid of NULL row
    assign(review_name, review_final)
    #rm(list = c(link_list_name)) 
  }
  close(pb2)
  rawdata_name <- sprintf("rawdata-%d-%d.Rda",max_movies_pick,pages_each_movie)
  raw_save_list = c(raw_save_list,"max_movies_pick","pages_each_movie")
  save(list = raw_save_list, file=sprintf("%s/rawdata/%s", dict_save_location,rawdata_name))
  source("imdb_score_clean_func.R")
  source("imdb_score_term_func.R")
  
  'K_input = 50
  
  dictionary_name <- sprintf("dict-%d-%d-%d.Rda",max_movies_pick,pages_each_movie,K_input)
  dict_save_location <- "dictionary"
  
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
    setTxtProgressBar(pb3, i)
  }
  close(pb3)
  save(list = objects_name_to_save, file=sprintf("%s/%s", dict_save_location, dictionary_name))'
}

#########################################################################
#start achieving all reviews for each score level
pages_list = seq(30, 40, 5)
movie_list = seq(40, 50, 10)
for(movie_choose in movie_list)
{
  for(pages_choose in pages_list)
  {
    generate_raw_dict(page = 10, movie = 30)
  }
}

########################################################################
unlink("dictionary/rawdata/filelist.Rda")
filelist = list.files("dictionary/rawdata")
save(list = "filelist", file=sprintf("dictionary/rawdata/filelist.Rda"))




