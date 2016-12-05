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


generate_raw_dict <- function(movie, reviews)
{
  dict_save_location <- "dictionary" #save location of rawdata and term tables
  source("imdb_review_scraping_func.R")
  source("imdb_class_tree_func.R") #read classify function to create table for raw data
  #choose inputs here
  reviews_each_movie = reviews
  max_movies_pick = movie
  total_movie_count = 0
  ####
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
      review_test = myimdb.reviews.full(get(link_list_name)$movie_imdb_link[[i]], max_reviews = reviews_each_movie)
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
  total = c(NULL)
  pb5 <- txtProgressBar(min = 0, max = 8, char = "=", style = 3)
  for(i in 2:9)
  {
    score_review_name = paste("score_",i,"_reviews", sep = "")
    total = rbind(total, get(score_review_name))
    setTxtProgressBar(pb5, i)
  } #input all score review objects names as list into "score_review_name_list"
  #and row combind reviews of each movie with scores into "total"
  close(pb5)
  total_clean_reviews = imdb_score_clean_func(total[,-ncol(total)]) #cleanup all reviews
  total_table = imdb_score_term_func(total_clean_reviews, K=100) #achieve 100 terms for all reviews
  all_variables = total_table[,1] #achieve only all the terms' name into a list
  
  training_table = imdb_train_data_generate(all_variables, total, processbar = TRUE) #use the function to create a training data table
  training_table$imdb_score = as.factor(training_table$imdb_score) #change numeric into factor for classification
  rawdata_name <- sprintf("rawdata-%d-%d.Rda",max_movies_pick,reviews_each_movie)
  raw_save_list = c(raw_save_list,"max_movies_pick","all_variables","training_table")
  save(list = raw_save_list, file=sprintf("%s/rawdata/%s", dict_save_location,rawdata_name)) #save rawdatas into files
  rm(list = ls()[-grep(paste(raw_save_list,collapse="|"), ls())])  #remove all the useless object
  'source("imdb_score_clean_func.R")
  source("imdb_score_term_func.R")
  
  K_input = 50
  
  dictionary_name <- sprintf("dict-%d-%d.Rda",max_movies_pick,K_input)
  dict_save_location <- "dictionary"
  
  pb3 <- txtProgressBar(min = 0, max = 9, char = "=", style = 3)
  objects_name_to_save = c(NULL)
  for(i in 2:9)
  {
    review_name = paste("score_",i,"_reviews",sep = "")
    term_name = paste("score_",i,"_term",sep = "")
    clean_reviews = imdb_score_clean_func(get(review_name)[,-2])
    table = imdb_score_term_func(clean_reviews, K=K_input)
    assign(term_name, table)
    #rm(list = c(review_name))
    objects_name_to_save = c(objects_name_to_save,review_name, term_name)
    setTxtProgressBar(pb3, i)
  }
  close(pb3)
  save(list = objects_name_to_save, file=sprintf("%s/%s", dict_save_location, dictionary_name))
  rm(list = ls()[-grep(paste(objects_name_to_save,collapse="|"), ls())])'
}

#########################################################################
#start achieving all reviews for each score level and save it into file

generate_raw_dict(20,300)


########################################################################
#create filelist for the shinyapp to read
unlink("dictionary/rawdata/filelist.Rda")
filelist = list.files("dictionary/rawdata")
save(list = "filelist", file=sprintf("dictionary/rawdata/filelist.Rda"))




