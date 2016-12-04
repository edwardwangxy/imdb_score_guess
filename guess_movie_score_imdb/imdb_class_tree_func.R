require(rpart)
require(rpart.plot)
require(DT)
require(dplyr)
source("imdb_score_clean_func.R")
source("imdb_score_term_func.R")


#######################################################################################
imdb_train_data_generate = function(variable_name_list, total_table, processbar = FALSE)
{
  final_freq_table = c(NULL)
  if(processbar)
  {
    bar=0
    total_bar = nrow(total_table)*length(variable_name_list)
    pb <- txtProgressBar(min = 0, max = total_bar+3, char = "=", style = 3) 
  }
  test_review = total_table
  for(i in 1:nrow(test_review))
  {
    test_review_n = test_review[i,]
    clean_test_review <- imdb_score_clean_func(test_review_n[-length(test_review_n)])
    prep_fun <- tolower # makes lowercase
    tok_fun <- word_tokenizer # look at words
    lower_review_test_tok <- itoken(clean_test_review, preprocessor = prep_fun, tokenizer = tok_fun,  progressbar = FALSE) 
    reviewVocab <- create_vocabulary(lower_review_test_tok, stopwords=c(stopwords("english"),"film","movie","can","films","movies","will","scenes","just")) #
    topK <- reviewVocab$vocab
    topK <- topK[order(topK$terms_counts,decreasing=TRUE),]
    topK <- subset(topK, select = c("terms","terms_counts"))
    guess_term <- rep(NA, length(variable_name_list))
    guess_prob <- rep(NA, length(variable_name_list))
    guess_term_freq <- rep(NA, length(variable_name_list))
    for(j in 1:length(variable_name_list))
    {
      term <- variable_name_list[j]
      if(grepl(term,test_review_n) && length(which(topK$terms == term))>0 && length(topK$terms_counts[which(topK$terms == term)])>0)
      {
        guess_term_freq[j] <- topK$terms_counts[which(topK$terms == term)]/sum(topK$terms_counts)*100
      }
      else
      {
        guess_term_freq[j] <- 0
      }
      if(processbar)
      {
        bar = bar+1
        setTxtProgressBar(pb, bar) 
      }
    }
    guess_term_freq[length(variable_name_list)+1]=floor(as.numeric(test_review_n[length(test_review_n)]))
    if(processbar)
    {
      bar = bar+1
      setTxtProgressBar(pb, bar) 
    }
    final_freq_table = rbind(final_freq_table, guess_term_freq)
    if(processbar)
    {
      bar = bar+1
      setTxtProgressBar(pb, bar) 
    }
  }
  
  colnames(final_freq_table) = c(variable_name_list, "imdb_score")
  final_freq_table = as.data.frame(final_freq_table)
  rownames(final_freq_table) = NULL
  if(processbar)
  {
    bar = bar+1
    setTxtProgressBar(pb, bar) 
  }
  return(final_freq_table)
}

#######################################################################################

imdb_test_data_generate = function(variable_name_list, test_review, processbar = FALSE)
{
  final_freq_table = c(NULL)
  if(processbar)
  {
    bar=0
    total_bar = length(variable_name_list)
    pb <- txtProgressBar(min = 0, max = total_bar+3, char = "=", style = 3) 
  }
  test_review_n = test_review
  clean_test_review <- imdb_score_clean_func(test_review_n)
  prep_fun <- tolower # makes lowercase
  tok_fun <- word_tokenizer # look at words
  lower_review_test_tok <- itoken(clean_test_review, preprocessor = prep_fun, tokenizer = tok_fun,  progressbar = FALSE) 
  reviewVocab <- create_vocabulary(lower_review_test_tok, stopwords=c(stopwords("english"),"film","movie","can","films","movies","will","scenes","just")) #
  topK <- reviewVocab$vocab
  topK <- topK[order(topK$terms_counts,decreasing=TRUE),]
  topK <- subset(topK, select = c("terms","terms_counts"))
  guess_term <- rep(NA, length(variable_name_list))
  guess_prob <- rep(NA, length(variable_name_list))
  guess_term_freq <- rep(NA, length(variable_name_list))
  for(j in 1:length(variable_name_list))
  {
    term <- variable_name_list[j]
    if(grepl(term,test_review_n) && length(which(topK$terms == term))>0 && length(topK$terms_counts[which(topK$terms == term)])>0)
    {
      guess_term_freq[j] <- topK$terms_counts[which(topK$terms == term)]/sum(topK$terms_counts)*100
    }
    else
    {
      guess_term_freq[j] <- 0
    }
    if(processbar)
    {
      bar = bar+1
      setTxtProgressBar(pb, bar) 
    }
  }
  guess_term_freq[length(variable_name_list)+1]=0
  if(processbar)
  {
    bar = bar+1
    setTxtProgressBar(pb, bar) 
  }
  final_freq_table = rbind(final_freq_table, guess_term_freq)
  if(processbar)
  {
    bar = bar+1
    setTxtProgressBar(pb, bar) 
  }
  
  colnames(final_freq_table) = c(variable_name_list, "imdb_score")
  final_freq_table = as.data.frame(final_freq_table)
  rownames(final_freq_table) = NULL
  if(processbar)
  {
    bar = bar+1
    setTxtProgressBar(pb, bar) 
  }
  return(final_freq_table)
}


