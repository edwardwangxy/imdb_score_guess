require(rpart)
require(rpart.plot)
require(DT)
require(dplyr)
source("imdb_score_clean_func.R")
source("imdb_score_term_func.R")

score_review_name_list = c(NULL)
total = c(NULL)
final_freq_table = c(NULL)
for(i in 2:9)
{
  score_review_name = paste("score_",i,"_reviews", sep = "")
  score_review_name_list = c(score_review_name_list, score_review_name)
  total = rbind(total, get(score_review_name))
}

total_clean_reviews = imdb_score_clean_func(total[,-ncol(total)])
total_table = imdb_score_term_func(total_clean_reviews, K=100)
all_variables = total_table[,1]

#######################################################################################
imdb_train_data_generate = function(variable_name_list, score_review_name_list, processbar = FALSE)
{
  if(processbar)
  {
    bar=0
    total_bar = nrow(score_review_name_list[1])*length(variable_name_list)
    pb <- txtProgressBar(min = 0, max = total_bar, char = "=", style = 3) 
  }
  for(name in score_review_name_list)
  {
    K=length(variable_name_list)
    test_review <- get(name)
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
      final_freq_table = rbind(final_freq_table, guess_term_freq)
    }
  }
  
  colnames(final_freq_table) = c(variable_name_list, "imdb_score")
}

test_table = imdb_train_data_generate(all_variables, score_review_name_list, processbar = TRUE)
str(final_freq_table)





for(i in 1:nrow(termtable)){
  term <- termtable[i,1]
  prop <- as.numeric(termtable[i,2])
  # see if the term is contained in the tweet and save the correct probability
  if(grepl(term,test_review) && length(which(topK$terms == term))>0){
    guess_prob[i] <- prop*topK$terms_counts[which(topK$terms == term)]
    if(length(topK$terms_counts[which(topK$terms == term)])>0)
    {
      guess_term_count[i] <- topK$terms_counts[which(topK$terms == term)]
    }
    else
    {
      guess_term_count[i] <- 0
    }
    guess_term[i] <- term
  }
  else{
    guess_prob[i] <- 0
    if(length(topK$terms_counts[which(topK$terms == term)])>0)
    {
      guess_term_count[i] <- topK$terms_counts[which(topK$terms == term)]
    }
    else
    {
      guess_term_count[i] <- 0
    }
    guess_term[i] <- term
  }
}
final_table = cbind(guess_term, guess_prob, guess_term_count)