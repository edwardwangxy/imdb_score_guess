source("imdb_score_clean_func.R")
guess_imdb_review_score <- function(test_review, termtable)
{
  K=nrow(termtable)
  test_review <- paste(test_review, collapse = " ")
  clean_test_review <- imdb_score_clean_func(test_review)
  prep_fun <- tolower # makes lowercase
  tok_fun <- word_tokenizer # look at words
  lower_review_test_tok <- itoken(clean_test_review, preprocessor = prep_fun, tokenizer = tok_fun,  progressbar = FALSE) 
  reviewVocab <- create_vocabulary(lower_review_test_tok, stopwords=c(stopwords("english"),"film","movie","can","films","movies","will","scenes","just","one","like")) #
  topK <- reviewVocab$vocab
  topK <- topK[order(topK$terms_counts,decreasing=TRUE),]
  topK <- subset(topK, select = c("terms","terms_counts"))
  guess_term <- rep(NA, nrow(termtable))
  guess_prob <- rep(NA, nrow(termtable))
  guess_term_count <- rep(NA, nrow(termtable))
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
  return(final_table)
}

