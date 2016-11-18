source("imdb_score_clean_func.R")
guess_imdb_review_score <- function(test_review, termtable)
{
  clean_test_review <- imdb_score_clean_func(test_review)
  prep_fun <- tolower # makes lowercase
  tok_fun <- word_tokenizer # look at words
  lower_review_test_tok <- itoken(clean_test_review, preprocessor = prep_fun, tokenizer = tok_fun,  progressbar = FALSE) 
  reviewVocab <- create_vocabulary(lower_review_test_tok, stopwords=c(stopwords("english"),"film","movie","can","films","movies","will","scenes"))
  inds <- which(reviewVocab$vocab$doc_counts >= sort(reviewVocab$vocab$doc_counts, decreasing=T)[K])
  topK <- reviewVocab$vocab[inds,]
  topK <- topK[order(topK$doc_counts,decreasing=TRUE),]
  guess_prob <- rep(NA, nrow(termtable))
  for(i in 1:nrow(termtable)){
    term <- termtable[i,1]
    prop <- as.numeric(termtable[i,2])
    # see if the term is contained in the tweet and save the correct probability
    if(grepl(term,demo_review)){
      outputHil[i] <- prop
    }
    else{
      outputHil[i] <- (1-prop)
    }
  }
}