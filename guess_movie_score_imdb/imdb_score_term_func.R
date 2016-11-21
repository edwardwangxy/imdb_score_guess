require(text2vec)
imdb_score_term_func <- function(clean_reviews, K)
{
  prep_fun <- tolower # makes lowercase
  tok_fun <- word_tokenizer # look at words
  lower_review_test_tok <- itoken(clean_reviews, preprocessor = prep_fun, tokenizer = tok_fun,  progressbar = FALSE) 
  reviewVocab <- create_vocabulary(lower_review_test_tok, stopwords=c(stopwords("english"),"film","movie","can","films","movies","will","scenes","just")) #, stopwords=c(stopwords("english"),"film","movie","can","films","movies","will","scenes","just")
  inds <- which(reviewVocab$vocab$terms_counts >= sort(reviewVocab$vocab$terms_counts, decreasing=T)[K])
  topK <- reviewVocab$vocab[inds,]
  topK <- topK[order(topK$terms_counts,decreasing=TRUE),]
  topK <- topK[1:K,]
  totalcount <- sum(topK$terms_counts)
  vocTerms <- topK$terms
  vocProp  <- topK$terms_counts/totalcount
  TermTable <- cbind(vocTerms,vocProp)
  return(TermTable)
}