require(text2vec)
imdb_score_term_func <- function(clean_reviews, K)
{
  prep_fun <- tolower # makes lowercase
  tok_fun <- word_tokenizer # look at words
  lower_review_test_tok <- itoken(clean_reviews, preprocessor = prep_fun, tokenizer = tok_fun,  progressbar = FALSE) 
  reviewVocab <- create_vocabulary(lower_review_test_tok, stopwords=c(stopwords("english"),"film","movie","can","films","movies","will","scenes"))
  inds <- which(reviewVocab$vocab$doc_counts >= sort(reviewVocab$vocab$doc_counts, decreasing=T)[K])
  topK <- reviewVocab$vocab[inds,]
  topK <- topK[order(topK$doc_counts,decreasing=TRUE),]
  totalcount <- sum(reviewVocab$vocab$doc_counts)
  vocTerms <- topK$terms
  vocProp  <- topK$doc_counts/totalcount
  TermTable <- cbind(vocTerms,vocProp)
  TermTable <- TermTable[1:K,]
  return(TermTable)
}