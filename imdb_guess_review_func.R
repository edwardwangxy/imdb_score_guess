source("imdb_score_clean_func.R")
guess_imdb_review_score <- function(demo_review, termtable)
{
  clean_demo_review <- imdb_score_clean_func(demo_review)
  guess_prob <- rep(NA, nrow(termtable))
  for(i in 1:nrow(termtable)){
    term <- termtable[i,1]
    prop <- as.numeric(termtable[i,2])
    
    # see if the term is contained in the tweet and save the correct probability
    if(grepl(term,tweet)){
      outputHil[i] <- prop
    }
    else{
      outputHil[i] <- (1-prop)
    }
  }
}