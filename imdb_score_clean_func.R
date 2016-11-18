require(text2vec)
imdb_score_clean_func <- function(score_reviews)
{
  tryTolower <- function(x){
    # create missing value
    y = NA
    # tryCatch error
    try_error = tryCatch(tolower(x), error=function(e) e)
    # if not an error
    if (!inherits(try_error, "error"))
      y = tolower(x)
    # result
    return(y)
  }
  reviews <- score_reviews
  reviews <- gsub("[\r\n]", "  ", reviews)
  reviews <- gsub("[[:punct:]]", "", reviews)    # remove punctuation
  reviews <- gsub("[[:cntrl:]]", "", reviews)   # remove control characters
  reviews <- gsub('\\d+', '', reviews)          # remove digits
  lower_review_test <- tryTolower(reviews)
  return(lower_review_test)
}
