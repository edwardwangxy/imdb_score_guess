# A function that takes an array of tweets and cleans them. The elements of the
# array should be twitteR objects

tweetCleanFunction <- function(dat){
require(twitteR)
require(text2vec)

single <- dat[1,1][[1]]
text <- single$text

tweet <- text
tweet <- gsub("[[:punct:]]", "", tweet)    # remove punctuation
tweet <- gsub("[[:cntrl:]]", "", tweet)   # remove control characters
tweet <- gsub('\\d+', '', tweet)          # remove digits



# we won't always be able to call tolower so we need a try-catch to make
# sure that we can loop effectively
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

lowerTweet <- tryTolower(tweet)

allTweets <- matrix(NA, nrow(dat),ncol(dat))


for(locIndex in 1:nrow(dat)){
	for(i in 1:ncol(dat)){
		single <- dat[locIndex,i][[1]]

		text <- single$text

		tweet <- text
		tweet <- gsub("[[:punct:]]", "", tweet)    # remove punctuation
		tweet <- gsub("[[:cntrl:]]", "", tweet)   # remove control characters
		tweet <- gsub('\\d+', '', tweet)          # remove digits

		lowerTweet <- tryTolower(tweet)

		allTweets[locIndex,i] <- lowerTweet
	}
}

return(allTweets)

}