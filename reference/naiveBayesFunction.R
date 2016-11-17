naiveBayesFunction <- function(tweetVocabs,K=25,removeInds=c(1,2)){
source("termFreqTab.R")
load("testData.Rda")
source("classifyTweet.R")

# setup our parameters
donaldDictionary  <- tweetVocabs$donaldVocab
hillaryDictionary <- tweetVocabs$hillaryVocab

donaldTermTable  <- termFreqTab(donaldDictionary,K,removeInds)
hillaryTermTable <- termFreqTab(hillaryDictionary,K,removeInds)

# now classify every tweet!
classification <- matrix(NA, nrow=500,ncol=2)
for(j in 1:2){
	for(i in 1:500){
		tweet <- testData[[j]][i]
		classification[i,j] <- classifyTweet(tweet,donaldTermTable,hillaryTermTable)
	}
}
sums <- colSums(classification)
error <- (sums[1]+(500-sums[2]))/1000

return(list(error=error, hillaryTermTable=hillaryTermTable,donaldTermTable=donaldTermTable))
}