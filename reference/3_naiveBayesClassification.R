# load in the data from the previous step
load("tweetVocabs.Rda")

# The number of 
K <- 25

# Build the term frequency table for Trump
donaldDictionary <- tweetVocabs$donaldVocab

inds <- which(donaldDictionary$vocab$doc_counts >= 
	sort(donaldDictionary$vocab$doc_counts, decreasing=T)[K+2])

topK <- donaldDictionary$vocab[inds,]
topK <- topK[order(topK$doc_counts,decreasing=TRUE),]


# turn this into a terms table
totalTweets <- 30*1302
donaldTerms <- topK$terms
donaldProp  <- topK$doc_counts/totalTweets

donaldTermTable <- cbind(donaldTerms,donaldProp)[-c(1,2),]


# This looks perfect to turn into a function
source("termFreqTab.R")

# setup our parameters
donaldDictionary  <- tweetVocabs$donaldVocab
hillaryDictionary <- tweetVocabs$hillaryVocab
K                 <- 25
removeInds        <- c(1,2)

donaldTermTable  <- termFreqTab(donaldDictionary,K,removeInds)
hillaryTermTable <- termFreqTab(hillaryDictionary,K,removeInds)


# Now let's load some tweets and try to classify them!
load("testData.Rda")

# try with just the first tweet (from donald search)
tweet <- testData[[1]][1]

# compute the naive bayes likelihood that the tweet is from hillary search
outputHil <- rep(NA, nrow(hillaryTermTable))
for(i in 1:nrow(hillaryTermTable)){
	term <- hillaryTermTable[i,1]
	prop <- as.numeric(hillaryTermTable[i,2])

	# see if the term is contained in the tweet and save the correct probability
	if(grepl(term,tweet)){
		outputHil[i] <- prop
	}
	else{
		outputHil[i] <- (1-prop)
	}
}

# compute the naive bayes likelihood that the tweet is from donald search
outputDon <- rep(NA, nrow(donaldTermTable))
for(i in 1:nrow(donaldTermTable)){
	term <- donaldTermTable[i,1]
	prop <- as.numeric(donaldTermTable[i,2])

	# see if the term is contained in the tweet and save the correct probability
	if(grepl(term,tweet)){
		outputDon[i] <- prop
	}
	else{
		outputDon[i] <- (1-prop)
	}
}

prod(outputHil)
prod(outputDon)

classification <- as.numeric((sum(log(outputHil)) - sum(log(outputDon)))>0)

# again, this looks perfect for a function!
source("classifyTweet.R")

classifyTweet(tweet,donaldTermTable,hillaryTermTable)

# now classify every tweet!
classification <- matrix(NA, nrow=500,ncol=2)
for(j in 1:2){
	for(i in 1:500){
		tweet <- testData[[j]][i]
		classification[i,j] <- classifyTweet(tweet,donaldTermTable,hillaryTermTable)
	}
}

# first check of how we did
sums <- colSums(classification)

# compute the total classification error
error <- (sums[1]+(500-sums[2]))/1000

# not so good! let's use a function to try tweaking the terms left off the top
# of the table and the length of the table to get a better error rate!
source("naiveBayesFunction.R")

naiveBayesFunction(tweetVocabs,K=25,removeInds=c(1,2))$error


# try to optimize to get the lowest test error!

