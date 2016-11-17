classifyTweet <- function(tweet,donaldTermTable,hillaryTermTable){

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

# 0 for donald, 1 for hillary
classification <- as.numeric((sum(log(outputHil)) - sum(log(outputDon)))>0)

return(classification)
}