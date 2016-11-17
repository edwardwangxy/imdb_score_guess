termFreqTab <- function(dictionary,K=25,removeInds=c(1,2)){

inds <- which(dictionary$vocab$doc_counts >= 
	sort(dictionary$vocab$doc_counts, decreasing=T)[K+length(removeInds)])

topK <- dictionary$vocab[inds,]
topK <- topK[order(topK$doc_counts,decreasing=TRUE),]


# turn this into a terms table
totalTweets <- 30*1302
terms <- topK$terms
prop  <- topK$doc_counts/totalTweets

termTable <- cbind(terms,prop)[-removeInds,]

return(termTable)
}