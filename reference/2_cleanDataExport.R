library(twitteR)
library(text2vec)
# create the dictionaries used for the classification

setwd()

# load in the function to clean the data
source("tweetCleanFunction.R")


# Build the donald term table (load takes a while!)
load("Donald1323.Rda")
donaldClean <- tweetCleanFunction(donald)

# clean out the workspace
rm(donald)
gc()

prep_fun <- tolower # makes lowercase
tok_fun <- word_tokenizer # look at words

# Create the vocabulary using itoken to iterate directly
donaldIt <- itoken(donaldClean, 
                  preprocessor = prep_fun, 
                  tokenizer = tok_fun,  
                  progressbar = FALSE) 

donaldVocab <- create_vocabulary(donaldIt)

# clean out the workspace
rm(donaldClean)
gc()

# Build the hillary term table (load takes a while!)
load("Hillary1323.Rda")
hillaryClean <- tweetCleanFunction(hillary)

# clean out the workspace
rm(hillary)
gc()

prep_fun <- tolower # makes lowercase
tok_fun <- word_tokenizer # look at words

# Create the vocabulary using itoken to iterate directly
hillaryIt <- itoken(hillaryClean, 
                  preprocessor = prep_fun, 
                  tokenizer = tok_fun,  
                  progressbar = FALSE) 

hillaryVocab <- create_vocabulary(hillaryIt)

# clean out the workspace
rm(hillaryClean)
gc()


# create an object saving the 
tweetVocabs <- list(donaldVocab=donaldVocab,
					hillaryVocab=hillaryVocab)

#save(tweetVocabs,file="tweetVocabs.Rda")