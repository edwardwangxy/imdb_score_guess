require(rpart)
require(rpart.plot)
require(DT)
require(dplyr)
source("imdb_score_clean_func.R")
source("imdb_score_term_func.R")

score_review_name_list = c(NULL)
total = c(NULL)

for(i in 2:9)
{
  score_review_name = paste("score_",i,"_reviews", sep = "")
  score_review_name_list = c(score_review_name_list, score_review_name)
  total = rbind(total, get(score_review_name))
}

total_clean_reviews = imdb_score_clean_func(total[,-ncol(total)])
total_table = imdb_score_term_func(total_clean_reviews, K=100)
all_variables = total_table[,1]
test_table = imdb_train_data_generate(all_variables, score_review_name_list, processbar = TRUE)
test_table$imdb_score = as.factor(test_table$imdb_score)
v_name = paste(all_variables, collapse = "+")
rpart_formu = paste("imdb_score~",v_name, sep = "")
fit <- rpart(imdb_score ~ .,
             method="class", data=test_table,
             control = rpart.control(minsplit=20, cp=0.001))
prp(fit, main="Classification Tree for Spam Data",
    #extra="auto", # display prob of survival and percent of obs
    fallen.leaves=TRUE, # put the leaves on the bottom of the page
    shadow.col="gray", # shadows under the leaves
    branch.lty=2, # draw branches using dotted lines
    #branch=.5, # change angle of branch lines
    faclen=0, # faclen=0 to print full factor names
    trace=1, # print the automatically calculated cex
    split.cex=1.2, # make the split text larger than the node text
    split.prefix="Is ", # put "is " before split text
    split.suffix="?", # put "?" after split text
    #col=cols, border.col=cols, # red if spam, blue if not
    split.box.col="lightgray", # lightgray split boxes (default is white)
    split.border.col="darkgray", # darkgray border on split boxes
    split.round=.5) # round the split box corners a tad

pfit<- prune(fit, cp=fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
prp(pfit, main="Classification Tree for Spam Data",
    #extra="auto", # display prob of survival and percent of obs
    fallen.leaves=TRUE, # put the leaves on the bottom of the page
    shadow.col="gray", # shadows under the leaves
    branch.lty=2, # draw branches using dotted lines
    #branch=.5, # change angle of branch lines
    faclen=0, # faclen=0 to print full factor names
    trace=1, # print the automatically calculated cex
    split.cex=1.2, # make the split text larger than the node text
    split.prefix="Is ", # put "is " before split text
    split.suffix="?", # put "?" after split text
    #col=cols, border.col=cols, # red if spam, blue if not
    split.box.col="lightgray", # lightgray split boxes (default is white)
    split.border.col="darkgray", # darkgray border on split boxes
    split.round=.5) # round the split box corners a tad
#######################################################################################
imdb_train_data_generate = function(variable_name_list, score_review_name_list, processbar = FALSE)
{
  final_freq_table = c(NULL)
  if(processbar)
  {
    bar=0
    total_bar = nrow(get(score_review_name_list[1]))*length(variable_name_list)
    pb <- txtProgressBar(min = 0, max = total_bar+3, char = "=", style = 3) 
  }
  for(a in 1:length(score_review_name_list))
  {
    test_review <- get(score_review_name_list[a])
    for(i in 1:nrow(test_review))
    {
      test_review_n = test_review[i,]
      clean_test_review <- imdb_score_clean_func(test_review_n[-length(test_review_n)])
      prep_fun <- tolower # makes lowercase
      tok_fun <- word_tokenizer # look at words
      lower_review_test_tok <- itoken(clean_test_review, preprocessor = prep_fun, tokenizer = tok_fun,  progressbar = FALSE) 
      reviewVocab <- create_vocabulary(lower_review_test_tok, stopwords=c(stopwords("english"),"film","movie","can","films","movies","will","scenes","just")) #
      topK <- reviewVocab$vocab
      topK <- topK[order(topK$terms_counts,decreasing=TRUE),]
      topK <- subset(topK, select = c("terms","terms_counts"))
      guess_term <- rep(NA, length(variable_name_list))
      guess_prob <- rep(NA, length(variable_name_list))
      guess_term_freq <- rep(NA, length(variable_name_list))
      for(j in 1:length(variable_name_list))
      {
        term <- variable_name_list[j]
        if(grepl(term,test_review_n) && length(which(topK$terms == term))>0 && length(topK$terms_counts[which(topK$terms == term)])>0)
        {
          guess_term_freq[j] <- topK$terms_counts[which(topK$terms == term)]/sum(topK$terms_counts)*100
        }
        else
        {
          guess_term_freq[j] <- 0
        }
        if(processbar)
        {
          bar = bar+1
          setTxtProgressBar(pb, bar) 
        }
      }
      guess_term_freq[length(variable_name_list)+1]=floor(as.numeric(test_review_n[length(test_review_n)]))
      if(processbar)
      {
        bar = bar+1
        setTxtProgressBar(pb, bar) 
      }
      final_freq_table = rbind(final_freq_table, guess_term_freq)
      if(processbar)
      {
        bar = bar+1
        setTxtProgressBar(pb, bar) 
      }
    }
  }
  
  colnames(final_freq_table) = c(variable_name_list, "imdb_score")
  if(processbar)
  {
    bar = bar+1
    setTxtProgressBar(pb, bar) 
  }
  return(as.data.frame(final_freq_table))
}

#######################################################################################