# data source: https://www.kaggle.com/deepmatrix/imdb-5000-movie-dataset

library(rvest)
library(RColorBrewer)
library(SnowballC)
library(tm)
library(wordcloud)
library(XML)
library(RColorBrewer)
library(rpart.plot)
###############################################################################
#                       Start picking movies from dataset                     #
###############################################################################
#selecing subset from dataset
#Set the minimum size of reviews of movie to pick as training set
#read the table file and grabe all the links of movies with reivews larger than the limit

set_review_num = 200 #setting minimum reviews for picking movies
imdb_data <- read.csv("movie_metadata.csv") #read the data set
#order the scores
imdb_data_select <- imdb_data[order(-imdb_data$imdb_score),c("imdb_score","movie_imdb_link","movie_title","num_user_for_reviews")]
imdb_data_select <- subset(imdb_data_select, num_user_for_reviews >= set_review_num) #subset dataset
pb <- txtProgressBar(0,9, style = 3)
for(i in 2:9) #start spliting movies into different scores.
{
  var_name = paste("imdb_score_",i,sep = "")
  imdb_data_score <- subset(imdb_data_select, imdb_score >=i)
  imdb_data_score <- subset(imdb_data_score, imdb_score <i+1)
  imdb_data_score$movie_imdb_link <- lapply(imdb_data_score$movie_imdb_link, as.character)
  assign(var_name, imdb_data_score)
  setTxtProgressBar(pb, i)
  Sys.sleep(0.5)
}
close(pb)


###############################################################################
#                       Start crawling reviews from imdb                     #
###############################################################################
#Using the links start achieving all reviews for each score level

dict_save_location <- "dictionary" #save location of rawdata and term tables
source("imdb_review_scraping_func.R")
source("imdb_class_tree_func.R") #read classify function to create table for raw data
#choose inputs here
reviews_each_movie = 1000
max_movies_pick = 20
total_movie_count = 0
####
progress_bar_counting = 0
for(n in 2:9)
{
  link_list_name = paste("imdb_score_",n,sep="")
  total_movie_count = total_movie_count + min(nrow(get(link_list_name)),max_movies_pick)
}
pb2 <- txtProgressBar(min = 0, max = total_movie_count, char = "=", style = 3)
raw_save_list = c(NULL)
for(n in 2:9)
{
  link_list_name = paste("imdb_score_",n,sep="")
  for(i in 1:min(nrow(get(link_list_name)),max_movies_pick)) #do not achieve more than available movies
  {
    progress_bar_counting = progress_bar_counting + 1
    if(i ==1) #create empty list for later use
    {
      review_final = list(NULL)
      review_score = list(NULL)
    }
    review_test = myimdb.reviews.full(get(link_list_name)$movie_imdb_link[[i]], max_reviews = reviews_each_movie)
    setTxtProgressBar(pb2, progress_bar_counting)
    review_score = c(review_test, get(link_list_name)$imdb_score[i])
    review_final = rbind(review_final, review_score)
  }
  review_name = paste("score_",n,"_reviews", sep = "")
  raw_save_list = c(raw_save_list, review_name, link_list_name)
  review_final = review_final[-1,] #get rid of NULL row
  assign(review_name, review_final)
  #rm(list = c(link_list_name)) 
}
close(pb2)
total = c(NULL)
pb5 <- txtProgressBar(min = 0, max = 8, char = "=", style = 3)
for(i in 2:9)
{
  score_review_name = paste("score_",i,"_reviews", sep = "")
  total = rbind(total, get(score_review_name))
  setTxtProgressBar(pb5, i)
} #input all score review objects names as list into "score_review_name_list"
#and row combind reviews of each movie with scores into "total"
close(pb5)
total_clean_reviews = imdb_score_clean_func(total[,-ncol(total)]) #cleanup all reviews
total_table = imdb_score_term_func(total_clean_reviews, K=100) #achieve 100 terms for all reviews
all_variables = total_table[,1] #achieve only all the terms' name into a list

training_table = imdb_train_data_generate(all_variables, total, processbar = TRUE) #use the function to create a training data table
training_table$imdb_score = as.factor(training_table$imdb_score) #change numeric into factor for classification
rawdata_name <- sprintf("rawdata-%d-%d.Rda",max_movies_pick,reviews_each_movie)
raw_save_list = c(raw_save_list,"max_movies_pick","all_variables","training_table")
save(list = raw_save_list, file=sprintf("%s/rawdata/%s", dict_save_location,rawdata_name)) #save rawdatas into files
rm(list = ls()[-grep(paste(raw_save_list,collapse="|"), ls())])  #remove all the useless object

###############################################################################
#                       Start creating term tables                            #
###############################################################################
#create term table for different scores and save to dictionary
load("dictionary/rawdata/rawdata-20-1000.Rda") #using this function to load data directly
source("imdb_score_clean_func.R")
source("imdb_score_term_func.R")

K_input = 50

dictionary_name <- sprintf("dict-%d-%d.Rda",max_movies_pick,K_input)
dict_save_location <- "dictionary"

pb3 <- txtProgressBar(min = 0, max = 9, char = "=", style = 3)
objects_name_to_save = c(NULL)
for(i in 2:9)
{
  review_name = paste("score_",i,"_reviews",sep = "")
  term_name = paste("score_",i,"_term",sep = "")
  clean_reviews = imdb_score_clean_func(get(review_name)[,-2])
  table = imdb_score_term_func(clean_reviews, K=K_input)
  assign(term_name, table)
  #rm(list = c(review_name))
  objects_name_to_save = c(objects_name_to_save,review_name, term_name)
  setTxtProgressBar(pb3, i)
}
close(pb3)
objects_name_to_save = c(objects_name_to_save,"all_variables","training_table")
save(list = objects_name_to_save, file=sprintf("%s/%s", dict_save_location, dictionary_name))
rm(list = ls()[-grep(paste(objects_name_to_save,collapse="|"), ls())])

###############################################################################
#                       Start Guessing using term tables                      #
###############################################################################
#start guessing the score
#load("dictionary/dict-30-70-50.Rda") #using this function to load dictionary directly
source("imdb_guess_review_func.R")
source("imdb_review_scraping_func.R")
test_web_url = "http://www.imdb.com/title/tt0209144/?ref_=fn_al_tt_1" #<westword> imdb_url
max_reviews_test = 1000
review_prob = c(NULL)
try_score_review = myimdb.reviews.full(test_web_url, max_reviews_test)

pb4 <- txtProgressBar(min = 0, max = 9, char = "=", style = 3)
for(i in 2:9)
{
  term_name = paste("score_",i,"_term",sep="")
  review_term_table_name = paste("review_",i,"_table",sep="")
  review_term_table = guess_imdb_review_score(try_score_review, get(term_name))
  review_prob = c(review_prob,sum(as.numeric(review_term_table[,2])))
  assign(review_term_table_name, review_term_table)
  setTxtProgressBar(pb4, i)
}
close(pb4)
guess_table = cbind(2:9,review_prob)
barplot(as.numeric(review_2_table[1:10,3]), legend.text = review_2_table[1:10,1],col=brewer.pal(10, "Paired"))
colnames(guess_table)=c("score","prob")
guess_table <- guess_table[order(guess_table[,2],decreasing=TRUE),]
#rm(list = ls()[-grep("guess_table", ls())])
cat(paste("First Guess is score ", guess_table[1,1],"\nSecond Guess is score ", guess_table[2,1]))


###############################################################################
#               Start creating classification tree and analysis               #
###############################################################################
source("imdb_class_tree_func.R") #read classify function to create table for raw data

test_table = imdb_test_data_generate(all_variables, try_score_review, processbar = TRUE) #use the function to create a training data table


#create classification trees
fit <- rpart(imdb_score ~ .,
             method="class", data=training_table,
             control = rpart.control(minsplit=20, cp=0.001))

#plot the tree
prp(fit, main="Classification Tree",
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

#prune the tree
pfit<- prune(fit, cp=fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
#plot the pruned tree
prp(pfit, main="Pruned Tree",
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


#using the training dataset retest the trees, these function could be used to see the difference
#conf.matrix <- table(training_table$imdb_score, predict(pfit,type="class"))
#conf.matrix 
#conf.matrix2 <- table(training_table$imdb_score, predict(fit,type="class"))
#conf.matrix2

predict = predict(fit, test_table, type="class")
as.character(predict)
predict2 = predict(pfit, test_table, type="class")
as.character(predict2)

###############################################################################
#               Try random forest analysis                                    #
###############################################################################
library(randomForest)
rownames(training_table) = NULL
ffit <- randomForest(imdb_score ~ .,
                    ntree=2000,
                    data=training_table)
print(fit) # view results 
importance(ffit)
as.character(predict(ffit, test_table, type="response"))
rm(list = ls())
