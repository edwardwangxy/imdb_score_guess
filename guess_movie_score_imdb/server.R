shinyServer(
  function(input, output) {
    source("my_wordcloud_func.R")
    source("imdb_review_scraping_func.R")
    source("imdb_score_clean_func.R")
    source("imdb_score_term_func.R")
    source("imdb_guess_review_func.R")
    source("imdb_class_tree_func.R")
    library(randomForest)
    library(rpart)
    ###############################################################
    output$plot1_1 <- renderPlot({
        withProgress(message = 'Creating score_2 wordcloud', value = 0, {
          rawdata_url = paste("rawdata/",input$rawdata, sep ="")
          incProgress(1/4, detail = "")
          env = load(rawdata_url)
          incProgress(1/4, detail = "")
          review_var_name = paste("score_2_reviews", sep = "")
          incProgress(1/4, detail = "")
          myfunc.wordcloud(get(review_var_name), remove_words = c(stopwords("english"),"film","movie","can","films","movies","will","scenes","just","one","like"))
          incProgress(1/4, detail = "")
      })
    })
    test_inside = "test_inside"
    output$plot1_2 <- renderPlot({
      withProgress(message = 'Creating score_3 wordcloud', value = 0, {
        rawdata_url = paste("rawdata/",input$rawdata, sep ="")
        incProgress(1/4, detail = "")
        env = load(rawdata_url)
        incProgress(1/4, detail = "")
        review_var_name = paste("score_3_reviews", sep = "")
        incProgress(1/2, detail = "")
        myfunc.wordcloud(get(review_var_name), remove_words = c(stopwords("english"),"film","movie","can","films","movies","will","scenes","just","one","like"))
        incProgress(1/2, detail = "")
      })
    })
    
    output$plot1_3 <- renderPlot({
      withProgress(message = 'Creating score_4 wordcloud', value = 0, {
        rawdata_url = paste("rawdata/",input$rawdata, sep ="")
        incProgress(1/4, detail = "")
        env = load(rawdata_url)
        incProgress(1/4, detail = "")
        review_var_name = paste("score_4_reviews", sep = "")
        incProgress(1/2, detail = "")
        myfunc.wordcloud(get(review_var_name), remove_words = c(stopwords("english"),"film","movie","can","films","movies","will","scenes","just","one","like"))
        incProgress(1/2, detail = "")
      })
    })
    
    output$plot1_4 <- renderPlot({
      withProgress(message = 'Creating score_5 wordcloud', value = 0, {
        rawdata_url = paste("rawdata/",input$rawdata, sep ="")
        incProgress(1/4, detail = "")
        env = load(rawdata_url)
        incProgress(1/4, detail = "")
        review_var_name = paste("score_5_reviews", sep = "")
        incProgress(1/2, detail = "")
        myfunc.wordcloud(get(review_var_name), remove_words = c(stopwords("english"),"film","movie","can","films","movies","will","scenes","just","one","like"))
        incProgress(1/2, detail = "")
      })
    })
    
    output$plot1_5 <- renderPlot({
      withProgress(message = 'Creating score_6 wordcloud', value = 0, {
        rawdata_url = paste("rawdata/",input$rawdata, sep ="")
        incProgress(1/4, detail = "")
        env = load(rawdata_url)
        incProgress(1/4, detail = "")
        review_var_name = paste("score_6_reviews", sep = "")
        incProgress(1/2, detail = "")
        myfunc.wordcloud(get(review_var_name), remove_words = c(stopwords("english"),"film","movie","can","films","movies","will","scenes","just","one","like"))
        incProgress(1/2, detail = "")
      })
    })
    
    output$plot1_6 <- renderPlot({
      withProgress(message = 'Creating score_7 wordcloud', value = 0, {
        rawdata_url = paste("rawdata/",input$rawdata, sep ="")
        incProgress(1/4, detail = "")
        env = load(rawdata_url)
        incProgress(1/4, detail = "")
        review_var_name = paste("score_7_reviews", sep = "")
        incProgress(1/2, detail = "")
        myfunc.wordcloud(get(review_var_name), remove_words = c(stopwords("english"),"film","movie","can","films","movies","will","scenes","just","one","like"))
        incProgress(1/2, detail = "")
      })
    })
    
    output$plot1_7 <- renderPlot({
      withProgress(message = 'Creating score_8 wordcloud', value = 0, {
        rawdata_url = paste("rawdata/",input$rawdata, sep ="")
        incProgress(1/4, detail = "")
        env = load(rawdata_url)
        incProgress(1/4, detail = "")
        review_var_name = paste("score_8_reviews", sep = "")
        incProgress(1/2, detail = "")
        myfunc.wordcloud(get(review_var_name), remove_words = c(stopwords("english"),"film","movie","can","films","movies","will","scenes","just","one","like"))
        incProgress(1/2, detail = "")
      })
    })
    
    output$plot1_8 <- renderPlot({
      withProgress(message = 'Creating score_9 wordcloud', value = 0, {
        rawdata_url = paste("rawdata/",input$rawdata, sep ="")
        incProgress(1/4, detail = "")
        env = load(rawdata_url)
        incProgress(1/4, detail = "")
        review_var_name = paste("score_9_reviews", sep = "")
        incProgress(1/2, detail = "")
        myfunc.wordcloud(get(review_var_name), remove_words = c(stopwords("english"),"film","movie","can","films","movies","will","scenes","just","one","like"))
        incProgress(1/2, detail = "")
      })
    })
    
    ##############################################################
    observeEvent(input$search_btn,{
      withProgress(message = 'Searching imdb movies', value = 0, {
      search_table = myimdb.search(isolate(input$search_movie), rate = TRUE)
      output$x1 = DT::renderDataTable(search_table, 
                                      options = list(
                                        lengthMenu = list(c(3, 5, 10), c('3', '5', '10')),
                                        pageLength = 5
                                      ),
                                      server = FALSE,
                                      selection = 'single')
      incProgress(1/1, detail = "")
      })
    })
    
    output$info = renderPrint({
      input$guess_btn
      rm(list = ls())
      s = isolate(input$x1_rows_selected)
      if(length(s)<1){cat("Please search and select a movie to guess\n\n")}
      else{
        withProgress(message = 'Starting the guess progress', value = 0, {
          search_table = myimdb.search(isolate(input$search_movie))
          incProgress(1/3, detail = "")
          rawdata_url = paste("rawdata/",isolate(input$rawdata), sep ="")
          incProgress(1/3, detail = "")
          env = load(rawdata_url)
          incProgress(1/3, detail = "")
        })
        #################################

        withProgress(message = paste("Retrieving Movie <",search_table[s,1],">'s reviews"), value = 0, {
          test_web_url = search_table[s,2]
          incProgress(1/4, detail = "")
          reviews_pages = isolate(input$reviews_to_guess)
          incProgress(1/4, detail = "")
          review_prob = c(NULL)
          incProgress(1/4, detail = "")
          try_score_review = myimdb.reviews.full(test_web_url, reviews_pages)
          incProgress(1/4, detail = "")
        })
        output$plot2 <- renderPlot({myfunc.wordcloud(try_score_review, remove_words = c(stopwords("english"),"film","movie","can","films","movies","will","scenes","just","one","like"))
        })         
        
        #################################
        withProgress(message = 'Generating Term Table', value = 0, {
          K_input = isolate(input$k_term)
          for(i in 2:9)
          {
            review_name = paste("score_",i,"_reviews",sep = "")
            term_name = paste("score_",i,"_term",sep = "")
            clean_reviews = imdb_score_clean_func(get(review_name)[,-2])
            table = imdb_score_term_func(clean_reviews, K=K_input)
            assign(term_name, table)
            incProgress(1/9, detail = paste("Generating score_", i," term table", sep = ""))
          }
        })
        #################################
        
      
        #################################

        #################################
        review_prob = c(NULL)
        withProgress(message = "Guessing movie score with term", value = 0, {
          for(i in 2:9)
          {
            term_name = paste("score_",i,"_term",sep="")
            review_term_table_name = paste("review_",i,"_table",sep="")
            review_term_table = guess_imdb_review_score(try_score_review, get(term_name))
            review_prob = c(review_prob,sum(as.numeric(review_term_table[,2])))
            assign(review_term_table_name, review_term_table)
            incProgress(1/9, detail = paste("generate score ",i," table", sep=""))
          }
        })
        output$bar_2 <- renderPlot({barplot(as.numeric(review_2_table[1:10,3]), legend.text = review_2_table[1:10,1],col=brewer.pal(10, "Paired"))
          })
        output$bar_3 <- renderPlot({barplot(as.numeric(review_3_table[1:10,3]), legend.text = review_3_table[1:10,1],col=brewer.pal(10, "Paired"))
        })
        output$bar_4 <- renderPlot({barplot(as.numeric(review_4_table[1:10,3]), legend.text = review_4_table[1:10,1],col=brewer.pal(10, "Paired"))
        })
        output$bar_5 <- renderPlot({barplot(as.numeric(review_5_table[1:10,3]), legend.text = review_5_table[1:10,1],col=brewer.pal(10, "Paired"))
        })
        output$bar_6 <- renderPlot({barplot(as.numeric(review_6_table[1:10,3]), legend.text = review_6_table[1:10,1],col=brewer.pal(10, "Paired"))
        })
        output$bar_7 <- renderPlot({barplot(as.numeric(review_7_table[1:10,3]), legend.text = review_7_table[1:10,1],col=brewer.pal(10, "Paired"))
        })
        output$bar_8 <- renderPlot({barplot(as.numeric(review_8_table[1:10,3]), legend.text = review_8_table[1:10,1],col=brewer.pal(10, "Paired"))
        })
        output$bar_9 <- renderPlot({barplot(as.numeric(review_9_table[1:10,3]), legend.text = review_9_table[1:10,1],col=brewer.pal(10, "Paired"))
        })
        guess_table = cbind(2:9,review_prob)
        colnames(guess_table)=c("score","prob")
        guess_table <- guess_table[order(guess_table[,2],decreasing=TRUE),]
        
        ###############################
       withProgress(message = "creating classification trees", value = 0, {
          incProgress(1/6, detail = paste("creating test freq table", sep=""))
          test_table = imdb_test_data_generate(all_variables, try_score_review, processbar = FALSE) #use the function to create a training data table
          incProgress(1/6, detail = paste("create class-tree", sep=""))
          fit <- rpart(imdb_score ~ .,
                       method="class", data=training_table,
                       control = rpart.control(minsplit=20, cp=0.001))
          incProgress(1/6, detail = paste("create pruned-tree", sep=""))
          output$class_tree <- renderPlot({prp(fit, main="Classification Tree", fallen.leaves=TRUE, shadow.col="gray", branch.lty=2, faclen=0, trace=1, split.cex=1.2, split.prefix="Is ", split.suffix="?", split.box.col="lightgray", split.border.col="darkgray", split.round=.5)})
          pfit<- prune(fit, cp=fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
          incProgress(1/6, detail = paste("Guessing with trees", sep=""))
          output$pruned_tree <- renderPlot({prp(pfit, main="Pruned Tree", fallen.leaves=TRUE, shadow.col="gray", branch.lty=2, faclen=0, trace=1, split.cex=1.2, split.prefix="Is ", split.suffix="?", split.box.col="lightgray", split.border.col="darkgray", split.round=.5)})
          predict = predict(fit, test_table, type="class")
          predict = as.character(predict)
          predict2 = predict(pfit, test_table, type="class")
          predict2 = as.character(predict2)
          conf.matrix <- table(training_table$imdb_score, predict(fit,type="class"))
          accuracy_training_ori = sum(diag(conf.matrix))/sum(conf.matrix)
          conf.matrix2 <- table(training_table$imdb_score, predict(pfit,type="class"))
          accuracy_training_pruned = sum(diag(conf.matrix2))/sum(conf.matrix2)
          incProgress(1/6, detail = paste("random forest processing", sep=""))
          rownames(training_table) = NULL
          ffit <- randomForest(imdb_score ~ .,ntree=isolate(input$no_tree),data=training_table)
          predict3 = as.character(predict(ffit, test_table, type="response"))
          incProgress(1/6, detail = paste("Finish", sep=""))
          })
        ###############################
       withProgress(message = "Printing out result", value = 0, {
        cat(paste("Computer Guess of movie <",search_table[s,1],">s score:\n", sep=""))
        cat("\n================Guess with Term Table==================\n")
        cat(paste("First Guess is score ", guess_table[1,1],"\nthe accuracy is: ", as.character(round(accuracy_training_ori*100, 2)), "%","\nSecond Guess is score ", guess_table[2,1],"\nthe accuracy is: ", as.character(round(accuracy_training_pruned*100, 2)), "%", sep = ""))
        cat("\n=======================================================\n")
        incProgress(1/4)
        cat("\n\n\n")
        cat("\n==========Guess with Trees and Random Forest===========\n")
        cat(paste("Original Tree Guess ", predict,"\nPruned Tree Guess ", predict2, "\nRandom Forest Guess ", predict3))
        cat("\n=======================================================\n")
        cat("\n")
        incProgress(1/4)
        output$ftt = renderPrint({
        cat("\n=======================reference=======================\n")
        cat("Here is a reference of percentage table for the score:\n\n")
        guess_table[,2] = round(guess_table[,2]/sum(guess_table[,2]),3)
        cat(write.table(guess_table))
        cat("\n=======================================================\n")
        })
        incProgress(1/4)
        output$vrf = renderPrint({
          cat("\n=======================reference=======================\n")
          cat("Here is some detail about the random forest:\n\n")
          cat("Importance of top 10 predictor: \n")
          import_fit = importance(ffit)
          cat(write.table(as.data.frame(import_fit[1:10,])))
          cat("\n")
          cat("\n=======================================================\n")
        })
        incProgress(1/4)
       })
      }
    })
    ##############################################################
 
  })