shinyServer(
  function(input, output) {
    source("my_wordcloud_func.R")
    source("imdb_review_scraping_func.R")
    source("imdb_score_clean_func.R")
    source("imdb_score_term_func.R")
    source("imdb_guess_review_func.R")
    
    ###############################################################
    output$plot1_1 <- renderPlot({
        withProgress(message = 'Creating score_2 wordcloud', value = 0, {
        rawdata_url = paste("http://www.uwwxy.com/rdata/",input$rawdata, sep ="")
        incProgress(1/4, detail = "")
        load(url(rawdata_url))
        incProgress(1/4, detail = "")
        review_var_name = paste("score_2_reviews", sep = "")
        incProgress(1/4, detail = "")
        myfunc.wordcloud(get(review_var_name), remove_words = c(stopwords("english"),"film","movie","can","films","movies","will","scenes","just"))
        incProgress(1/4, detail = "")
      })
    })
    
    output$plot1_2 <- renderPlot({
      withProgress(message = 'Creating score_3 wordcloud', value = 0, {
        rawdata_url = paste("http://www.uwwxy.com/rdata/",input$rawdata, sep ="")
        incProgress(1/4, detail = "")
        load(url(rawdata_url))
        incProgress(1/4, detail = "")
        review_var_name = paste("score_3_reviews", sep = "")
        incProgress(1/4, detail = "")
        myfunc.wordcloud(get(review_var_name), remove_words = c(stopwords("english"),"film","movie","can","films","movies","will","scenes","just"))
        incProgress(1/4, detail = "")
      })
    })
    
    output$plot1_3 <- renderPlot({
      withProgress(message = 'Creating score_4 wordcloud', value = 0, {
        rawdata_url = paste("http://www.uwwxy.com/rdata/",input$rawdata, sep ="")
        incProgress(1/4, detail = "")
        load(url(rawdata_url))
        incProgress(1/4, detail = "")
        review_var_name = paste("score_4_reviews", sep = "")
        incProgress(1/4, detail = "")
        myfunc.wordcloud(get(review_var_name), remove_words = c(stopwords("english"),"film","movie","can","films","movies","will","scenes","just"))
        incProgress(1/4, detail = "")
      })
    })
    
    output$plot1_4 <- renderPlot({
      withProgress(message = 'Creating score_5 wordcloud', value = 0, {
        rawdata_url = paste("http://www.uwwxy.com/rdata/",input$rawdata, sep ="")
        incProgress(1/4, detail = "")
        load(url(rawdata_url))
        incProgress(1/4, detail = "")
        review_var_name = paste("score_5_reviews", sep = "")
        incProgress(1/4, detail = "")
        myfunc.wordcloud(get(review_var_name), remove_words = c(stopwords("english"),"film","movie","can","films","movies","will","scenes","just"))
        incProgress(1/4, detail = "")
      })
    })
    
    output$plot1_5 <- renderPlot({
      withProgress(message = 'Creating score_6 wordcloud', value = 0, {
        rawdata_url = paste("http://www.uwwxy.com/rdata/",input$rawdata, sep ="")
        incProgress(1/4, detail = "")
        load(url(rawdata_url))
        incProgress(1/4, detail = "")
        review_var_name = paste("score_6_reviews", sep = "")
        incProgress(1/4, detail = "")
        myfunc.wordcloud(get(review_var_name), remove_words = c(stopwords("english"),"film","movie","can","films","movies","will","scenes","just"))
        incProgress(1/4, detail = "")
      })
    })
    
    output$plot1_6 <- renderPlot({
      withProgress(message = 'Creating score_7 wordcloud', value = 0, {
        rawdata_url = paste("http://www.uwwxy.com/rdata/",input$rawdata, sep ="")
        incProgress(1/4, detail = "")
        load(url(rawdata_url))
        incProgress(1/4, detail = "")
        review_var_name = paste("score_7_reviews", sep = "")
        incProgress(1/4, detail = "")
        myfunc.wordcloud(get(review_var_name), remove_words = c(stopwords("english"),"film","movie","can","films","movies","will","scenes","just"))
        incProgress(1/4, detail = "")
      })
    })
    
    output$plot1_7 <- renderPlot({
      withProgress(message = 'Creating score_8 wordcloud', value = 0, {
        rawdata_url = paste("http://www.uwwxy.com/rdata/",input$rawdata, sep ="")
        incProgress(1/4, detail = "")
        load(url(rawdata_url))
        incProgress(1/4, detail = "")
        review_var_name = paste("score_8_reviews", sep = "")
        incProgress(1/4, detail = "")
        myfunc.wordcloud(get(review_var_name), remove_words = c(stopwords("english"),"film","movie","can","films","movies","will","scenes","just"))
        incProgress(1/4, detail = "")
      })
    })
    
    output$plot1_8 <- renderPlot({
      withProgress(message = 'Creating score_9 wordcloud', value = 0, {
        rawdata_url = paste("http://www.uwwxy.com/rdata/",input$rawdata, sep ="")
        incProgress(1/4, detail = "")
        load(url(rawdata_url))
        incProgress(1/4, detail = "")
        review_var_name = paste("score_9_reviews", sep = "")
        incProgress(1/4, detail = "")
        myfunc.wordcloud(get(review_var_name), remove_words = c(stopwords("english"),"film","movie","can","films","movies","will","scenes","just"))
        incProgress(1/4, detail = "")
      })
    })
    
    ##############################################################
    observeEvent(input$search_btn,{
      output$x1 = DT::renderDataTable(myimdb.search(input$search_movie), 
                                      options = list(
                                        lengthMenu = list(c(3, 5, 10), c('3', '5', '10')),
                                        pageLength = 5
                                      ),
                                      server = FALSE,
                                      selection = 'single')
    })
    
    
    
    output$info = renderPrint({
      s = input$x1_rows_selected
      if(length(s)<1){cat("Please search and select a movie to guess\n\n")}
      else{
        #s = input$x1_rows_selected
        withProgress(message = 'Starting the guess progress', value = 0, {
        search_table = myimdb.search(input$search_movie)
        incProgress(1/1, detail = "")
        })
        
        
        withProgress(message = 'Retrieving sample Rawdata', value = 0, {
          rawdata_url = paste("http://www.uwwxy.com/rdata/",input$rawdata, sep ="")
          incProgress(1/3, detail = "")
          load(url(rawdata_url))
          incProgress(1/3, detail = "")
          K_input = input$k_term
          incProgress(1/3, detail = "")
        })
        #################################
        
        withProgress(message = 'Generating Term Table', value = 0, {
          for(i in 2:9)
          {
            review_name = paste("score_",i,"_reviews",sep = "")
            term_name = paste("score_",i,"_term",sep = "")
            clean_reviews = imdb_score_clean_func(get(review_name))
            table = imdb_score_term_func(clean_reviews, K=K_input)
            assign(term_name, table)
            incProgress(1/9, detail = paste("Generating score_", i," term table", sep = ""))
          }
        })
        #################################
        
        withProgress(message = paste("Retrieving Movie <",search_table[s,1],">'s reviews"), value = 0, {
          test_web_url = search_table[s,2]
          incProgress(1/4, detail = "")
          grab_pages = input$pages_to_guess
          incProgress(1/4, detail = "")
          review_prob = c(NULL)
          incProgress(1/4, detail = "")
          try_score_review = myimdb.rangereviews(test_web_url, grab_pages)
          incProgress(1/4, detail = "")
        })
        #################################
        withProgress(message = 'Creating review wordcloud', value = 0, {
        output$plot2 <- renderPlot({myfunc.wordcloud(try_score_review, remove_words = c(stopwords("english"),"film","movie","can","films","movies","will","scenes","just"))})
        incProgress(1/1, detail = "")
        })
        #################################
        
        withProgress(message = 'Guessing movie score', value = 0, {
          for(i in 2:9)
          {
            term_name = paste("score_",i,"_term",sep="")
            review_term_prob = guess_imdb_review_score(try_score_review, get(term_name))
            review_prob = c(review_prob,review_term_prob)
            incProgress(1/9, detail = "")
          }
        })
        guess_table = cbind(2:9,review_prob)
        colnames(guess_table)=c("score","prob")
        guess_table <- guess_table[order(guess_table[,2],decreasing=TRUE),]
        cat(paste("Computer Guess of movie <",search_table[s,1],">'s score:\n", sep=""))
        cat(paste("First Guess is score ", guess_table[1,1],"\nSecond Guess is score ", guess_table[2,1]))
        cat("\n\n")
      }
    })
    ##############################################################
  
  })