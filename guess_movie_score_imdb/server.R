shinyServer(
  function(input, output) {
    output$plot1 <- renderPlot({
      if(input$xvar == "Day"){
        temp_date = c(1:153)
        temp_data_day = data.frame(temp_date, airquality[input$yvar])
        clean_data_day = temp_data_day[complete.cases(temp_data_day),]
        colnames(clean_data_day) <- c("day","data")
        plot(clean_data_day,xlab="Day",ylab=input$yvar)
        lines(clean_data_day)
      }else{
        month_factor <- as.factor(airquality$Month)
        temp_data_month = data.frame(month_factor, airquality[input$yvar])
        clean_data_month = temp_data_month[complete.cases(temp_data_month),]
        colnames(clean_data_month) <- c("month","data")
        sum_data_month <- ddply(clean_data_month, .(month), summarize, mean=mean(data))
        plot(sum_data_month,xlab="Month",ylab=input$yvar)
        lines(sum_data_month)
      }
    })
  
    
    selectedData <- 
      #airquality[,c(input$xcol,input$ycol)]
      reactive({
        airquality[, c(input$xcol, input$ycol)]
      })
    nn <- nrow(airquality)
    
    output$x1 = DT::renderDataTable(airquality[,-c(5,6)], 
                                    options = list(
                                      lengthMenu = list(c(3, 5, 10), c('3', '5', '10')),
                                      pageLength = 5
                                    ),
                                    server = FALSE,
                                    selection = list(target = 'row+column'))
    
    proxy = dataTableProxy('x1')
    
    observeEvent(input$resetSelection, {
      proxy %>% selectRows(sample(1:nn, input$subsample, replace=F))
    })
    
    
    # highlight selected rows in the scatterplot
    output$x2 = renderPlot(height = 400, {
      par(mar = c(4, 4, 1, .1))
      plot(airquality[, c(input$xcol, input$ycol)])
      s = input$x1_rows_selected
      if (length(s)>=2) {
        points(airquality[s, c(input$xcol, input$ycol), drop = FALSE], 
               pch = 19, cex = 2)
        abline(lsfit(airquality[s,input$xcol], 
                     airquality[s,input$ycol])$coef, col=2)
      }
    })
    
    output$info = renderPrint({
      s = input$x1_rows_selected
      cor.sel=NA
      cat("These rows are selected:\n")
      cat(s,sep=",")
      cat("\n\n")
      if(length(s)<=1){cat("Please select 2 rows for regression\n\n")}
      if(length(s)) cor.sel=cor(airquality[s,input$xcol], 
                                airquality[s,input$ycol],
                                use="pairwise.complete.obs")
      list(xcol=input$xcol, ycol=input$ycol, 
           cor.all=cor(airquality[,input$xcol], 
                       airquality[,input$ycol],
                       use="pairwise.complete.obs"),
           cor.sel=cor.sel)
      
    })
    
    insertion_sort <- eventReactive(input$act_sort,{
      insertion.sort <- function(A){
        for(i in c(2:length(A))){
          for(j in c(i:2)){
            if(A[j-1]>A[j]){
              temp_val <- A[j-1]
              A[j-1] <- A[j]
              A[j]=temp_val
            }
          }
        }
        return(A)
      }
      temp_input = airquality[input$sortvar]
      clean_temp_input = temp_input[complete.cases(temp_input),]
      insertion.sort(clean_temp_input)
    })
    
    output$sort = renderPrint({
      
      temp_input = airquality[input$sortvar]
      clean_temp_input = temp_input[complete.cases(temp_input),]
      cat("The original data of ")
      cat(input$sortvar)
      cat("is: \n")
      cat(clean_temp_input)
      cat("\n")
      cat("After insertion sort:\n")
      cat(insertion_sort())
    })
  })