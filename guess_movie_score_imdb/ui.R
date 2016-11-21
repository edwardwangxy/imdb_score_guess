library(shinydashboard)
library(shiny)
library(DT)
library(rvest)
library(RColorBrewer)
library(SnowballC)
library(tm)
library(wordcloud)
library(XML)
shinyUI(
  dashboardPage(
    dashboardHeader(title = "Guess Movie Score"),
    dashboardSidebar(
      fluidPage(
        load(url("http://www.uwwxy.com/rdata/filelist.Rda")),
        selectInput('rawdata', 'Pick Data', filelist,
                    selected=filelist[1]),
        
        sliderInput("k_term", label = "choose # of terms",
                    min = 10, max = 80, value = 50, step = 10),
        
        sliderInput("pages_to_guess", label = "review pages for guessing",
                    min = 10, max = 80, value = 50, step = 10),
        
        textInput("search_movie", "Search a movie", 
                  placeholder = "enter movie name"),
        
        actionButton('search_btn',
                     label = "Search"),
        
        actionButton('guess_btn',
                     label = "start guess")
        
###########################################################################

      )
    ),
    dashboardBody(
      # Boxes need to be put in a row (or column)
      fluidRow(
        tabBox(
          title = "Sample wordcloud",
          id = "tabset1", height = "500px", width = 6,
          tabPanel("Score_2", plotOutput("plot1_1")),
          tabPanel("Score_3", plotOutput("plot1_2")),
          tabPanel("Score_4", plotOutput("plot1_3")),
          tabPanel("Score_5", plotOutput("plot1_4")),
          tabPanel("Score_6", plotOutput("plot1_5")),
          tabPanel("Score_7", plotOutput("plot1_6")),
          tabPanel("Score_8", plotOutput("plot1_7")),
          tabPanel("Score_9", plotOutput("plot1_8"))
        ),
        box(title = "Picked Movie wordcloud",
            id = "piccloud", height = "500px", width = 6,
            plotOutput("plot2")),
        box(title = "Search Result",
            id = "search", height = "500px", width = 6
            ,DT::dataTableOutput('x1')),
        box(title = "Search Result",
            id = "result", height = "500px", width = 6,
            verbatimTextOutput('info'))
      )
    )
  ))