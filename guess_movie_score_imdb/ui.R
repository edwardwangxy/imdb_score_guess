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
        load("rawdata/filelist.Rda"),
        selectInput('rawdata', 'Pick Data', filelist,
                    selected=filelist[1]),
        
        sliderInput("k_term", label = "choose # of terms",
                    min = 10, max = 80, value = 50, step = 10),
        
        sliderInput("reviews_to_guess", label = "reviews for guessing",
                    min = 100, max = 5000, value = 1000, step = 100),
        
        sliderInput("no_tree", label = "# of trees in forest",
                    min = 100, max = 2000, value = 1000, step = 100),
        
        textInput("search_movie", "Search a movie", 
                  placeholder = "enter movie name"),
        
        actionButton('search_btn',
                     label = "Search"),
        
        actionButton('guess_btn',
                     label = "start guess"),
        
###########################################################################

      )
    ),
    dashboardBody(
      # Boxes need to be put in a row (or column)
      fluidRow(
        tabBox(
          title = "Training dataset",
          id = "tabset1", height = "500px", width = 6,
          tabPanel("Score_2", plotOutput("plot1_1")),
          tabPanel("Score_3", plotOutput("plot1_2")),
          tabPanel("Score_4", plotOutput("plot1_3")),
          tabPanel("Score_5", plotOutput("plot1_4")),
          tabPanel("Score_6", plotOutput("plot1_5")),
          tabPanel("Score_7", plotOutput("plot1_6")),
          tabPanel("Score_8", plotOutput("plot1_7")),
          tabPanel("Score_9", plotOutput("plot1_8")),
          tabPanel("Tree", plotOutput("class_tree")),
          tabPanel("Pruned_Tree", plotOutput("pruned_tree"))
        ),
        tabBox(title = "Picked Movie",
            id = "piccloud", height = "500px", width = 6,
            tabPanel("Wordcloud", plotOutput("plot2")),
            tabPanel("term_2", plotOutput("bar_2")),
            tabPanel("term_3", plotOutput("bar_3")),
            tabPanel("term_4", plotOutput("bar_4")),
            tabPanel("term_5", plotOutput("bar_5")),
            tabPanel("term_6", plotOutput("bar_6")),
            tabPanel("term_7", plotOutput("bar_7")),
            tabPanel("term_8", plotOutput("bar_8")),
            tabPanel("term_9", plotOutput("bar_9"))
        ),
        box(title = "Search Result",
            id = "search", height = "500px", width = 6
            ,DT::dataTableOutput('x1')),
        box(title = "Guess Result",
            id = "result", height = "500px", width = 6,
            verbatimTextOutput('info')),
        box(title = "Full Term Table",
            id = "result", height = "500px", width = 6,
            verbatimTextOutput('ftt')),
        box(title = "View Random Forest",
            id = "result", height = "500px", width = 6,
            verbatimTextOutput('vrf'))
      )
    )
  ))