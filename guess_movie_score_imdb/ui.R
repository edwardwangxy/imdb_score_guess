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
        
        sliderInput("plot_sample_score", label = "choose a Score to plot",
                    min = 2, max = 9, value = 2, step = 1),
        
        sliderInput("k_term", label = "choose # of terms",
                    min = 10, max = 80, value = 50, step = 10),
        
        sliderInput("pages_to_guess", label = "review pages for guessing",
                    min = 10, max = 80, value = 50, step = 10),
        
        textInput("search_movie", "Search a movie", 
                  placeholder = "enter movie name"),
        
        actionButton('search_btn',
                     label = "Search")
        
###########################################################################

      )
    ),
    dashboardBody(
      # Boxes need to be put in a row (or column)
      fluidRow(
        box(column(12,h1('Term plot'),plotOutput("plot1"))),
        box(column(12, h1('Search Result'),DT::dataTableOutput('x1'))),
        box(column(12, h1('Computer Guess'),verbatimTextOutput('info'))),
        box(column(12,h1('Guess plot'),plotOutput("plot2")))
      )
    )
  ))