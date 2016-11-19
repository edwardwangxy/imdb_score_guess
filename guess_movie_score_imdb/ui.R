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
        
        selectInput('rawdata', 'Pick Data', c("rawdata-2-2.Rda","rawdata-5-5.Rda"),
                    selected=names(airquality)[[4]]),
        selectInput('xvar', 'Pick Length', names(airquality)[c(5,6)],
                    selected=names(airquality)[[6]]),
        # actionButton('select2', 'Select the above variables.'),
        selectInput('xcol', 'X Variable', names(airquality)[-c(5,6)],
                    selected=names(airquality)[[1]]),
        selectInput('ycol', 'Y Variable', names(airquality)[-c(5,6)],
                    selected=names(airquality)[[2]]),
        # actionButton('select2', 'Select the above variables.'),
        sliderInput("subsample", label = "Size of random samples",
                    min = 10, max = 90, value = 30, step = 10),
        actionButton('resetSelection',
                     label = "Click to reset row selection"
        ), # end of action button
        selectInput('sortvar', 'Sort Data', names(airquality)[-c(5,6)],
                    selected=names(airquality)[[4]]),
        actionButton('act_sort',
                     label = "Click to insertion sort data"
        ) # end of action button

      )
    ),
    dashboardBody(
      # Boxes need to be put in a row (or column)
      fluidRow(
        box(column(12,plotOutput("plot1"))),
        box(column(12, h1('select rows'),DT::dataTableOutput('x1'))),
        box(column(12,plotOutput('x2'))),
        box(column(12, h1('Raw Infos'),verbatimTextOutput('info'))),
        box(column(12, h1('Insertion Sort'),verbatimTextOutput("sort")))
      )
    )
  ))