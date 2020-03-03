library(shiny)
library(tidyverse)
library(wordcloud)
library(ggplot2)
library(shinythemes)
library(RColorBrewer)
library(tidytext)
library(dplyr)

getFreq <- function(book, stopwords = TRUE) {
  # check that only one of three books is selected
  if (!(book %in% books))
    stop("Unknown book")
  
  text <-  tibble(text = readLines(sprintf("./data/%s.txt", book), encoding="UTF-8"))
  
  # could also pass column of text/character instead
  text <- text %>%
    unnest_tokens(word, text) %>%
    count(word, sort = TRUE) 
  
  if(stopwords){
    text <- text %>%
      anti_join(stop_words)
  }
  
  return(text)
}

books <- list("A Mid Summer Night's Dream" = "summer",
              "The Merchant of Venice" = "merchant",
              "Romeo and Juliet" = "romeo")

# task4: add in getFreq function for pre-processing

# task6: add in shinythemes function

ui <- fluidPage(theme= shinytheme('flatly'),
  titlePanel("Shakespeare's Plays Word Frequencies"), # Application title
  
  # task1: add in the sidebarLayout with sidebarPanel and mainPanel
  sidebarLayout(
    sidebarPanel(
      selectInput("books", label = 'Books',books),
      checkboxInput("stopwords","Stop Words",value= TRUE),
      actionButton('action', 'Go!'),
      hr('Word Cloud Settings'),
      sliderInput('maxwords','Max # of Words', min=10, max=200, value=100,step=10),
      sliderInput('largewords',"Size of Large Words", min=1,max=8,value=4),
      sliderInput('smallwords','Size of Small Words', min=.1,max=4,value=.5),
      hr('Word Count Settings'),
      sliderInput('minwords','Minimum Word Count', min=10,max=100,value=25),
      sliderInput('fontsize',"Font Size", min=8, max=30, value=14)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Word Cloud", plotOutput('cloud', height='600px')),
        tabPanel("Word Counts",plotOutput('freq', height='600px'))
        
      )
    )
  )
  
  # task2: add in the inputs in the sidebarPanel
  
  # task1: within the mainPanel, create two tabs (Word Cloud and Frequency)
  
  # task3: add in the outputs in the sidebarPanel
  
  # task6: and modify your figure heights
)

server <- function(input, output) {
  
  # task5: add in reactivity for getFreq function based on inputs
  freq <- eventReactive(input$action, withProgress({
    setProgress(message = "Processing corpus...")
    getFreq(input$books,input$stopwords) # ... = replace with the two inputs from Task 2
  }))
  
  output$freq <- renderPlot(
    {q <- freq()
    
     q %>% 
       filter(n>input$minwords) %>%
       ggplot(q,mapping=aes(reorder(word,n)))+geom_col(aes(y=n))+coord_flip()+
       theme(axis.title.x=element_blank(),axis.title.y = element_blank(),axis.text=element_text(size=input$fontsize))
              
                    #theme(axis.text=element_text(input$fontsize), 
                    
     }
  )

  
  output$cloud<- renderPlot(
  {v <- freq()
  pal <- brewer.pal(8,"Dark2")
  
  v %>% 
    with(
      wordcloud(
        word, 
        n, 
        scale = c(input$largewords,input$smallwords),
        random.order = FALSE, 
        max.words = input$maxwords, 
        colors=pal))
    
  })
  
  
  
}

shinyApp(ui = ui, server = server)
