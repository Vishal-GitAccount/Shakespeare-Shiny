#Vishal Problem Set 3
#Worked with Aarush and Shina 
library(shiny)
library(tidyverse)
library(wordcloud)
library(ggplot2)
library(shinythemes)
library(RColorBrewer)
library(tidytext)
books <- list("A Mid Summer Night's Dream" = "summer",
              "The Merchant of Venice" = "merchant",
              "Romeo and Juliet" = "romeo")

# task4: add in getFreq function for pre-processing
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
# task6: add in shinythemes function

ui <- fluidPage(
  theme=shinytheme("cerulean"),
  titlePanel("Shakespeare's Plays Word Frequencies"), # Application title
  
  # task1: add in the sidebarLayout with sidebarPanel and mainPanel
  sidebarLayout(
    sidebarPanel(
                 selectInput(inputId = "books", label = "Choose a book",
                             choices = books),
                 
                 checkboxInput(inputId = "stopwords", label = "Stop Words", 
                               value = TRUE),
                 
                 actionButton(inputId = "run_button", label = "Run"),
                 
                 hr(),
                 
                 h3("Word Cloud Settings"),
                 sliderInput(inputId = "maxwords", label = "Max Words:",
                             min = 10, max = 200, value = 100, step = 10),
                 
                 sliderInput(inputId = "largest_word_size", label = "Size for largest Word",
                             min = 1, max = 8, value = 4),
                 
                 sliderInput(inputId = "smallest_word_size", label = "Smallest Word Size",
                             min = 0.1, max=4, value = 0.5),
                 #plotOutput(outputId = "cloud"),
                 
                 hr(),
                 
                 h3("Word Count Settings"),
                 sliderInput(inputId = "word_count_min", label = "Min Word Count for Counts Chart",
                             min=10, max = 100, value = 25),
                 sliderInput(inputId = "font_size", label = "Font Size for Counts Chart",
                             min = 8, max = 30, value = 14)
                 #plotOutput(outputId = "freq")
                 
                 
    ),
                 
    mainPanel(
      tabsetPanel(
        tabPanel("Word Cloud",plotOutput("cloud",height="600px")), 
        tabPanel("Word Counts",plotOutput("freq",height="600px"))
                                      
                             )
              
    )
  
  # task2: add in the inputs in the sidebarPanel
  
  # task1: within the mainPanel, create two tabs (Word Cloud and Frequency)
    
  # task3: add in the outputs in the sidebarPanel
  
  # task6: and modify your figure heights
  
  )

)

server <- function(input, output) { 
  
  freq <- eventReactive(input$run_button,{
    withProgress({
      setProgress(message = "Processing Corpus...")
      getFreq(input$books,input$stopwords)
    })
  })
  # task5: add in reactivity for getFreq function based on inputs
  output$cloud <- renderPlot({
    v <- freq()
      pal <- brewer.pal(8,"Dark2")
      v %>% 
        with(
          wordcloud(
            word, 
            n, 
            scale = c(input$largest_word_size,input$smallest_word_size),
            random.order = FALSE, 
            max.words = input$maxwords, 
            colors=pal))
    })
  
  
  output$freq<-renderPlot({
    v<-freq()
    v%>%
      filter(n>input$word_count_min)%>%
      ggplot(aes(reorder(word,n),n))+
      geom_col()+
      coord_flip()+
      theme(text=element_text(size=input$font_size),
            axis.title.x=element_blank(),
            axis.title.y=element_blank())
  })
}  

shinyApp(ui = ui, server = server)
