#libraries 

library(datasets)
library(memoise)
library(wordcloud)
library(shiny)
library(tm)

books <- list("Anna at the Art Museum", 
              "Dear Evan Hansen",
              "Katie's Highlander",
              "Tumble",
              "Natural Causes",
              "Educated")



#Global 

getTermMatrix <- memoise(function(book ) {
  
  df <- read.csv("Sentiment_Books.csv")
  
  if(!(book %in% books))
    stop("Unknown book")
  
  #get reviews
  reviews <- subset(df, Book== book)
  docs <- Corpus(VectorSource(reviews$Clean_Text))
  
  dtm <- TermDocumentMatrix(docs)
  mat <- as.matrix(dtm)
  sort(rowSums(mat),decreasing = TRUE)
  
})


#Define server logic ---- 
server <- function(input , output , session) {
  
  terms <- reactive({
    input$update
    
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        getTermMatrix(input$selection)
      })
    })
  })
  
  wordcloud_rep <- repeatable(wordcloud)
  
  output$plot <- renderPlot({
    v <- terms()
    wordcloud(names(v), v,  min.freq = input$freq,
              max.words=input$max, random.order=FALSE, rot.per=0.35,
              colors=brewer.pal(8, "Dark2"))
  })
}

#Define UI.... 
ui <- fluidPage(
  
  titlePanel("Word Cloud"), 
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput("selection", "Choose a book:",
                  choices = books),
      
      actionButton("update","Change"), 
      hr(),
      
      sliderInput("freq",
                  "Minimum Frequency:",
                  min = 1, max = 50 , value=15),
      sliderInput("max",
                  "Maximum Number of Words:",
                  min=1 , max = 300, value=100)
    ),
    
    #Show Image 
    mainPanel(
      plotOutput("plot")
    )))

# Run the application 
shinyApp(ui = ui, server = server)

