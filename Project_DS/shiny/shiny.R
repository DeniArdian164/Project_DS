library(shiny)
library(vroom)
library(here)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(SnowballC)
library(Rstem)
library(sentiment)
library(plyr)

# Define UI for application that draws a histogram
ui <- fluidPage(
  headerPanel("Analisis Sentimen Mental Health issue"),
  headerPanel("dengan Naive Bayes"),
  mainPanel(
    tabsetPanel(
      tabPanel("Data Twitter", DT::dataTableOutput('dataTwitter')),
      tabPanel("Data Bersih", DT::dataTableOutput('dataBersih')),
      tabPanel("Data Sentimen", DT::dataTableOutput('tbl')),
      tabPanel("Sentimen Analisis by emotion", plotOutput("sent")),
      tabPanel("Sentimen Analisis by polarity", plotOutput("sent2"))
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  dataTwitter <- vroom('D:/Project_DS/dataTwitter.csv')
  dataTwitter <- data.frame(dataTwitter)
  
  # Output Data
  output$dataTwitter = DT::renderDataTable({
    DT::datatable(dataTwitter, options = list(lengthChange = FALSE))
  })
  
  dataBersih <- vroom('D:/Project_DS/dataBersih.csv')
  dataBersih <- data.frame(dataBersih)
  # Output Data
  output$dataBersih = DT::renderDataTable({
    DT::datatable(dataBersih, options = list(lengthChange = FALSE))
  })
  
  sent_df <- vroom('D:/Project_DS/dataSentimen.csv')
  sent_df <- data.frame(sent_df)
  
  # Output Data
  output$tbl = DT::renderDataTable({
    DT::datatable(sent_df, options = list(lengthChange = FALSE))
  })
  
  # plot distribution of emotion
  plotSentiments1 <- function(sent_df, title)
  {
      ggplot(sent_df, aes(x=emotion)) +
      geom_bar(aes(y=..count.., fill=emotion)) +
      scale_fill_brewer(palette="Dark2") +
      ggtitle(title) +
      theme(legend.position="right") +
      ylab("Number of Tweets") +
      xlab("Categories")
  }
  
  output$sent <- renderPlot({
    plotSentiments1(sent_df, "Sentiment Analysis of Mental Health issue by Emotion")
  })

  # plot distribution of polarity
  plotSentiments2 <- function(sent_df, title)
  {
    ggplot(sent_df, aes(x=polarity)) +
      geom_bar(aes(y=..count.., fill=polarity)) +
      scale_fill_brewer(palette="Dark2") +
      ggtitle(title) +
      theme(legend.position="right") +
      ylab("Number of Tweets") +
      xlab("Categories")
  }
  
  output$sent2 <- renderPlot({
    plotSentiments2(sent_df, "Sentiment Analysis of Mental Health issue by Polarity")
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)