library(shiny)
library(shinycssloaders)
library(wordcloud)
library(ggplot2)
library(shinydashboard)
library(dplyr)
library(tidytext)
library(DT)

source("web_scrapper/get_review.R")
source("classifier/naive_bayes.R")

features <- readRDS(features_rds_path)


ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = tags$img(src = "amazon.png",height='60',width='170')),
  dashboardSidebar(
    helpText(
      h4(strong("Product Review Sentiment Analysis"), align = "center", style = "color:white"),
    ),
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("glyphicon glyphicon-home", lib = "glyphicon")),
      menuItem("Data Visualization", tabName = "charts", icon = icon("glyphicon glyphicon-stats", lib = "glyphicon")),
      menuItem("Info", tabName = "info", icon = icon("glyphicon glyphicon-info-sign", lib = "glyphicon"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
        fluidRow(
          box(
            title = "Input Review Data",status = "primary", solidHeader = T,
            width = 12,
              textInput("url", "Enter the Amazon product review URL", 
                placeholder = "url", 
                value = "https://www.amazon.com/Apple-32GB-Space-Model-Refurbished/product-reviews/B074PWW6NS/ref=cm_cr_arp_d_viewopt_srt?ie=UTF8&reviewerType=all_reviews&sortBy=recent"
                ),
            sliderInput("size","Total reviews",min = 0,max = 1000,value = 50),
            div(fluidPage(submitButton("Submit")), style="text-align: center;")
          ),
        ),
        fluidRow(
          box(
            title = "Review Data",status = "primary",solidHeader = T,
            width = 12,collapsible = T,
            div(DT::dataTableOutput("table_review") %>% withSpinner(color="#1167b1"), 
            style = "font-size: 100%;")
          ),
        ),
        fluidRow(
          box(
            title = "Positif Review Data", status = "primary", solidHeader = T,
            width = 12, collapsible = T,
            div(DT::dataTableOutput("pos_table_review") %>% withSpinner(color="#1167b1"), 
            style = "font-size: 100%;")
          ),
        ),
        fluidRow(
          box(
            title = "Negatif Review Data", status = "primary", solidHeader = T,
            width = 12, collapsible = T,
            div(DT::dataTableOutput("neg_table_review") %>% withSpinner(color="#1167b1"), 
            style = "font-size: 100%;")
          ),
        ),
      ),
      tabItem(tabName = "charts",
        fluidRow(
          valueBoxOutput("total_review"),
          valueBoxOutput("positive_review"),
          valueBoxOutput("negative_review")
        ),
        fluidRow(
          box(
            title = "Histogram of Sentiment Analysis",status = "primary", solidHeader = T,
            width = 12,
            plotOutput("hist_sentiment") %>% withSpinner(color="#1167b1")
          ),
        ),
        fluidRow(
          box(
            title = "Word Cloud",status = "primary", solidHeader = T,
            width = 6,
            plotOutput("wordcloud") %>% withSpinner(color="#1167b1")
          ),
          box(
            title = "Most Used Words",status = "primary", solidHeader = T,
            width = 6,
            plotOutput("word_count") %>% withSpinner(color="#1167b1")
          )
        ),
        fluidRow(
          box(
            title = "Common Sentiment Words",status = "primary", solidHeader = T,
            width = 12,
            plotOutput("sentiment_count") %>% withSpinner(color="#1167b1")
          ),
        )
      ),
      tabItem(tabName = "info",
        fluidRow(
          box(
            title = strong("About Application"),width = 12,
            p("This Amazon product sentiment analysis application uses the Naive Bayes classification and works using the web scraping method to get product review data, then the reviews will be grouped into two groups of positive or negative sentiments, and also display them in the form of data visualization.",
              align = "justify")
          )
        ),
        fluidRow(
          box(
            title = strong("How to Use?"),width = 12,
            p('Find the electronic product page you want to analyze, then scroll down to find the review column and then click "See all reviews".',
              align = "justify"),br(),
            div(tags$img(src = "step1.png",height='400',width='650'), style="text-align: center;"),br(),
            p('After moving to the "See all reviews" page, copy the link you get, paste it in the URL input field in the application, set the number of reviews you want to take, and then click the Submit button.',
              align = "justify"),br(),
            div(tags$img(src = "step2.png",height='400',width='650'), style="text-align: center;"),br(),
          )
        ),
      )
    
    )
  )
) 

server <- function(input, output) {
  
  data <- reactive({
    withProgress({
      setProgress(message = "Collecting data", value = 0)
      
      result <- get_product_reviews(input$url, input$size, incProgress)
    })
    
    return(result)
  })
  
  prediction_data <- reactive({
    withProgress({
      setProgress(message = "Predicting sentiment", value = 0)
      
      reviews <- data()$review
      incProgress(1/2)
      prediction <- predict_sentiment(reviews)
      incProgress(1/2)
    })
    prediction$reviewer <- data()$reviewer
    
    return(prediction)
  })
  
  data_word <- reactive({
    dtm <- TermDocumentMatrix(clean_data(prediction_data()$review))
    m <- as.matrix(dtm)
    v <- sort(rowSums(m),decreasing=TRUE)
    data.frame(word = names(v),n=v)
  })
  
  output$table_review <- renderDataTable(datatable({
    prediction_data()
  }))
  
  output$pos_table_review <- renderDataTable(datatable({
    prediction_data() %>% filter(sentiment == "Positive")
  }))
  
  output$neg_table_review <- renderDataTable(datatable({
    prediction_data() %>% filter(sentiment == "Negative")
  }))
  
  output$total_review <- renderValueBox({
    valueBox(
      "Total", 
      paste0(nrow(prediction_data()), " review"),
      icon = icon("pen"),
      color = "blue"
    )
  })
  
  output$positive_review <- renderValueBox({
    valueBox(
      "Positive", 
      paste0(nrow(prediction_data() %>% filter(sentiment == "Positive")), " review"),
      icon = icon("smile"),
      color = "green")
  })
  
  output$negative_review <- renderValueBox({
    valueBox(
      "Negative",
      paste0(nrow(prediction_data() %>% filter(sentiment == "Negative")), " review"), 
      icon = icon("frown"),
      color = "red")
  })
  
  output$wordcloud <- renderPlot({
    data.corpus <- clean_data(data()$review)
    wordcloud(data.corpus, min.freq = 5 , max.words = 200, random.order=F, 
              rot.per=0.35, colors=brewer.pal(6, "Dark2"))
  })
  
  output$word_count <- renderPlot({
    countedWord <- data_word() %>%
      top_n(20, n) %>%
      mutate(word = reorder(word, n))
    
    ggplot(countedWord, aes(x = word, y = n, fill = word, label = n)) +
      geom_bar(stat="identity", show.legend = FALSE) +
      coord_flip() +
      labs(x = "Word", y = "Word Count") +
      geom_label(aes(fill = word),colour = "white", fontface = "bold", show.legend = FALSE)
  })
  
  output$hist_sentiment <- renderPlot({
    text <- prediction_data()$review
    value <- prediction_data()$sentiment
    sent_df <- data.frame(text, value, stringsAsFactors=FALSE)
    sent_df <- within(sent_df,
                      value<- factor(value, levels=names(sort(table(value), decreasing=TRUE))))
    
    ggplot(sent_df, aes(x=value)) +
      geom_bar(aes(y=..count.., fill=value)) +
      scale_fill_brewer(palette="Set1") +
      xlab("Sentiment Categories") + ylab("Number of Review")
  })
  
  output$sentiment_count <- renderPlot({
    bing_word_counts <- data_word() %>%
      inner_join(get_sentiments("bing")) %>%
      count(word, sentiment, n, sort = TRUE) %>%
      ungroup()
    
    bing_word_counts %>%
      group_by(sentiment) %>%
      top_n(10, n) %>%
      ggplot(aes(reorder(word, n), n, fill = sentiment, label = n)) +
      geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
      facet_wrap(~sentiment, scales = "free_y") +
      labs(y = "Contribution to Sentiment", x = NULL) + coord_flip() +
      geom_label(aes(fill = sentiment),colour = "white", fontface = "bold", show.legend = FALSE)
  })
}

shinyApp(ui, server)