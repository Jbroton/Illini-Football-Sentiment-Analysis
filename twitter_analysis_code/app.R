#
# STAT 385 Group Project
#

library(shinythemes)
library(shiny)

#####---- define Function section ----####

pkg_list = c("rtweet", "shinythemes", "ggplot2", "tidyr", "dplyr", "tidytext", "tm", "widyr", "RColorBrewer", "reshape2", "syuzhet", "shinyjs", "igraph", "twitteR", "httr", "wordcloud", "base64enc", "openssl", "httpuv", "DT")
mia_pkgs = pkg_list[!(pkg_list %in% installed.packages()[,"Package"])]
if(length(mia_pkgs) > 0) install.packages(mia_pkgs)
loaded_pkgs = lapply(pkg_list, require, character.only=TRUE)

# Change the next four lines based on your own consumer_key, consume_secret, access_token, and access_secret.
consumer_key <- "oJ2IQ5HrUgTbla4uFdYe7Zm8J"
consumer_secret <-"p5Hz8PWubUdJTNTDm4pY5WYYYby0sDvgqW4R6kJhHmFSOX40ZS"
access_token <- "2189043781-rZzKPC8Yqga1FrgCWHlGjM0nADAnJvWnkUOplGo"
access_secret <- "1gA7j3v5Bhm2gYQFEqx3PKbH4azfRWc6LyCjgQ689EQ0N" 


searching <- function(search_string) {
  search_input <- paste0(search_string, " -filter:retweets")
  setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
  tw <- searchTwitter(search_input, n = 180,retryOnRateLimit = 1e3)
  d <- twListToDF(tw)
}

text2word <- function(d) {
  text_df <- data_frame(line=1:length(d$text), text=c(d$text))
  words <- text_df %>%
    unnest_tokens(word, text)
  words
}

merge.words.dictionary <- function(words) {
  afinn <- words %>%
    inner_join(get_sentiments("afinn")) %>%
    group_by(index = line) %>%
    summarise(sentiment = sum(score)) %>%
    mutate(method = "AFINN")
  bing_and_nrc <- bind_rows(words %>%
                              inner_join(get_sentiments("bing")) %>%
                              mutate(method = "Bing"), words %>%
                              inner_join(get_sentiments("nrc") %>%
                                           filter(sentiment %in% c("positive", "negative"))) %>%
                              mutate(method = "NRC")) %>%
    count(method, index = line, sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment = positive - negative)
  all <- bind_rows(afinn, bing_and_nrc)
  all
}

making.plots <- function(words, dict) {
  all <- merge.words.dictionary(words=words)
  subset <- all[all$method == dict[1], ]
  if (length(dict) > 1) {
    for (i in c(2:length(dict))) {
      subset <- rbind(subset, all[all$method == dict[i], ])
    }}
  subset %>%
    ggplot(aes(index, sentiment, fill = method)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~method, ncol = 1, scales = "free_y")
}


# Define UI for application that draws a histogram
myui <- fluidPage(theme = shinytheme("flatly"), 
                  tags$title("STAT 385 Group Project - Sentiment of Twitter"),
                  
                  column(width = 12,
                         h1("Sentiment Analysis of Twitter API")
                  ),
                  
                  sidebarPanel(
                    radioButtons("radio", label = h3("Tweet Data"),
                                 choices = list("Default Data" = 1, "New Search" = 2), 
                                 selected = 1),
                    
                    h3("If new search - Input search String:"), 
                    textInput(inputId = "Search",    label = "Search String"),
                    
                    hr(),
                    actionButton("do","Run Sentiment Analysis"),
                    checkboxGroupInput("Dictionary", h3("Dictionary choices"),
                                       choices = list("AFINN" = "AFINN", "Bing" = "Bing", "NRC" = "NRC"), selected ="AFINN"),
                    helpText("To change the default score, click on the cells in the last column below."),
                    h3("Score Data Table"),
                    DT::dataTableOutput('x1')
                    
                  ),
                  mainPanel(
                    
                    fluidRow(
                      
                      column(width=4, h4("Bing Words"),
                             plotOutput(outputId = "bingword", height=280)),
                      
                      column(width=4, h4("Score Distribution"),
                             plotOutput(outputId = "scores", height=280)),
                      
                      column(width=4, h4("User Score Distribution"),
                             plotOutput(outputId = "newscores", height=280)),
                      
                      column(width=12, h4("Sentiment Lexicons"),
                             plotOutput(outputId = "plot", height=300)),
                      
                      column(width=6, h3("By Tweet"),
                             htmlOutput(outputId = "most")),      
                      tags$head(tags$style("#most{color: 	#FF0000;
                                 font-size: 20px;
                                 }")),
                      
                      column(width=6, h4("By Tweet - Distribution"),
                             plotOutput(outputId = "tweets", height=250))     
                    )   
                  )
)
# Define server logic required to draw a histogram
myserver <- function(input, output) {
  observeEvent(input$do, {
    
    if (input$radio == "1" ) {
      load("twitter_data.Rda")
      d1 <- rt
    }
    else {
      search.out <- reactive({
        searching(input$Search)
      })
      d1 <- search.out()
    }
    tw <- text2word(d1)
    
    tweets.df <- gsub("http.*","",d1$text)
    tweets.df <- gsub("https.*","",tweets.df)
    tweets.df <- gsub("#.*","",tweets.df)
    tweets.df <- gsub("@.*","",tweets.df)
    word.df <- as.vector(tweets.df)
    word.df <- gsub("[^[:alnum:]///' ]", "", word.df)
    emotion.df <- get_nrc_sentiment(word.df,  language = "english")
    emotion.df2 <- cbind(tweets.df, emotion.df) 
    
    sent.value <- get_sentiment(word.df, method="afinn")
    most.positive <- word.df[sent.value == max(sent.value)][1]
    
    most.negative <- word.df[sent.value <= min(sent.value)][1]
    
    category_senti <- ifelse(sent.value < 0, "Negative", ifelse(sent.value > 0, "Positive", "Neutral"))
    senti_summary <- table(category_senti)
    
    senti_summary <- as.data.frame(senti_summary)
    
    output$tweets <- renderPlot(
      ggplot(data=senti_summary, aes(x=category_senti, y=Freq, fill=category_senti)) +
        geom_bar(stat="identity")
    )
    
    output$most <-
      renderText(
        HTML(paste("<b>", "The most positive:", most.positive, "<br/>", "<br/>", "The most negative:", most.negative, "</b>")))
    
    output$plot <- renderPlot(
      making.plots(tw, input$Dictionary)
    )
    output$cloud <- renderPlot(
      tw %>%
        inner_join(get_sentiments("bing")) %>%
        count(word, sentiment, sort = TRUE) %>%
        acast(word ~ sentiment, value.var = "n", fill = 0) %>%
        comparison.cloud(colors = c("gray20", "gray80"),
                         max.words = 100)
    )
    output$bingword <- renderPlot(
      tw %>%
        inner_join(get_sentiments("bing")) %>%
        count(word, sentiment, sort = TRUE) %>%
        ungroup() %>%
        group_by(sentiment) %>%
        top_n(10) %>%
        ungroup() %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(word, n, fill = sentiment)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~sentiment, scales = "free_y") +
        labs(y = "Contribution to sentiment",
             x = NULL) +
        coord_flip()
    )
    
    x0 <- tw %>% count(word, sort = TRUE) %>% inner_join(get_sentiments("afinn"))
    x0 <- as.data.frame(x0)
    x0$newscore <- x0$score
    x <- x0
    
    output$scores <- renderPlot(
      x0 %>%
        group_by(score) %>%
        summarize(Count = sum(n)) %>%
        ggplot(aes(x=as.factor(score), y=Count)) +
        geom_bar(stat="identity", show.legend = FALSE, fill="steelblue") +
        labs(y = "Count", x = "Score")
    )
    
    output$x1 = DT::renderDataTable(x, selection = 'none', editable = TRUE)
    
    proxy = dataTableProxy('x1')
    y <- reactive({
      input$x1_cell_edit
      x
    })
    
    observeEvent(input$x1_cell_edit, {
      info = input$x1_cell_edit
      str(info)
      i = info$row
      j = info$col
      v = info$value
      x[i, j] <<- DT::coerceValue(v, x[i, j])
      replaceData(proxy, x, resetPaging = FALSE)
    })
    
    output$newscores <- renderPlot(
      y() %>%
        group_by(newscore) %>%
        summarize(Count = sum(n)) %>%
        ggplot(aes(x=as.factor(newscore), y=Count)) +
        geom_bar(stat="identity", show.legend = FALSE, fill="orange") +
        labs(y = "Count", x = "New Score")
    )
    
  })
  
}

# Run the application
shinyApp(ui = myui, server = myserver)

