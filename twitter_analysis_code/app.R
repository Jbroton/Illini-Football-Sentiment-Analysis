#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#install.packages("shiny")
library(shiny)


#####---- define Function section ----####

pkg_list = c("rtweet", "ggplot2", "tidyr", "dplyr", "tidytext", "tm", "widyr", "RColorBrewer", "igraph", "twitteR", "httr", "wordcloud", "base64enc", "openssl", "httpuv")
mia_pkgs = pkg_list[!(pkg_list %in% installed.packages()[,"Package"])]
if(length(mia_pkgs) > 0) install.packages(mia_pkgs)
loaded_pkgs = lapply(pkg_list, require, character.only=TRUE)

# Change the next four lines based on your own consumer_key, consume_secret, access_token, and access_secret.
consumer_key <- "KGipiCFKISmGkruIrb2NgjUuh"
consumer_secret <- "JLzG6LKYMqGbxZYguwPYSyqgC39orNdI4tkyWWthBEUiYRCPaP"
access_token <- "834110859785932800-126anTJOQDb6aiO99EdHIaSa6ZBZaeb"
access_secret <- "01RcJ45hBWFmHrRunO8R0vU87d6KWQ7VukGdBAi0R7bre"

searching <- function(search_string) {
    search_input <- paste0(search_string, " -filter:retweets")
    setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
    tw <- searchTwitter(search_input, n = 1e6, since = '2016-06-08', retryOnRateLimit = 1e3)
    d <- twListToDF(tw)
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
myui <- fluidPage(
    titlePanel("STAT 385 Group Project"),
    sidebarLayout(
        sidebarPanel(
            textInput(inputId = "Search",    label = "Search Strings"),
            actionButton("do","Run Sentiment Analysis"),
            checkboxGroupInput("Dictionary", h3("Dictionary choices"),
                               choices = list("AFINN" = "AFINN", "Bing" = "Bing", "NRC" = "NRC"), selected ="AFINN")
        ),
        mainPanel(
            tabsetPanel(
                type="tabs",
                tabPanel("Plot",         plotOutput(outputId = "plot")),
                tabPanel("cloud",        plotOutput(outputId = "cloud")),
                tabPanel("Bing Words",         plotOutput(outputId = "bingword")),
                tabPanel("Score Distribution",         plotOutput(outputId = "scores"))
            )
        )
    )
)
# Define server logic required to draw a histogram
myserver <- function(input, output) {
    observeEvent(input$do, {
        search.out <- reactive({
            searching(input$Search)
        })
        output$plot <- renderPlot(
            making.plots(search.out(), input$Dictionary)
        )
        output$cloud <- renderPlot(
            search.out() %>%
                inner_join(get_sentiments("bing")) %>%
                count(word, sentiment, sort = TRUE) %>%
                acast(word ~ sentiment, value.var = "n", fill = 0) %>%
                comparison.cloud(colors = c("gray20", "gray80"),
                                 max.words = 5000)
        )
        output$bingword <- renderPlot(
            search.out() %>%
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
        output$scores <- renderPlot(
            search.out() %>%
                count(word, sort = TRUE) %>%
                inner_join(get_sentiments("afinn"), by = NULL) %>%
                group_by(score) %>%
                summarize(Count = sum(n)) %>%
                ggplot(aes(x=as.factor(score), y=Count)) +
                geom_bar(stat="identity", show.legend = FALSE, fill="steelblue") +
                labs(y = "Count", x = "Score")
        )
    })
}

# Run the application
shinyApp(ui = myui, server = myserver)

