### Webscraping Goodreads Reviews
# https://www.r-bloggers.com/goodreads-webscraping-and-text-analysis-with-r-part-1/

# LOAD LIBRARIES
library(data.table)  
library(dplyr)        
library(magrittr)     
library(rvest)        
library(RSelenium)
library(optparse)

# SET PARAMETERS
# let book url string, book title, and num review pages be parameters
#url <- "https://www.goodreads.com/book/show/18619684-the-time-traveler-s-wife#other_reviews"
url <- "https://www.goodreads.com/book/show/26114291-ghost-talkers#other_reviews"
book.title <- "Ghost Talkers"
p = 50

#output.dir <- paste0("../Deep Writing/data/", book.title) # for automation later
#output.csv <- paste0(book.title, ".csv") # for automation later
#output.txt <- paste0(book.title, ".txt") # for automation later, 
                                          # NEED TO FIGURE OUT HOW TO CONVERT FROM CSV TO TXT LATER
#output.filename.csv <- paste0(output.dir, output.csv)
output.filename <- "GhostTalkers.csv"

# START SELENIUM SERVER
rD = rsDriver(port = 4444L, browser = "chrome")
remDr = rD[['client']]
remDr$navigate(url)

# INITIALIZE DATAFRAME

global.df <- data.frame(book=character(),
                        reviewer = character(),
                        rating = character(),
                        review = character(), 
                        stringsAsFactors = F)

# FOR THE FIRST 50 PAGES
for(i in 1:p) {
  # EXTRACT REVIEWS
  reviews <- remDr$findElements("css selector", "#bookReviews .stacked")
  reviews.html <- lapply(reviews, 
                         function(x) { 
                           x$getElementAttribute("outerHTML")[[1]]
                         })
  reviews.list <- lapply(reviews.html, function(x){read_html(x) %>% html_text()} )
  reviews.text <- unlist(reviews.html)
  
  # CLEAN REVIEWS
  reviews.text2 <- gsub("[^A-Za-z\\-]|\\.+", " ", reviews.text)
  reviews.clean <- gsub("\n|[ \t]+", " ", reviews.text2) 
  
  # PUT REVIEWS IN TABLE
  n <- floor(length(reviews)/2)
  reviews.df <- data.frame(book = character(n),
                           reviewer = character(n),
                           rating = character(n),
                           review = character(n), 
                           stringsAsFactors = F)
  
  # Populating a data frame with the relevant fields
  for(j in 1:n){
    reviews.df$book[j] <- book.title
    
    #Isolating the name of the author of the review
    auth.rat.sep <- regexpr(" rated it | marked it | added it ", 
                            reviews.clean[2*j-1]) 
    reviews.df$reviewer[j] <- substr(reviews.clean[2*j-1], 5, auth.rat.sep-1)
    
    #Isolating the rating
    rat.end <- regexpr("· | Shelves| Recommend| review of another edition",
                       reviews.clean[2*j-1])
    if (rat.end==-1){rat.end <- nchar(reviews.clean[2*j-1])}
    reviews.df$rating[j] <- substr(reviews.clean[2*j-1], auth.rat.sep+10, rat.end-1)
    
    #Removing the beginning of each review that was repeated on the html file
    short.str <- substr(reviews.clean[2*j], 1, 50)
    rev.start <- unlist(gregexpr(short.str, reviews.clean[2*j]))[2]
    if (is.na(rev.start)){rev.start <- 1}
    rev.end <- regexpr("\\.+more|Blog", reviews.clean[2*j])
    if (rev.end==-1){rev.end <- nchar(reviews.clean[2*j])}
    reviews.df$review[j] <- substr(reviews.clean[2*j], rev.start, rev.end-1)
  }
  
  global.lst <- list(global.df, reviews.df)
  global.df <- rbindlist(global.lst)
  
  NextPageButton <- remDr$findElement("css selector", ".next_page")
  if(is.null(NextPageButton)) {
    break
  }
  NextPageButton$clickElement()
  Sys.sleep(3)
}


# CLEAN GLOBAL DF
global.df$reviewer = unlist(lapply(global.df$reviewer,
                                   function(x) {
                                     unlist(strsplit(
                                       unlist(strsplit(x, "a title "))[2],
                                       " class"))[1]
                                   }))

global.df$rating = unlist(lapply(global.df$rating,
                                 function(x) {
                                   unlist(strsplit(
                                     unlist(strsplit(x, "title "))[2], 
                                     " size x"))[1]
                                 }))

global.df$review = unlist(lapply(global.df$review,
                                 function(x) {
                                   unlist(strsplit(
                                     unlist(strsplit(x, "freeTextContainer "))[2],
                                     " span a"))[1]
                                 }))

# WRITE TO CSV AND TXT
#write.csv(global.df, output.filename.csv)
write.csv(global.df, output.filename, row.names = F)

write(global.df$review, file = "GhostTalkers.txt", sep = "\n")

gc()

### Sentiment Analysis Goodreads Reviews
# https://www.r-bloggers.com/goodreads-exploratory-data-analysis-and-sentiment-analysis-part-2/

library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(tm)
library(magrittr)
library(textcat)
library(tidytext)
library(RTextTools)
library(optparse)

# SET PARAMETERS
file = "GhostTalkers.csv"

# READ DATA
data <- read.csv(file, stringsAsFactors = FALSE)
data <- data.table(data)

# KEEP ENGLISH ONLY
data$language <- as.factor(textcat(data$review))
data <- data[language == "english"]

# TURN RATINGS INTO NUMBERS 
data <- data[rating %in% c('did not like it',
                           'it was ok',
                           'liked it',
                           'really liked it',
                           'it was amazing')]
data <- data[length(data$review) >= 5]
data$rating[data$rating == 'did not like it'] <- 1
data$rating[data$rating == 'it was ok'      ] <- 2
data$rating[data$rating == 'liked it'       ] <- 3
data$rating[data$rating == 'really liked it'] <- 4
data$rating[data$rating == 'it was amazing' ] <- 5
data$rating <- as.integer(data$rating)

data$language <- NULL
data$reviewer <- NULL
data$review.id <- 1:nrow(data)

# LOADING LEXICONS
# Loading the first sentiment score lexicon
AFINN <- sentiments %>%
  filter(lexicon == "AFINN") %>%
  select(word, afinn_score = score)
head(AFINN)

# Loading the second sentiment score lexicon
Bing <- sentiments %>%
  filter(lexicon == "bing") %>%
  select(word, bing_sentiment = sentiment)
head(Bing)

# "tidying" up the data (1 word per row) and adding the sentiment scores for each word
review_words <- data %>%
  unnest_tokens(word, review) %>%
  #select(-c(book, review_length)) %>%
  select(-c(book)) %>%
  left_join(AFINN, by = "word") %>%
  left_join(Bing, by = "word")

# Grouping by mean for observation 
review_mean_sentiment <- review_words %>%
  group_by(review.id, rating) %>%
  summarize(mean_sentiment = mean(afinn_score, na.rm = TRUE))

theme_set(theme_bw())
ggplot(review_mean_sentiment, aes(rating, mean_sentiment, group = rating)) +
  geom_boxplot() +
  ylab("Average sentiment score")

# REJOIN WITH REVIEW TEXT DATA
## output into different .txt files

## high rating, lower sentiment
## high rating, highest sentiment
## low rating, highest sentiment
## low rating, lowest sentiment



