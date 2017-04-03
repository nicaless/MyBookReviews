### Sentiment Analysis Goodreads Reviews
# https://www.r-bloggers.com/goodreads-exploratory-data-analysis-and-sentiment-analysis-part-2/

# LOAD LIBRARIES
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

# PARAMETER DEFAULTS
DEFAULT_TXT_FILE = "../DeepWriting/data/Ghost Talkers/Ghost Talkers_full.csv"
DEFAULT_TITLE = "Ghost Talkers"

# CREATE PARAMETER OPTION LIST
option_list = list(
  #BOOK TITLE CAN BE PASSED FROM ScrapeGR SCRIPT
  make_option(c("-t", "--title"), action = "store", default = DEFAULT_TITLE, type = "character",
              help="book title"),
  make_option(c("-f", "--file"), action = "store", default = DEFAULT_TXT_FILE, type = "character",
              help="csv file of book reviews")
  
)

# PARSE AND SET PARAMETERS
opt = parse_args(OptionParser(option_list = option_list))

file = opt$file
book.title = opt$title

output.dir <- paste0("../DeepWriting/data/", book.title)
output.HiPos.txt <- paste0(output.dir, book.title, "_HiPos.txt")
output.HiNeg.txt <- paste0(output.dir, book.title, "_HiNeg.txt")
output.LoPos.txt <- paste0(output.dir, book.title, "_LoPos.txt")
output.LoNeg.txt <- paste0(output.dir, book.title, "_LoNeg.txt")

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

#data = unique(data)

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
reviews_joined = merge(data, review_mean_sentiment[, c("review.id", "mean_sentiment")], by = "review.id")


## Double Down on High Reviews and High Sentiment
High = rbind(reviews_joined[reviews_joined$rating > mean(reviews_joined$rating), ],
             reviews_joined[reviews_joined$mean_sentiment > mean(reviews_joined$mean_sentiment), ])


## Double Down on Low Reviews and Low Sentiment
Low = rbind(reviews_joined[reviews_joined$rating <= mean(reviews_joined$rating), ],
             reviews_joined[reviews_joined$mean_sentiment <= mean(reviews_joined$mean_sentiment), ])

## Down on Medium Reviews and Medium Sentiment
Med = rbind(reviews_joined[reviews_joined$rating < mean(reviews_joined$rating) + sd(reviews_joined$rating) &
                             reviews_joined$rating >= mean(reviews_joined$rating) - sd(reviews_joined$rating), ],
            reviews_joined[reviews_joined$mean_sentiment < mean(reviews_joined$mean_sentiment) + sd(reviews_joined$mean_sentiment) &
                             reviews_joined$mean_sentiment >= mean(reviews_joined$mean_sentiment) - sd(reviews_joined$mean_sentiment), ])
            

## Extract Text from above dataframes and save


