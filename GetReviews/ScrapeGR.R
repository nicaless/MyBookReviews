### Webscraping Goodreads Reviews
# https://www.r-bloggers.com/goodreads-webscraping-and-text-analysis-with-r-part-1/
# Halt Server use on command line lsof -t -i :4444 | xargs kill
# https://www.r-bloggers.com/rselenium-a-wonderful-tool-for-web-scraping/

# LOAD LIBRARIES
library(data.table)  
library(dplyr)        
library(magrittr)     
library(rvest)        
library(RSelenium)
library(optparse)

# PARAMETER DEFAULTS
DEFAULT_URL = "https://www.goodreads.com/book/show/26114291-ghost-talkers#other_reviews" 
DEFAULT_TITLE = "Ghost Talkers"
DEFAULT_PAGES = 30

# CREATE PARAMETER OPTION LIST
option_list = list(
  #PERHAPS THIS URL CAN BE GENERATED BY ANOTHER SCRIPT UTILIZING GOODREADS API 
  make_option(c("-u", "--url"), action = "store", default = DEFAULT_URL, type = "character", 
              help="full url for the book's goodreads review page"),
  #BOOK TITLE CAN BE PASSED FROM ABOVE MENTIONED SCRIPT
  make_option(c("-t", "--title"), action = "store", default = DEFAULT_TITLE, type = "character",
              help="book title"),
  make_option(c("-p", "--pages"), action = "store", default = DEFAULT_PAGES, type = "integer",
              help="number of pages of reviews to scrape")
  
)

# PARSE AND SET PARAMETERS
opt = parse_args(OptionParser(option_list = option_list))

url = opt$url
book.title = opt$title
p = opt$pages

output.dir <- paste0("../DeepWriting/data/", book.title, "/")
output.full.csv <- paste0(output.dir, book.title, "_full.csv")
output.full.txt <- paste0(output.dir, book.title, "_full.txt")



# START SELENIUM SERVER
gc()
rD = rsDriver(port = 4444L, browser = "chrome")
remDr = rD[['client']]
remDr$navigate(url)

# INITIALIZE DATAFRAME

global.df <- data.frame(book=character(),
                        reviewer = character(),
                        rating = character(),
                        review = character(), 
                        stringsAsFactors = F)

# FOR THE FIRST P PAGES
for(i in 1:p) {
  print(paste0("On page ", i))
  # EXTRACT REVIEWS
  message("Extract Reviews")
  print("find elements")
  reviews <- remDr$findElements("css selector", "#bookReviews .stacked")
  print("lapply get element attribute")
  reviews.html <- lapply(reviews, 
                         function(x) { 
                           x$getElementAttribute("outerHTML")[[1]]
                         })
  print("lapply read_html")
  reviews.list <- lapply(reviews.html, function(x){read_html(x) %>% html_text()} )
  reviews.text <- unlist(reviews.html)
  
  # CLEAN REVIEWS
  message("Clean Reviews")
  reviews.text2 <- gsub("<.*?>", "", reviews.text)
  #reviews.text3 <- gsub("[^A-Za-z\\-]|\\.+", " ", reviews.text2)
  reviews.clean <- gsub("\n|[ \t]+", " ", reviews.text2) 
  
  # PUT REVIEWS IN TABLE
  message("Initiate Table for Reviews")
  n <- floor(length(reviews)/2)
  reviews.df <- data.frame(book = character(n),
                           reviewer = character(n),
                           rating = character(n),
                           review = character(n), 
                           stringsAsFactors = F)
  
  # Populating a data frame with the relevant fields
  message("Populating Table")
  for(j in 1:n){
    message("Adding Title")
    reviews.df$book[j] <- book.title
    
    message("Adding Reviewer Name")
    #Isolating the name of the author of the review
    auth.rat.sep <- regexpr(" rated it | marked it | added it ", 
                            reviews.clean[2*j-1]) 
    #reviews.df$reviewer[j] <- substr(reviews.clean[2*j-1], 5, auth.rat.sep-1)
    reviews.df$reviewer[j] <- substr(reviews.clean[2*j-1], 18, auth.rat.sep-3)
    
    #Isolating the rating
    message("Adding Rating")
    rat.end <- regexpr("· | Shelves| Recommend| review of another edition",
                       reviews.clean[2*j-1])
    if (rat.end==-1){rat.end <- nchar(reviews.clean[2*j-1])}
    #reviews.df$rating[j] <- substr(reviews.clean[2*j-1], auth.rat.sep+10, rat.end-1)
    reviews.df$rating[j] <- substr(reviews.clean[2*j-1], auth.rat.sep+11, rat.end-9)
    
    #Removing the beginning of each review that was repeated on the html file
    message("Removing Duplicate String in Reiew")
    #short.str <- substr(reviews.clean[2*j], 1, 50)
    #rev.start <- unlist(gregexpr(short.str, reviews.clean[2*j]))[2]
    short.str <- substr(reviews.clean[2*j], 6, 50)
    rev.start <- unlist(gregexpr(short.str, reviews.clean[2*j], fixed = T))[2]
    if (is.na(rev.start)){rev.start <- 1}
    rev.end <- regexpr("\\.+more|Blog", reviews.clean[2*j])
    if (rev.end==-1){rev.end <- nchar(reviews.clean[2*j])}
    reviews.df$review[j] <- substr(reviews.clean[2*j], rev.start, rev.end-1)
  }
  
  message("Binding All Reviews")
  global.lst <- list(global.df, reviews.df)
  global.df <- rbindlist(global.lst)
  
  message("Turn Page")
  #.next_page doesn't exist if no more reviews.  
  NextPageButton <- remDr$findElement("css selector", ".next_page")
  # need try/catch for the error to break loop, THE BELOW IF WILL NOT WORK
  #http://stackoverflow.com/questions/16166261/selenium-webdriver-how-to-resolve-stale-element-reference-exception
  if(is.null(NextPageButton)) {
    break
  }
  NextPageButton$clickElement()
  Sys.sleep(3)
}


# # SPLIT GLOBAL DF
# global.df$reviewer = unlist(lapply(global.df$reviewer,
#                                    function(x) {
#                                      unlist(strsplit(
#                                        unlist(strsplit(x, "a title "))[2],
#                                        " class"))[1]
#                                    }))
# 
# global.df$rating = unlist(lapply(global.df$rating,
#                                  function(x) {
#                                    unlist(strsplit(
#                                      unlist(strsplit(x, "title "))[2], 
#                                      " size x"))[1]
#                                  }))
# 
# global.df$review = unlist(lapply(global.df$review,
#                                  function(x) {
#                                    unlist(strsplit(
#                                      unlist(strsplit(x, "freeTextContainer "))[2],
#                                      " span a"))[1]
#                                  }))

# WRITE TO CSV AND TXT 
global.df = unique(global.df)
write.csv(global.df, file = output.full.csv, row.names = F)
write(global.df$review, file = output.full.txt, sep = "\n")

#TEMP
#write(global.df$review, file = "GhostTalkers2.txt", sep = "\n")

remDr$closeServer()
gc()
