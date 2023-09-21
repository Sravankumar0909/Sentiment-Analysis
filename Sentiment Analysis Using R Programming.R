library(RSelenium)                                                        #loading the RSelenium Library
library(tidyverse)  #loading the tidyverse Library
#library(binman)
rs_driver_obj = rsDriver(browser = "chrome", chromever = "111.0.5563.64") #binmam::list_versions  Creating rsDriver object
remDr = rs_driver_obj$client                                              #creating rsDriver object for the client side

remDr$open()                                                                           #opening the web browser
remDr$navigate("https://play.google.com/store/apps/details?id=com.aim.racingstreet")   #navigating to the url specified

remDr$findElement(using = "xpath", "//button[@class='VfPpkd-LgbsSe VfPpkd-LgbsSe-OWXEXe-dgl2Hf ksBjEc lKxP2d LQeN7 aLey0c']")$clickElement()     #nesting through the html tags on the web page to get all the reviews
all_reviews = remDr$findElement(using = "xpath", "//*[@id='yDmH0d']/div[4]/div[2]/div/div/div/div")                                              
reviews = all_reviews$findElement(using = "xpath","//div[@class='h3YV2d']")$getElementText()                                                     #scraping the reviews
print(reviews)

rs_driver_obj$server$stop()          #closing the web browser

library(dplyr)
library(tidytext)

gpreviews = data.frame(reviews)                                    #Converting the scraped content into a dataframe 
colnames(gpreviews) = c("text")
reviews_tokens = gpreviews %>% unnest_tokens(word, text)           #Dividing the entire text into individual words
sentiments = get_sentiments("afinn")                               #Getting the sentiments from the afinn lexicon and storing
reviews_sentiments = reviews_tokens %>% inner_join(sentiments, by = "word")   #Assigning values to the words scraped by using the afinn 
total_sentiments = reviews_sentiments %>% summarize(total = sum(value))       #calculating the entire sentiment of the reviews
total_sentiments




reviews_sentiments$category = ifelse(reviews_sentiments$value>=0,"positive","negative")      #categorizing the words into negative and positive

posi = reviews_sentiments %>% filter(category=="positive")                                   #Storing the positive words in a variable

negi = reviews_sentiments %>% filter(category == "negative")                                 #Storing the negative words in a variable

negi_score = sum(negi$value)                                                                 #Calculating the sum of positive values
posi_score = sum(posi$value)                                                                 #Calculating the sum of negative values
posi_negi = c(negi_score,posi_score)                                                         #Storing the sum of positive and negative in another variable
posi_negi
barplot(posi_negi,ylim=c(-500,2000),col=c("red","green"),xlab="category",ylab="values",names.arg = c("negative","positive"))  #visualizing 
legend("topright",legend=c("negative","positive"),fill=c("red","green"))                                         #the positive and negative



