
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(rjson)
library(ggplot2)
library(twitteR)
library(sentiment)
library(plyr)

#counter<-0
filenames1 <- list.files(pattern="hillary+.*json") 
filenames2 <- list.files(pattern="trump+.*json")

#print(filenames1)
#print(filenames2)

hillary_json<-fromJSON(file=filenames1[1])
hillary<-data.frame(hillary_json)

trump_json<-fromJSON(file=filenames2[1])
trump<-data.frame(trump_json)
print("here i am")
for (i in 2:7){
  hillary_json<-fromJSON(file=filenames1[i])
  temp<-data.frame(hillary_json)
  hillary<-rbind(hillary, temp)
}
rm(temp)
hillary<-hillary[c("text")]
#hillary<-hillary[1:100,]

for (i in 2:7){
  trump_json<-fromJSON(file=filenames2[i])
  temp<-data.frame(trump_json)
  trump<-rbind(trump, temp)
}
View(trump)

trump<-trump[c("text")]
View(trump)

senti_analysis<-function(final){
  print("entering analysis function")
  some_tweet<-as.vector(final[['text']])
  some_txt =gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_tweet)
  some_txt = gsub("@\\w+", "", some_txt)
  some_txt = gsub("[[:punct:]]", "", some_txt)
  some_txt = gsub("[[:digit:]]", "", some_txt) # remove numbers
  some_txt = gsub("http\\w+", "", some_txt) # remove html links
  some_txt = gsub("[ \t]{2,}", "", some_txt) # remove unnecessary spaces
  some_txt = gsub("^\\s+|\\s+$", "", some_txt)
  try.error = function(x)
  {
    y = NA # create missing value
    # tryCatch error
    try_error = tryCatch(tolower(x), error=function(e) e)
    # if not an error
    if (!inherits(try_error, "error"))
      y = tolower(x)
    return(y)
  }
  some_txt = sapply(some_txt, try.error) # lower case using try.error with sapply
  some_txt = some_txt[!is.na(some_txt)] # remove NAs in some_txt
  names(some_txt) = NULL
  
  # Perform Sentiment Analysis
  class_emo = classify_emotion(some_txt, algorithm="bayes", prior=1.0) # classify emotion
  emotion = class_emo[,7] # get emotion best fit
  emotion[is.na(emotion)] = "unknown" # substitute NA's by "unknown"
  
  # classify polarity
  class_pol = classify_polarity(some_txt, algorithm="bayes")
  polarity = class_pol[,4] # get polarity best fit
  # Create data frame with the results and obtain some general statistics
  # data frame with results
  sent_df = data.frame(text=some_txt, emotion=emotion,
                       polarity=polarity, stringsAsFactors=FALSE)
  sent_df = within(sent_df,
                   emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
  print("analysis done once")
  return(sent_df)
  
  
}

stream<-function(aString){
  print("entering streaming")
  load("my_oauth.Rdata")
  filterStream(file.name = "random.json", # Save tweets in a json file
               track = c(aString),
               language = "en",
               timeout = 10, # Keep connection alive for 60 seconds
               oauth = my_oauth) # Use my_oauth file as the OAuth credentials
  
  tweets.df <- parseTweets("random.json", simplify = FALSE)
  fn<-"random.json"
  # if (file.exists(fn)) file.remove(fn)
  someDF<-senti_analysis(tweets.df)
  someDF$polarity<-as.factor(someDF$polarity)
  return(someDF)
}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
     layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
     # Make each plot, in the correct location
    for (i in 1:numPlots) {
     matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

shinyServer(function(input, output,session) {
  print("entered into server")
  # hillary_senti<-senti_analysis(hillary)
  counter<-0
  trump_senti<-senti_analysis(trump)
  trump_senti$polarity<-as.factor(trump_senti$polarity)
  hillary_senti<-senti_analysis(hillary)
  hillary_senti$polarity<-as.factor(hillary_senti$polarity)
  
  print("after calling analysis")
  #counter<-counter+1
  autoInvalidate <- reactiveTimer(120000, session)
   output$distPlot <- renderPlot({
      print("rendering")
     
     autoInvalidate()
     print(counter)
     
       print("going for stream")
       trump_senti1<-stream("donald trump")
       hillary_senti1<-stream("hillary clinton")
       
       trump_senti<-rbind(trump_senti,trump_senti1)
       hillary_senti<-rbind(hillary_senti,hillary_senti1)
       
       print(nrow(trump_senti))
     
     
     a<-ggplot(trump_senti, aes(x=polarity, fill=polarity))+geom_bar()+ggtitle("Donald Trump related tweets - Polarity")
     b<-ggplot(hillary_senti, aes(x=polarity, fill=polarity))+geom_bar()+ggtitle("Hillary Clinton related tweets - Polarity")
     c<-ggplot(trump_senti, aes(x=emotion, fill=polarity))+geom_bar()+ggtitle("Donald Trump related tweets - Emotions")
     d<-ggplot(hillary_senti, aes(x=emotion, fill=polarity))+geom_bar()+ggtitle("Hillary Clinton related tweets - Emotions")
     multiplot(a,b,c,d,cols=2)
#      counter<-2
#      print(counter)
 })
   
#    output$distPlot2 <- renderPlot({
#      autoInvalidate()
#      if(counter>1){
#        hillary_senti<-stream("hillary clinton")
#      }
#      ggplot(hillary_senti, aes(x=polarity, fill=polarity))+geom_bar()+ggtitle("Hillary Clinton related tweets - Polarity")
#    })
#    
#    output$distPlot3 <- renderPlot({
#      autoInvalidate()
#      ggplot(trump_senti, aes(x=emotion, fill=polarity))+geom_bar()+ggtitle("Donald Trump related tweets - Emotions")
#    })
#    
#    output$distPlot4 <- renderPlot({
#      autoInvalidate()
#      ggplot(hillary_senti, aes(x=emotion, fill=polarity))+geom_bar()+ggtitle("Hillary Clinton related tweets - Emotions")
#    })
#    
   

})
