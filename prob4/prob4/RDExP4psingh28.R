library(jsonlite)
library(RJSONIO)
library(rjson)
require(gdata)
require(plyr)

filenames <- list.files(pattern="tweets+.*json") 
df11<-fromJSON(filenames[1])
df22<-fromJSON(filenames[2])
df33<-fromJSON(filenames[3])
df44<-fromJSON(filenames[4])
df55<-fromJSON(filenames[5])
df66<-fromJSON(filenames[6])

#some_txt = sapply(df11, function(x) x$getText())

df1<-data.frame(df11)
df2<-data.frame(df22)
df3<-data.frame(df33)
df4<-data.frame(df44)
df5<-data.frame(df55)
df6<-data.frame(df66)

# for (i in 2:7){
#   dframe<-fromJSON(filenames[i])
#   final<-rbind(final, dframe)
# }

df1<-df1['text']
df2<-df2['text']
df3<-df3['text']
df4<-df4['text']
df5<-df5['text']
df6<-df6['text']

final<-rbind(df1,df2)
final<-rbind(final,df3)
final<-rbind(final,df4)
final<-rbind(final,df5)
final<-rbind(final,df6)

some_tweet<-as.vector(final[['text']])

some_txt =gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_tweet)
some_txt = gsub("@\\w+", "", some_txt)
some_txt = gsub("[[:punct:]]", "", some_txt)
# remove numbers
some_txt = gsub("[[:digit:]]", "", some_txt)
# remove html links
some_txt = gsub("http\\w+", "", some_txt)
# remove unnecessary spaces
some_txt = gsub("[ \t]{2,}", "", some_txt)
some_txt = gsub("^\\s+|\\s+$", "", some_txt)

try.error = function(x)
{
  # create missing value
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # result
  return(y)
}
# lower case using try.error with sapply
some_txt = sapply(some_txt, try.error)

# remove NAs in some_txt
some_txt = some_txt[!is.na(some_txt)]
names(some_txt) = NULL

if (!require("pacman")) install.packages("pacman")
pacman::p_load(devtools, installr)
install.Rtools()
install_url("http://cran.r-project.org/src/contrib/Archive/Rstem/Rstem_0.4-1.tar.gz")
install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")


if (!require("pacman")) install.packages("pacman")
pacman::p_load(twitteR, sentiment, plyr, ggplot2, wordcloud, RColorBrewer, httpuv, RCurl, base64enc)

# Perform Sentiment Analysis
# classify emotion
class_emo = classify_emotion(some_txt, algorithm="bayes", prior=1.0)
# get emotion best fit
emotion = class_emo[,7]
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown"

# classify polarity
class_pol = classify_polarity(some_txt, algorithm="bayes")
# get polarity best fit
polarity = class_pol[,4]
# Create data frame with the results and obtain some general statistics
# data frame with results
sent_df = data.frame(text=some_txt, emotion=emotion,
                     polarity=polarity, stringsAsFactors=FALSE)

# sort data frame
sent_df = within(sent_df,
                 emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))


# Let???s do some plots of the obtained results
# plot distribution of emotions
ggplot(sent_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="emotion categories", y="number of tweets") +
  ggtitle("Sentiment Analysis of Tweets about Starbucks\n(classification by emotion)") +
  theme(plot.title = element_text(size=12, face="bold"))


# plot distribution of polarity
ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x="polarity categories", y="number of tweets") +
  ggtitle("Sentiment Analysis of Tweets about Starbucks\n(classification by polarity)") +
  theme(plot.title = element_text(size=12, face="bold"))



temp = list.files(pattern="rollingsales_+.*csv")
temp
bk<-read.csv(temp[1], header = TRUE, skip = 4)
for(i in 2:5){
  data2<-read.csv(temp[i], header = TRUE, skip = 4)
  bk<-rbind(bk, data2)
}

rm(data2)
names(bk) <- tolower(names(bk))
bk$sale.price.n <- as.numeric(gsub("[^[:digit:]]","", bk$sale.price))
bk$gross.sqft <- as.numeric(gsub("[^[:digit:]]","", bk$gross.square.feet))
bk$land.sqft <- as.numeric(gsub("[^[:digit:]]","", bk$land.square.feet))
bk$sale.date <- as.Date(bk$sale.date, "%m/%d/%y")
bk$year.built <- as.numeric(as.character(bk$year.built))
bk.sale <- bk[bk$sale.price.n!=0,]
bk.homes <- bk.sale[which(grepl("FAMILY", bk.sale$building.class.category)),]
bk.lowsale<-bk.homes[which(bk.homes$sale.price.n<100000),][order(bk.homes[which(bk.homes$sale.price.n<100000),] $sale.price.n),]

bk.homes$outliers <- (log(bk.homes$sale.price.n) <=5) + 0
bk.homes <- bk.homes[which(bk.homes$outliers==0),]
bk.final<-bk.homes[bk.homes$year.built!=0,]

bk.final$spcat<-cut(bk.final$sale.price.n, c(0,100000,200000,300000,400000,500000,600000,700000,800000,900000,1000000,Inf))

ggplot(bk.final,aes(x=spcat, fill= building.class.category))+geom_bar()
mean(bk.final$sale.price.n)

