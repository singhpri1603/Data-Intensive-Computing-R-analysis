library(twitteR)
library(RCurl)
library(RJSONIO)
library(stringr)
 
# Declare Twitter API Credentials
api_key <- "QqgF5CYeBK8ZvVnFlPrhNNdis" # From dev.twitter.com
api_secret <- "dAQYJPfv0ctgnP3uVgm4Xa85u7mCHXn201Orxp1KPdodg6G9AD" # From dev.twitter.com
token <- "3578768241-0glX8cga4a6W4L8FzJSSCXLz3lC5enPVCAbqdFr" # From dev.twitter.com
token_secret <- "fzpHRgIeQDyepAAsZZJgueM1CiX4DpTl0wRSijGdjlybh" # From dev.twitter.com

# Create Twitter Connection
setup_twitter_oauth(api_key, api_secret, token, token_secret)

# Search through twitter
tweets <- searchTwitter("nba", lang="en", n=1500, since="2016-02-23", until = "2016-02-29")

tweets1.df <- twListToDF(tweets)

exportJsonfull <- toJSON(tweets1.df)
write(exportJsonfull, file=paste("nba",".json",sep=""))