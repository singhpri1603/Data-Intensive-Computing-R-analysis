library(streamR)
library(jsonlite)
library(rjson)
library(doBy)

### making a list of file names
filenames <- list.files(pattern="nba+.*json") 
### a new dataframe
final<-data.frame(day=NA, isRetweet=NA, RetweetAve= NA )

for (i in 1:7){
  dframe<-fromJSON(filenames[i]) ### reading json file
  twdf<-data.frame(dframe) ### converting to dataframe
  
  twdf2<-twdf[twdf$isRetweet==TRUE,]
  
  x<-length(twdf2$isRetweet)
  y<-sum(twdf2$retweetCount)
  z<-y/x
  
  final[i,1]<-paste("DAY", toString(i), sep=" ")
  final[i,2]<-x
  final[i,3]<-z
}

library(ggplot2)
ggplot(final, aes(x=day, y=isRetweet))+geom_point(size= 7, aes(colour = day))
ggplot(final, aes(x=day, y=RetweetAve))+geom_point(size= 7, aes(colour = day))

