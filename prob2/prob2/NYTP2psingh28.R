data1<-read.csv(file = "dds_ch2_nyt/nyt01.csv")
head(data1)
data1$agecat<- cut(data1$Age,c(-Inf,0,18,24,34,44,54,64,Inf))
summary(data1)
library(doBy)
siterange<-function(x){c(length(x), min(x), mean(x), max(x))}
summaryBy(Age~agecat, data = data1,FUN=siterange)
summaryBy(Gender+Signed_In+Impressions+Clicks~agecat, data = data1,FUN=siterange)
library(ggplot2)
ggplot(data1, aes(x=Impressions, fill=agecat))+geom_histogram(binwidth=1)
ggplot(data1, aes(x=agecat, y= Impressions, fill=agecat))+geom_boxplot()

data1$hasimps<-cut(data1$Impressions, c(-Inf,0,Inf))
summaryBy(Clicks~hasimps,data=data1, FUN = siterange)
ggplot(subset(data1,Impressions>0), aes(x=Clicks/Impressions, colour= agecat))+geom_density()
ggplot(subset(data1,Clicks>0), aes(x=Clicks/Impressions, colour= agecat))+geom_density()
ggplot(subset(data1,Clicks>0), aes(x=agecat, y= Clicks, fill= agecat))+geom_boxplot()

data1$scode[data1$Impressions==0]<-"No Imps"
data1$scode[data1$Impressions>0]<-"Imps"
data1$scode[data1$Clicks>0]<-"Clicks"

data1$scode<-factor(data1$scode)
head(data1)

clen<-function(x){c(length(x))}
etable<-summaryBy(Impressions~scode+Gender+agecat, data= data1, FUN = clen)