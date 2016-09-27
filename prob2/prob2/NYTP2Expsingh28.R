setwd('dds_ch2_nyt/')
library(doBy)
siterange<-function(x){c(length(x), min(x), mean(x), max(x))}
clen<-function(x){c(length(x))}
addition<-function(x){c(sum(x))}
temp = list.files(pattern="nyt+.*csv")
temp
collect<- data.frame(day=NA, agecat=NA, pcount=NA, Clicks=NA, Impressions=NA, No_imps=NA)
for (i in 1:31) {
  data1<-read.csv(temp[i], header = TRUE)
  data1$agecat<- cut(data1$Age,c(-Inf,0,18,24,34,44,54,64,Inf))
  summary(data1)
  summaryBy(Age~agecat, data = data1,FUN=siterange)
  summaryBy(Gender+Signed_In+Impressions+Clicks~agecat, data = data1, FUN=siterange)
  data1$hasimps<-cut(data1$Impressions, c(-Inf,0,Inf))
  summaryBy(Clicks~hasimps,data=data1, FUN = siterange)
  
  cni<-summaryBy(agecat~agecat, data=data1, FUN=clen)
  onemore<-summaryBy(Clicks+Impressions~agecat, data=data1, FUN=addition)
  imp<-summaryBy(Clicks~hasimps+agecat,data= data1, FUN=clen)
  for (j in 1:8){
    collect[j+(i-1)*8,1]<-i
    collect[j+(i-1)*8,2]<-cni[j,1]
    collect[j+(i-1)*8,3]<-cni[j,2]
    collect[j+(i-1)*8,4]<-onemore[j,2]
    collect[j+(i-1)*8,5]<-onemore[j,3]
    collect[j+(i-1)*8,6]<-imp[j,3]}
  rm(cni)
  rm(imp)
  rm(onemore)
  
  rm(data1)}
collect$agecat<-factor(collect$agecat)
ggplot(collect, aes(x=Impressions/10000, fill=agecat))+geom_histogram(binwidth=5)
ggplot(collect, aes(x=agecat, y= Impressions, fill=agecat))+geom_boxplot()
ggplot(collect, aes(x=Clicks/Impressions, colour= agecat))+geom_density()
ggplot(collect, aes(x=agecat, y= Clicks, fill= agecat))+geom_boxplot()

collect$day<-factor(collect$day)
ggplot(collect, aes(x=day, y= Impressions, fill=day))+geom_boxplot()
ggplot(collect, aes(x=day, y= Clicks, fill= day))+geom_boxplot()

setwd('/Users/priyanka/')