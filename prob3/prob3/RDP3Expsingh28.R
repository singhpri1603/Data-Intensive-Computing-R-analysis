#setwd('dds_ch2_rollingsales/')
require(gdata)
require(plyr)
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
count(is.na(bk$sale.price.n))
bk$gross.sqft <- as.numeric(gsub("[^[:digit:]]","", bk$gross.square.feet))
bk$land.sqft <- as.numeric(gsub("[^[:digit:]]","", bk$land.square.feet))
bk$sale.date <- as.Date(bk$sale.date, "%m/%d/%y")
bk$year.built <- as.numeric(as.character(bk$year.built))

attach(bk)
hist(sale.price.n)
hist(sale.price.n[sale.price.n>0])
hist(gross.sqft[sale.price.n==0])
detach(bk)
bk.sale <- bk[bk$sale.price.n!=0,]
plot(bk.sale$gross.sqft,bk.sale$sale.price.n)
plot(log(bk.sale$gross.sqft),log(bk.sale$sale.price.n))
bk.homes <- bk.sale[which(grepl("FAMILY", bk.sale$building.class.category)),]
plot(log(bk.homes$gross.sqft),log(bk.homes$sale.price.n))

bk.lowsale<-bk.homes[which(bk.homes$sale.price.n<100000),][order(bk.homes[which(bk.homes$sale.price.n<100000),] $sale.price.n),]

bk.homes$outliers <- (log(bk.homes$sale.price.n) <=5) + 0
bk.homes <- bk.homes[which(bk.homes$outliers==0),]
plot(log(bk.homes$gross.sqft),log(bk.homes$sale.price.n))
bk.final<-bk.homes[bk.homes$year.built!=0,]

ggplot(bk.homes, aes(x=building.class.category, y= sale.price.n, fill=building.class.category))+geom_boxplot()
ggplot(bk.homes, aes(x=building.class.category, y= gross.sqft, fill=building.class.category))+geom_boxplot()

ggplot(bk.final, aes(x=year.built))+geom_histogram(binwidth = 5, aes(fill= building.class.category))
bk.homes$log.gross.sqft<-log(bk.homes$gross.sqft)
bk.homes$log.sale.price.n<-log(bk.homes$sale.price.n)

ggplot(bk.homes, aes(x=log.gross.sqft, y=log.sale.price.n))+geom_point(aes(colour= borough))

ggplot(bk.final, aes(x=borough))+geom_bar(aes(fill=building.class.category))
bk.final$spcat<-cut(bk.final$sale.price.n, c(0,100000,200000,300000,400000,500000,600000,700000,800000,900000,1000000,Inf))

ggplot(bk.final,aes(x=spcat, fill= building.class.category))+geom_bar()
mean(bk.final$sale.price.n)