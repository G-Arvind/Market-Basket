getwd()
setwd("C://Users//Arvind//Documents//R")
mb<-read.csv("C://Users//Arvind//Documents//R//mb.csv")
mb$Quantity[mb$Quantity<0]<-NA
mb$UnitPrice[mb$UnitPrice<0]<-NA
mbc<-na.omit(mb)
mbc2 <- aggregate(Description ~ InvoiceNo, data = mbc, paste, collapse = ",")
write.table(mbc2, file = "mbc4.csv", sep=",",col.names = FALSE,row.names = FALSE)
require(arules)
mar<-read.transactions("mbc4.csv",sep = ",")
mar
inspect(mar[1:3])
itemFrequency(mar[,1])
itemFrequencyPlot(mar,support=0.20)
itemFrequencyPlot(mar,topN=5)
itemFrequencyPlot(mar,topN=20)
itemFrequencyPlot(mar,topN=10)
mar1<-apriori(mar,parameter = list(support=0.0002,confidence=0.6,minlen=2))
mar1
mar1<-apriori(mar,parameter = list(support=0.0005,confidence=0.6,minlen=2))
mar1
inspect(sort(mar1,by="lift")[1:4])
plot(mar1)
inspect(sort(mar1,by="support",decreasing = FALSE)[1:4])
marred<-is.redundant(mar1)
summary(mar1)
summary(marred)
mar1<-mar1[!marred]
mar1
inspect(mar1[1:3])
inspect(sort(mar1,by="lift")[1:10])
mar1
plot(mar1)
itemFrequencyPlot(mar1,topN=5)
itemFrequencyPlot(mar1,support=0.0003)
itemFrequencyPlot(mar1,topN=10)
plot(mar1,method = "grouped")
plot(martest,method = "graph", control=list(type="Description"))
martest1<-apriori(mar,parameter = list(support=0.0005,confidence=0.80,minlen=5))
martest1<-martest1[!marred1]
martest1
plot(martest1,method = "graph", control=list(type="Description"))

