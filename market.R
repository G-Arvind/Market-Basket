getwd()
#setting up working directory
setwd("C://Users//Arvind//Documents//R")
#reading dataset and storing it as dataframe
mb<-read.csv("C://Users//Arvind//Documents//R//mb.csv")
#seeing the values
summary(mb)
#setting absurd values to NA
mb$Quantity[mb$Quantity<0]<-NA
mb$UnitPrice[mb$UnitPrice<0]<-NA
#omiting NA values
mbc<-na.omit(mb)
#Making 2 columns for creating sparse matrix
mbc2 <- aggregate(Description ~ InvoiceNo, data = mbc, paste, collapse = ",")
#saving modified Dataset for applying apriori algorithm
write.table(mbc2, file = "mbc4.csv", sep=",",col.names = FALSE,row.names = FALSE)
#for apriori algorithm
require(arules)
#creating sparse matrix
mar<-read.transactions("mbc4.csv",sep = ",")
mar
#inspeting the matrix
inspect(mar[1:3])
#Gives the most frequent items
itemFrequency(mar[,1])
itemFrequencyPlot(mar,support=0.20)
#gives the top5 frequently bought items
itemFrequencyPlot(mar,topN=5)
itemFrequencyPlot(mar,topN=20)
itemFrequencyPlot(mar,topN=10)
#appplying apriori algorithm
mar1<-apriori(mar,parameter = list(support=0.0002,confidence=0.6,minlen=2))
mar1
#appplying apriori algorithm with increased support
mar1<-apriori(mar,parameter = list(support=0.0005,confidence=0.6,minlen=2))
#lists the number of rules created
mar1
inspect(sort(mar1,by="lift")[1:4])
plot(mar1)
inspect(sort(mar1,by="support",decreasing = FALSE)[1:4])
marred<-is.redundant(mar1)
summary(mar1)
summary(marred)
#removing redundant rules
mar1<-mar1[!marred]
mar1
inspect(mar1[1:3])
inspect(sort(mar1,by="lift")[1:10])
mar1
#graph
plot(mar1)
itemFrequencyPlot(mar1,topN=5)
itemFrequencyPlot(mar1,support=0.0003)
itemFrequencyPlot(mar1,topN=10)
plot(mar1,method = "grouped")
plot(martest,method = "graph", control=list(type="Description"))
#applying apriori algorithm with increased confidence
martest1<-apriori(mar,parameter = list(support=0.0005,confidence=0.80,minlen=5))
#removing redundant rules
martest1<-martest1[!marred1]
martest1
#graph
plot(martest1,method = "graph", control=list(type="Description"))

