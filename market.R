Online_Retail <- read_excel("C://Users//Arvind//Documents//R//works//Online Retail.xlsx")
View(Online_Retail)

# 1. Dataset size and dimensions.
print(object.size(Online_Retail), units = "Mb")
dim(Online_Retail)

## Unclean (Raw) dataset. ##


# 1. Check for the missingness indicator.
colSums(is.na(Online_Retail))

# 2. Verify the correctness of the above for individual variables.
table(Online_Retail$Description, useNA = "ifany")
table(Online_Retail$CustomerID, useNA = "ifany")
table(Online_Retail$Country, useNA = "ifany")

Online_Retail1 <- Online_Retail
Online_Retail1$wrongQuantz = ifelse(Online_Retail$Quantity < 0 , TRUE, FALSE)
table(Online_Retail1$wrongQuantz)

# 3. Unclean data analysis.
summary(Online_Retail)    

## Tidying the dataset. ##

OnlineRetail_df = as.data.frame(Online_Retail)
CleanOnlineRetail_df <- OnlineRetail_df[complete.cases(OnlineRetail_df), ]
CleanOnlineRetail1_df <- CleanOnlineRetail_df

# 1. Cleaning unspecified countries.
CleanOnlineRetail2_df <- CleanOnlineRetail1_df[!grepl("Unspecified", CleanOnlineRetail1_df$Country),]

# 2. Cleaning irrelevant stock codes.
CleanOnlineRetail2_df <- CleanOnlineRetail2_df[!grepl("BANK CHARGES", CleanOnlineRetail2_df$StockCode),]
CleanOnlineRetail2_df <- CleanOnlineRetail2_df[!grepl("C2", CleanOnlineRetail2_df$StockCode),]
CleanOnlineRetail2_df <- CleanOnlineRetail2_df[!grepl("CRUK", CleanOnlineRetail2_df$StockCode),]
CleanOnlineRetail2_df <- CleanOnlineRetail2_df[!grepl("D", CleanOnlineRetail2_df$StockCode),]
CleanOnlineRetail2_df <- CleanOnlineRetail2_df[!grepl("DOT", CleanOnlineRetail2_df$StockCode),]
CleanOnlineRetail2_df <- CleanOnlineRetail2_df[!grepl("M", CleanOnlineRetail2_df$StockCode),]
CleanOnlineRetail2_df <- CleanOnlineRetail2_df[!grepl("PADS", CleanOnlineRetail2_df$StockCode),]
CleanOnlineRetail2_df <- CleanOnlineRetail2_df[!grepl("POST", CleanOnlineRetail2_df$StockCode),]
CleanOnlineRetail3_df <- CleanOnlineRetail2_df

# 3. Cleaning negative quantities.
CleanOnlineRetail4_df <- CleanOnlineRetail3_df[CleanOnlineRetail3_df$Quantity > 0, ]

# 4. Cleaning zero unit prices.
CleanOnlineRetail4_df <- CleanOnlineRetail4_df[CleanOnlineRetail4_df$UnitPrice > 0, ]

# 5. No changes with Invoice number, Invoice date and Description.

colSums(is.na(CleanOnlineRetail4_df))

#code for finding association rules

#considering invoice number and description alone
mbc2 <- aggregate(Description ~ InvoiceNo, data = CleanOnlineRetail4_df, paste, collapse = ",")
View(mbc2)

#creating new csv file with invoice nuber and decription alone
write.table(mbc2, file = "mbc4.csv", sep=",",col.names = FALSE,row.names = FALSE)

#loading arules for applying apriori algorithm
require(arules)

#setting up directory & creating sparse matrix
setwd("C:/Users/Arvind/Documents/R/works")
mar<-read.transactions("mbc4.csv",sep = ",")

#shows structure of sparse matrix
mar

#inspecting the sparse matrix
inspect(mar[1:3])

#shows the frequency of items purchased
itemFrequency(mar[,1])
itemFrequencyPlot(mar,topN=10)

#applying the apriori algorithm with low support value
mar1<-apriori(mar,parameter = list(support=0.0002,confidence=0.6,minlen=2))
mar1

#applying apriori algorithm with increased support
mar1<-apriori(mar,parameter = list(support=0.0005,confidence=0.6,minlen=2))
mar1
mar1<-apriori(mar,parameter = list(support=0.0004,confidence=0.6,minlen=2))

#since rules generated are less we are considering a low support value
mar1<-apriori(mar,parameter = list(support=0.0002,confidence=0.6,minlen=2))
inspect(sort(mar1,by="lift")[1:4])

#loading arules viz for graph
require(arulesViz)
plot(mar1)
inspect(sort(mar1,by="support",decreasing = FALSE)[1:4])

#checking for redundant rules 
marred<-is.redundant(mar1)
summary(mar1)
summary(marred)


#removing redundant rules
mar1<-mar1[!marred]
mar1
inspect(mar1[1:3])

#showing sorted value 
inspect(sort(mar1,by="lift")[1:10])
plot(mar1)

itemFrequencyPlot(items(mar1),topN=5)
plot(mar1,method = "grouped")








