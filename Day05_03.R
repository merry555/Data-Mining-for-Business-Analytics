getwd()
setwd("/Users/kimjisu/Desktop/Data_mining/")
fp.df <-read.csv("Faceplate.csv")
install.packages("arules")
library(arules)
str(fp.df)
# remove first column and convert to binary incident matrix format
fp.mat <- as.matrix(fp.df[, -1])

# convert the binary incidence matrix into a transactions database
fp.trans <- as(fp.mat, "transactions") #arules package 설치되어 있어야 함
inspect(fp.trans)

## get rules
# when running apriori(), include minimum support & confidence, & target as arguments.
rules <- apriori(fp.trans, parameter = list(supp = 0.2, conf = 0.5, target = "rules"))
inspect(head(sort(rules, by="lift"), n=10))

#################################################
all.books.df <- read.csv("CharlesBookClub.csv")
str(all.books.df)
#create a binary incident matrix
count.books.df <- all.books.df[ , 8:18]
incid.books.df <- ifelse(count.books.df > 0, 1, 0)
#첫번째 변수 제거
str(incid.books.df)
incid.books.mat <- as.matrix(incid.books.df[ ,-1]) # 오타
#첫번째 변수 포함: as.matrix(incid.book.df) 사용
#convert the binary incident matrix into a transactions database
books.trans <- as(incid.books.mat, "transactions")
inspect(books.trans,n=20)
itemFrequencyPlot(books.trans, type="absolute") #type=“relative”
rules <- apriori(books.trans, parameter=list(supp=200/4000,conf=0.5, target="rules"))
inspect(head(sort(rules, by="lift"), n=10))
