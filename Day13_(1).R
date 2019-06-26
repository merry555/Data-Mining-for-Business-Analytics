getwd()
setwd("/Users/kimjisu/Desktop/Data_mining/")
install.packages("rpart")
install.packages("rpart.plot") ## tree
library(rpart)
library(caret)
library(rpart.plot)
mower.df <- read.csv("RidingMowers.csv")
str(mower.df)
class.tree <- rpart(Ownership ~ ., data=mower.df, 
                    control = rpart.control(maxdepth=2), method="class")
prp(class.tree, type=1, under=TRUE,extra=1, split.font =1, varlen=-10)
class.tree <- rpart ( Ownership ~ ., data=mower.df, 
                      control = rpart.control(minsplit=2), method="class")  #Full Grown Tree
prp(class.tree, type=1, under=TRUE,extra=1, split.font =1, varlen=-10) ## 그리는건 똑같음
#######################################
bank.df <- read.csv("UniversalBank.csv")
bank.df <- bank.df[ , -c(1,5)]
set.seed(1)
train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6)
train.df <- bank.df[train.index,]
valid.df <- bank.df[-train.index, ]
###############Default CT
default.ct <- rpart(Personal.Loan ~ ., data=train.df, method="class")
prp(default.ct, type=1, extra=1,  under=TRUE, split.font=1, varlen=-10)
length(default.ct$frame$var[default.ct$frame$var=="<leaf>"])
default.ct$frame$var  # tree 순회 
prp(default.ct, type=1, extra=1,  under=TRUE, split.font=1, varlen=-10,
    box.col=ifelse(default.ct$frame$var=="<leaf>", 'gray', 'white'))  #leaf node gray
str(default.ct)
#################Full CT
deeper.ct <- rpart(Personal.Loan ~ ., data=train.df, method="class", cp=0, minsplit=1)
length(deeper.ct$frame$var[deeper.ct$frame$var=="<leaf>"])
prp(deeper.ct, type=1, extra=1,  under=TRUE, split.font=1, varlen=-10,
    box.col=ifelse(deeper.ct$frame$var=="<leaf>", 'gray', 'white'))
#train.df
## integer -> factor로 형변환
default.ct.point.pred.train <- predict(default.ct, train.df, type="class")
confusionMatrix(default.ct.point.pred.train, factor(train.df$Personal.Loan))
#valid.df
## defult tree가 훨씬 효율적이다. deeper tree가 새로운 데이터가 온다면
## 과적합이 된다 따라서 가지치기가 필요
default.ct.point.pred.valid <- predict(default.ct, valid.df, type="class")
confusionMatrix(default.ct.point.pred.valid, factor(valid.df$Personal.Loan))
#################deeper
## train 은 accuracy 가 1이 나왔기 때문에 과적합됨.
deeper.ct.point.pred.train <- predict(deeper.ct, train.df, type="class")
confusionMatrix(deeper.ct.point.pred.train, factor(train.df$Personal.Loan))
deeper.ct.point.pred.valid <- predict(deeper.ct, valid.df, type="class")
confusionMatrix(deeper.ct.point.pred.valid, factor(valid.df$Personal.Loan))
