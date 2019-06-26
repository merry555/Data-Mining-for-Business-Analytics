getwd()
setwd("/Users/kimjisu/Desktop/Data_mining/")
mower.df <- read.csv("RidingMowers.csv")
str(mower.df)
mower.df
set.seed(111)
train.index <- sample(row.names(mower.df), 0.6*dim(mower.df)[1])
length(train.index)
valid.index <- setdiff(row.names(mower.df), train.index)
train.df <- mower.df[train.index, ]
train.df
valid.df <- mower.df[valid.index, ]
valid.df
##new data
new.df <- data.frame(Income=60, Lot_Size=20)
##scatter plot
plot(Lot_Size ~ Income, data=train.df, pch=ifelse(train.df$Ownership=="Owner", 1, 3))
text(train.df$Income, train.df$Lot_Size, rownames(train.df), pos=4)
#text(60, 20, 4)
text(60, 20, "X")
legend("topright", c("owner", "nonowner", "newhousehold"), pch=c(1,3,4))
#Initialzing normalized training, validation, complete data frame
train.norm.df <- train.df
valid.norm.df <- valid.df
mower.norm.df <- mower.df
#use preprocess() from caret package to normalize Income and Lot_Size
library(caret)
norm.values <- preProcess(train.df[ , 1:2], method=c("center", "scale")) 
str(norm.values)
mean(train.df[,1]);mean(train.df[ , 2]);sd(train.df[,1]);sd(train.df[ , 2])
train.norm.df[ , 1:2] <- predict(norm.values, train.df[ , 1:2])
valid.norm.df[ , 1:2] <- predict(norm.values, valid.df[ , 1:2])
mower.norm.df[ , 1:2] <- predict(norm.values, mower.df[ , 1:2])
# predic 
new.norm.df <- predict(norm.values, new.df)
new.norm.df
#train.df의 변수의 평균과 표준편차를 이용하여 new.norm.df 표준화 체크해 보기
(60-mean(train.df[,1]))/sd(train.df[,1])
(20-mean(train.df[,2]))/sd(train.df[,2]) # 20 second value 
#use knn() to compute knn. knn is available in the library FNN (provide  a list of #the nearest neighbors) and a library class. 
library(FNN)
nn <- knn(train=train.norm.df[ , 1:2], test=new.norm.df, cl=train.norm.df[ , 3], k=3, prob=TRUE)
# training data row name -> nn
row.names(train.df)[attr(nn, "nn.index")]
nn
train.df

##### find k ######
accuracy.df <- data.frame(k = seq(1,14,1), accuracy = rep(0,14)) # 반복 
# 실제값 예측값 차이로 정오분류표 만든 다음 overall의 첫번째 값이 K가됨
# k 를 i로 잡았을 때 validation data 어느 클래스에 속하는지 accuracy계산
# k는 1~20 사이의 Maximum값을 찾으면 된다.
# k = 4 로 결정되면 전체 데이터를 다 써서 test데이터에서는 new데이터, k = 4, prob = TRUE
# 제일 가까운 것 0.4064 3/4 Level : Owner, 
# index를 rowname으로 잡아서 값 같음.
library(caret)
accuracy.df <- data.frame(k=seq(1,14,1), accuracy=rep(0,14))
dim(accuracy.df)
accuracy.df
for(i in 1:14){
  knn.pred <- knn(train=train.norm.df[ , 1:2], test=valid.norm.df[ , 1:2], 
                  cl=train.norm.df[,3], k=i)
  accuracy.df[i,2]<- confusionMatrix(knn.pred, valid.norm.df[, 3])$overall[1]
}
#conf <-confusionMatrix(knn.pred, valid.norm.df[, 3])
#str(conf) #overall의 첫번째 원소가  Accuracy
accuracy.df # 1.0이 제일 좋음
conf <- confusionMatrix(knn.pred, valid.norm.df[, 3])
conf
str(conf) # overall 속성, 1번째 값이 accuracy 값 0.4
conf
knn.pred.new <- knn(train=mower.norm.df[ , 1:2], test=new.norm.df, 
                    cl=mower.norm.df[ , 3], k=4, prob=TRUE)
knn.pred.new
#row.names(train.df)[attr(nn, "nn.index")]: 교과서가  틀림
row.names(mower.df)[attr(knn.pred.new, "nn.index")]
#### HW
## 가변수 만든 다음( c combine)
## 60%, 40% 만들고, 예측변수 많으니깐 plot은 안됨.
## newdata 초기화, 모든 데이터에 다 적용해야함 따라서 preProcess(train.df[,1:data 개수])
## train.norm.df[,1:데이터 개수]
## k에 대한 accuracy 같음. * 데이터 개수만 주의하면 함수 그대로 적용하면 됨.

