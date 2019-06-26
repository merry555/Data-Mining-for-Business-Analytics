getwd()
setwd("/Users/kimjisu/Desktop/Data_mining/")
univer.df<- read.csv("UniversalBank.csv")
#### HW
## 가변수 만든 다음( c combine )
## 60%, 40% 만들고, 예측변수 많으니깐 plot은 안됨.
## newdata 초기화, 모든 데이터에 다 적용해야함 따라서 preProcess(train.df[,1:data 개수])
## train.norm.df[,1:데이터 개수]
## k에 대한 accuracy 같음. * 데이터 개수만 주의하면 함수 그대로 적용하면 됨.
univer.df <- read.csv("UniversalBank.csv")
univer.df$Education <- factor(univer.df$Education)
edu.df <- as.data.frame(model.matrix(~ 0 + Education, data=univer.df))
univer.df <- cbind(univer.df[ , -c(1,5,8)], edu.df[ ,])
head(univer.df)


set.seed(111)
train.index <- sample(row.names(univer.df), 0.6*dim(univer.df)[1])
valid.index <- setdiff(row.names(univer.df), train.index)
train.df <- univer.df[train.index, ]
valid.df <- univer.df[valid.index, ]

# new data
new.df <- data.frame(Age = 40, Experience = 10, Income = 84, Family = 2, CCAvg = 2, 
                     Mortgage = 0, Securities.Account = 0, CD.Account = 0, Online = 1, 
                     CreditCard = 1, Education_1 = 0, Education_2 = 1, Education_3 = 0)
str(new.df)

# new sample 600 obj 새로운 데이터 샘플로 plot차트 그리기
train.df.new <- train.df[sample(row.names(train.df), 0.03*dim(train.df)[1]),]
str(train.df.new)
plot(Income ~ Age, data = train.df.new, pch = ifelse(train.df.new$Personal.Loan==1, 1, 3))
#text(train.df.new$Income, train.df.new$Age, rownames(train.df.new), pos = 4)
text(40, 84, "X")
legend("topright", c(1, 0, "newData"), pch = c(1, 3, 4))


# 표준화 전처리를 위한 사전 작업
train.personal.loan <- data.frame(train.df[, 7])
names(train.personal.loan)[c(1)] <- c("Personal.Loan")
train.norm.df <- train.df[,-7]
train.norm.df <- cbind(train.norm.df, train.personal.loan)
str(train.norm.df)

valid.personal.loan <- data.frame(valid.df[, 7])
names(valid.personal.loan)[c(1)] <- c("Personal.Loan")
valid.norm.df <- valid.df[,-7]
valid.norm.df <- cbind(valid.norm.df, valid.personal.loan)
str(valid.norm.df)

univer.personal.loan <- data.frame(univer.df[, 7])
names(univer.personal.loan)[c(1)] <- c("Personal.Loan")
univer.norm.df <- univer.df[,-7]
univer.norm.df <- cbind(univer.norm.df, univer.personal.loan)
str(univer.norm.df)


library(forecast)
norm.values <- preProcess(train.norm.df[,1:13], method = c("center", "scale"))
str(norm.values)
train.norm.df[, 1:13] <- predict(norm.values, train.norm.df[, 1:13])
valid.norm.df[, 1:13] <- predict(norm.values, valid.norm.df[, 1:13])
univer.norm.df[, 1:13] <- predict(norm.values, univer.norm.df[, 1:13])
new.norm.df <- predict(norm.values, new.df)
new.norm.df


library(FNN)
nn <- knn(train = train.norm.df[, 1:13], test = new.norm.df, cl = train.norm.df[, 14], k = 1)
row.names(train.df)[attr(nn, "nn.index")]
nn
train.df[c("4035"),]
train.norm.df[c(2899),]
#train.df

library(caret)
accuracy.df <- data.frame(k = seq(1, 14, 1), accuracy = rep(0, 14))
# confusionMatrix 레벨오류로인한 문제를 해결하기위해 data의level과 reference의 level을 맞춰준다 
valid.norm.df[, 14] <- factor(valid.norm.df[, 14], levels = c(0,1), labels = c(0,1))
for(i in 1:14){
  knn.pred <- knn(train.norm.df[, 1:13], valid.norm.df[, 1:13],
                  cl = train.norm.df[, 14], k = i)
  accuracy.df[i, 2] <- confusionMatrix(knn.pred, valid.norm.df[, 14])$overall[1]
}
accuracy.df
knn.pred <- knn(train.norm.df[, 1:13], valid.norm.df[, 1:13],
                cl = train.norm.df[, 14], k = 3)
conf <- confusionMatrix(knn.pred, valid.norm.df[, 14])
conf
str(conf)

# k = 3
knn.pred.new <- knn(train = univer.norm.df[, 1:13], test = new.norm.df, cl = univer.norm.df[, 14], 
                    k=3, prob = TRUE)
str(knn.pred.new)
univer.df[c("4035", "4408", "3399"),]

# 자료형 integer -> factor로 변환
# 7번이 목표변수, 7번만 빼고 나머지 변수만 normalization.
# 정규화 : 전체 데이터 정규화 해야함.
# confusionMatrix 인자 integer일 경우 facotr로 바꾸기