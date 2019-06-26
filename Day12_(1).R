getwd()
setwd("/Users/kimjisu/Desktop/Data_mining/")
install.packages('e1071')
library(e1071)
delay.df <- read.csv("FlightDelays.csv")
str(delay.df)
###change numerical variables to categorical first
delay.df$DAY_WEEK <-factor(delay.df$DAY_WEEK)
delay.df$DEP_TIME <-factor(delay.df$DEP_TIME)
delay.df$CRS_DEP_TIME <- factor(round(delay.df$CRS_DEP_TIME/100))
###Create training and validation sets
selected.var  <- c(10,1,8,4,2,13)
train.index <- sample(c(1:dim(delay.df)[1]), dim(delay.df)[1]*0.6)
train.df <-delay.df[ train.index, selected.var]
valid.df <-delay.df[ - train.index, selected.var]
delay.nb <-naiveBayes(Flight.Status ~ . , data=train.df)
delay.nb # delayed : C1, ontime : C2
##############
pred.prob <- predict(delay.nb, newdata=valid.df, type = "raw")
pred.class <- predict(delay.nb, newdata=valid.df)
df <- data.frame(actual=valid.df$Flight.Status, predicted=pred.class, pred.prob)
df
df[valid.df$CARRIER=="DL"& valid.df$DAY_WEEK==7 & valid.df$CRS_DEP_TIME==10 &    valid.df$DEST=="LGA" & valid.df$ORIGIN == "DCA",  ]
## 8.6 new.df <- data.frame(과제7 참고)
## pred.class <- predict(delay.nb, new.df)
####table
prop.table(table(train.df$Flight.Status))
table(train.df$Flight.Statu, train.df$DEST)
prop.table(table(train.df$Flight.Status, train.df$DEST), margin=1)
prop.table(table(train.df$Flight.Status, train.df$DEST), margin=2)
##### Accuracy
library(caret)
pred.class <- predict(delay.nb, newdata=train.df)
confusionMatrix(pred.class, train.df$Flight.Status)

# 'Positive' Class : delayed      : OK
# Accuracy : 0.8045, Sensitivity : 0.10757, Specificity : 0.96819 -> ontime제대로, delay는 잘못함
pred.class <- predict(delay.nb, newdata=valid.df)
confusionMatrix(pred.class, valid.df$Flight.Status)
# Accuracy : 0.7946, Sensitivity : 0.10734, Specificity : 0.96733 -> train 보다 떨어짐 하지만 그렇게 많이 차이나는건 아님
# 민감도는 train보다 좋음 따라서 과적합 아님
# 민감도, 예측변수 더 좋은것 넣기
##############lift chart : naive rule이 훨씬 랜덤하게 했을때보다 잘 찾음, 향상도가 높아서 나이브베이즈 적당함
library(gains)
gain <- gains(ifelse(valid.df$Flight.Status=="delayed",1,0), pred.prob[ ,1], groups=100)
plot(c(0, gain$cume.pct.of.total*sum(valid.df$Flight.Status=="delayed")) ~c(0, gain$cume.obs), xlab="# cases", ylab="Cummulative", main="Lift chart",type="l")                     
lines(c(0, sum(valid.df$Flight.Status=="delayed"))~c(0, dim(valid.df)[1]), lty=2)
