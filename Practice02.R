# 1. 목표변수는 TOTAL.VALUE 이며 선형회귀모형을 구할 때 TAX 변수를 사용하지 않는다.
# 2. 데이터를 분할할 때 set.seed(1)을 사용하시오. 

setwd("/Users/kimjisu/Desktop/Data_mining/Day01")
housing.df <- read.csv("WestRoxbury.csv",header = TRUE)
set.seed(1)
dim(housing.df)
t(t(names(housing.df)))
# training 50%
train.rows <-sample(rownames(housing.df), dim(housing.df)[1]*0.5)
# validation 30%
valid.rows <-sample(setdiff(rownames(housing.df), train.rows), dim(housing.df)[1]*0.3)
# test 20%
test.rows <-setdiff(rownames(housing.df), union(train.rows,valid.rows))

# train
train.data <- housing.df[train.rows,]
valid.data <- housing.df[valid.rows,]
test.data <-housing.df[test.rows,]
str(test.data)

reg <- lm(TOTAL.VALUE ~ .-TAX, data = housing.df, subset = train.rows)
reg

#train
tr.res <- data.frame(train.data$TOTAL.VALUE, reg$fitted.values, reg$residuals)
head(tr.res)

#valid
pred <- predict(reg, newdata = valid.data)
vl.res <- data.frame(valid.data$TOTAL.VALUE, pred, residuals = valid.data$TOTAL.VALUE - pred)
head(vl.res)

#test
tpred <- predict(reg, newdata = test.data)
te.res <-data.frame(test.data$TOTAL.VALUE, tpred, residuals = test.data$TOTAL.VALUE - tpred)
head(te.res)

library(forecast)
# train
accuracy(reg$fitted.values, train.data$TOTAL.VALUE)

# valud
accuracy(pred, valid.data$TOTAL.VALUE)

#test
accuracy(tpred, test.data$TOTAL.VALUE)

