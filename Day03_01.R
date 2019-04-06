setwd("/Users/kimjisu/Desktop/Data_mining/Day01")
housing.df <- read.csv("WestRoxbury.csv",header = TRUE)
str(housing.df)
dim(housing.df)
head(housing.df) # 위에서부터 6개의 데이터
View(housing.df) # data 전체 볼 수 있음
housing.df$TOTAL.VALUE[1:10] # 10개 출력
length(housing.df$TOTAL.VALUE) # 길이
mean(housing.df$TOTAL.VALUE) # 평균값
summary(housing.df) # 모든 변수에 대한 요약한 평균 값

#sampling
s<-sample(row.names(housing.df),5) # 표본으로 뽑힐 확률 다 같음
s
housing.df[s, ]

s<-sample(row.names(housing.df),5, prob = ifelse(housing.df$ROOMS>10, 0.9, 0.01)) # 확률 달라짐
s
housing.df[s, ]
?sample
### week3
names(housing.df)
t(t(names(housing.df)))
colnames(housing.df)[1] <-c("TOTAL_VALUE")
class(housing.df$REMODEL)
class(housing.df[,14])
levels(housing.df[,14])
class(housing.df$BEDROOMS)
class(housing.df[,1])

## dummy variable
xtotal <- model.matrix(~0 + BEDROOMS + REMODEL, data = housing.df)
xtotal<-as.data.frame(xtotal)
t(t(names(xtotal)))
head(xtotal)
xtotal <- xtotal[,-4] # 4번째 열 삭제
head(xtotal)
housing.df <- cbind(housing.df[, -c(9, 14)], xtotal)
t(t(names(housing.df)))

## missing
rows.to.missing <- sample(row.names(housing.df), 10)
rows.to.missing
housing.df[rows.to.missing, ]$BEDROOMS <- NA
summary(housing.df$BEDROOMS)
housing.df[rows.to.missing, ]$BEDROOMS <- median(housing.df$BEDROOMS, na.rm = TRUE)
summary(housing.df$BEDROOMS)

## Partitioning Data
set.seed(1)
dim(housing.df)
train.rows <-sample(rownames(housing.df), dim(housing.df)[1]*0.6)
head(train.rows)
train.data <- housing.df[train.rows, ]
str(train.data)
valid.rows <- setdiff(rownames(housing.df), train.rows)
head(valid.rows)
valid.data <- housing.df[valid.rows,]

## regression
reg <- lm(TOTAL_VALUE ~ .-TAX, data = housing.df, subset = train.rows)
str(housing.df)
str(reg)
reg
tr.res <- data.frame(train.data$TOTAL_VALUE, reg$fitted.values, reg$residuals)
head(tr.res)
pred <- predict(reg, newdata = valid.data)
vl.res <- data.frame(valid.data$TOTAL_VALUE, pred, residuals = valid.data$TOTAL_VALUE - pred)
head(vl.res) # residuals가 크면 과적합
library(forecast)
