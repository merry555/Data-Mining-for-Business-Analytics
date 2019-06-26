getwd()
setwd("/Users/kimjisu/Desktop/Data_mining/")
car.df <- read.csv("ToyotaCorolla.csv")
car.df <- car.df[1:1000, ]
car.df
#select variables for regression
selected.var <- c(3,4,7,8,9,10,12,13,14,17,18)
#partition data
set.seed(1)
train.index <- sample(c(1:1000), 600) # 60% training
train.df <- car.df[train.index, selected.var] 
valid.df <- car.df[-train.index, selected.var]
str(train.df)
#use lm() to run a linear regression of Price on all 10 predictors in 
#training set.
#use . after ~ to include all the remaining columns in train.df as predictors.
car.lm <- lm(Price~., data=train.df) # 선형회귀모형 lm
class(car.lm)
str(car.lm)
options(scipen=999)
summary(car.lm) # residuals y-y^ / intercept : 베타
# P(잘못결정할 확률) value : 계산해 놓은 값 < 알파:유의수준 (0.05) 잘못 결정할 확률이 0.05의 오차 허용
# 0.5이상이면 의미없음
# * : 오차허용범위
# price = -1774 / -135.43*Age_08_04 - 0.019*KM + 1208.33*Fuel_TypeDiesel ... + 12.667*Weight : 찾은 회기모형
# Age_08_04 가 증가하면 price -135 감소, 디젤인 경우는 1208유로 만큼 CNG연료 쓰는거에 비해 증가. 
# 휘발류는 CNG에 비해 2425유로 만큼 높음.
# 무게가 더 나가면 12.66유로만큼 가격이 올라간다.
#############some residuals#############
library(forecast)
#use predict() to make predictions on a new set
car.lm.pred <- predict(car.lm, valid.df)
options(scipen=999, digits=0)
some.residuals <-valid.df$Price[1:20] - car.lm.pred[1:20]
data.frame("Predicted"=car.lm.pred[1:20], "Actual"=valid.df$Price[1:20],
           "Residual"=some.residuals)
#############all residuals##############
all.residuals <- valid.df$Price - car.lm.pred
options(scipen=999, digits=5) # 소수점 이해 5자리 digits
length(all.residuals[which(all.residuals > -1406 & all.residuals < 1406)])/400
hist(all.residuals, breaks=25, xlab="Residuals")
accuracy(car.lm.pred, valid.df$Price) # 예측 잘 한 모델
accuracy(car.lm$fitted.values, train.df$Price)
