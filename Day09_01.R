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
########################################
##########exhaustive####################
## training 까지, lm 까지 한 후
library(leaps)
Fuel_Type <- as.data.frame(model.matrix(~ 0 + Fuel_Type, 	data=train.df))
train.df <- cbind(train.df[,-4], Fuel_Type[,])
head(Fuel_Type)
head(train.df)
search <- regsubsets(Price ~ ., data = train.df, nbest = 1, 
                     nvmax = dim(train.df)[2], method = "exhaustive")
sum <- summary(search)
sum$which
sum$rsq
sum$adjr2
sum$cp
########################search best model############
# lm을 위한 data model
# car.df ~ valid.df 까지 (line3 ~ line13)
#select variables for regression
#fuel type 2개 따라서 8개의 예측변수
selected.var <- c(3,4,7,8,9,12,17,18)
#partition data
set.seed(1)
train.index <- sample(c(1:1000), 600) # 60% training
train.df <- car.df[train.index, selected.var] 
valid.df <- car.df[-train.index, selected.var]
str(train.df)
car.lm.best_8 <- lm(Price~., data=train.df) # 선형회귀모형 lm
options(scipen=999)
summary(car.lm.best_8) # residuals y-y^ / intercept : 베타
## best model 구하라 : y^=-1623 -135*AGE_08_04 -0.019*KM ... 12.45*Weight
## 절편은 의미없음
## 의미 있다 : error 26% -> * 없음, 따라서 Automatic 빼는게 좋음
########### best automatic 뺀 것 ########
selected.var <- c(3,4,7,8,9,17,18)
#partition data
set.seed(1)
train.index <- sample(c(1:1000), 600) # 60% training
train.df <- car.df[train.index, selected.var] 
valid.df <- car.df[-train.index, selected.var]
str(train.df)
car.lm.best_8 <- lm(Price~., data=train.df) # 선형회귀모형 lm
options(scipen=999)
summary(car.lm.best_8)
######################################
#### 전방선택
### make training data 
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
# create model with no predictors for bottom of search range, 1 : 절편만 있는 것 예측변수 없음.
car.lm.null <- lm(Price~1, data = train.df)
# use step() to run forward selection
car.lm.step <- step(car.lm.null,   
                    scope=list(lower=car.lm.null, upper=car.lm), direction =  
                      "forward")
# + model 변수, SSR의 증가량 Sum of Sq
# Age_08_04 넣었을 때 설명할 수 있는 SSR의 양이 줄어듦.
# none : 선택 대상에서 제외
# Weight SSR 증가량 반영
# none 갯수 증가
# Fuel_Type : 전방 선택으로 dummy 2개 만든 후 최종 모델 생성
summary(car.lm.step) 
####### 후방제거
# 모든 data 다 insert
car.lm.step <- step(car.lm, direction = "backward")
summary(car.lm.step) 
####### backward -> forward
car.lm.step <- step(car.lm, direction = "both")
summary(car.lm.step) 
# 과제 :  관계 변수간의 상관계수를 구하는 것
# i
# 상관계수 높으면 빼서 계산. 관계가 높다 낮다 서술
# 높은 상관관계 갖는 변수의 쌍 :  예측 변수들만 뽑아서 data.frame 만들어서 data.frame내에 상관계수 구하는 함수
# 높은 상관관계 계수 쌍을 찾아서 어떤것을 뺄지 결정.
# ii
# 다중공선성 : 심각한 문제가 됨.
# 다중공선성 제거된 data 만들어서
# 그 data를 갖고 예측 변수들을 감소
# iii
# 상위 3개 모델을 
# 학습세트 lm구해서
# 검증 세트에 대해 예측 정확도
# validation -> RMSE, mean error, lift 도표.
# 비교해서 가장 좋은 모델 선택
# 근거 : exhaustive 이용한 search 모델