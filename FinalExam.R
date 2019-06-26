################ 6장 다중선형 회귀분석 ################
##다중 선형 회귀분석은  정량적인  종속변수(목표변수, 출력변수 또는 반응변수) Y와  
##예측변수(설명변수, 독립변수, 입력변수, 회귀변수, 공변량 ) 인  X1, X2,…, Xp 사이의  
##선형관계를  적합하기 위해 사용된다.
#######################################################
##### 가격 선형 회귀 모델
# 도요타 코롤라 중고차 가격
# 연료유형(Fuel Type)은 범주형, 반드시 이진변수로 전환되어야 함
# -> lm을 이용하면 문제를 자동으로 처리
# use first 1000 rows of data
car.df <- read.csv("ToyotaCorolla.csv")
car.df <- car.df[1:1000, ]
#select variables for regression
selected.var <- c(3,4,7,8,9,10,12,13,14,17,18)
#partition data
set.seed(1)
train.index <- sample(c(1:1000), 600) # 60% 비율의 train data
train.df <- car.df[train.index, selected.var]
valid.df <- car.df[-train.index, selected.var]
str(train.df)
#use lm() to run a linear regression of Price on all 10 predictors in 
#training set.
#use . after ~ to include all the remaining columns in train.df as predictors.
car.lm <- lm(Price~., data=train.df)
options(scipen=999)
summary(car.lm)
## 예측변수인 연료유형은 모델에서는 두 개의 가변수[휘발유,경유]를 사용
## 위 변수들의 계수들은 예측에 사용됨
# residuals y-y^ / intercept : 베타
# P(잘못결정할 확률) - value, Pr : 계산해 놓은 값 < 알파:유의수준 (0.05) 잘못 결정할 확률이 0.05의 오차 허용
# 0.5이상이면 의미없음
# Adjusted R-squared : 0.854 => 85%의 설명력
# Multiple R-squared : 0.856 => 85%의 설명력
# * : 오차허용범위
# price = -1774 + -135.43*Age_08_04 - 0.019*KM + 1208.33*Fuel_TypePetrol ... + 12.667*Weight : 찾은 회기모형
##### 예측값 및 검증 세트에 대한 성능 요약
library(forecast)
#use predict() to make predictions on a new set
car.lm.pred <- predict(car.lm, valid.df)
options(scipen=999, digits=0)
# 20대의 자동차 판매가격에 대한 모델의 예측값
some.residuals <-valid.df$Price[1:20] - car.lm.pred[1:20]
data.frame("Predicted"=car.lm.pred[1:20], "Actual"=valid.df$Price[1:20],
           "Residual"=some.residuals)
# accuracy measures.
accuracy(car.lm.pred, valid.df$Price)
# RMSE : 평균제곱근 편차, ME : 평균 오차
###### 검증 오차의 히스토그램 (크면 오차가 많음)
all.residuals <- valid.df$Price - car.lm.pred
length(all.residuals[which(all.residuals > -1406 & all.residuals < 1406)])/400
hist(all.residuals, breaks=25, xlab="Residuals")
#######################################################
###### 예측변수(다중공선성 : 2개 이상의 예측변수가 출력변수와 동일한 선형관계 문제 발생) 축소 방법 : 전역탐색, 부분집합 선택 알고리즘
## 비용, 더 정확한 측정, 결측값 존재, 간결성이 좋은 모델
###### 전역탐색 방법 : 예측변수들의 모든 가능한 부분집합들을 평가, p가 작으면서 Mallow’s _𝐶_𝑝_≈_𝑝+1_ 일 때 모델이 좋다
###### R2 는 모델이 설명할 수 있는 변동성의 비율로 높을 수록 좋은 모델이며 “1”이 최대치
###### 예측 변수 감소를 위한 전역탐색 방법
library(leaps)
Fuel_Type <- as.data.frame(model.matrix(~ 0 + Fuel_Type, 	data=train.df))
train.df <- cbind(train.df[,-4], Fuel_Type[,])
head(train.df)
search <- regsubsets(Price ~ ., data = train.df, nbest = 1, nvmax = dim(train.df)[2], method = "exhaustive")
sum <- summary(search)
sum$which
sum$rsq
sum$adjr2
sum$cp
###### 부분집합 선택 알고리즘 방법
# 장점 : 예측변수 수가 많을 때 사용 / 예측변수 없이 시작하여 예측변수를 하나씩 추가(모델의 R2 를 가장 크게 증가시키는데 기여하는 변수 추가)
# 추가된 예측변수들의 기여도가 통계적으로 유의하지 않을 때 중단
# 단점 : 둘 이상의 예측변수들이 함께 사용될 때는 효과적이나 단일 변수로 사용될 때 낮은 성능을 보이는 예측변수들을 누락
### 전방 선택 방법 : 예측변수가 없는 상태에서 예측변수를 하나씩 추가
# create model with no predictors for bottom of search range
car.lm.null <- lm(Price~1, data = train.df)
# use step() to run forward selection
car.lm.step <- step(car.lm.null,   
                    scope=list(lower=car.lm.null, upper=car.lm), direction =  
                      "forward")
summary(car.lm.step) 
### 후방 소거 방법 : 처음에는 모든 예측변수를 사용 -> 단계별로 가장 유용하지 않은 예측변수들 제거(기여도 유의하면 중단)
# 초기에 시간많이 소요
car.lm.step <- step(car.lm, direction = "backward")
summary(car.lm.step) 
### 단계적 선택방법
car.lm.step <- step(car.lm, direction = "both")
summary(car.lm.step) 
#######################################################
###### 과제 Review
# data 학습세트와 검증세트로 나누는 이유 : 학습세트는 모델을 추정하는데 사용, 검증 세트는 새로운, 즉 이전에 관측되지 않았던 데이터에 대한 모델의 예측 성능을 평가
# 어떠한 예측 변수들이 동일한 것을 측정할 것 같은지 "INDUS, NOX, TAX" 간의 관계
bostonHousing.df <-read.csv("BostonHousing.csv")
bosstr(bostonHousing.df)
tonHousing.df <- bostonHousing.df[,c(-14)]
cor(bostonHousing.df[,c("INDUS","NOX","TAX")]) # 유사성이 가장 높은 것 상관계수 높은 값 찾기, 상관계수 높으면 한 변수로 대체 가능
cor(car.df)
# 12개의 수치형 예측변수에 대한 상관관계 표 계산, 어떤 변수 제거 --> 높은 상관계수 갖는 값, 상관계수 높으면 한 변수로 대체 가능
library(leaps)
set.seed(1)
train.index <- sample(c(1:dim(bostonHousing.df)[1]),0.6*dim(bostonHousing.df)[1])
valid.index <- setdiff(c(1:dim(bostonHousing.df)[1]), train.index)
train.df <- bostonHousing.df[train.index, ]
valid.df <- bostonHousing.df[valid.index, ]
search <- regsubsets(MEDV ~., data = bostonHousing.df, nbest = 1, nvmax = dim(train.df)[2], method = "exhaustive")
sum <- summary(search)
sum$which
sum$adjr2 # 감소하는 것은 좋은 모델이 아님, 감소하기 전 3개 모델
sum$cp
# 3개의 모델에 대한 비교, 위 결과 예측변수 8,9,10개 사용하는 것이 적절
##, 전체 12개 중에서 2,3,4개 빼기#train.df <- bostonHousing.df[train.index, -c(2,3,5,7)]
#train.df <- bostonHousing.df[train.index, -c(2,3,5)]
#train.df <- bostonHousing.df[train.index, -c(2,3)]

valid.df <- bostonHousing.df[valid.index, -c(2,3,5,7)]
#valid.df <- bostonHousing.df[valid.index, -c(2,3,5)]
#valid.df <- bostonHousing.df[valid.index, -c(2,3)]

boston.lm <- lm(MEDV ~., data = train.df)
summary(boston.lm)
library(forecast) # 예측하기 위한 라이브러리
boston.lm.pred <- predict(boston.lm, valid.df)
accuracy(boston.lm.pred, valid.df$MEDV) # 검증 세트에 대한 예측 정확도 비교
##################### Summary ###############################
# 선형 회귀분석 모델은 설명모델뿐만 아니라 예측모델로도 매우 유명한 도구다. 
# 좋은 예측모델은 높은 예측 정확도를 갖는다(실용적 수준에 유용)
# 예측모델은 학습 데이터 세트를 사용하고 별개의 검증 데이터 세트에서 평가하도록 만들어진다. 
# 여분의 예측변수를 제거하는 것은 예측 정확도와 강건함을 얻는 데 핵심적이다.
# 예측변수들의 부분집합을 선택하는 것은 “좋은” 후보 모델을 찾기 위함이다. 후보모델들은 반드시 실행 및 평가되어야 한다.
#####################################################################################################################################################################
################ 7장 K- NN ##########################################################################################################################################
#####################################################################################################################################################################
# 새로운 레코드를 분류하거나 예측할 때 사용
# 분류되어야 할 주어진 레코드에서 근접(유사) 레코드를 찾는다. ( k 개를 선정)
# 데이터에 근거한 추론
# 데이터에 대한 어떠한 가정도 없다
# 이웃 결정하기 --> 유클리드 거리 : 계산비용 작음
# k의 값은 검증 데이터에서 가장 낮은 오류율을 가진 것을 선택, k는 보통 1~20의 범위 내에서 홀수
##### 승차식 잔디깎이 기계 데이터에 로딩, 분할, 산점도 (기계 구입 할 가구, 구입하지 않을 가구 분류)
getwd()
setwd("/Users/kimjisu/Desktop/Data_mining/")
mower.df <- read.csv("RidingMowers.csv")
str(mower.df)
set.seed(111)
train.index <- sample(row.names(mower.df), 0.6*dim(mower.df)[1])
length(train.index)
valid.index <- setdiff(row.names(mower.df), train.index) # 리스트에 없는 항목 출력하기 x에 있고, y에 없는 것 고르기
train.df <- mower.df[train.index, ]
valid.df <- mower.df[valid.index, ]
##new data
new.df <- data.frame(Income=60, Lot_Size=20) # income 60, Lot_size 20인 새로운 데이터 프레임 생성
##scatter plot
plot(Lot_Size ~ Income, data=train.df, pch=ifelse(train.df$Ownership=="Owner", 1, 3))
text(train.df$Income, train.df$Lot_Size, rownames(train.df), pos=4)
text(60, 20, "X") # 그래프를 통해서 14,1,9가 X와 근접
legend("topright", c("owner", "nonowner", "newhousehold"), pch=c(1,3,4))
##### K - 최근접이웃 실행
# Initialzing normalized training, validation, complete data frame
train.norm.df <- train.df
valid.norm.df <- valid.df
mower.norm.df <- mower.df
#use preprocess() from caret package to normalize Income and Lot_Size
library(caret)
norm.values <- preProcess(train.df[ , 1:2], method=c("center", "scale"))  # train.df[,1:데이터개수]
mean(train.df[,1]);mean(train.df[ , 2]);sd(train.df[,1]);sd(train.df[ , 2])
train.norm.df[ , 1:2] <- predict(norm.values, train.df[ , 1:2])
valid.norm.df[ , 1:2] <- predict(norm.values, valid.df[ , 1:2])
mower.norm.df[ , 1:2] <- predict(norm.values, mower.df[ , 1:2])
new.norm.df <- predict(norm.values, new.df)
#train.df의 변수의 평균과 표준편차를 이용하여 new.norm.df 표준화 체크해 보기
(60-mean(train.df[,1]))/sd(train.df[,1])
(20-mean(train.df[,2]))/sd(train.df[,2]) # 20 second value 
#use knn() to compute knn. knn is available in the library FNN (provide  a list of #the nearest neighbors) and a library class. 
library(FNN)
nn <- knn(train=train.norm.df[ , 1:2], test=new.norm.df, cl=train.norm.df[ , 3], k=3, prob=TRUE)
# training data row name -> nn
row.names(train.df)[attr(nn, "nn.index")] # 최근접 K 출력
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
### 최적의 k=4를 사용한 새로운 가구에 대한 분류
knn.pred.new <- knn(train=mower.norm.df[ , 1:2], test=new.norm.df, 
                    cl=mower.norm.df[ , 3], k=4, prob=TRUE)
knn.pred.new
#row.names(train.df)[attr(nn, "nn.index")]: 교과서가  틀림
row.names(mower.df)[attr(knn.pred.new, "nn.index")]
######################## Summary ###############################
# 장점 : 간단 / 분포 가정이 필요하지 않음 / 충분한 학습 데이터가 있을 때 좋은 성능을 보임 / 통계적 모델을 정의하지 않고 변수들 사이에서 복잡한 상호작용을 수집하는 데 효과적
# 단점 : 예측변수의 수 p가 증가함에 따라 학습 세트로 필요한 레코드의 수가 기하급수적으로 증가 / 대용량 학습세트에서, 모든 이웃까지의 거리를 찾는 데 시간이 오래 걸리고 따라서 가장 가까운 이웃을 찾는 데도 시간이 오래 걸린다.
# 예측변수의 수가 많아지면 “차원의 저주”의 영향을 받는다.
# 극복 : 예측변수의 차원 축수(e.g., 주성분 분석으로) / 검색 Tree 등을 사용하여 근접 이웃을 정확히 찾기 보다는 “거의 가장 근접한 이웃”을 찾는다. / 중복되었거나 거의 중복된 학습레코드를 제거
#######################################################
###### 과제 Review
# 개인 대출을 받아들이기 위한 데이터분석
# 1. 데이터 전처리 과정
bank.df <- read.csv("UniversalBank.csv")
str(bank.df)
bank.df$Education <- factor(bank.df$Education) # Education 범주형 변수 -> 가변수 만들어주기
edu.df <- as.data.frame(model.matrix(~0 + Education, data=bank.df)) # education 변수 정의
bank.df <- cbind(bank.df[, -c(1,5,8)], edu.df[,]) # ID, ZIP, Personal Loan(목표변수) 변수를 제외하고 edu.df(새롭게 만든 가변수 포함)와 결합
str(bank.df)
set.seed(111)
train.index <- sample(row.names(bank.df), 0.6*dim(bank.df)[1]) # 60%
valid.index <- setdiff(row.names(bank.df), train.index)
train.df <- bank.df[train.index,]
valid.df <- bank.df[valid.index,]
# new data
new.df <- data.frame(Age = 40, Experience = 10, Income = 84, Family = 2, CCAvg = 2, Mortgage = 0, Securities.Account = 0,
                    CD.Account = 0, Online = 1, CreditCard = 1,
                    Education1 = 0, Education2 = 1, Education3 = 0)
library(caret)
norm.values <- preProcess(train.df[,-7], method = c("center","scale")) # 데이터 정규화 (단위 일치 시켜줌) / -7 : Personal.Loan(목표변수) 제외하여 표준화
train.norm.df <- train.df
valid.norm.df <- valid.df
bank.norm.df <- bank.df
train.norm.df[,-7] <- predict(norm.values, train.df[,-7])
valid.norm.df[,-7] <- predict(norm.values, valid.df[,-7])
bank.norm.df[,-7] <- predict(norm.values, bank.df[,-7])
new.norm.df <- predict(norm.values, new.df) # 목표변수값이 아예 없음 

library(FNN)
nn <- knn(train = train.norm.df[,-7], test = new.norm.df, cl = train.norm.df[,7], k=1, prob = TRUE) # test : 예측하기 위한 값, new data의 class값이 출력
nn # 결과 해석 : 하나는 Level 0 라는 클래스에 있었고, 하나가 속할 확률은 1이 나옴, 거리는 new data 와 list가 2899(train data index)에 해당하는 값 0.4796033
row.names(train.df)[attr(nn,"nn.index")] # 행번호 출력
# 실제(bank.df)는 list가 4035
# best k 찾기
accuracy.df <- data.frame(k=seq(1,20,1), accuracy = rep(0,20))
for (i in 1:20) { # 1~20까지
  knn.pred <- knn(train.norm.df[,-7], valid.norm.df[,-7], cl = train.norm.df[,7], k=i) # KNN 찾기
  accuracy.df[i,2] <- confusionMatrix(knn.pred, as.factor(valid.norm.df[,7]))$overall[1] # 예측값, 실제값 주고 (confusionMartix data value must be a factor), overall 첫번째 값이 accuracy
}
accuracy.df # 증가되다가 감소하는 값 선정 k=3일 때가 적절 (train.df, valid.df)어떻게 들어가느냐에 따라 다름 (set.seed에 따라 달라짐)
## 따라서 k=3일때 분류
knn.pred.new <- knn(bank.norm.df[,-7], new.norm.df, cl = bank.norm.df[,7], k=3, prob = TRUE)
row.names(bank.df)[attr(knn.pred.new,"nn.index")] # 행번호 출력
knn.pred.new # 3개중 3개가 Personal loan값 0이된다. (출력값, 0, 확률 1) => 대출받지 않음
## train, valid, test 50%, 30%, 20%로 분할 , test data 20%는 K를 결정할 때 고려되지 않은 데이터, 
train.index <- sample(row.names(bank.df), 0.5*dim(bank.df)[1])
valid.index <- sample(row.names(bank.df), 0.3*dim(bank.df)[1])
test.index <-sample(row.names(bank.df), 0.2*dim(bank.df)[1])

train.df <- bank.df[train.index,]
valid.df <- bank.df[valid.index,]
test.df <- bank.df[test.index,]

library(caret)
norm.values <- preProcess(train.df[,-7], method = c("center","scale")) # 데이터 정규화 (단위 일치 시켜줌) / -7 : Personal.Loan(목표변수) 제외하여 표준화
train.norm.df <- train.df
valid.norm.df <- valid.df
test.norm.df <- test.df
bank.norm.df <- bank.df

train.norm.df[,-7] <- predict(norm.values, train.df[,-7])
valid.norm.df[,-7] <- predict(norm.values, valid.df[,-7])
test.norm.df[,-7] <- predict(norm.values, test.df[,-7])
bank.norm.df[,-7] <- predict(norm.values, bank.df[,-7])
new.norm.df <- predict(norm.values, new.df) # 목표변수값이 아예 없음 

knn.pred.new <- knn(train = train.norm.df[,-7], test = new.norm.df, cl=train.norm.df[,7], k=3, prob = TRUE)
knn.pred.new
## 검증데이터 성능 평가
knn.pred <- knn(train.norm.df[,-7], test.norm.df[,-7], cl = train.norm.df[,7], k=3)
c <- confusionMatrix(knn.pred, as.factor(test.norm.df[,7]))
c # 95% , 두 결과는 매우 유사함 따라서 분류기가 크게 영향 받지 않음
accuracy.df # k=3
#####################################################################################################################################################################
################ 8장 Naive Base ##################################################################################################################################### 
#####################################################################################################################################################################
# 범주형 예측변수로 구성된 데이터에 적용하는 분류기
# 분류되어야 할 주어진 새로운 레코드와 유사한 레코드 발견 (i.e., 예측변수의 값이 동일한 다른 레코드들을 찾는다)
# 이러한 레코드들의 소속 클래스를 조사하여 어느 클래스에 많이 속하였는지를 조사
# 컷오프 확률 방법
# 1. 해당 클래스에 속한다고 간주되는 관심 있는 클래스에 대한 기준값(cut-off) 확률을 정한다
# 2. 새로운 레코드와 유사한(예측변수 값이 동일한) 모든 학습 레코드를 찾는다
# 3. 그 레코드들이 관심 있는 클래스 소속 확률을 구한다.
# 4. 그 확률이 기준값 확률보다 크면 새로운 레코드를 관심 있는 클래스로 배정
# 완전한 베이즈 절차의 실용적 어려움 : 나이브 베이즈 해결책에선 더 이상 분류될 레코드와 정확히 일치하는 레코드의 확률 계산에 얽매이지 않는다. 대신에 전체 데이터 세트를 사용한다.
##### 두 개의 예측변수를 사용한 사기 재무보고의 예측
# 완전한 베이즈 계산 : 사기 일때의 조건부 확률
# --> P(사기 | 이전 법적문제 = y, 크기 = 작음) = 1/2 = 0.5 ...
# 나이브 베이즈 계산 : 사기 일때의 조건부 확률
# --> P(사기 | 이전 법적문제 = y, 크기 = 작음)일 때, 분모 : {사기 회사들 중, 이전 법적문제 = y} * {사기 회사들 중, 크기 = 작음} * {사기 회사들의 비율 }
# 분자 : {사기 회사들 중, 이전 법적문제 = y} * {사기 회사들 중, 크기 = 작음} * {사기 회사들의 비율 } + {정직 회사들 중, 이전 법적문제 = y} * {정작 회사들 중, 크기 = 작음} * {정직 회사들의 비율 }
##### 지연된 운항의 예측
# 항공지연 학습 데이터에 적용된 나이브 베이즈 분류기 (지연이 될지 안될지 예측)
library(e1071) # 나이브 베이즈 분류기 사용
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
#run naive bayes
delay.nb <-naiveBayes(Flight.Status ~ . , data=train.df)
delay.nb # 첫번째 출력부는 학습 세트에서 지연된 운항과 정시 운항의 빙ㄹ, 뒤에 각 클래스의 조건부 확률을 예측변수 값들의 함수로 보여줌
prop.table(table(train.df$Flight.Status, train.df$DEST), margin=1) # 목적지 공항에 따른 운항상태 피벗 테이블
# 운항의 평가점수 : 관심있는 데이터 평가
pred.prob <- predict(delay.nb, newdata=valid.df, type = "raw")
pred.class <- predict(delay.nb, newdata=valid.df)
df <- data.frame(actual=valid.df$Flight.Status, predicted=pred.class, pred.prob)
df[valid.df$CARRIER=="DL"& valid.df$DAY_WEEK==7 & valid.df$CRS_DEP_TIME==10 & valid.df$DEST=="LGA" & valid.df$ORIGIN == "DCA",  ]
## 나이브베이즈 분류기의 성능평가
# 정오행렬
library(caret)
# training
pred.class <- predict(delay.nb, newdata = train.df)
confusionMatrix(pred.class, train.df$Flight.Status)
# validation
pred.class <- predict(delay.nb, newdata = valid.df)
confusionMatrix(pred.class, valid.df$Flight.Status)
##############lift chart
library(gains)
gain <- gains(ifelse(valid.df$Flight.Status=="delayed",1,0), pred.prob[ ,1], groups=100)
plot(c(0, gain$cume.pct.of.total*sum(valid.df$Flight.Status=="delayed")) ~c(0, gain$cume.obs), xlab="# cases", ylab="Cummulative", main="Lift chart",type="l")                     
lines(c(0, sum(valid.df$Flight.Status=="delayed"))~c(0, dim(valid.df)[1]), lty=2)
# 나이브베이즈 추정 확률이 정확한 베이즈와 많이 근사함.
# 향상차트를 검토해 보면 목표가 랭킹일 때 지연된 운항을 효과적으로 보여줌
######################## Summary ###############################
# 장점 : 순전한 범주형 데이터에 적합하다,분류성과가 좋다 / 매우 대용량 데이터 세트에서 잘 작동한다. / 간단하고 계산이 효과적이다.
# 단점 : 다량의 레코드를 요구한다. / 예측변수의 범주가 학습용 데이터에서 존재하지 않는 경우 단순 베이즈는 이러한 예측변수의 범주를 갖는 새로운 레코드는 0의 확률값을 갖는다고 가정한다.
#######################################################
###### 과제 Review
# 데이터세트 정보를 사용하여 사고가 막 보도되는데, 추가정보가 없다면, 예측은 무엇이 되어야 하는지
library(e1071)
accidents.df <- read.csv("accidents.csv")
str(accidents.df)
accidents.df$INJURY <- ifelse(accidents.df$MAX_SEV_IR>0, "yes","no") # MAX_SEV_IR = 1,2 yes, 아니면 0인 가변수 INJURY생성
for (i in c(1:dim(accidents.df)[2])){
  accidents.df[,i] <- as.factor(accidents.df[,i]) # 모든 변수 범주형 변수로 만들어주기
}
prop.table(table(accidents.df$INJURY)) # 더 높은 값인 yes로 분류
# 데이터 세트의 처음 12개의 레코드를 선택하여 INJURY와 두개의 예측 변수만을 고려
# i. 12개의 레코드들에 대해서 두 예측변수들의 함수로서 INJURY를 검토하는 피벗 테이블 작성. 세 변수들 모두 행/열로 사용
str(accidents.df)
table(accidents.df[1:12, c("INJURY", "WEATHER_R", "TRAF_CON_R")]) # 3차원 테이블
# ii. INJURY = YES 정확한 나이브베이즈 확률 계산
head(accidents.df[, c("INJURY", "WEATHER_R", "TRAF_CON_R")], 12) # 결과 값 보고 확률 계산
# 12개의 레코드에 대해서 나이브베이즈 분류기를 돌려 결과에 대한 정오분류표
accidents.df <- head(accidents.df[, c("INJURY", "WEATHER_R", "TRAF_CON_R")], 12) # record 12개, dataframe은3개
accidents.nb <- naiveBayes(INJURY~., data = accidents.df)
accidents.nb
pred.class <- predict(accidents.nb, newdata = accidents.df)
confusionMatrix(pred.class, accidents.df$INJURY) # ACCURACY : 75%, Positive : no -> yes는 제대로 찾지 못함
#####################################################################################################################################################################
################ 9장 분류회기나무 ##################################################################################################################################### 
#####################################################################################################################################################################
# 분류 회기 나무 핵심 아이디어
# 재귀적 분할, 가지치기
##### 승차식 잔디깎이 기계
getwd()
setwd("/Users/kimjisu/Desktop/Data_mining/")
install.packages("rpart")
install.packages("rpart.plot") ## tree
library(rpart)
library(caret)
library(rpart.plot)
mower.df <- read.csv("RidingMowers.csv")
str(mower.df)
# 결정노드는 흰색 타원, 단말 노드는 회색
class.tree <- rpart(Ownership ~ ., data=mower.df, 
                    control = rpart.control(maxdepth=2), method="class")
prp(class.tree, type=1, under=TRUE,extra=1, split.font =1, varlen=-10)
class.tree <- rpart ( Ownership ~ ., data=mower.df, 
                      control = rpart.control(minsplit=2), method="class")  #Full Grown Tree
prp(class.tree, type=1, under=TRUE,extra=1, split.font =1, varlen=-10) ## 그리는건 똑같음
##### 개인대출 수락 (나무 구조는 선택된 샘플에 따라서 다소 불안정하고 크게 별할 수 있다.
# 완전 적합된 나무 모델은 대개 과적합을 초래한다.)
bank.df <- read.csv("UniversalBank.csv")
bank.df <- bank.df[ , -c(1,5)]
# partition
set.seed(1)
train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6)
train.df <- bank.df[train.index,]
valid.df <- bank.df[-train.index, ]
############### Default CT
default.ct <- rpart(Personal.Loan ~ ., data=train.df, method="class")
prp(default.ct, type=1, extra=1,  under=TRUE, split.font=1, varlen=-10) # 0 은 받아들여지지 않음
length(default.ct$frame$var[default.ct$frame$var=="<leaf>"])
default.ct$frame$var  # tree 순회 
prp(default.ct, type=1, extra=1,  under=TRUE, split.font=1, varlen=-10,
    box.col=ifelse(default.ct$frame$var=="<leaf>", 'gray', 'white'))  #leaf node gray
str(default.ct)
################# Full CT (완전 성장한 나무모델)
deeper.ct <- rpart(Personal.Loan ~ ., data=train.df, method="class", cp=0, minsplit=1)
length(deeper.ct$frame$var[deeper.ct$frame$var=="<leaf>"])
prp(deeper.ct, type=1, extra=1,  under=TRUE, split.font=1, varlen=-10,
    box.col=ifelse(deeper.ct$frame$var=="<leaf>", 'gray', 'white'))
################# Default 정확도 비교
# train.df 정오행렬표를 통한 정확도 비교
## integer -> factor로 형변환
default.ct.point.pred.train <- predict(default.ct, train.df, type="class")
confusionMatrix(default.ct.point.pred.train, factor(train.df$Personal.Loan))
# valid.df
default.ct.point.pred.valid <- predict(default.ct, valid.df, type="class")
confusionMatrix(default.ct.point.pred.valid, factor(valid.df$Personal.Loan))
################# deeper 정확도 비교
## train 은 accuracy 가 1이 나왔기 때문에 과적합됨.
## 과적합(잡음 기반으로 발생)이 된다 따라서 가지치기가 필요
deeper.ct.point.pred.train <- predict(deeper.ct, train.df, type="class")
confusionMatrix(deeper.ct.point.pred.train, factor(train.df$Personal.Loan))
# valid.df
deeper.ct.point.pred.valid <- predict(deeper.ct, valid.df, type="class")
confusionMatrix(deeper.ct.point.pred.valid, factor(valid.df$Personal.Loan))
################# 과적합 방지하기
# 복잡도 파라미터(CP) 값 및 해당 나무모델의 에러 표
cv.ct <- rpart(Personal.Loan ~ ., data=train.df, method="class", cp=0.00001, 
               minsplit=5, xval=5)
prp(cv.ct, type=1, extra=1,  under=TRUE, split.font=1, varlen=-10,
    box.col=ifelse(cv.ct$frame$var=="<leaf>", 'gray', 'white'))
length(cv.ct$frame$var[cv.ct$frame$var=="<leaf>"])
printcp(cv.ct) # 교차검정 에러의 복잡도 파라미터 값, 교차검정 에러(xerror)가 가장 작은 모델을 선택.
# 교차검정에러 가장 낮은 값을 토대로 다시 가지치기
pruned.ct <- prune(cv.ct, cp=cv.ct$cptable[which.min(cv.ct$cptable[ , "xerror"]), 
                                           "CP"] )
length(pruned.ct$frame$var[pruned.ct$frame$var=="<leaf>"])
prp(pruned.ct, type=1, extra=1, split.font=1, varlen=-10,
    box.col=ifelse(pruned.ct$frame$var=="<leaf>", 'gray', 'white'))
# 모델을 되도록이면 복잡하지 않게, 샘플링 오차 고려 --> 나무를 더 가지치기 하기 위해 교차검정 에러에 대한 추정 표준오차(xstd)사용
# 즉, xerror+xstd 범위 속하는 모델, 다시 가지치기
pruned.ct <- prune(cv.ct, cp=0.0068729 )  #minimum cp within xerror +- std(xerror)
length(pruned.ct$frame$var[pruned.ct$frame$var=="<leaf>"])
prp(pruned.ct, type=1, extra=1, split.font=1, varlen=-10,
    box.col=ifelse(pruned.ct$frame$var=="<leaf>", 'gray', 'white')) # 최적으로 가지치기 된 모델
# 따라서 최적으로 가지치기 된 모델에서 잎 노드해당 규칙 찾기
# IF(소득 >= 114) AND (교육수준 >=1.5), THEN 클래스 = 1
#######회귀나무 모델. 분류회귀 나무와 차이점
# 예측값은 직사각형에서 수치형 타깃 변수들의 평균으로 계산된다 (CT에서는 다수결투표). / 불순도는 잎 평균의 제곱편차의 합으로 측정된다. / 성능은 RMSE (근의 평균제곱 오류)로 측정된다.
# 장점 : 사용하고 이해하기에 용이 / 해석하고 구현하기에 쉬운 규칙 생성 / 변수 선택과 감소가 자동 / 통계적 모델에 대한 가정을 요구하지 않음 / 결측 데이터에 대한 처리 없이도 작동
# 단점 : 데이터의 구조가 수평적 또는 수직적 분할에 의해 잘 포착되지 않을 경우, 잘 작동하지 않음 / 한 번에 하나의 변수를 다루기 때문에 변수들 사이의 상호작용을 포착할 방법이 없음 / 교호작용과 같은 변수간의 관계를 체크할 수 없음 / 계산량이 많음
#################### 분류, 예측 알고리즘 조합하여 예측성능 향상 : random forest
install.packages("randomForest")
library(randomForest)
rf <- randomForest(as.factor(Personal.Loan)~., data=train.df, ntree=500, mtry=4, 
                   nodesize=5, importance=TRUE) # 복원추출로 샘플 생성
varImpPlot(rf, type=1)
rf.pred <- predict(rf, valid.df)
library(caret)
confusionMatrix(rf.pred, factor(valid.df$Personal.Loan))
######################### boosting
install.packages("adabag")
library(adabag)
train.df$Personal.Loan <- as.factor(train.df$Personal.Loan)
valid.df$Personal.Loan <- as.factor(valid.df$Personal.Loan)
boost <- boosting(Personal.Loan ~ ., data = train.df)
pred <- predict(boost, valid.df)
str(pred)
pred$class <- as.factor(pred$class)
confusionMatrix(pred$class, valid.df$Personal.Loan)
######################## Summary ###############################
# 분류 회귀나무는 새로운 레코드를 예측하거나 분류할 수 있는 쉽고 투명한 방법이다.
# 나무는 일련의 규칙들에 대한 그래프적 표현이다.
# 나무는 학습 데이터의 과적합을 피하기 위해 가지치기를 해야만 한다.
# 나무가 데이터 구조에 대한 어떠한 가정도 갖지 않기 때문에, 큰 데이터가 필요하다.
