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
