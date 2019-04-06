getwd()
setwd("/Users/kimjisu/Desktop/Data_mining/Day01")
getwd()

x <- c(1,2,3)
x
print(x)
names(x) <- c("total","A","B")
x
y <- seq(1,10,0.5)
y
class(x)
length(y)
x <- list("홍길동", "1595013", 20, c("데이터마이닝", "확률통계"))
class(x)          
names(x) <- c("성명", "학번", "나이", "수강과목")
x
x$성명
x[[1]]
x[1]
x$학번
y<-matrix(1:6,nrow=2)
y
y <-matrix(1:6, nrow=2, byrow=TRUE)
y
v1 <- c(1,2,3); v2 <- c(3,4,5)
x<- cbind(v1, v2)
x
y<-rbind(v1,v2)
y
x
x[1,]
x[,2]
x[1,2]
x<-array(1:24, dim=c(2,3,4))
x
x <- c("M","F","M","F","M")
x
x.fct <- factor(x, levels =c("M","F"),labels=c("Male","Female"))
x.fct
table(x.fct)
x<-data.frame(foo=1:4,bar=c(T,T,F,F))
x
names(x)
row.names(x)
height <- c(164, 172, 177)
weight <- c(65, 72, 78)
gender <-c("female", "male", "male")
my.df <- data.frame(height, weight, gender, row.names=c("ahn", "moon", "choi"))
dim(my.df); names(my.df); length(my.df)
my.df
class(my.df)
my.df$weight
my.df$gender
is.factor(my.df$gender); is.numeric(my.df$gender)
my.df$grade<-c("A", "B", "C")
str(my.df)
my.df
dim(my.df); names(my.df); length(my.df)
read.csv(file = "/Users/kimjisu/Desktop/Data_mining/Day01/WestRoxbury.csv", header=TRUE)
survival<-"https://vincentarelbundock.github.io/Rdatasets/csv/datasets/Titanic.csv"
x<-read.csv(survival)
head(x)
tail(x)
quit()
