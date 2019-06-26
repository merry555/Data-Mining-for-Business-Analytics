getwd()
setwd("/Users/kimjisu/Desktop/Data_mining/")
install.packages("recommenderlab")
library(recommenderlab)
# simulate matrix with 1000 users and 100 movies
m <- matrix(nrow = 1000, ncol = 100)
dim(m)
# simulated ratings (1% of the data)
m[sample.int(100*1000, 1000)] <- ceiling(runif(1000, 0, 5)) 
m[2,]#runif: 1000 uniform random numbers between 0 to 5
#m[31004] m[4,32] have same value.
## convert into a realRatingMatrix
r <- as(m, "realRatingMatrix") #binaryRatingMatrix

# user-based collaborative filtering
UB.Rec <- Recommender(r, "UBCF")#method, parameter=Cosine
pred <- predict(UB.Rec, r, type="ratings")
mm<-as(pred, "matrix") 
ll<-as(pred, "list")
# item-based collaborative filtering
m[2,]
mm[2,]
ll[2]
IB.Rec <- Recommender(r, "IBCF")
ipred <- predict(IB.Rec, r, type="ratings")
iipred <- predict(IB.Rec, r, n=5) #추천아이템 번호 5개 출력
imm<-as(ipred, "matrix") 
ill<-as(ipred, "list")
m[2,]
imm[2,]
ill[2]
iimm<-as(iipred, "matrix") 
iill<-as(iipred, "list")
m[2,]
iimm[2,]
iill[2]
