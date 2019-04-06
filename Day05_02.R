getwd()
setwd("/Users/kimjisu/Desktop/Data_mining/Day02")
library(gains)
df <- read.csv("liftExample.csv", header = TRUE)
str(df)
dim(df)
gain <- gains(df$actual, df$prob, groups = dim(df)[1])
gain
# total의 누적 값
plot(c(0,gain$cume.pct.of.total*sum(df$actual))~c(0,gain$cume.obs), xlab = "#cased", ylab = "Cumm", main="Lift Chart",type="l")
lines(c(0, sum(df$actual)) ~ c(0,dim(df)[1]), col = "red", lty=2)
# 십분위 분류
gain <- gains(df$actual, df$prob)
barplot(gain$mean.resp/mean(df$actual), names.arg = gain$cume.obs, xlab = "Percentile", ylab = "Mean Response", main = "Decline-wise lift chart")
