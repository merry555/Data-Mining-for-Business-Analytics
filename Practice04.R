getwd()
setwd("/Users/kimjisu/Desktop/Data_mining/")
df <-read.csv("4th.csv")
library(gains)
gain <- gains(df$actual, df$prob)
barplot(gain$mean.resp/mean(df$actual), names.arg = gain$depth, 
        xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")

install.packages('pROC')
library(pROC)
table(ifelse(df$prob>0.8,'Prediction1', 'Prediction0'),df$actual)

r <-roc(df$actual,df$prob)
plot.roc(r)
auc(r)
