getwd()
setwd("/Users/kimjisu/Desktop/Data_mining/Day02")
owner.df <- read.csv("ownerExample.csv", header = TRUE)
str(owner.df)
head(owner.df)
install.packages("caret")
install.packages("e1071")
library(e1071)
library(caret)
confusionMatrix(as.factor(ifelse(owner.df$Probability > 0.5, 'owner'
                       ,'nonowner')), owner.df$Class, positive = 'owner')
# sensitivity 높아지면, Accuracy 반비례
confusionMatrix(as.factor(ifelse(owner.df$Probability > 0.25, 'owner'
                                 ,'nonowner')), owner.df$Class,  positive = 'owner')
