rm(list = ls())
#Author:Chundru Rohith
#Date:12/07/2018
#Purpose: Individual Project
library(data.table)
library(ggplot2)
#####Reading the dataset
rawdata <- read.csv("ENB2012_data.csv",header = TRUE)
rawdata <- subset(rawdata,select = -c(X,X.1))
dim(rawdata)
summary(rawdata)
#####Removing NA Values
sum(is.na(rawdata))
Finaldata <- na.omit(rawdata)
summary(Finaldata)
str(Finaldata)
View(Finaldata)
#####plots of every variable
for(i in 1:length(Finaldata)){
  plot(Finaldata[,i],ylab = names(Finaldata)[i])
}
#####ggplots of every predictor with  responses Y1 and Y2
for(i in 1:(length(Finaldata)-2)){
  print(ggplot(data = Finaldata)+geom_point(mapping = aes(x =Finaldata[,i] , y = Finaldata$Y1)))
}
for(i in 1:(length(Finaldata)-2)){
  print(ggplot(data = Finaldata)+geom_point(mapping = aes(x =Finaldata[,i] , y = Finaldata$Y2)))
}
#####correlation matrix
library(Hmisc)
library(corrplot)
cormat <- cor(rawdata,use = "everything", method = c("pearson","kendall","spearman"))
corrplot(cormat, type = "lower", order = "hclust", 
         tl.col = "black", tl.srt = 45)
cormat1 <- cor(Finaldata,use = "everything", method = c("pearson","kendall","spearman"))
corrplot(cormat1, type = "lower", order = "hclust", 
         tl.col = "black", tl.srt = 45)
#####Multiple linear regression for both responses Y1 and Y2
y1.lm <- lm(Y1~.-Y2,data = Finaldata)
summary(y1.lm)#got NA for X4 predictor
confint(y1.lm)
y2.lm <- lm(Y2~.-Y1,data = Finaldata)
summary(y2.lm)#got NA for X4 predictor
confint(y2.lm)
#####Variable selection using forward and step selection for two responses Y1 and Y2
#backward selection
step(y1.lm, direction = "backward")#x4 got deleted
step(y2.lm,direction = "backward")#x4 and x8 got deleted
#forward selection
step(y1.lm,direction = "forward")#no variable got deleted
step(y2.lm, direction = "forward" )#no variable got deleted
#Removed  X4 predictor
Finaldata <- subset(Finaldata,select = -c(X4))
finaly1.glm <- glm(Y1~-Y2,data = Finaldata)
summary(finaly1.glm)
finaly2.glm <- glm(Y2~.-Y1,data = Finaldata)
summary(finaly2.glm)
#####K-Fold
library(boot)
cv.errory1.10 <-cv.glm(Finaldata,finaly1.glm,K=10)
cv.errory1.10$delta# 101.8922 101.8810 
cv.errory2.10 <-cv.glm(Finaldata,finaly2.glm,K=10)
cv.errory2.10$delta# 10.408 10.394
#####LOOCV
loocv.errory1 <- cv.glm(Finaldata,finaly1.glm,K=nrow(Finaldata))
loocv.errory1$delta#101.9448 101.9446
loocv.errory2 <- cv.glm(Finaldata,finaly2.glm,K=nrow(Finaldata))
loocv.errory2$delta#10.34028 10.34015
###########tree based models
#####Fitting regression trees
## Y1 as response variable##
library(tree)
library(rpart)
set.seed(1)
N <- nrow(Finaldata)
traindatafortree.y1 <- sample(1:N, N/2)
testdatafortree.y1 <- seq(1:N)[-traindatafortree.y1]
tree.Finaldata.y1 <- tree(Y1~X1+X2+X3+X5+X6+X7+X8, Finaldata, subset=traindatafortree.y1)
summary(tree.Finaldata.y1)
plot(tree.Finaldata.y1)
text(tree.Finaldata.y1,cex=.5)
cv.Finaldata.y1 <- cv.tree(tree.Finaldata.y1)
plot(cv.Finaldata.y1$size,cv.Finaldata.y1$dev, type='b')
prune.y1.tree <- prune.tree(tree.Finaldata.y1,best = 6)
plot(prune.y1.tree)
text(prune.y1.tree,cex=.5)
#MSE of regression trees
y1.hat.tree <- predict(tree.Finaldata.y1,Finaldata[testdatafortree.y1,])
findata.y1.test <- Finaldata[testdatafortree.y1,"Y1"]
plot(y1.hat.tree,findata.y1.test)
abline(0,1)
mean((y1.hat.tree-findata.y1.test)^2)#9.371754
#MSE of a linear regression
lm.y1 <-lm(Y1~X1+X2+X3+X5+X6+X7+X8,Finaldata, subset = traindatafortree.y1)
y1.hat.lm <-predict(lm.y1, Finaldata[testdatafortree.y1,])
Final.test.y1 <- Finaldata[testdatafortree.y1,"Y1"]
mean((y1.hat.lm - Final.test.y1)^2)#10.13064
#bagging
library(randomForest)
set.seed(1)
NC <- ncol(Finaldata)-2# since there are 2 response variables
bag.y1.finaldata <- randomForest(Y1~X1+X2+X3+X5+X6+X7+X8, data = Finaldata, subset = traindatafortree.y1, mtry=NC,importance=TRUE)
bag.y1.finaldata
#MSE of a bagging
y1.hat.bag <- predict(bag.y1.finaldata, Finaldata[testdatafortree.y1,])
mean((y1.hat.bag - Final.test.y1)^2)#around 0.57
#Random Forest
rf.y1.finaldata <- randomForest(Y1~X1+X2+X3+X5+X6+X7+X8, data = Finaldata, subset = traindatafortree.y1, mtry=4,importance=TRUE)
rf.y1.finaldata
#MSE of a random FOrest
y1.hat.rf <- predict(rf.y1.finaldata,Finaldata[testdatafortree.y1,])
mean((y1.hat.rf - Final.test.y1)^2)#around 0.81
#Boosting
library(gbm)
boost.finaldata.y1 <- gbm(Y1~X1+X2+X3+X5+X6+X7+X8, data=Finaldata[traindatafortree.y1,], distribution="gaussian", n.trees
                    =1000, interaction.depth=4)
summary(boost.finaldata.y1)
#MSE
y1.hat.boost <- predict(boost.finaldata.y1, newdata=Finaldata[testdatafortree.y1,], n.trees=1000)
mean((y1.hat.boost - Final.test.y1)^2)#around 0.35
##Y2 as a response variable##
traindatafortree.y2 <- sample(1:N, N/2)
testdatafortree.y2 <- seq(1:N)[-traindatafortree.y2]
tree.Finaldata.y2 <- tree(Y2~X1+X2+X3+X5+X6+X7+X8, Finaldata, subset=traindatafortree.y2)
summary(tree.Finaldata.y2)
plot(tree.Finaldata.y2)
text(tree.Finaldata.y2,cex=.5)
#checking whether tree pruning will improve performance or not
cv.Finaldata.y2 <- cv.tree(tree.Finaldata.y2)
plot(cv.Finaldata.y2$size,cv.Finaldata.y2$dev, type='b')
#Pruning the tree
prune.y2.tree <- prune.tree(tree.Finaldata.y2,best = 5)
plot(prune.y2.tree)
text(prune.y2.tree,cex=.5)
y2.hat.tree <- predict(tree.Finaldata.y2,Finaldata[testdatafortree.y2,])
findata.y2.test <- Finaldata[testdatafortree.y2,"Y2"]
plot(y2.hat.tree,findata.y2.test)
abline(0,1)
mean((y2.hat.tree-findata.y2.test)^2)#9.709781
#comparison to a linear model
lm.y2 <-lm(Y2~X1+X2+X3+X5+X6+X7+X8,Finaldata, subset = traindatafortree.y2)
y2.hat.lm <-predict(lm.y2, Finaldata[testdatafortree.y2,])
Final.test.y2 <- Finaldata[testdatafortree.y2,"Y2"]
mean((y2.hat.lm - Final.test.y2)^2)#11.26187
#bagging#
NC2 <- ncol(Finaldata)-2# since there are 2 response variables
bag.y2.finaldata <- randomForest(Y2~X1+X2+X3+X5+X6+X7+X8, data = Finaldata, subset = traindatafortree.y2, mtry=NC2,importance=TRUE)
bag.y2.finaldata
y2.hat.bag <- predict(bag.y2.finaldata, Finaldata[testdatafortree.y2,])
mean((y2.hat.bag- Final.test.y2)^2)#approximately 3.5
#Random Forest#
rf.y2.finaldata <- randomForest(Y2~X1+X2+X3+X5+X6+X7+X8, data = Finaldata, subset = traindatafortree.y2, mtry=4,importance=TRUE)
rf.y2.finaldata
#MSE of a random FOrest
y2.hat.rf <- predict(rf.y2.finaldata,Finaldata[testdatafortree.y2,])
mean((y2.hat.rf - Final.test.y2)^2)#approximately 3.4
#Boosting#
boost.finaldata.y2 <- gbm(Y2~X1+X2+X3+X5+X6+X7+X8, data=Finaldata[traindatafortree.y2,], distribution="gaussian", n.trees
                          =1000, interaction.depth=4)
summary(boost.finaldata.y2)
#MSE
y2.hat.boost <- predict(boost.finaldata.y2, newdata=Finaldata[testdatafortree.y2,], n.trees=1000)
mean((y2.hat.boost - Final.test.y2)^2)#around 1.96