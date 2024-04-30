setwd("C:/Codes/SY19/data")
data=read.table("TP5_a23_reg_app.txt")
head(data)
reg.cor=cor(data)
#corrplot(reg.cor)
#rm(list = ls())

dev.off()

# Traitement des données
n=nrow(data)
train_indices=sample(1:n,round(4*n/5))
data_train=data[train_indices,]
data_test=data[-train_indices,]
# cross validation
K <- 10
fold <- sample(K, n, replace = TRUE)
MSE=rep(0, K)

# KNN Regression
library('FNN')
x.app=scale(data_train[,1:100])
y.app=data_train[,101]
x.tst=scale(data_test[,1:100])
y.tst=data_test[,101]
MSE=rep(0,15)
for(k in 1:15){
  reg=knn.reg(train=x.app,test=x.tst,y=y.app,k=k)
  MSE[k]=mean((y.tst-reg$pred)^2)
}
plot(1:15,MSE)
km=which.min(MSE)
print(km)
reg=knn.reg(train=x.app,test=x.tst,y=y.app,k=km)
knn.mse=mean((y.tst-reg$pred)^2)
print(knn.mse) #2934
plot(y.tst,reg$pred,xlab='y',ylab='prediction')
abline(0,1)
# knn cross validation
for (k in 1:K) {
  cat(sprintf("KNN Processing fold %i\n", k))
  MSE_in=rep(0,15)
  for(j in 1:15){
    reg=knn.reg(train=data[fold!=k,1:100],test=data[fold==k,1:100],y=data[fold!=k,101],k=j)
    MSE_in[j]=mean((data[fold==k,101]-reg$pred)^2)
  }
  km=which.min(MSE_in)
  reg=knn.reg(train=data[fold!=k,1:100],test=data[fold==k,1:100],y=data[fold!=k,101],k=km)
  MSE[k] <- mean((data[fold==k,101]-reg$pred)^2)
}
print("Results :")
print(mean(MSE)) #2832


# Linear Regression
mode.reg=lm(y~.,data=data_train)
lm.pred.test=predict(mode.reg,newdata=data_test)
lm.mse=mean((y.tst-lm.pred.test)^2)
print(lm.mse) #222 #175
plot(y.tst,lm.pred.test,xlab='y',ylab='prediction')
abline(0,1)
# Linear Regression cross validation
for (k in 1:K) {
  cat(sprintf("Linear Regression Processing fold %i\n", k))
  fit <- lm(y ~ ., data = data, subset = fold != k)
  pred <- predict(fit, newdata = data[fold == k, ])
  MSE[k] <- mean((data[fold==k,101]-pred)^2)
}
print("Results :")
print(mean(MSE)) #210 #220

# Best subset selection
#library(leaps)
#bss.fit=regsubsets(y~.,data=data_train,method='exhaustive',really.big = T)
#plot(reg.fit,scale="r2")
# Forward stepwise selection
#reg.fit=regsubsets(y~.,data=data_train,method='forward',nvmax=100)
#plot(reg.fit,scale="r2")
# Backward stepwise selection
#reg.fit=regsubsets(y~.,data=data_train,method='backward',nvmax=100)
#plot(reg.fit,scale="r2")

# Step AIC 
library(MASS)
fit=lm(y~.,data=data_train)
data.aic=stepAIC(fit,scope=y~.,direction="both")#print(formula(data.aic))
aic.pred.test=predict(data.aic,newdata = data_test)
aic.mse=mean((y.tst-aic.pred.test)^2)
print(aic.mse) #223 #150
# AIC cross validation
for (k in 1:K) {
  cat(sprintf("AIC Processing fold %i\n", k))
  fit <- lm(y ~ ., data = data, subset = fold != k)
  aic.fit=stepAIC(fit,scope=y~.,direction="both")
  pred <- predict(aic.fit, newdata = data[fold == k, ])
  MSE[k] <- mean((data[fold==k,101]-pred)^2)
}
print("Results :")
print(mean(MSE))#203#217


# Step BIC
library(MASS)
fit=lm(y~.,data=data_train)
data.bic=stepAIC(fit,scope=y~.,direction="both",k=log(n))#print(formula(data.bic))
bic.pred.test=predict(data.bic,newdata = data_test)
bic.mse=mean((y.tst-bic.pred.test)^2)
print(bic.mse) #214#141
# BIC cross validation
for (k in 1:K) {
  cat(sprintf("BIC Processing fold %i\n", k))
  fit <- lm(y ~ ., data = data, subset = fold != k)
  bic.fit=stepAIC(fit,scope=y~.,direction="both",k = log(sum(fold != k)))
  pred <- predict(bic.fit, newdata = data[fold == k, ])
  MSE[k] <- mean((data[fold==k,101]-pred)^2)
}
print("Results :")
print(mean(MSE)) #186 #199

# Ridge regression
library(glmnet)
cv.out=cv.glmnet(x.app,y.app,alpha=0)
plot(cv.out)
ridge.fit=glmnet(x.app,y.app,lambda=cv.out$lambda.min,alpha=0)
ridge.pred=predict(ridge.fit,s=cv.out$lambda.min,newx = x.tst)
ridge.mse=mean((y.tst-ridge.pred)^2)
print(ridge.mse) #230#213
# Ridge cross validation
for (k in 1:K){
  datak=data[fold!=k,]
  datax=model.matrix(y~.,datak)
  datay=datak$y
  newdatak=data[fold==k,]
  newdatax=model.matrix(y~.,newdatak)
  #cv.out=cv.glmnet(data[fold!=k,1:100],data[fold!=k,101],alpha=0)
  cv.out=cv.glmnet(datax,datay,alpha=0)
  #ridge.fit=glmnet(data[fold!=k,1:100],data[fold!=k,101],lambda=cv.out$lambda.min,alpha=0)
  ridge.fit=glmnet(datax,datay,lambda=cv.out$lambda.min,alpha=0)
  #ridge.pred=predict(ridge.fit,s=cv.out$lambda.min,newx = data[fold==k,1:100])
  ridge.pred=predict(ridge.fit,s=cv.out$lambda.min,newx = newdatax)
  MSE[k]=mean((data[fold==k,101]-ridge.pred)^2)
}
print("Results :")
print(mean(MSE)) #214#223

# Lasso
cv.out=cv.glmnet(x.app,y.app,alpha=1)
plot(cv.out)
lasso.fit=glmnet(x.app,y.app,lambda=cv.out$lambda.min,alpha=1)
lasso.pred=predict(lasso.fit,s=cv.out$lambda.min,newx = x.tst)
lasso.mse=mean((y.tst-lasso.pred)^2)
print(lasso.mse) #206#180
# Lasso cross validation
for (k in 1:K){
  datak=data[fold!=k,]
  datax=model.matrix(y~.,datak)
  datay=datak$y
  newdatak=data[fold==k,]
  newdatax=model.matrix(y~.,newdatak)
  cv.out=cv.glmnet(datax,datay,alpha=1)
  lasso.fit=glmnet(datax,datay,lambda=cv.out$lambda.min,alpha=1)
  lasso.pred=predict(lasso.fit,s=cv.out$lambda.min,newx = newdatax)
  MSE[k]=mean((data[fold==k,101]-lasso.pred)^2)
}
print("Results :")
print(mean(MSE)) #198#210


# Principal component regression
library(pls)
pcr.fit=pcr(y~.,data=data_train,scale=TRUE,validation="CV")
pcr.pred=predict(pcr.fit,newdata = data_test)
pcr.mse=mean((y.tst-pcr.pred)^2)
print(pcr.mse) #2317#1973

# Regression tree
library(rpart)
fit=rpart(y~.,data=data_train,method="anova")
tree.pred=predict(fit,newdata = data_test)
tree.mse=mean((y.tst-tree.pred)^2)
print(tree.mse) #4158#3833

# Prune tree
library(rpart)
fit=rpart(y~.,data=data_train,method="anova",control=rpart.control(xval=10,minbucket = 2,))
printcp(fit)
pruned_tree=prune(fit,cp=0.018679)
yhat=predict(pruned_tree,newdata = data_test)
mse=mean((y.tst-yhat)^2)
print(mse) #4225#3182

# Random Forest
library(randomForest)
fit=randomForest(y~.,data=data_train,importance=TRUE)
f.pred=predict(fit,data_test)
mse=mean((y.tst-f.pred)^2)
print(mse) #2699#2338




