setwd("E:/codes/r/tp12_3")


library(corrplot)
library(kernlab)
library(MASS)
library(glmnet)
#library(pls)

set.seed(12345)

construction=read.table("construction_train.txt")

construction
dotchart(construction$y)

construction.cor=cor(construction)
corrplot(construction.cor)


#prepa
n=nrow(construction)

train_indices=sample(1:n,round(4*n/5))
data.train=construction[train_indices,]
data.test=construction[-train_indices,]

#randomForest
#library(randomForest)
#fit=randomForest(y~.,data=data.train,importance=TRUE)
#f.pred=predict(fit,data.test)
#mse=mean((data.test$y-f.pred)^2)
#print(mse) #897.491


#svm
#svmfit<-ksvm(y~.,data=data_pca,scaled=TRUE,type="eps-svr",kernel="rbfdot",C=100,epsilon=0.1,kpar=list(sigma=0.1))
#svm_pred=predict(svmfit,newdata=data_test_pca)
#svm.mse=mean((svm_pred-data_test_pca$y)^2)
#svm.mse

#lm
reg.lm <- lm(y ~., data = data.train)
summary(reg.lm)
lm.pred=predict(reg.lm,newdata=data.test)
lm.mse=mean((lm.pred-data.test$y)^2)
lm.mse#1935.67


#pca
lm.pca.mse=numeric(31)
for (i in 1:31){
  pca=prcomp(data.train[,1:103],scale=TRUE)
  cumulative_variance <- cumsum(pca$sdev^2) / sum(pca$sdev^2)
  num_components <- which(cumulative_variance >= (i+69)/100)[1]
  data_pca <- as.data.frame(pca$x[, 1:num_components])
  data_pca$y=data.train$y
  data_test_pca <- predict(pca, newdata = data.test[, 1:103])
  data_test_pca <- as.data.frame(data_test_pca[, 1:num_components])
  data_test_pca$y <- data.test$y
  
  lm.pca=lm(y~.,data=data_pca)
  lm.pca.pred=predict(lm.pca,newdata=data_test_pca)
  lm.pca.mse[i]=mean((lm.pca.pred-data.test$y)^2)
}
x=seq(0.70,1.00,by=0.01)
plot(x,lm.pca.mse)
#
pca=prcomp(data.train[,1:103],scale=TRUE)
cumulative_variance <- cumsum(pca$sdev^2) / sum(pca$sdev^2)
num_components <- which(cumulative_variance >= 1)[1]
data_pca <- as.data.frame(pca$x[, 1:num_components])
data_pca$y=data.train$y
data_test_pca <- predict(pca, newdata = data.test[, 1:103])
data_test_pca <- as.data.frame(data_test_pca[, 1:num_components])
data_test_pca$y <- data.test$y
#data_pca_cor=cor(data_pca)
#data_pca_cor
#corrplot(data_pca_cor)






######pca处理完成


#aic bic
#lm.pca=lm(y~.,data=data_pca)
#data.aic=stepAIC(lm.pca,scope=y~.,direction="both",k=log(n))
#aic.pred.test=predict(data.aic,newdata = data_test_pca)
#aic.mse=mean((aic.pred.test-data_test_pca$y)^2)
#aic.mse #aic 1177.616 #bic 1084.565

#lm
#lm.pca=lm(y~.,data=data_pca)
#summary(lm.pca)
#lm.pca.pred=predict(lm.pca,newdata=data_test_pca)
#lm.pca.mse=mean((lm.pca.pred-data.test$y)^2)
#lm.pca.mse #946.0034


nn=num_components+1
x.train=as.matrix((data_pca[,-nn]))
y.train=as.matrix((data_pca[,nn]))
x.tst=as.matrix((data_test_pca[,-nn]))
y.tst=as.matrix((data_test_pca[,nn]))

#ridge
cv.out=cv.glmnet(x.train,y.train,alpha=0)
plot(cv.out)
ridge.fit=glmnet(x.train,y.train,lambda=cv.out$lambda.min,alpha=0)
ridge.pred=predict(ridge.fit,s=cv.out$lambda.min,newx = x.tst)
ridge.mse=mean((data_test_pca$y-ridge.pred)^2)
print(ridge.mse) #757.7074

#lasso
#cv.out=cv.glmnet(x.train,y.train,alpha=1)
#plot(cv.out)
#lasso.fit=glmnet(x.train,y.train,lambda=cv.out$lambda.min,alpha=1)
#lasso.pred=predict(lasso.fit,s=cv.out$lambda.min,newx = x.tst)
#lasso.mse=mean((y.tst-lasso.pred)^2)
#print(lasso.mse) #898.4562
svmfit <- ksvm(y ~ ., data = data_pca, scaled = TRUE, type = "nu-svr", kernel = "vanilladot", C = 1, epsilon = 0.001)
pred <- predict(svmfit, newdata = data_test_pca) 
MSE_SVM <- mean((pred - data_test_pca$y)^2) 
data_pca

library(gam)
fit<-gam(y~s(PC1)+s(PC2)+s(PC3)+s(PC4)+s(PC5)+s(PC6)+s(PC7)+s(PC8)+s(PC9)+s(PC10)+s(PC11)+s(PC12)+s(PC13)+s(PC14)+s(PC15)+s(PC16)+s(PC17)+s(PC18)+s(PC19)+s(PC20)+s(PC21)+s(PC22)+s(PC23)+s(PC24)+s(PC25)+s(PC26)+s(PC27)+s(PC28)+s(PC29)+s(PC30)+s(PC31)+s(PC32)+s(PC33),data = data_pca)
pred <- predict(fit, newdata = data_test_pca) 
MSE_SVM <- mean((pred - data_test_pca$y)^2) 
data_pca