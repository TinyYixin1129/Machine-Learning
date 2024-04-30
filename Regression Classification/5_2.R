setwd("E:/codes/r/data")
library(MASS)
library(leaps)
library(corrplot)
library(glmnet)
reg.data <- read.table("TP5_a23_reg_app.txt")
reg.cor <- cor(reg.data)
corrplot(reg.cor)

reg <- lm(y ~., data = reg.data)
summary(reg)

#dev.off()

reg.fit <-regsubsets(y ~ .,data=reg.data,method="forward",nvmax=100)
reg.subsets <- summary(reg.fit)
plot(reg.fit,scale="r2", main ="")
title(main = "Forward stepwise selection" )


n.reg = dim(reg.data)[1]
p.reg = dim(reg.data)[2]

reg.models.list <- c("lm", "ridge", "lasso") # Liste des modeles
reg.subset.error <- c()
reg.models.error <- matrix(0,nrow = 98,ncol = 3)
colnames(reg.models.error) <- reg.models.list
rownames(reg.models.error) <- c(2:99)

reg.error.model <- function(model, reg.n_folds, reg.trials, which.subsets)
{
  for (subset in which.subsets)
  {
    reg.subset.error[subset] <- 0
    for(i in c(1:reg.trials)) # On fait plusieurs essais
    {
      reg.folds <- sample(rep(1:reg.n_folds, length.out=n.reg))
      for(k in 1:reg.n_folds) # Sur chaque segment de la cross-validation
      {
        reg.train <- reg.data[reg.folds!=k,]
        reg.train.X <- reg.train[,-p.reg]
        reg.train.y <- reg.train$y
        reg.test <- reg.data[reg.folds==k,]
        reg.test.X <- reg.test[,-p.reg]
        reg.test.y <- reg.test$y
        
        reg.subset <- reg.train.X[,reg.subsets$which[subset,2:101]]
        reg.subset.test <- reg.test.X[,reg.subsets$which[subset,2:101]]
        
        # Apprentissage du modele
        reg.model <- NULL
        if(model == "lm")
        {
          reg.model <- lm(reg.train.y ~., data = reg.subset)
        }
        else
        {
          if(model == "ridge")
          {
            cv.out<-cv.glmnet(as.matrix(reg.subset),reg.train.y,alpha=0)
            reg.model<-glmnet(as.matrix(reg.subset),reg.train.y,lambda=cv.out$lambda.min,alpha=0)
          }
          
          else if(model == "lasso")
          {
            cv.out<-cv.glmnet(as.matrix(reg.subset),reg.train.y,alpha=1)
            reg.model<-glmnet(as.matrix(reg.subset),reg.train.y,lambda=cv.out$lambda.min,alpha=1)
          }
          else
          {
            reg.model <- NULL
          }
        }
        
        if(!is.null(reg.model))
        {
          if(model == "lm")
          {
            reg.predict <- predict(reg.model, newdata = reg.subset.test)
            reg.subset.error[subset] <- reg.subset.error[subset] + mean((reg.test.y - reg.predict)^2)
          }
          else
          {
            reg.predict <- predict(reg.model,s=cv.out$lambda.min,newx=as.matrix(reg.subset.test))
            reg.subset.error[subset] <- reg.subset.error[subset] + mean((reg.test.y - reg.predict)^2)
          }
        }
      }
    }
    reg.subset.error[subset] <- reg.subset.error[subset]/(reg.n_folds*reg.trials)
  }
  reg.subset.error
}


for (model in reg.models.list){
  reg.models.error[,model] <- as.vector(reg.error.model(model, 5,10, c(2:99))[2:99])
}

plot(reg.models.error[,"lm"], col = "red", type = "l", xlab = "nombre de prC)dicteurs", ylab = "espC)rance de l'erreur quadratique")
lines(reg.models.error[,"ridge"], col = "blue", type = "l")
lines(reg.models.error[,"lasso"], col = "green", type = "l")
legend(30,7000,legend = c("linC)aire", "ridge", "lasso"), col = c("red","blue","green"), lty = 1:1)

which.min(reg.models.error[,1])
which.min(reg.models.error[,2])
which.min(reg.models.error[,3])

#40-60
reg.subset.error <- c()
reg.models.error.zoom <- matrix(0,nrow = 21,ncol = 3)
colnames(reg.models.error.zoom) <- reg.models.list
rownames(reg.models.error.zoom) <- c(40:60)

for (model in reg.models.list)
{
  reg.models.error.zoom[,model] <- as.vector(reg.error.model(model, 5,100, c(40:60))[40:60])
}

plot(x=c(40:60),y=reg.models.error.zoom[,"lm"], col = "red", type = "b", xlab = "nombre de prC)dicteurs", ylab = "espC)rance de l'erreur quadratique")
lines(x=c(40:60),y=reg.models.error.zoom[,"ridge"], col = "blue", type = "b")
lines(x=c(40:60),y=reg.models.error.zoom[,"lasso"], col = "green", type = "b")
legend(23,1425,legend = c("linC)aire", "ridge", "lasso"), col = c("red","blue","green"), lty = 1:1)

which.min(reg.models.error.zoom[,1])
which.min(reg.models.error.zoom[,2])
which.min(reg.models.error.zoom[,3])
reg.models.error.zoom[11:12,]

subset_51=reg.subsets$which[51,]
included_variables_51 <- colnames(reg.data)[subset_51]
included_variables_51


plot(reg.fit,scale="adjr2", main = "") 
title(main = "AIC") #65
plot(reg.fit,scale="bic", main = "")
title(main = "BIC") #44



reg.trials <- 100
reg.n_folds <- 5
reg.models.subsets <- c("lm51", "lasso50","lmAIC", "lassoAIC", "lmBIC", "lassoBIC") # Liste des modeles
models.subsets.error <- matrix(0,nrow = 6, ncol = 1)
rownames(models.subsets.error) <- reg.models.subsets
colnames(models.subsets.error) <- c("espC)rance")

for(model in reg.models.subsets){ # Pour chaque modele
  #models.subsets.error[model,1] <- 0
  for(i in c(1:reg.trials)) # On fait plusieurs essais
  {
    reg.folds <- sample(rep(1:reg.n_folds, length.out=n.reg))
    for(k in 1:reg.n_folds) # Sur chaque segment de la cross-validation
    {
      reg.train <- reg.data[reg.folds!=k,]
      reg.train.X <- reg.train[,-p.reg]
      reg.train.y <- reg.train$y
      reg.test <- reg.data[reg.folds==k,]
      reg.test.X <- reg.test[,-p.reg]
      reg.test.y <- reg.test$y
      
      reg.X.51 <- reg.train.X[,reg.subsets$which[51, 2:101]]
      reg.test.51 <- reg.test.X[,reg.subsets$which[51, 2:101]]
      reg.X.50 <- reg.train.X[,reg.subsets$which[50, 2:101]]
      reg.test.50 <- reg.test.X[,reg.subsets$which[50, 2:101]]
      
      # Apprentissage du modele
      if(model == "lm51")
      {
        reg.model <- lm(reg.train.y~.,data=reg.X.51)
      }
      else if(model == "lasso50")
      {
        cv.out<-cv.glmnet(as.matrix(reg.X.50),reg.train.y,alpha=1)
        reg.model<-glmnet(as.matrix(reg.X.50),reg.train.y,lambda=cv.out$lambda.min,alpha=1)
      }
      else if(model == "lmAIC")
      {
        reg.model <- lm(y ~ X4+X6+X8+X10+X12+X13+X14+X15+X16+X18+X20+X21+X22+X23+X23+X25+X26+X27+X30+X31+X32+X34+X35+X37+X39+X41+X42+X43+X46+X47+X49+X50+X51+X52+X53+X55+X56+X57+X58+X59+X65+X66+X67+X68+X70+X71+X72+X73+X74+X76+X79+X80+X82+X83+X84+X86+X87+X90+X91+X92+X93+X94+X97+X98+X100, data = reg.train)
      }
      else if(model == "lassoAIC")
      {
        x=model.matrix(y~X4+X6+X8+X10+X12+X13+X14+X15+X16+X18+X20+X21+X22+X23+X23+X25+X26+X27+X30+X31+X32+X34+X35+X37+X39+X41+X42+X43+X46+X47+X49+X50+X51+X52+X53+X55+X56+X57+X58+X59+X65+X66+X67+X68+X70+X71+X72+X73+X74+X76+X79+X80+X82+X83+X84+X86+X87+X90+X91+X92+X93+X94+X97+X98+X100,reg.data)
        xapp <- x[reg.folds!=k,]
        xtst <- x[reg.folds == k,]
        cv.out<-cv.glmnet(xapp,reg.train.y,alpha=1)
        reg.model<-glmnet(xapp,reg.train.y,lambda=cv.out$lambda.min,alpha=1)
      }
      else if(model == "lmBIC")
      {
        reg.model <- lm(y ~ X4+X6+X8+X10+X12+X13+X14+X16+X18+X20+X21+X22+X24+X26+X30+X31+X35+X37+X43+X46+X47+X49+X50+X51+X52+X53+X55+X56+X57+X58+X59+X65+X66+X67+X70+X71+X74+X80+X82+X84+X92+X93+X94+X100 , data = reg.train)
      }
      else if(model == "lassoBIC")
      {
        x <- model.matrix(y ~ X4+X6+X8+X10+X12+X13+X14+X16+X18+X20+X21+X22+X24+X26+X30+X31+X35+X37+X43+X46+X47+X49+X50+X51+X52+X53+X55+X56+X57+X58+X59+X65+X66+X67+X70+X71+X74+X80+X82+X84+X92+X93+X94+X100, reg.data)
        xapp <- x[reg.folds!=k,]
        xtst <- x[reg.folds == k,]
        
        cv.out<-cv.glmnet(xapp,reg.train.y,alpha=1)
        
        reg.model<-glmnet(xapp,reg.train.y,lambda=cv.out$lambda.min,alpha=1)
        
      }
      else
      {
        reg.model <- NULL
      }
      
      if(!is.null(reg.model))
      {
        if(model == "lm51" || model == "lmAIC" || model == "lmBIC")
        {
          reg.predict <- predict(reg.model, newdata = reg.test)
          models.subsets.error[model,1] <- models.subsets.error[model,1] + mean((reg.test.y - reg.predict)^2)
        }
        else if (model == "lassoAIC" || model == "lassoBIC")
        {
          reg.predict <- predict(reg.model,s=cv.out$lambda.min,newx=xtst)
          models.subsets.error[model,1] <- models.subsets.error[model,1] + mean((reg.test.y - reg.predict)^2)
        }
        else if (model == "lasso50")
        {
          reg.predict <- predict(reg.model,s=cv.out$lambda.min,newx=as.matrix(reg.test.50))
          models.subsets.error[model,1] <- models.subsets.error[model,1] + mean((reg.test.y - reg.predict)^2)
        }
      }
    }
  }
  models.subsets.error[model,1] <- models.subsets.error[model,1]/(reg.n_folds*reg.trials)
}

plot(x = c(1:6), y = models.subsets.error, xlab = "modC(le", ylab = "espC)rance de l'erreur quadratique")
models.subsets.error

#LM51
model.fit=lm(reg.data$y~.,data=reg.data[,reg.subsets$which[51, 2:101]])

#pred111=predict(model.fit1,newdata = reg.data)
#mean((pred111-reg.data$y)^2)
#reg.data2=reg.data[,-101]
#reg.data2=reg.data2[,reg.subsets$which[50, 2:101]]
#pred222=predict(model.fit2,s=cv.out$lambda.min,newx = as.matrix(reg.data2))
#mean((pred222-reg.data$y)^2)
#newx=as.matrix(reg.test.50)


rres.model = model.fit$residuals
rstd.model = rstandard(model.fit)
rstu.model = rstudent(model.fit)

plot(reg.data$y,rstd.model, main ="")
title("RC)sidus standarisC)s en fonction de y")
plot(reg.data$y,rstu.model, main = "")
title("RC)sidus studentisC)s en fonction de y")

shapiro.test(rres.model)
#Shapiro-Wilk normality test
#data:  rres.model
#W = 0.98478, p-value = 4.312e-05

## Q-Q plots
qqnorm(rres.model, asp = 1)
qqline(rres.model, dist = qnorm)

qqnorm(rstd.model, asp = 1)
qqline(rstd.model, dist = qnorm)

qqnorm(rstu.model, asp = 1)
qqline(rstu.model, dist = qnorm)

#influence globale
plot(model.fit, which = 4, cook.levels = c(0, 0.1))
plot(model.fit, which = 5, cook.levels = c(0, 0.1))

