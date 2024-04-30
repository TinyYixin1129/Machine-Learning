# Regression
data.reg=read.table("TP5_a23_reg_app.txt")

library(MASS)
fit_1=lm(y~.,data=data.reg)
reg=stepAIC(fit_1,scope=y~.,direction="both",k=log(nrow(data.reg)))

# Function
regresseur=function(test_set){
  library(MASS)
  predict(reg,test_set)
}

save("reg","regresseur",file="test.RData")
load("test.RData")
#mean((regresseur(data.reg)-data.reg$y)^2)
