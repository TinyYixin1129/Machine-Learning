rm(list=ls())
setwd("E:/codes/r/tp12_3")
load("construction_env_test.Rdata")


construction=read.table("construction_train.txt")
y=prediction_construction(construction)

mean((y-construction$y)^2)





####
setwd("E:/codes/r/tp12")
#load("32_image_resnet50_ep70_nogen.RData") val_loss: 0.06557 val_accuracy: 0.9833
load("image_vgg16_ep100.RData")
plot(history)
print(history)


