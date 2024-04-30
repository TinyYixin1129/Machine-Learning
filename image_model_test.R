setwd("E:/codes/r/tp12")
load("32_image_vgg19_ep40_nogen.RData")
print(history)
plot(history)



lista=c(
  "./images_test/car/car_train_21.jpg",
  "./images_test/car/car_train_22.jpg",
  "./images_test/car/car_train_23.jpg",
  "./images_test/car/car_train_24.jpg",
  "./images_test/car/car_train_25.jpg",
  "./images_test/dog/dog_train_21.jpg",
  "./images_test/dog/dog_train_22.jpg",
  "./images_test/dog/dog_train_23.jpg",
  "./images_test/dog/dog_train_24.jpg",
  "./images_test/dog/dog_train_25.jpg",
  "./images_test/fruit/fruit_train_21.jpg",
  "./images_test/fruit/fruit_train_22.jpg",
  "./images_test/fruit/fruit_train_23.jpg",
  "./images_test/fruit/fruit_train_24.jpg",
  "./images_test/fruit/fruit_train_25.jpg"
)
listb=c(
  "./images_validation/car/car_train_601.jpg",
  "./images_validation/car/car_train_602.jpg",
  "./images_validation/car/car_train_603.jpg",
  "./images_validation/car/car_train_604.jpg",
  "./images_validation/car/car_train_605.jpg",
  "./images_validation/dog/dog_train_431.jpg",
  "./images_validation/dog/dog_train_432.jpg",
  "./images_validation/dog/dog_train_433.jpg",
  "./images_validation/dog/dog_train_434.jpg",
  "./images_validation/dog/dog_train_435.jpg",
  "./images_validation/fruit/fruit_train_601.jpg",
  "./images_validation/fruit/fruit_train_602.jpg",
  "./images_validation/fruit/fruit_train_603.jpg",
  "./images_validation/fruit/fruit_train_604.jpg",
  "./images_validation/fruit/fruit_train_605.jpg"
)
listc=c(
  "./images_train/car/car_train_301.jpg",
  "./images_train/car/car_train_302.jpg",
  "./images_train/car/car_train_303.jpg",
  "./images_train/car/car_train_304.jpg",
  "./images_train/car/car_train_305.jpg",
  "./images_train/dog/dog_train_301.jpg",
  "./images_train/dog/dog_train_302.jpg",
  "./images_train/dog/dog_train_303.jpg",
  "./images_train/dog/dog_train_304.jpg",
  "./images_train/dog/dog_train_305.jpg",
  "./images_train/fruit/fruit_train_301.jpg",
  "./images_train/fruit/fruit_train_302.jpg",
  "./images_train/fruit/fruit_train_303.jpg",
  "./images_train/fruit/fruit_train_304.jpg",
  "./images_train/fruit/fruit_train_305.jpg"
)

listd=c(
  "./image_internet/1.jpg",
  "./image_internet/2.jpg",
  "./image_internet/3.jpg",
  "./image_internet/4.jpg",
  "./image_internet/5.jpg",
  "./image_internet/6.jpg",
  "./image_internet/7.jpg",
  "./image_internet/8.jpg",
  "./image_internet/9.jpg",
  "./image_internet/10.jpg",
  "./image_internet/11.jpg",
  "./image_internet/12.jpg",
  "./image_internet/13.jpg",
  "./image_internet/14.jpg",
  "./image_internet/15.jpg"
)

#summary(model)
#summary(model.image)

model.image=loaded_model

prediction_image <- function(list) {
  class_name=c("car","dog","fruit")
  n<-length(list)
  predictions <- character(n)
  for (i in 1:n) {
    image <- image_load(list[i], target_size = c(32, 32))
    image <- image_to_array(image)
    image <- array_reshape(image, c(1, dim(image)))
    image <- image / 255
    
    predictions_class <- predict(model.image, image)
    colnames(predictions_class)=class_name
    predictions[i] <- colnames(predictions_class)[which.max(predictions_class)]
  }
  return(predictions)
}



prediction_image(lista)
prediction_image(listb)
prediction_image(listc)
prediction_image(listd)
