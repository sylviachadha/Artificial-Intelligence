library(keras)
library(tensorflow)
library(rsample)
library(broom)
library(tidyr)
library(dplyr)

getwd()
#mydata <- read.csv(file.choose())
mydata <- read.csv(file="iOS Device Data.csv")

selectdata1 = subset(mydata, activity==1)
data1 <- head(selectdata1,n=3000)

selectdata0 = subset(mydata, activity==0)
data2 <- head(selectdata0,n=3000)

selectdf <- bind_rows(data1,data2)
head(selectdf, n=10)

data_split <- initial_split(selectdf, prop = .75)
data_train <- training(data_split)
data_test  <- testing(data_split)

x_train <- as.matrix(data_train[, 1:6])
y_train <- data_train[, 7]

model <- keras_model_sequential()

model %>% layer_dense(units = 10,
                      activation = 'relu',
                      input_shape = c(6)) %>%
          layer_dense(units = 10,
                      activation = 'relu',
                      input_shape = c(10)) %>%
          layer_dense(units = 1,
                      activation = 'sigmoid')

summary(model)

model %>% compile(
   optimizer = optimizer_adam(lr = 0.01),
   loss = 'binary_crossentropy',
   metrics = c('accuracy')
)

history <- model %>% fit(x_train,y_train, epochs = 100, batch_size = 50)

x_test <- as.matrix(data_test[, 1:6])
y_test <- data_test[, 7]

#Evaluate model on test data

model %>% evaluate(x_test,y_test)


