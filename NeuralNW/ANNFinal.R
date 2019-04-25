#MultiLayer Neural Network  - XOR+AND Gate
#-----------------------------------------
library(keras)
library(tensorflow)
library("rgl")
library("magick")

rep_matrix <- function(m,rows){
  matrix( rep( t( m ) , rows/nrow(m) ) , ncol = ncol(m) , byrow = TRUE )
}

addnoise <- function(x, scale = 1) {
  #generate noise based on normal distribution mean=0 and sd=1
  noise <- matrix(rnorm(nrow(x) * (ncol(x) - 1)), nrow = nrow(x),
                  ncol = ncol(x) - 1) * scale
  cbind(x[, 1:3] + noise, x[, 4])
}

genxorand <- function(n) {
  # Eight data points of XOR+AND
  xorand <-
    matrix(
      c(0,0,0,0,0,0,1,0,0,1,0,0,0,1,1,1,1,0,0,0,1,0,1,1,1,1,0,0,1,1,1,0),
      nrow = 8,
      ncol = 4,
      byrow = TRUE
    )
  # duplicate eight data points to n_train data points,add noise
  nMatrix <- rep_matrix(xorand, n)
  addnoise(nMatrix,.2)
}

data_train <- genxorand(1000)


x_train <- data_train[, 1:3]
y_train <- data_train[, 4]

x <- data_train[,1] 
y <- data_train[,2] 
z <- data_train[,3]
open3d()
plot3d(x, y, z,
       col=c("red","blue","green"), 
       size = 0.7, type='s')


model <- keras_model_sequential()

model %>% layer_dense(units = 10,
                      activation = 'sigmoid',
                      input_shape = c(3)) %>%
  layer_dense(units = 1,
              activation = 'sigmoid')

summary(model)


model %>% compile(
  optimizer = optimizer_sgd(lr = 0.01),
  loss = 'mean_squared_error',
  metrics = c('accuracy')
)

history <- model %>% fit(x_train,y_train, epochs = 100, batch_size = 1)

plot(history)

data_test <- genxorand(1000) 

x_test <- data_test[, 1:3]
y_test <- data_test[, 4]

#evaluate model on test data

model %>% predict_classes(x_test)

model %>% evaluate(x_test,y_test)


